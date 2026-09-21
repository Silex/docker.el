;;; docker-utils.el --- Random utilities  -*- lexical-binding: t -*-

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:
(require 's)
(require 'aio)
(require 'dash)
(require 'json)
(require 'tramp)
(require 'tablist)
(require 'transient)
(require 'docker-group)

(defvar docker-utils-history nil
  "History list bound while reading with `docker-utils-with-history'.")

(defun docker-utils-with-history (key reader)
  "Call READER with a history variable holding the `transient-history' entry KEY.
READER receives the symbol to pass as HIST; the updated list is stored back
under KEY, so it is saved with the other transient histories."
  (let ((docker-utils-history (alist-get key transient-history)))
    (prog1 (funcall reader 'docker-utils-history)
      (setf (alist-get key transient-history) docker-utils-history))))

(defun docker-utils-read-string (prompt key)
  "Read a string with PROMPT using the history KEY."
  (docker-utils-with-history key (lambda (history) (read-string prompt nil history))))

(defun docker-utils-completing-read (prompt collection key)
  "Read a string with PROMPT, completing from COLLECTION, using the history KEY."
  (docker-utils-with-history key (lambda (history) (completing-read prompt collection nil nil nil history))))

(defconst docker-option-separator-regexp "[ \t]*|[ \t]*"
  "Regexp separating the values of a repeatable `docker-option'.")

(defclass docker-option (transient-option)
  ((always-read :initform t))
  "Command-line option whose current value is edited in place.
A repeatable option (`:multi-value repeat') reads all its values in one prompt,
separated by \"|\".")

(cl-defmethod transient-prompt ((obj docker-option))
  "Prompt for OBJ based on its description."
  (let ((description (oref obj description)))
    (if (or (oref obj prompt) (not (stringp description)))
        (cl-call-next-method)
      (format (if (eq (oref obj multi-value) 'repeat) "%s (separate with |): " "%s: ")
              description))))

(cl-defmethod transient-infix-read ((obj docker-option))
  "Read the value of OBJ, starting from its current value.
When OBJ is unset and `transient-read-with-initial-input' is non-nil, start
from the last history entry instead.  Empty input unsets the option."
  (let* ((enable-recursive-minibuffers t)
         (repeat (eq (oref obj multi-value) 'repeat))
         (key (or (oref obj history-key) (oref obj command)))
         (value (oref obj value))
         (initial-input (cond ((and value repeat) (string-join value "|"))
                              (value)
                              (transient-read-with-initial-input (car (alist-get key transient-history)))))
         (reader (or (oref obj reader) #'docker-option-read-string))
         (input (docker-utils-with-history key
                                           (lambda (history)
                                             (funcall reader (transient-prompt obj) initial-input history)))))
    (cond ((not (stringp input)) input)
          (repeat (split-string input docker-option-separator-regexp t))
          ((not (string-empty-p input)) input))))

(defun docker-option-read-string (prompt initial-input history)
  "Read a string with PROMPT, INITIAL-INPUT and HISTORY."
  (read-string prompt initial-input history))

(cl-defmethod transient-format-value ((obj docker-option))
  "Format the value of OBJ, without the argument's trailing space when unset."
  (let ((formatted (cl-call-next-method))
        (argument (oref obj argument)))
    (if (or (oref obj value) (not (string-suffix-p " " argument)))
        formatted
      (concat (substring formatted 0 (1- (length argument)))
              (substring formatted (length argument))))))

(transient-define-infix docker-option-env ()
  :description "Env KEY=VAL"
  :class 'docker-option
  :argument "-e "
  :multi-value 'repeat
  :history-key 'docker-container-environment)

(transient-define-infix docker-option-user ()
  :description "User"
  :class 'docker-option
  :argument "-u "
  :history-key 'docker-container-user)

(transient-define-infix docker-option-workdir ()
  :description "Workdir"
  :class 'docker-option
  :argument "-w "
  :history-key 'docker-container-workdir)

(transient-define-infix docker-option-entrypoint ()
  :description "Entrypoint"
  :class 'docker-option
  :argument "--entrypoint "
  :history-key 'docker-container-entrypoint)

(transient-define-infix docker-option-name ()
  :description "Name"
  :class 'docker-option
  :argument "--name "
  :history-key 'docker-container-name)

(transient-define-infix docker-option-host ()
  :description "Host"
  :class 'docker-option
  :argument "--host "
  :history-key 'docker-host)

(transient-define-infix docker-option-tail ()
  :description "Tail"
  :class 'docker-option
  :argument "--tail "
  :history-key 'docker-logs-tail)

(transient-define-infix docker-option-timeout ()
  :description "Timeout"
  :class 'docker-option
  :argument "-t "
  :reader #'transient-read-number-N0)

(defun docker-utils-get-marked-items-ids ()
  "Get the id part of `tablist-get-marked-items'."
  (-map #'car (tablist-get-marked-items)))

(defun docker-utils-compute-args (default custom)
  "Helper function for merging DEFAULT and CUSTOM args."
  (let* ((objs (tablist-get-marked-items))
         (name (caar objs))
         (matched-args (when name
                         (--first (string-match (car it) name)
                                  custom))))
    (if matched-args
        (cadr matched-args)
      default)))

(defun docker-utils-ensure-items ()
  "Ensure at least one item is selected."
  (when (null (docker-utils-get-marked-items-ids))
    (user-error "This action cannot be used in an empty list")))

(defun docker-utils-generate-new-buffer-name (program &rest args)
  "Wrapper around `generate-new-buffer-name' using PROGRAM and ARGS."
  (generate-new-buffer-name (format "* %s %s *" program (s-join " " args))))

(defun docker-utils-generate-new-buffer (program &rest args)
  "Wrapper around `generate-new-buffer' using PROGRAM and ARGS."
  (generate-new-buffer (apply #'docker-utils-generate-new-buffer-name program args)))

(defmacro docker-utils-with-buffer (name &rest body)
  "Wrapper around `with-current-buffer'.
Execute BODY in a buffer named with the help of NAME."
  (declare (indent defun))
  `(with-current-buffer (docker-utils-generate-new-buffer "docker" ,name)
     (setq buffer-read-only nil)
     (erase-buffer)
     ,@body
     (setq buffer-read-only t)
     (goto-char (point-min))
     (pop-to-buffer (current-buffer))))

(defmacro docker-utils-transient-define-prefix (name arglist &rest args)
  "Wrapper around `transient-define-prefix' that requires a selection.

NAME, ARGLIST and ARGS are forwarded to it, and `docker-utils-ensure-items'
runs before the transient is set up."
  `(transient-define-prefix ,name ,arglist
     ,@args
     (interactive)
     (docker-utils-ensure-items)
     (transient-setup ',name)))

(defmacro docker-utils-define-transient-arguments (name)
  "Define NAME-arguments, returning the latest value of the NAME transient.

It falls back to the transient default value when the history is empty."
  `(defun ,(intern (format "%s-arguments" name)) ()
     ,(format "Return the latest used arguments in the `%s' transient." name)
     (let ((history (alist-get ',name transient-history))
           (default (transient-default-value (get ',name 'transient--prefix))))
       (if (equal 0 (length history))
           (car default)
         (car history)))))

(defmacro docker-utils-refresh-entries (promise)
  "Update the current buffer with the results of PROMISE."
  `(let ((buffer (current-buffer))
         (entries (aio-await ,promise)))
     (with-current-buffer buffer
       (setq tabulated-list-entries entries)
       (tabulated-list-print t))))

(defcustom docker-pop-to-buffer-action nil
  "Action `docker-utils-pop-to-buffer' passes to `pop-to-buffer'."
  :group 'docker
  :type 'sexp)

(defun docker-utils-pop-to-buffer (name)
  "Like `pop-to-buffer', but suffix NAME with the host if on a remote host."
  (pop-to-buffer
   (if (file-remote-p default-directory)
       (with-parsed-tramp-file-name default-directory nil (concat name " - " host))
     name)
   docker-pop-to-buffer-action))

(defun docker-utils-unit-multiplier (str)
  "Return the correct multiplier for STR."
  (let* ((unit (or str "B"))
         (idx (-elem-index (upcase unit) '("B" "KB" "MB" "GB" "TB" "PB" "EB"))))
    (expt 1024 (or idx 0))))

(defun docker-utils-human-size-to-bytes (str)
  "Parse STR and return size in bytes."
  (let* ((parts (s-match "^\\([0-9\\.]+\\)\\([A-Za-z]+\\)?$" str)))
    (unless parts
      (error "Unexpected size format: %s" str))
    (let* ((value (string-to-number (-second-item parts)))
           (multiplier (docker-utils-unit-multiplier (-third-item parts))))
      (* value multiplier))))

(defun docker-utils-human-size-predicate (a b)
  "Sort A and B by image size."
    (< (docker-utils-human-size-to-bytes a) (docker-utils-human-size-to-bytes b)))

(defun docker-utils-columns-list-format (columns-spec)
  "Convert COLUMNS-SPEC, a list of plists, to a `tabulated-list-format' vector.

Each element of the vector is (NAME WIDTH SORT-FN)."
  (apply 'vector
  (--map-indexed
   (-let* (((&plist :name name :width width :sort sort-fn-inner) it)
           (sort-fn (if sort-fn-inner
                        (let ((idx it-index)) ;; Rebind the closure var!
                          ;; Sort fn is called with (id [entries..])
                          ;; Extract the column value and pass to inner function
                          (-on sort-fn-inner (lambda (x) (elt (cadr x) idx))))
                      t)))
     (list name width sort-fn))
   columns-spec)))

(defun docker-utils-make-format-string (id-template column-spec)
  "Make the format string to pass to docker-ls commands.

ID-TEMPLATE is the Go template used to extract the property that
identifies the object (usually its id).
COLUMN-SPEC is the value of docker-X-columns."
  (let* ((templates (--map (plist-get it :template) column-spec))
         (delimited (string-join templates ",")))
    (format "[%s,%s]" id-template delimited)))

(defun docker-utils-parse (column-specs line)
  "Convert a LINE from \"docker ls\" to a `tabulated-list-entries' entry.

LINE is expected to be a JSON formatted array.  COLUMN-SPECS is the relevant
defcustom (e.g. `docker-image-columns') used to apply any custom format
functions."
  (condition-case nil
      (let* ((data (json-read-from-string line)))
        ;; apply format function, if any
        (--each-indexed
            column-specs
          (let ((fmt-fn (plist-get it :format))
                (data-index (+ it-index 1)))
            (when fmt-fn (aset data data-index (apply fmt-fn (list (aref data data-index)))))))

        (list (aref data 0) (seq-drop data 1)))
    (json-readtable-error
     (error "Could not read following string as json:\n%s" line))))

(defun docker-utils-columns-setter (sym new-value)
  "Convert NEW-VALUE into a list of plists, then assign to SYM.

If NEW-VALUE already looks like a list of plists, no conversion is performed and
 NEW-VALUE is assigned to SYM unchanged.  This is expected to be used as the
value of :set in a defcustom."
  (let ((is-plist (plist-member (car new-value) :name))
        (new-value-plist (--map
                          (-interleave '(:name :width :template :sort :format) it)
                          new-value)))
    (set sym (if is-plist new-value new-value-plist))))

(defun docker-utils-columns-getter (sym)
  "Convert the value of SYM for displaying in the customization menu.

Just strips the plist symbols and returns only values.
This has no effect on the actual value of the variable."
  (--map
   (-map (-partial #'plist-get it) '(:name :width :template :sort :format))
   (symbol-value sym)))

(defun docker-utils-package-p (package)
  "Check if PACKAGE is available."
  (or (featurep package)
      (ignore-errors (require package))))

(provide 'docker-utils)

;;; docker-utils.el ends here
