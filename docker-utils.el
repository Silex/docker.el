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
(require 'dash)
(require 'tramp)
(require 'tablist)
(require 'json-mode)
(require 'transient)

(require 'docker-core)

(defun docker-utils-get-marked-items-ids ()
  "Get the id part of `tablist-get-marked-items'."
  (-map #'car (tablist-get-marked-items)))

(defun docker-utils-ensure-items ()
  (when (null (docker-utils-get-marked-items-ids))
    (user-error "This action cannot be used in an empty list")))

(defmacro docker-utils-with-buffer (name &rest body)
  "Wrapper around `with-current-buffer'.
Execute BODY in a buffer named with the help of NAME."
  (declare (indent defun))
  `(with-current-buffer (docker-generate-new-buffer ,name)
     (setq buffer-read-only nil)
     (erase-buffer)
     ,@body
     (setq buffer-read-only t)
     (goto-char (point-min))
     (pop-to-buffer (current-buffer))))

(defmacro docker-utils-transient-define-prefix (name arglist &rest args)
  `(transient-define-prefix ,name ,arglist
     ,@args
     (interactive)
     (docker-utils-ensure-items)
     (transient-setup ',name)))

(defun docker-utils-get-transient-action ()
  (s-replace "-" " " (s-chop-prefix "docker-" (symbol-name transient-current-command))))

(defun docker-utils-generic-actions-heading ()
  (let ((items (s-join ", " (docker-utils-get-marked-items-ids))))
    (format "%s %s"
            (propertize "Actions on" 'face 'transient-heading)
            (propertize items        'face 'transient-value))))

(defun docker-utils-generic-action (action args)
  (interactive (list (docker-utils-get-transient-action)
                     (transient-args transient-current-command)))
  (--each (docker-utils-get-marked-items-ids)
    (docker-run-docker action args it))
  (tablist-revert))

(defun docker-utils-generic-action-async (action args)
  (interactive (list (docker-utils-get-transient-action)
                     (transient-args transient-current-command)))
  (--each (docker-utils-get-marked-items-ids)
    (docker-run-docker-async action args it))
  (tablist-revert))

(defun docker-utils-generic-action-with-buffer (action args)
  (interactive (list (docker-utils-get-transient-action)
                     (transient-args transient-current-command)))
  (--each (docker-utils-get-marked-items-ids)
    (docker-utils-with-buffer (format "%s %s" action it)
      (insert (docker-run-docker action args it))))
  (tablist-revert))

(defun docker-utils-generic-action-with-buffer:json (action args)
  (interactive (list (docker-utils-get-transient-action)
                     (transient-args transient-current-command)))
  (--each (docker-utils-get-marked-items-ids)
    (docker-utils-with-buffer (format "%s %s" action it)
      (insert (docker-run-docker action args it))
      (json-mode)))
  (tablist-revert))

(defun docker-utils-pop-to-buffer (name)
  "Like `pop-to-buffer', but suffix NAME with the host if on a remote host."
  (pop-to-buffer
   (if (file-remote-p default-directory)
       (with-parsed-tramp-file-name default-directory nil (concat name " - " host))
     name)))

(defun docker-utils-unit-multiplier (str)
  "Return the correct multiplier for STR."
  (expt 1024 (-elem-index (upcase str) '("B" "KB" "MB" "GB" "TB" "PB" "EB"))))

(defun docker-utils-human-size-to-bytes (str)
  "Parse STR and return size in bytes."
  (let* ((parts (s-match "^\\([0-9\\.]+\\)\\([A-Z]+\\)?$" str))
         (value (string-to-number (-second-item parts)))
         (multiplier (docker-utils-unit-multiplier (-third-item parts))))
    (* value multiplier)))

(defun docker-utils-inspect ()
  "Docker Inspect the tablist entry under point."
  (interactive)
  (let ((entry-id (tabulated-list-get-id)))
    (docker-utils-with-buffer (format "inspect %s" entry-id)
      (insert (docker-run-docker "inspect" () entry-id))
      (js-mode)
      (view-mode))))

(defun docker-utils-human-size-predicate (a b)
  "Sort A and B by image size."
  (let* ((a-size (elt (cadr a) 4))
         (b-size (elt (cadr b) 4)))
    (< (docker-utils-human-size-to-bytes a-size) (docker-utils-human-size-to-bytes b-size))))

(defun docker-utils-columns-list-format (columns-spec)
  "Convert COLUMNS-SPEC (a list of plists) to 'tabulated-list-format', i.e. a vector of (name width bool)."
  (seq-into
   (--map (list (plist-get it :name) (plist-get it :width) (or (plist-get it :sort) t)) columns-spec)
   'vector))

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

LINE is expected to be a JSON formatted array, and COLUMN-SPECS is the relevant
defcustom (e.g. `docker-image-columns`) used to apply any custom format functions."
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

(provide 'docker-utils)

;;; docker-utils.el ends here
