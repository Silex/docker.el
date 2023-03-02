;;; docker-context.el --- Interface to docker-context  -*- lexical-binding: t -*-

;; Author: Pablo González Carrizo <unmonoqueteclea@gmail.com>

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
(require 'tablist)
(require 'transient)

(require 'docker-core)
(require 'docker-faces)
(require 'docker-utils)

(defgroup docker-context nil
  "Docker context customization group."
  :group 'docker)

(defconst docker-context-id-template
  "{{ json .Name }}"
  "This Go template extracts the context id which will be passed to transient commands.")

(defcustom docker-context-default-sort-key '("Name" . nil)
  "Sort key for docker contexts.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'docker-context
  :type '(cons (string :tag "Column Name"
                       :validate (lambda (widget)
                                   (unless (--any-p (equal (plist-get it :name) (widget-value widget)) docker-context-columns)
                                     (widget-put widget :error "Default Sort Key must match a column name")
                                     widget)))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defcustom docker-context-columns
  '((:name "Name" :width 40 :template "{{ json .Name }}" :sort nil :format nil)
    (:name "Description" :width 40 :template "{{ json .Description }}" :sort nil :format nil)
    (:name "Endpoint" :width 40 :template "{{ json .DockerEndpoint }}" :sort nil :format nil))
  "Column specification for docker contexts.

The order of entries defines the displayed column order.
'Template' is the Go template passed to `docker-context-ls' to create the column
data.   It should return a string delimited with double quotes.
'Sort function' is a binary predicate that should return true when the first
argument should be sorted before the second.
'Format function' is a function from string to string that transforms the
displayed values in the column."
  :group 'docker-context
  :set 'docker-utils-columns-setter
  :get 'docker-utils-columns-getter
  :type '(repeat (list :tag "Column"
                       (string :tag "Name")
                       (integer :tag "Width")
                       (string :tag "Template")
                       (sexp :tag "Sort function")
                       (sexp :tag "Format function"))))

(aio-defun docker-context-inspect ()
  "Run \"`docker-command' context inspect\" on the selected items."
  (interactive)
  (docker-inspect "context"))

(aio-defun docker-context-entries (&rest args)
  "Return the docker contexts data for `tabulated-list-entries'."
  (let* ((fmt (docker-utils-make-format-string docker-context-id-template docker-context-columns))
         (data (aio-await (docker-run-docker-async "context" "ls" args (format "--format=\"%s\"" fmt))))
         (lines (s-split "\n" data t)))
    (-map (-partial #'docker-utils-parse docker-context-columns) lines)))

(aio-defun docker-context-entries-propertized (&rest args)
  "Return the propertized docker contexts data for `tabulated-list-entries'."
  (let ((entries (aio-await (docker-context-entries args)))
        (active (car (s-split "\n" (aio-await (docker-run-docker-async "context" "show")) t))))
    (--map-when (string= active (car it)) (docker-context-entry-set-active it) entries)))

(defun docker-context-entry-set-active (entry)
  "Mark ENTRY (output of `docker-context-entries') as active.

The result is the tabulated list id for an entry is propertized with
'docker-context-active and the entry is fontified with 'docker-face-active."
  (list (propertize (car entry) 'docker-context-active t)
        (apply #'vector (--map (propertize it 'font-lock-face 'docker-face-active) (cadr entry)))))


(aio-defun docker-context-update-status-async ()
  "Write the status to `docker-status-strings'."
  (plist-put docker-status-strings :contexts "Contexts")
  (when docker-show-status
    (let* ((entries (aio-await (docker-context-entries-propertized (docker-context-ls-arguments)))))
      (plist-put docker-status-strings
                 :contexts
                 (format "Context (%s total)" (number-to-string (length entries))))
      (transient--redisplay))))

(add-hook 'docker-open-hook #'docker-context-update-status-async)

(aio-defun docker-context-refresh ()
  "Refresh the contexts list."
  (docker-utils-refresh-entries
   (docker-context-entries-propertized (docker-context-ls-arguments))))

(docker-utils-define-transient-arguments docker-context-ls)

(docker-utils-transient-define-prefix docker-context-rm ()
  "Transient for removing contexts."
  :man-page "docker-context-rm"
  [:description docker-generic-action-description
		("D" "Remove" docker-generic-action-multiple-ids)])

(docker-utils-transient-define-prefix docker-context-use ()
  "Transient for using contexts."
  :man-page "docker-context-use"
  [:description docker-generic-action-description
		("X" "Use" docker-generic-action)])

(transient-define-prefix docker-context-help ()
  "Help transient for docker contexts."
  ["Docker contexts help"
   ("D" "Remove"  docker-context-rm)
   ("I" "Inspect" docker-context-inspect)
   ("X" "Use"     docker-context-use)])

(defvar docker-context-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'docker-context-help)
    (define-key map "D" 'docker-context-rm)
    (define-key map "I" 'docker-context-inspect)
    (define-key map "X" 'docker-context-use)
    map)
  "Keymap for `docker-context-mode'.")

;;;###autoload (autoload 'docker-contexts "docker-context" nil t)
(defun docker-contexts ()
  "List docker contexts."
  (interactive)
  (docker-utils-pop-to-buffer "*docker-contexts*")
  (docker-context-mode)
  (tablist-revert))

(define-derived-mode docker-context-mode tabulated-list-mode "Contexts Menu"
  "Major mode for handling a list of docker contexts."
  (setq tabulated-list-format (docker-utils-columns-list-format docker-context-columns))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key docker-context-default-sort-key)
  (add-hook 'tabulated-list-revert-hook 'docker-context-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-context)

;;; docker-context.el ends here
