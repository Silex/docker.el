;;; docker-volume.el --- Emacs interface to docker-volume  -*- lexical-binding: t -*-

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
(require 'json)
(require 'tablist)
(require 'transient)

(require 'docker-core)
(require 'docker-utils)

(defgroup docker-volume nil
  "Docker volume customization group."
  :group 'docker)

(defconst docker-volume-id-template
  "{{ json .Name }}"
  "This Go template extracts the volume id which will be passed to transient commands.")

(defconst docker-volume-default-columns
  '((:name "Driver" :width 10 :template "{{json .Driver}}" :sort nil :format nil)
    (:name "Name" :width 40 :template "{{json .Name}}" :sort nil :format nil))
  "Default columns for docker-volume.")

(defcustom docker-volume-default-sort-key '("Driver" . nil)
  "Sort key for docker volumes.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'docker-volume
  ;; TODO could generate column choices from docker-image-column-order
  :type '(cons (string :tag "Column Name")
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defcustom docker-volume-column-order docker-volume-default-columns
  "Column specification for docker volumes.

The order of entries defines the displayed column order.
'Template' is the Go template passed to docker-image-ls to generate the column data."
  :group 'docker-volume

  ;; add plist symbols
  :set (lambda (sym xs)
         (let ((res (--map (-interleave '(:name :width :template :sort :format) it)
                           xs)))
           (set sym res)))
  ;; removes plist symbols
  :get (lambda (sym)
         (--map
          (-map (-partial #'plist-get it) '(:name :width :template :sort :format))
          (symbol-value sym)))
  :type '(repeat (list :tag "Column"
                       (string :tag "Name")
                       (integer :tag "Width")
                       (string :tag "Template")
                       (sexp :tag "Sort function")
                       (sexp :tag "Format function"))))

;; TODO copied from docker-image
(defun docker-volume-parse (column-specs line)
  "Convert a LINE from \"docker volume ls\" to a `tabulated-list-entries' entry."
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

(defun docker-volume-entries ()
  "Return the docker volumes data for `tabulated-list-entries'."
  (let* ((fmt (docker-utils-make-format-string docker-volume-id-template docker-volume-column-order))
         (data (docker-run-docker "volume ls" (docker-volume-ls-arguments) (format "--format=\"%s\"" fmt)))
         (lines (s-split "\n" data t)))
    (-map (-partial #'docker-volume-parse docker-volume-column-order) lines)))

(defun docker-volume-refresh ()
  "Refresh the volumes list."
  (setq tabulated-list-entries (docker-volume-entries)))

(defun docker-volume-read-name ()
  "Read a volume name."
  (completing-read "Volume: " (-map #'car (docker-volume-entries))))

;;;###autoload
(defun docker-volume-dired (name)
  "Enter `dired' in the volume named NAME."
  (interactive (list (docker-volume-read-name)))
  (let ((path (docker-run-docker "inspect" "-f" "\"{{ .Mountpoint }}\"" name)))
    (dired (format "/sudo::%s" path))))

(defun docker-volume-dired-selection ()
  "Run `docker-volume-dired' on the volumes selection."
  (interactive)
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (docker-volume-dired it)))

(defun docker-volume-ls-arguments ()
  "Return the latest used arguments in the `docker-volume-ls' transient."
  (car (alist-get 'docker-volume-ls transient-history)))

(transient-define-prefix docker-volume-ls ()
  "Transient for listing volumes."
  :man-page "docker-volume-ls"
  ["Arguments"
   ("f" "Filter" "--filter " read-string)]
  ["Actions"
   ("l" "List" tablist-revert)])

(docker-utils-transient-define-prefix docker-volume-rm ()
  "Transient for removing volumes."
  :man-page "docker-volume-rm"
  [:description docker-utils-generic-actions-heading
   ("D" "Remove" docker-utils-generic-action)])

(transient-define-prefix docker-volume-help ()
  "Help transient for docker volumes."
  ["Docker volumes help"
   ("D" "Remove"     docker-volume-rm)
   ("d" "Dired"      docker-volume-dired-selection)
   ("I" "Inspect"    docker-utils-inspect)
   ("l" "List"       docker-volume-ls)])

(defvar docker-volume-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'docker-volume-help)
    (define-key map "D" 'docker-volume-rm)
    (define-key map "d" 'docker-volume-dired-selection)
    (define-key map "l" 'docker-volume-ls)
    (define-key map "I" 'docker-utils-inspect)
    map)
  "Keymap for `docker-volume-mode'.")

;;;###autoload
(defun docker-volumes ()
  "List docker volumes."
  (interactive)
  (docker-utils-pop-to-buffer "*docker-volumes*")
  (docker-volume-mode)
  (tablist-revert))

(define-derived-mode docker-volume-mode tabulated-list-mode "Volumes Menu"
  "Major mode for handling a list of docker volumes."
  (setq tabulated-list-format (docker-utils-column-order-list-format docker-volume-column-order))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key docker-volume-default-sort-key)
  (add-hook 'tabulated-list-revert-hook 'docker-volume-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-volume)

;;; docker-volume.el ends here
