;;; docker-volume.el --- Interface to docker-volume  -*- lexical-binding: t -*-

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
(require 'tablist)
(require 'transient)

(require 'docker-core)
(require 'docker-faces)
(require 'docker-utils)

(defgroup docker-volume nil
  "Docker volume customization group."
  :group 'docker)

(defconst docker-volume-id-template
  "{{ json .Name }}"
  "This Go template extracts the volume id which will be passed to transient commands.")

(defcustom docker-volume-default-sort-key '("Driver" . nil)
  "Sort key for docker volumes.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'docker-volume
  :type '(cons (string :tag "Column Name"
                       :validate (lambda (widget)
                                   (unless (--any-p (equal (plist-get it :name) (widget-value widget)) docker-volume-columns)
                                     (widget-put widget :error "Default Sort Key must match a column name")
                                     widget)))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defcustom docker-volume-columns
  '((:name "Driver" :width 10 :template "{{ json .Driver }}" :sort nil :format nil)
    (:name "Name" :width 40 :template "{{ json .Name }}" :sort nil :format nil))
  "Column specification for docker volumes.

The order of entries defines the displayed column order.
'Template' is the Go template passed to `docker-volume-ls' to create the column
data.   It should return a string delimited with double quotes.
'Sort function' is a binary predicate that should return true when the first
argument should be sorted before the second.
'Format function' is a function from string to string that transforms the
displayed values in the column."
  :group 'docker-volume
  :set 'docker-utils-columns-setter
  :get 'docker-utils-columns-getter
  :type '(repeat (list :tag "Column"
                       (string :tag "Name")
                       (integer :tag "Width")
                       (string :tag "Template")
                       (sexp :tag "Sort function")
                       (sexp :tag "Format function"))))

(defalias 'docker-volume-inspect 'docker-inspect)

(aio-defun docker-volume-entries (&rest args)
  "Return the docker volumes data for `tabulated-list-entries'."
  (let* ((fmt (docker-utils-make-format-string docker-volume-id-template docker-volume-columns))
         (data (aio-await (docker-run-docker-async "volume" "ls" args (format "--format=\"%s\"" fmt))))
         (lines (s-split "\n" data t)))
    (-map (-partial #'docker-utils-parse docker-volume-columns) lines)))

(aio-defun docker-volume-entries-propertized (&rest args)
  "Return the propertized docker volumes data for `tabulated-list-entries'."
  (let ((entries (aio-await (docker-volume-entries args)))
        (dangling (aio-await (docker-volume-entries args "--filter dangling=true"))))
    (--map-when (-contains? dangling it) (docker-volume-entry-set-dangling it) entries)))

(defun docker-volume-dangling-p (entry-id)
  "Predicate for if ENTRY-ID is dangling.

For example (docker-volume-dangling-p (tabulated-list-get-id)) is t when the entry under point is dangling."
  (get-text-property 0 'docker-volume-dangling entry-id))

(defun docker-volume-entry-set-dangling (entry)
  "Mark ENTRY (output of `docker-volume-entries') as dangling.

The result is the tabulated list id for an entry is propertized with
'docker-volume-dangling and the entry is fontified with 'docker-face-dangling."
  (list (propertize (car entry) 'docker-volume-dangling t)
        (apply #'vector (--map (propertize it 'font-lock-face 'docker-face-dangling) (cadr entry)))))

(aio-defun docker-volume-update-status-async ()
  "Write the status to `docker-status-strings'."
  (plist-put docker-status-strings :volumes "Volumes")
  (when docker-show-status
    (let* ((entries (aio-await (docker-volume-entries-propertized (docker-volume-ls-arguments))))
           (dangling (--filter (docker-volume-dangling-p (car it)) entries)))
      (plist-put docker-status-strings
                 :volumes
                 (format "Volumes (%s total, %s dangling)"
                         (number-to-string (length entries))
                         (propertize (number-to-string (length dangling)) 'face 'docker-face-dangling)))
      (transient--redisplay))))

(add-hook 'docker-open-hook #'docker-volume-update-status-async)

(aio-defun docker-volume-refresh ()
  "Refresh the volumes list."
  (docker-utils-refresh-entries
   (docker-volume-entries-propertized (docker-volume-ls-arguments))))

(defun docker-volume-read-name ()
  "Read a volume name."
  (completing-read "Volume: " (-map #'car (docker-volume-entries))))

;;;###autoload (autoload 'docker-volume-dired "docker-volume" nil t)
(aio-defun docker-volume-dired (name)
  "Enter `dired' in the volume named NAME."
  (interactive (list (docker-volume-read-name)))
  (let ((path (aio-await (docker-run-docker-async "inspect" "-f" "\"{{ .Mountpoint }}\"" name))))
    (dired (format "/sudo::%s" path))))

(defun docker-volume-dired-selection ()
  "Run `docker-volume-dired' on the volumes selection."
  (interactive)
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (docker-volume-dired it)))

(defun docker-volume-mark-dangling ()
  "Mark only the dangling volumes listed in *docker-volumes*.

This clears any user marks first and respects any tablist filters
applied to the buffer."
  (interactive)
  (switch-to-buffer "*docker-volumes*")
  (tablist-unmark-all-marks)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (docker-volume-dangling-p (tabulated-list-get-id))
        (tablist-put-mark))
      (forward-line))))

(docker-utils-define-transient-arguments docker-volume-ls)

(transient-define-prefix docker-volume-ls ()
  "Transient for listing volumes."
  :man-page "docker-volume-ls"
  ["Arguments"
   ("d" "Dangling" "--filter dangling=true")
   ("f" "Filter" "--filter " read-string)]
  ["Actions"
   ("l" "List" tablist-revert)])

(docker-utils-transient-define-prefix docker-volume-rm ()
  "Transient for removing volumes."
  :man-page "docker-volume-rm"
  [:description docker-generic-action-description
   ("D" "Remove" docker-generic-action-multiple-ids)])

(transient-define-prefix docker-volume-help ()
  "Help transient for docker volumes."
  ["Docker volumes help"
   ("D" "Remove"        docker-volume-rm)
   ("I" "Inspect"       docker-volume-inspect)
   ("d" "Mark Dangling" docker-volume-mark-dangling)
   ("f" "Dired"         docker-volume-dired-selection)
   ("l" "List"          docker-volume-ls)])

(defvar docker-volume-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'docker-volume-help)
    (define-key map "D" 'docker-volume-rm)
    (define-key map "I" 'docker-volume-inspect)
    (define-key map "d" 'docker-volume-mark-dangling)
    (define-key map "f" 'docker-volume-dired-selection)
    (define-key map "l" 'docker-volume-ls)
    map)
  "Keymap for `docker-volume-mode'.")

;;;###autoload (autoload 'docker-volumes "docker-volume" nil t)
(defun docker-volumes ()
  "List docker volumes."
  (interactive)
  (docker-utils-pop-to-buffer "*docker-volumes*")
  (docker-volume-mode)
  (tablist-revert))

(define-derived-mode docker-volume-mode tabulated-list-mode "Volumes Menu"
  "Major mode for handling a list of docker volumes."
  (setq tabulated-list-format (docker-utils-columns-list-format docker-volume-columns))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key docker-volume-default-sort-key)
  (add-hook 'tabulated-list-revert-hook 'docker-volume-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-volume)

;;; docker-volume.el ends here
