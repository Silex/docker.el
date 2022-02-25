;;; docker-network.el --- Interface to docker-network  -*- lexical-binding: t -*-

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

(defgroup docker-network nil
  "Docker network customization group."
  :group 'docker)

(defconst docker-network-id-template
  "{{ json .ID }}"
  "This Go template extracts the id which will be passed to transient commands.")

(defcustom docker-network-default-sort-key '("Name" . nil)
  "Sort key for docker networks.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'docker-network
  :type '(cons (string :tag "Column Name"
                       :validate (lambda (widget)
                                   (unless (--any-p (equal (plist-get it :name) (widget-value widget)) docker-network-columns)
                                     (widget-put widget :error "Default Sort Key must match a column name")
                                     widget)))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defcustom docker-network-columns
  '((:name "Network ID" :width 20 :template "{{ json .ID }}" :sort nil :format nil)
    (:name "Name" :width 50 :template "{{ json .Name }}" :sort nil :format nil)
    (:name "Driver" :width 10 :template "{{ json .Driver }}" :sort nil :format nil)
    (:name "Scope" :width 10 :template "{{ json .Scope }}" :sort nil :format nil))
  "Column specification for docker networks.

The order of entries defines the displayed column order.
'Template' is the Go template passed to `docker-network-ls' to create the column
data.   It should return a string delimited with double quotes.
'Sort function' is a binary predicate that should return true when the first
argument should be sorted before the second.
'Format function' is a function from string to string that transforms the
displayed values in the column."
  :group 'docker-network
  :set 'docker-utils-columns-setter
  :get 'docker-utils-columns-getter
  :type '(repeat (list :tag "Column"
                       (string :tag "Name")
                       (integer :tag "Width")
                       (string :tag "Template")
                       (sexp :tag "Sort function")
                       (sexp :tag "Format function"))))

(defalias 'docker-network-inspect 'docker-inspect)

(aio-defun docker-network-entries (&rest args)
  "Return the docker networks data for `tabulated-list-entries'."
  (let* ((fmt (docker-utils-make-format-string docker-network-id-template docker-network-columns))
         (data (aio-await (docker-run-docker-async "network" "ls" args (format "--format=\"%s\"" fmt))))
         (lines (s-split "\n" data t)))
    (-map (-partial #'docker-utils-parse docker-network-columns) lines)))

(aio-defun docker-network-entries-propertized (&rest args)
  "Return the propertized docker networks data for `tabulated-list-entries'."
  (let ((entries (aio-await (docker-network-entries args)))
        (dangling (aio-await (docker-network-entries args "--filter dangling=true"))))
    (--map-when (-contains? dangling it) (docker-network-entry-set-dangling it) entries)))

(defun docker-network-dangling-p (entry-id)
  "Predicate for if ENTRY-ID is dangling.

For example (docker-network-dangling-p (tabulated-list-get-id)) is t when the entry under point is dangling."
  (get-text-property 0 'docker-network-dangling entry-id))

(defun docker-network-entry-set-dangling (entry)
  "Mark ENTRY (output of `docker-network-entries') as dangling.

The result is the tabulated list id for an entry is propertized with
'docker-network-dangling and the entry is fontified with 'docker-face-dangling."
  (list (propertize (car entry) 'docker-network-dangling t)
        (apply #'vector (--map (propertize it 'font-lock-face 'docker-face-dangling) (cadr entry)))))

(aio-defun docker-network-update-status-async ()
  "Write the status to `docker-status-strings'."
  (plist-put docker-status-strings :networks "Networks")
  (when docker-display-status-in-transient
    (let* ((entries (aio-await (docker-network-entries-propertized (docker-network-ls-arguments))))
           (dangling (--filter (docker-network-dangling-p (car it)) entries)))
      (plist-put docker-status-strings
                 :networks
                 (format "Networks (%s total, %s dangling)"
                         (number-to-string (length entries))
                         (propertize (number-to-string (length dangling)) 'face 'docker-face-dangling)))
      (transient--redisplay))))

(add-hook 'docker-open-hook #'docker-network-update-status-async)

(aio-defun docker-network-refresh ()
  "Refresh the networks list."
  (setq tabulated-list-entries (aio-await (docker-network-entries-propertized (docker-network-ls-arguments))))
  (tabulated-list-print t))

(defun docker-network-read-name ()
  "Read a network name."
  (completing-read "Network: " (-map #'car (docker-network-entries))))

(defun docker-network-mark-dangling ()
  "Mark only the dangling networks listed in *docker-networks*.

This clears any user marks first and respects any tablist filters
applied to the buffer."
  (interactive)
  (switch-to-buffer "*docker-networks*")
  (tablist-unmark-all-marks)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when (docker-network-dangling-p (tabulated-list-get-id))
        (tablist-put-mark))
      (forward-line))))

(docker-utils-define-transient-arguments docker-network-ls)

(transient-define-prefix docker-network-ls ()
  "Transient for listing networks."
  :man-page "docker-network-ls"
  ["Arguments"
   ("d" "Dangling" "--filter dangling=true")
   ("f" "Filter" "--filter " read-string)
   ("n" "Don't truncate" "--no-trunc")]
  ["Actions"
   ("l" "List" tablist-revert)])

(docker-utils-transient-define-prefix docker-network-rm ()
  "Transient for removing networks."
  :man-page "docker-network-rm"
  [:description docker-generic-action-description
   ("D" "Remove" docker-generic-action-multiple-ids)])

(transient-define-prefix docker-network-help ()
  "Help transient for docker networks."
  ["Docker networks help"
   ("D" "Remove"        docker-network-rm)
   ("I" "Inspect"       docker-network-inspect)
   ("d" "Mark Dangling" docker-network-mark-dangling)
   ("l" "List"          docker-network-ls)])

(defvar docker-network-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'docker-network-help)
    (define-key map "D" 'docker-network-rm)
    (define-key map "I" 'docker-network-inspect)
    (define-key map "d" 'docker-network-mark-dangling)
    (define-key map "l" 'docker-network-ls)
    map)
  "Keymap for `docker-network-mode'.")

;;;###autoload (autoload 'docker-networks "docker-network" nil t)
(defun docker-networks ()
  "List docker networks."
  (interactive)
  (docker-utils-pop-to-buffer "*docker-networks*")
  (docker-network-mode)
  (tablist-revert))

(define-derived-mode docker-network-mode tabulated-list-mode "Networks Menu"
  "Major mode for handling a list of docker networks."
  (setq tabulated-list-format (docker-utils-columns-list-format docker-network-columns))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key docker-network-default-sort-key)
  (add-hook 'tabulated-list-revert-hook 'docker-network-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-network)

;;; docker-network.el ends here
