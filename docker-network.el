;;; docker-network.el --- Emacs interface to docker-network  -*- lexical-binding: t -*-

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

(defgroup docker-network nil
  "Docker network customization group."
  :group 'docker)

(defcustom docker-network-default-sort-key '("Name" . nil)
  "Sort key for docker networks.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'docker-network
  :type '(cons (choice (const "Network ID")
                       (const "Name")
                       (const "Driver"))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defun docker-network-parse (line)
  "Convert a LINE from \"docker network ls\" to a `tabulated-list-entries' entry."
  (condition-case nil
      (let ((data (json-read-from-string line)))
        (list (aref data 1) data))
    (json-readtable-error
     (error "Could not read following string as json:\n%s" line))))

(defun docker-network-entries ()
  "Return the docker networks data for `tabulated-list-entries'."
  (let* ((fmt "[{{json .ID}},{{json .Name}},{{json .Driver}},{{json .Scope}}]")
         (data (docker-run-docker "network ls" (docker-network-ls-arguments) (format "--format=\"%s\"" fmt)))
         (lines (s-split "\n" data t)))
    (-map #'docker-network-parse lines)))

(defun docker-network-refresh ()
  "Refresh the networks list."
  (setq tabulated-list-entries (docker-network-entries)))

(defun docker-network-read-name ()
  "Read a network name."
  (completing-read "Network: " (-map #'car (docker-network-entries))))

(defun docker-network-ls-arguments ()
  "Return the latest used arguments in the `docker-network-ls' transient."
  (car (alist-get 'docker-network-ls transient-history)))

(transient-define-prefix docker-network-ls ()
  "Transient for listing networks."
  :man-page "docker-network-ls"
  ["Arguments"
   ("-f" "Filter" "--filter " read-string)
   ("-n" "Don't truncate" "--no-trunc")]
  ["Actions"
   ("l" "List" tablist-revert)])

(docker-utils-transient-define-prefix docker-network-rm ()
  "Transient for removing networks."
  :man-page "docker-network-rm"
  [:description docker-utils-generic-actions-heading
   ("D" "Remove" docker-utils-generic-action)])

(transient-define-prefix docker-network-help ()
  "Help transient for docker networks."
  ["Docker networks help"
   ("D" "Remove"     docker-network-rm)
   ("l" "List"       docker-network-ls)])

(defvar docker-network-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'docker-network-help)
    (define-key map "D" 'docker-network-rm)
    (define-key map "l" 'docker-network-ls)
    map)
  "Keymap for `docker-network-mode'.")

;;;###autoload
(defun docker-networks ()
  "List docker networks."
  (interactive)
  (docker-utils-pop-to-buffer "*docker-networks*")
  (docker-network-mode)
  (tablist-revert))

(define-derived-mode docker-network-mode tabulated-list-mode "Networks Menu"
  "Major mode for handling a list of docker networks."
  (setq tabulated-list-format [("Network ID" 20 t)("Name" 50 t)("Driver" 10 t)("Scope" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key docker-network-default-sort-key)
  (add-hook 'tabulated-list-revert-hook 'docker-network-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-network)

;;; docker-network.el ends here
