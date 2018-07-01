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

(require 'docker-process)
(require 'docker-utils)
(require 'magit-popup)
(require 'tablist)

(defun docker-network-parse (line)
  "Convert a LINE from \"docker network ls\" to a `tabulated-list-entries' entry."
  (let ((data (s-split " \\{3,\\}" line t)))
    (list (nth 1 data) (apply #'vector data))))

(defun docker-network-entries ()
  "Return the docker networks data for `tabulated-list-entries'."
  (let* ((data (docker-run "network" "ls"))
         (lines (cdr (s-split "\n" data t))))
    (-map #'docker-network-parse lines)))

(defun docker-network-refresh ()
  "Refresh the networks list."
  (setq tabulated-list-entries (docker-network-entries)))

(defun docker-network-read-name ()
  "Read a network name."
  (completing-read "Network: " (-map #'car (docker-network-entries))))

;;;###autoload
(defun docker-network-rm (name)
  "Destroy the network named NAME."
  (interactive (list (docker-network-read-name)))
  (docker-run "network rm" name))

(defun docker-network-rm-selection ()
  "Run \"docker network rm\" on the selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run "network rm" it))
  (tablist-revert))

(magit-define-popup docker-network-rm-popup
  "Popup for removing networks."
  'docker-network-popups
  :man-page "docker-network-rm"
  :actions  '((?D "Remove" docker-network-rm-selection))
  :setup-function #'docker-utils-popup-setup)

(magit-define-popup docker-network-help-popup
  "Help popup for docker networks."
  :actions '("Docker networks help"
             (?D "Remove"     docker-network-rm-popup)
             "Switch to other parts"
             (?c "Containers" docker-containers)
             (?i "Images"     docker-images)
             (?m "Machines"   docker-machines)
             (?v "Volumes"    docker-volumes)))

(defvar docker-network-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'docker-network-help-popup)
    (define-key map "D" 'docker-network-rm-popup)
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
  (setq tabulated-list-format [("Network ID" 20 t)("Name" 50 t)("Driver" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (add-hook 'tabulated-list-revert-hook 'docker-network-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-network)

;;; docker-network.el ends here
