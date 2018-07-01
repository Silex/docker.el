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

(require 'docker-process)
(require 'docker-utils)
(require 'magit-popup)
(require 'tablist)

(defun docker-volume-parse (line)
  "Convert a LINE from \"docker volume ls\" to a `tabulated-list-entries' entry."
  (let ((data (s-split " \\{3,15\\}" line t)))
    (list (nth 1 data) (apply #'vector data))))

(defun docker-volume-entries ()
  "Return the docker volumes data for `tabulated-list-entries'."
  (let* ((data (docker-run "volume" "ls"))
         (lines (cdr (s-split "\n" data t))))
    (-map #'docker-volume-parse lines)))

(defun docker-volume-refresh ()
  "Refresh the volumes list."
  (setq tabulated-list-entries (docker-volume-entries)))

(defun docker-volume-read-name ()
  "Read a volume name."
  (completing-read "Volume: " (-map #'car (docker-volume-entries))))

;;;###autoload
(defun docker-volume-rm (name)
  "Destroy the volume named NAME."
  (interactive (list (docker-volume-read-name)))
  (docker-run "volume rm" name))

(defun docker-volume-rm-selection ()
  "Run \"docker volume rm\" on the volumes selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run "volume rm" it))
  (tablist-revert))

(magit-define-popup docker-volume-rm-popup
  "Popup for removing volumes."
  'docker-volume-popups
  :man-page "docker-volume-rm"
  :actions  '((?D "Remove" docker-volume-rm-selection))
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-volume-help-popup
  "Help popup for docker volumes."
  :actions '("Docker volumes help"
             (?D "Remove"     docker-volume-rm-popup)
             "Switch to other parts"
             (?c "Containers" docker-containers)
             (?i "Images"     docker-images)
             (?m "Machines"   docker-machines)
             (?n "Networks"   docker-networks)))

(defvar docker-volume-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'docker-volume-help-popup)
    (define-key map "D" 'docker-volume-rm-popup)
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
  (setq tabulated-list-format [("Driver" 10 t)("Name" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Driver" nil))
  (add-hook 'tabulated-list-revert-hook 'docker-volume-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-volume)

;;; docker-volume.el ends here
