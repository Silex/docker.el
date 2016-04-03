;;; docker-volumes.el --- Emacs interface to docker-volume

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
(require 'tle)

(require 'eieio)
(require 'magit-popup)

(defclass docker-volume ()
  ((driver       :initarg :driver       :initform nil)
   (name         :initarg :name         :initform nil)))

(defmethod docker-volume-to-tabulated-list ((this docker-volume))
  "Convert `docker-volume' to tabulated list."
  (list (oref this :name)
        `[,(oref this :driver)
          ,(oref this :name)]))

(defun make-docker-volume (driver name)
  "Helper to create a `eieio` docker volume object."
  (docker-volume name :driver driver :name name))

(defun docker-volume-parse (line)
  "Convert LINE from 'docker volume ls' to `docker-volume'."
  (apply #'make-docker-volume (s-split " \\{3,15\\}" line)))

(defun docker-volume-names ()
  "Return the list of volume names."
  (--map (oref it :name) (docker-get-volumes)))

(defun docker-read-volume-name (prompt)
  "Read a volume name."
  (completing-read prompt (docker-volume-names)))

(defun docker-volume-rm (name)
  "Destroy a volume."
  (interactive (list (docker-read-volume-name "Delete volume: ")))
  (docker "volume rm" name))

(defun docker-get-volumes (&optional quiet filters)
  "Get volumes as eieio objects."
  (let* ((data (docker-get-volumes-raw quiet filters))
         (lines (s-split "\n" data t))
         (lines (cdr lines)))
    (-map 'docker-volume-parse lines)))

(defun docker-get-volumes-raw (&optional quiet filters)
  "Equivalent of \"docker volume ls\"."
  (docker "volume ls" (when quiet "-q ") (when filters (s-join " --filter=" filters))))

(defun docker-volumes-selection ()
  "Get the volumes selection as a list of ids."
  (tle-selection-ids))

(defun docker-volumes-rm-selection ()
  "Run `docker-volume-rm' on the volumes selection."
  (interactive)
  (--each (docker-volumes-selection)
    (docker "volume rm" it))
  (tabulated-list-revert))

(docker-utils-define-popup docker-volumes-rm-popup
  "Popup for removing volumes."
  'docker-volumes-popups
  :man-page "docker-volume-rm"
  :actions  '((?D "Remove" docker-volumes-rm-selection)))

(defun docker-volumes-refresh ()
  "Refresh the volumes list."
  (setq tabulated-list-entries (-map 'docker-volume-to-tabulated-list (docker-get-volumes))))

(defvar docker-volumes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "D" 'docker-volumes-rm-popup)
    map)
  "Keymap for `docker-volumes-mode'.")

;;;###autoload
(defun docker-volumes ()
  "List docker volumes."
  (interactive)
  (pop-to-buffer "*docker-volumes*")
  (docker-volumes-mode)
  (docker-volumes-refresh)
  (tabulated-list-revert))

(define-derived-mode docker-volumes-mode tabulated-list-mode "Volumes Menu"
  "Major mode for handling a list of docker volumes."
  (setq tabulated-list-format [("Driver" 10 t)("Name" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Driver" nil))
  (add-hook 'tabulated-list-revert-hook 'docker-volumes-refresh nil t)
  (tabulated-list-init-header)
  (tle-mode))

(provide 'docker-volumes)

;;; docker-volumes.el ends here
