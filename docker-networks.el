;;; docker-networks.el --- Emacs interface to docker-network

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
(require 'tle)

(require 'eieio)
(require 'magit-popup)

(defclass docker-network ()
  ((id           :initarg :id           :initform nil)
   (name         :initarg :name         :initform nil)
   (driver       :initarg :driver       :initform nil)))

(defmethod docker-network-to-tabulated-list ((this docker-network))
  "Convert `docker-network' to tabulated list."
  (list (oref this :id)
        `[,(oref this :id)
          ,(oref this :name)
          ,(oref this :driver)]))

(defun make-docker-network (id name driver)
  "Helper to create a `eieio` docker network object."
  (docker-network id :id id :name name :driver driver))

(defun docker-network-parse (line)
  "Convert LINE from 'docker network ls' to `docker-network'."
  (apply #'make-docker-network (s-split " \\{3,\\}" line t)))

(defun docker-network-names ()
  "Return the list of network names."
  (--map (oref it :name) (docker-get-networks)))

(defun docker-read-network-name (prompt)
  "Read a network name."
  (completing-read prompt (docker-network-names)))

(defun docker-network-rm (name)
  "Destroy a network."
  (interactive (list (docker-read-network-name "Delete network: ")))
  (docker "network rm" name))

(defun docker-get-networks (&optional quiet filters)
  "Get networks as eieio objects."
  (let* ((data (docker-get-networks-raw quiet filters))
         (lines (s-split "\n" data t))
         (lines (cdr lines)))
    (-map 'docker-network-parse lines)))

(defun docker-get-networks-raw (&optional quiet filters)
  "Equivalent of \"docker network ls\"."
  (docker "network ls" (when quiet "-q ") (when filters (s-join " --filter=" filters))))

(defun docker-networks-selection ()
  "Get the networks selection as a list of names."
  (let ((selection (tle-selection-ids)))
    (when (null selection)
      (error "No networks selected."))
    selection))

(defun docker-networks-rm-selection ()
  "Run `docker-network-rm' on the networks selection."
  (interactive)
  (--each (docker-networks-selection)
    (docker "network rm" it))
  (tabulated-list-revert))

(magit-define-popup docker-networks-rm-popup
  "Popup for removing networks."
  'docker-networks-popups
  :man-page "docker-network-rm"
  :actions  '((?D "Remove" docker-networks-rm-selection)))

(defun docker-networks-refresh ()
  (setq tabulated-list-entries (-map 'docker-network-to-tabulated-list (docker-get-networks))))

(defvar docker-networks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "D" 'docker-networks-rm-popup)
    map)
  "Keymap for `docker-networks-mode'.")

;;;###autoload
(defun docker-networks ()
  "List docker networks."
  (interactive)
  (pop-to-buffer "*docker-networks*")
  (docker-networks-mode)
  (docker-networks-refresh)
  (tabulated-list-revert))

(define-derived-mode docker-networks-mode tabulated-list-mode "Networks Menu"
  "Major mode for handling a list of docker networks."
  (setq tabulated-list-format [("Network ID" 20 t)("Name" 50 t)("Driver" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (add-hook 'tabulated-list-revert-hook 'docker-networks-refresh nil t)
  (tabulated-list-init-header)
  (tle-mode))

(provide 'docker-networks)

;;; docker-networks.el ends here
