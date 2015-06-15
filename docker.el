;;; docker.el --- Emacs interface to Docker

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/docker
;; Keywords: filename, convenience

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

(defun docker (action &rest args)
  "Execute docker ACTION passing arguments ARGS."
  (let ((command (format "docker %s %s" action (s-join " " (-non-nil args)))))
    (message command)
    (shell-command-to-string command)))

(defvar docker-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" 'docker-images)
    (define-key map "b" 'dockerfile-build-buffer)
    map)
  "Keymap for `docker-mode' after `C-c d' was pressed.")

(defvar docker-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c d") docker-command-map)
    map)
  "Keymap for `docker-mode'.")

;;;###autoload
(define-minor-mode docker-mode
  "Minor mode for `docker'."
  nil
  " docker"
  docker-mode-map)

;;;###autoload
(define-globalized-minor-mode docker-global-mode
  docker-mode
  docker-mode)

(provide 'docker)

;;; docker.el ends here
