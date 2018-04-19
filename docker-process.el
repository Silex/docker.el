;;; docker-process.el --- Emacs interface to Docker  -*- lexical-binding: t -*-

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

(defcustom docker-run-as-root nil
  "Controls wether docker runs as root or not."
  :type 'boolean
  :group 'docker)

(defcustom docker-command "docker"
  "The command for \\[docker] package."
  :type 'string
  :group 'docker)

(defun docker (action &rest args)
  "Execute docker ACTION passing arguments ARGS."
  (let ((default-directory (if (and docker-run-as-root (not (file-remote-p default-directory))) "/sudo::" default-directory)))
    (let ((command (format "%s %s %s" docker-command action (s-join " " (-non-nil args)))))
      (message command)
      (shell-command-to-string command))))

(provide 'docker-process)

;;; docker-process.el ends here
