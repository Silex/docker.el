;;; docker-faces.el --- Emacs interface to docker-container  -*- lexical-binding: t -*-

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

(require 'docker-core)

(defgroup docker-faces nil
  "Docker faces."
  :group 'docker
  :group 'faces)

(defface docker-face-status-success
  '((t :foreground "Green"))
  "Face for successes."
  :group 'docker-faces)

(defface docker-face-status-warning
  '((t :foreground "Gold"))
  "Face for warnings."
  :group 'docker-faces)

(defface docker-face-status-error
  '((t :foreground "Red"))
  "Face for errors."
  :group 'docker-faces)

(provide 'docker-faces)

;;; docker-faces.el ends here
