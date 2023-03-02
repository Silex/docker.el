;;; docker-faces.el --- Docker faces  -*- lexical-binding: t -*-

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
  "Docker faces customization group."
  :group 'docker
  :group 'faces)

(defface docker-face-status-up
  '((t :inherit success))
  "Face used when the status is up."
  :group 'docker-faces)

(defface docker-face-status-down
  '((t :inherit error))
  "Face used when the status is down."
  :group 'docker-faces)

(defface docker-face-status-other
  '((t :inherit warning))
  "Face used when the status is not up/down."
  :group 'docker-faces)

(defface docker-face-dangling
  '((t :inherit shadow))
  "Face for dangling items."
  :group 'docker-faces)

(defface docker-face-active
  '((t :inherit bold))
  "Face for active items."
  :group 'docker-faces)

(provide 'docker-faces)

;;; docker-faces.el ends here
