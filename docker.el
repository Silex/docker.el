;;; docker.el --- Emacs interface to Docker  -*- lexical-binding: t -*-

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/docker.el
;; Keywords: filename, convenience
;; Version: 0.7.0
;; Package-Requires: ((emacs "24.5") (dash "2.14.1") (docker-tramp "0.1") (magit-popup "2.12.3") (s "1.12.0") (tablist "0.70") (json-mode "1.7.0"))

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

;; # Emacs interface to Docker!
;;
;; This package allows you to manipulate docker images, containers & more from Emacs.

;;; Code:

(require 'magit-popup)

(defgroup docker nil
  "Docker customization group."
  :group 'convenience)

(magit-define-popup docker
  "Popup console for dispatching other popups."
  :actions '("Docker"
             (?c "Containers" docker-containers)
             (?i "Images"     docker-images)
             (?m "Machines"   docker-machines)
             (?n "Networks"   docker-networks)
             (?v "Volumes"    docker-volumes)))

(provide 'docker)

;;; docker.el ends here
