;;; docker.el --- Emacs interface to Docker  -*- lexical-binding: t -*-

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/docker.el
;; Keywords: filename, convenience
;; Version: 1.3.0
;; Package-Requires: ((emacs "24.5") (dash "2.14.1") (docker-tramp "0.1") (magit-popup "2.12.4") (s "1.12.0") (tablist "0.70") (json-mode "1.7.0"))

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

(require 'docker-group)
(require 'docker-utils)
(require 'docker-container)
(require 'docker-image)
(require 'docker-machine)
(require 'docker-network)
(require 'docker-volume)

;;;###autoload (autoload 'docker "docker" nil t)
(magit-define-popup docker
  "Popup for docker."
  'docker
  :man-page "docker"
  :options  '((?H "Host" "--host "))
  :actions  `("Docker"
              (?c "Containers" ,(docker-utils-set-then-call 'docker-arguments 'docker-containers))
              (?i "Images"     ,(docker-utils-set-then-call 'docker-arguments 'docker-images))
              (?n "Networks"   ,(docker-utils-set-then-call 'docker-arguments 'docker-networks))
              (?v "Volumes"    ,(docker-utils-set-then-call 'docker-arguments 'docker-volumes))
              "Other"
              (?C "Compose"    docker-compose)
              (?M "Machines"   docker-machines)))

(provide 'docker)

;;; docker.el ends here
