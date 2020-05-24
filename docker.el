;;; docker.el --- Emacs interface to Docker  -*- lexical-binding: t -*-

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/docker.el
;; Keywords: filename, convenience
;; Version: 1.3.0
;; Package-Requires: ((dash "2.14.1") (docker-tramp "0.1") (emacs "24.5") (json-mode "1.7.0") (s "1.12.0") (tablist "0.70") (transient "0.2.0"))

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

;; This package allows you to manipulate docker images, containers & more from Emacs.

;;; Code:

(require 'docker-compose)
(require 'docker-container)
(require 'docker-image)
(require 'docker-machine)
(require 'docker-network)
(require 'docker-volume)

(defun docker-read-log-level (prompt &rest _args)
  "Read the docker log level using PROMPT."
  (completing-read prompt '(debug info warn error fatal)))

(defun docker-read-certificate (prompt &optional initial-input _history)
  "Wrapper around `read-file-name'."
  (read-file-name prompt nil nil t initial-input (apply-partially 'string-match ".*\\.pem")))

;;;###autoload (autoload 'docker "docker" nil t)
(transient-define-prefix docker ()
  "Transient for docker."
  :man-page "docker"
  ["Arguments"
   (5 "-H" "Host" "--host " read-string)
   (5 "-Tt" "TLS" "--tls")
   (5 "-Tv" "TLS verify remote" "--tlsverify")
   (5 "-Ta" "TLS CA" "--tlscacert" docker-read-certificate)
   (5 "-Tc" "TLS certificate" "--tlscert" docker-read-certificate)
   (5 "-Tk" "TLS key" "--tlskey" docker-read-certificate)
   (5 "-l" "Log level" "--log-level " docker-read-log-level)]
  ["Docker"
   ("c" "Containers" docker-containers)
   ("i" "Images"     docker-images)
   ("n" "Networks"   docker-networks)
   ("v" "Volumes"    docker-volumes)]
  ["Other"
   ("C" "Compose"    docker-compose)
   ("M" "Machines"   docker-machines)])

(provide 'docker)

;;; docker.el ends here
