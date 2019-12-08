;;; docker-core.el --- Docker core  -*- lexical-binding: t -*-

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
(require 'transient)

(defgroup docker nil
  "Docker customization group."
  :group 'convenience)

(defcustom docker-command "docker"
  "The docker binary."
  :group 'docker
  :type 'string)

(defcustom docker-run-as-root nil
  "Run docker as root."
  :group 'docker
  :type 'boolean)

(defun docker-read-log-level (prompt &rest _args)
  "Read the docker log level using PROMPT."
  (completing-read prompt '(debug info warn error fatal)))

;;;###autoload (autoload 'docker "docker" nil t)
(define-transient-command docker ()
  "Transient for docker."
  :man-page "docker"
  ["Arguments"
   ("-l" "Log level" "--log-level " docker-read-log-level)
   ("-H" "Host" "--host " read-string)]
  ["Docker"
   ("c" "Containers" docker-containers)
   ("i" "Images"     docker-images)
   ("n" "Networks"   docker-networks)
   ("v" "Volumes"    docker-volumes)]
  ["Other"
   ("C" "Compose"    docker-compose)
   ("M" "Machines"   docker-machines)])

(defun docker-arguments ()
  "Return the latest used arguments in the `docker' transient."
  (car (alist-get 'docker transient-history)))

(defun docker-run-docker (action &rest args)
  "Execute \"`docker-command' ACTION ARGS\"."
  (docker-utils-with-sudo
    (let* ((command (s-join " " (-remove 's-blank? (-flatten (list docker-command (docker-arguments) action args))))))
      (message command)
      (docker-utils-shell-command-to-string command))))

(provide 'docker-core)

;;; docker-core.el ends here
