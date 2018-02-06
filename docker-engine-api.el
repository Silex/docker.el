;;; docker-engine-api.el --- operating system specific utilities

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;;         Yuki Inoue <inouetakahiroki@gmail.com>

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

(require 'request)

(defun docker-engine-api-get (path params parser)
  "Get PATH from Docker Engine API and parse with PARSER."
  (let (result)
    (pcase system-type
      ('windows-nt (request
                    (concat "http://localhost:2375/v1.26" path)
                    :type "GET"
                    :params params
                    :sync t
                    :timeout 5
                    :parser parser
                    :success (cl-function
                              (lambda (&key data &allow-other-keys)
                                (setq result data)
                                (let ((inhibit-message t))
                                  (message "Docker Engine API: %s" data))))))
      (_           (request
                    (concat "http:/v1.26" path)
                    :unix-socket "/var/run/docker.sock"
                    :type "GET"
                    :params params
                    :sync t
                    :timeout 5
                    :parser parser
                    :success (cl-function
                              (lambda (&key data &allow-other-keys)
                                (setq result data)
                                (let ((inhibit-message t))
                                  (message "Docker Engine API: %s" data)))))))
    result))

(provide 'docker-engine-api)

;;; docker-engine-api.el ends here
