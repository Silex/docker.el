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

(require 'aio)
(require 'transient)

(require 'docker-group)
(require 'docker-utils)
(require 'docker-process)

(defcustom docker-command "docker"
  "The docker binary."
  :group 'docker
  :type 'string)

(defvar docker-open-hook ()
  "Called when `docker' transient is opened.")

(defvar docker-status-strings '(:containers "" :images "" :networks "" :volumes "")
  "Plist of statuses for `docker' transient.")

(defcustom docker-display-status-in-transient t
  "Whether to display docker status in the main transient buffer."
  :group 'docker
  :type 'boolean)

(defun docker-run-docker-async (&rest args)
  "Execute \"`docker-command' ARGS\" and return a promise with the results."
  (apply #'docker-run-async docker-command (docker-arguments) args))

(defun docker-run-docker-async-with-buffer (&rest args)
  "Execute \"`docker-command' ARGS\" and display the results in a buffer."
  (apply #'docker-run-async-with-buffer docker-command (docker-arguments) args))

(defun docker-get-transient-action ()
  "Extract the action out of `current-transient-command'."
  (s-replace "-" " " (s-chop-prefix "docker-" (symbol-name transient-current-command))))

(defun docker-generic-action-description ()
  "Make the actions description for the selected items."
  (let ((items (s-join ", " (docker-utils-get-marked-items-ids))))
    (format "%s %s"
            (propertize "Actions on" 'face 'transient-heading)
            (propertize items        'face 'transient-value))))

(aio-defun docker-generic-action (action args)
  "Run \"`docker-command' ACTION ARGS\" on each of the selected items."
  (interactive (list (docker-get-transient-action)
                     (transient-args transient-current-command)))
  (let* ((ids (docker-utils-get-marked-items-ids))
         (promises (--map (docker-run-docker-async action args it) ids)))
    (aio-await (aio-all promises))
    (tablist-revert)))

(aio-defun docker-generic-action-multiple-ids (action args)
  "Same as `docker-generic-action', but group selection ids into a single command."
  (interactive (list (docker-get-transient-action)
                     (transient-args transient-current-command)))
  (aio-await (docker-run-docker-async action args (docker-utils-get-marked-items-ids)))
  (tablist-revert))

(defun docker-generic-action-with-buffer (action args)
  "Run \"`docker-command' ACTION ARGS\" and print output to a new buffer."
  (interactive (list (docker-get-transient-action)
                     (transient-args transient-current-command)))
  (--each (docker-utils-get-marked-items-ids)
    (docker-run-docker-async-with-buffer (s-split " " action) args it)))

(aio-defun docker-inspect ()
  "Run \"`docker-command' inspect\" on the selected items."
  (interactive)
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (let* ((id it)
           (data (aio-await (docker-run-docker-async "inspect" id))))
      (docker-utils-with-buffer (format "inspect %s" id)
        (insert data)
        (js-mode)
        (view-mode)))))

(defun docker-read-log-level (prompt &rest _args)
  "Read the docker log level using PROMPT."
  (completing-read prompt '(debug info warn error fatal)))

(defun docker-read-certificate (prompt &optional initial-input _history)
  "Wrapper around `read-file-name' forwarding PROMPT and INITIAL-INPUT."
  (read-file-name prompt nil nil t initial-input (apply-partially 'string-match ".*\\.pem")))

(docker-utils-define-transient-arguments docker)

;;;###autoload (autoload 'docker "docker" nil t)
(transient-define-prefix docker ()
  "Transient for docker."
  :man-page "docker"
  ["Arguments"
   (5 "H" "Host" "--host " read-string)
   (5 "Tt" "TLS" "--tls")
   (5 "Tv" "TLS verify remote" "--tlsverify")
   (5 "Ta" "TLS CA" "--tlscacert" docker-read-certificate)
   (5 "Tc" "TLS certificate" "--tlscert" docker-read-certificate)
   (5 "Tk" "TLS key" "--tlskey" docker-read-certificate)
   (5 "l" "Log level" "--log-level " docker-read-log-level)]
  ["Docker"
   ("c" (lambda ()(plist-get docker-status-strings :containers)) docker-containers)
   ("i" (lambda ()(plist-get docker-status-strings :images))     docker-images)
   ("n" (lambda ()(plist-get docker-status-strings :networks))   docker-networks)
   ("v" (lambda ()(plist-get docker-status-strings :volumes))    docker-volumes)]
  ["Other"
   ("C" "Compose" docker-compose)]
  (interactive)
  (run-hooks 'docker-open-hook)
  (transient-setup 'docker))

(provide 'docker-core)

;;; docker-core.el ends here
