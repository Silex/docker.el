;;; docker-compose.el --- Interface to docker-compose  -*- lexical-binding: t -*-

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
(require 'aio)
(require 'dash)
(require 'transient)

(require 'docker-group)
(require 'docker-utils)
(require 'docker-process)

(defgroup docker-compose nil
  "Docker compose customization group."
  :group 'docker)

(defcustom docker-compose-command "docker-compose"
  "The `docker-compose' binary."
  :group 'docker-compose
  :type 'string)

(defun docker-compose-run-docker-compose-async (action &rest args)
  "Execute \"`docker-compose-command' ACTION ARGS\" and return a promise with the results."
  (apply #'docker-run-async docker-compose-command (docker-compose-arguments) action args))

(defun docker-compose-run-docker-compose-async-with-buffer (action &rest args)
  "Execute \"`docker-compose-command' ACTION ARGS\" and display output in a new buffer."
  (apply #'docker-run-async-with-buffer docker-compose-command (docker-compose-arguments) action args))

(aio-defun docker-compose-services ()
  "Return the list of services."
  (s-split "\n" (aio-await (docker-compose-run-docker-compose-async "config" "--services" "2>/dev/null")) t))

(aio-defun docker-compose-read-services-names ()
  "Read the services names."
  (completing-read-multiple "Services: " (aio-await (docker-compose-services))))

(aio-defun docker-compose-read-service-name ()
  "Read one service name."
  (completing-read "Service: " (aio-await (docker-compose-services))))

(defun docker-compose-read-log-level (prompt &rest _args)
  "Read the `docker-compose' log level forwarding PROMPT."
  (completing-read prompt '(DEBUG INFO WARNING ERROR CRITICAL)))

(defun docker-compose-read-directory (prompt &optional initial-input _history)
  "Wrapper around `read-directory-name' forwarding PROMPT and INITIAL-INPUT."
  (read-directory-name prompt nil nil t initial-input))

(defun docker-compose-read-environment-file (prompt &optional initial-input _history)
  "Wrapper around `read-file-name' forwarding PROMPT and INITIAL-INPUT."
  (read-file-name prompt nil nil t initial-input))

(defun docker-compose-read-compose-file (prompt &optional initial-input _history)
  "Wrapper around `read-file-name' forwarding PROMPT and INITIAL-INPUT."
  (read-file-name prompt nil nil t initial-input (apply-partially 'string-match ".*\\.yml\\|.*\\.yaml")))

(aio-defun docker-compose-run-action-for-one-service (action args services)
  "Run \"docker-compose ACTION ARGS SERVICES\"."
  (interactive (list
                (-last-item (s-split "-" (symbol-name transient-current-command)))
                (transient-args transient-current-command)
                nil))
  (setq services (aio-await (docker-compose-read-services-names)))
  (docker-compose-run-docker-compose-async-with-buffer action args services))

(defun docker-compose-run-action-for-all-services (action args)
  "Run \"docker-compose ACTION ARGS\"."
  (interactive (list
                (-last-item (s-split "-" (symbol-name transient-current-command)))
                (transient-args transient-current-command)))
  (docker-compose-run-docker-compose-async-with-buffer action args))

(aio-defun docker-compose-run-action-with-command (action args service command)
  "Run \"docker-compose ACTION ARGS SERVICE COMMAND\"."
  (interactive (list
                (-last-item (s-split "-" (symbol-name transient-current-command)))
                (transient-args transient-current-command)
                nil
                (read-string "Command: ")))
  (setq service (aio-await (docker-compose-read-service-name)))
  (docker-compose-run-docker-compose-async-with-buffer action args service command))

(transient-define-prefix docker-compose-build ()
  "Transient for \"docker-compose build\"."
  :man-page "docker-compose build"
  ["Arguments"
   ("b" "Build argument" "--build-arg " read-string)
   ("c" "Compress build context" "--compress")
   ("f" "Always remove intermediate containers" "--force-rm")
   ("m" "Memory limit" "--memory " transient-read-number-N0)
   ("n" "Do not use cache" "--no-cache")
   ("p" "Attempt to pull a newer version of the image" "--pull")
   ("r" "Build images in parallel" "--parallel")]
  ["Actions"
   ("B" "Build" docker-compose-run-action-for-one-service)
   ("A" "All services" docker-compose-run-action-for-all-services)])

(transient-define-prefix docker-compose-config ()
  "Transient for \"docker-compose config\"."
  :man-page "docker-compose config"
  ["Arguments"

   ("r" "Pin image tags to digests" "--resolve-image-digests")
   ("s" "Print the service names" "--services")
   ("v" "Print the volume names" "--volumes")]
  ["Actions"
   ("V" "Config" docker-compose-run-action-for-all-services)])

(transient-define-prefix docker-compose-create ()
  "Transient for \"docker-compose create\"."
  :man-page "docker-compose create"
  ["Arguments"
   ("b" "Build" "--build")
   ("f" "Force recreate" "--force-recreate")
   ("n" "No recreate" "--no-recreate")]
  ["Actions"
   ("C" "Create" docker-compose-run-action-for-one-service)
   ("A" "All services" docker-compose-run-action-for-all-services)])

(transient-define-prefix docker-compose-down ()
  "Transient for \"docker-compose down\"."
  :man-page "docker-compose down"
  ["Arguments"
   ("o" "Remove orphans" "--remove-orphans")
   ("t" "Timeout" "--timeout " transient-read-number-N0)
   ("v" "Remove volumes" "--volumes")]
  ["Actions"
   ("W" "Down" docker-compose-run-action-for-one-service)
   ("A" "All services" docker-compose-run-action-for-all-services)])

(transient-define-prefix docker-compose-exec ()
  "Transient for \"docker-compose exec\"."
  :man-page "docker-compose exec"
  ["Arguments"
   ("P" "Privileged" "--privileged")
   ("T" "Disable pseudo-tty" "-T")
   ("d" "Detach" "-d")
   ("e" "Env KEY=VAL" "-e " read-string)
   ("u" "User " "--user " read-string)
   ("w" "Workdir" "--workdir " read-string)]
  ["Actions"
   ("E" "Exec" docker-compose-run-action-with-command)])

(transient-define-prefix docker-compose-logs ()
  "Transient for \"docker-compose logs\"."
  :man-page "docker-compose logs"
  ["Arguments"
   ("T" "Tail" "--tail=" read-string)
   ("f" "Follow" "--follow")
   ("n" "No color" "--no-color")
   ("t" "Timestamps" "--timestamps")]
  ["Actions"

   ("L" "Logs" docker-compose-run-action-for-one-service)
   ("A" "All services" docker-compose-run-action-for-all-services)])

(transient-define-prefix docker-compose-pull ()
  "Transient for \"docker-compose pull\"."
  :man-page "docker-compose pull"
  ["Arguments"
   ("d" "Include dependencies" "--include-deps")
   ("i" "Ignore pull failures" "--ignore-pull-failures")
   ("n" "No parallel" "--no-parallel")]
  ["Actions"
   ("F" "Pull" docker-compose-run-action-for-one-service)
   ("A" "All services" docker-compose-run-action-for-all-services)])

(transient-define-prefix docker-compose-push ()
  "Transient for \"docker-compose push\"."
  :man-page "docker-compose push"
  ["Arguments"
   ("i" "Ignore push failures" "--ignore-push-failures")]
  ["Actions"
   ("P" "Push" docker-compose-run-action-for-one-service)
   ("A" "All services" docker-compose-run-action-for-all-services)])

(transient-define-prefix docker-compose-restart ()
  "Transient for \"docker-compose restart\"."
  :man-page "docker-compose restart"
  ["Arguments"
   ("t" "Timeout" "--timeout " transient-read-number-N0)]
  ["Actions"
   ("T" "Restart" docker-compose-run-action-for-one-service)
   ("A" "All services" docker-compose-run-action-for-all-services)])

(transient-define-prefix docker-compose-rm ()
  "Transient for \"docker-compose rm\"."
  :man-page "docker-compose rm"
  ["Arguments"
   ("f" "Force" "--force")
   ("s" "Stop" "--stop")
   ("v" "Remove anonymous volumes" "-v")]
  ["Actions"
   ("D" "Remove" docker-compose-run-action-for-one-service)
   ("A" "All services" docker-compose-run-action-for-all-services)])

(transient-define-prefix docker-compose-run ()
  "Transient for \"docker-compose run\"."
  :man-page "docker-compose run"
  :value '("--rm")
  ["Arguments"
   ("E" "Entrypoint" "--entrypoint " read-string)
   ("N" "Name" "--name " read-string)
   ("T" "Disable pseudo-tty" "-T")
   ("d" "Detach" "-d")
   ("e" "Env KEY=VAL" "-e " read-string)
   ("l" "Label" "--label " read-string)
   ("n" "No deps" "--no-deps")
   ("r" "Remove container when it exits" "--rm")
   ("s" "Enable services ports" "--service-ports")
   ("u" "User " "--user " read-string)
   ("w" "Workdir" "--workdir " read-string)]
  ["Actions"
   ("R" "Run" docker-compose-run-action-with-command)])

(transient-define-prefix docker-compose-start ()
  "Transient for \"docker-compose start\"."
  :man-page "docker-compose start"
  ["Actions"
   ("S" "Start" docker-compose-run-action-for-one-service)
   ("A" "All services" docker-compose-run-action-for-all-services)])

(transient-define-prefix docker-compose-stop ()
  "Transient for \"docker-compose stop\"."
  :man-page "docker-compose stop"
  ["Arguments"
   ("t" "Timeout" "--timeout " transient-read-number-N0)]
  ["Actions"
   ("O" "Stop" docker-compose-run-action-for-one-service)
   ("A" "All services" docker-compose-run-action-for-all-services)])

(transient-define-prefix docker-compose-up ()
  "Transient for \"docker-compose up\"."
  :man-page "docker-compose up"
  ["Arguments"
   ("b" "Build" "--build")
   ("c" "Scale" "--scale " transient-read-number-N0)
   ("d" "Detach" "-d")
   ("f" "Force recreate" "--force-recreate")
   ("n" "No deps" "--no-deps")
   ("q" "Quiet pull" "--quiet-pull")
   ("r" "Remove orphans" "--remove-orphans")
   ("t" "Timeout" "--timeout " transient-read-number-N0)]
  ["Actions"
   ("U" "Up" docker-compose-run-action-for-one-service)
   ("A" "All services" docker-compose-run-action-for-all-services)])

(docker-utils-define-transient-arguments docker-compose)

;;;###autoload (autoload 'docker-compose "docker-compose" nil t)
(transient-define-prefix docker-compose ()
  "Transient for docker-compose."
  :man-page "docker-compose"
  ["Arguments"
   ("a" "No ANSI" "--no-ansi")
   ("c" "Compatibility" "--compatibility")
   ("d" "Project directory" "--project-directory " docker-compose-read-directory)
   ("e" "Environment file" "--env-file " docker-compose-read-environment-file)
   ("f" "Compose file" "--file " docker-compose-read-compose-file)
   ("h" "Host" "--host " read-string)
   ("l" "Log level" "--log-level " docker-compose-read-log-level)
   ("p" "Project name" "--project-name " read-string)
   ("r" "Profile" "--profile " read-string)
   ("v" "Verbose" "--verbose")]
  [["Images"
    ("B" "Build"      docker-compose-build)
    ("F" "Pull"       docker-compose-pull)
    ("P" "Push"       docker-compose-push)]
   ["Containers"
    ("C" "Create"     docker-compose-create)
    ("D" "Remove"     docker-compose-rm)
    ("U" "Up"         docker-compose-up)
    ("W" "Down"       docker-compose-down)]
   ["State"
    ("O" "Stop"       docker-compose-stop)
    ("S" "Start"      docker-compose-start)
    ("T" "Restart"    docker-compose-restart)]
   ["Other"
    ("R" "Run"        docker-compose-run)
    ("L" "Logs"       docker-compose-logs)
    ("E" "Exec"       docker-compose-exec)
    ("V" "Config"     docker-compose-config)]])

(provide 'docker-compose)

;;; docker-compose.el ends here
