;;; docker-compose.el --- Emacs interface to docker-compose  -*- lexical-binding: t -*-

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
(require 'tablist)
(require 'magit-popup)

(require 'docker-group)
(require 'docker-utils)

(defgroup docker-compose nil
  "Docker compose customization group."
  :group 'docker)

(defcustom docker-compose-arguments '()
  "Arguments to use when calling \"docker-compose\"."
  :group 'docker-compose
  :type '(repeat (string :tag "Argument")))

(defcustom docker-compose-run-arguments '("--rm")
  "Default arguments for `docker-compose-run-popup'."
  :group 'docker-compose
  :type '(repeat (string :tag "Argument")))

(defun docker-compose--run (action &rest args)
  "Execute docker ACTION passing arguments ARGS."
  (let ((command (format "docker-compose %s %s %s"
                         (s-join " " docker-compose-arguments)
                         action
                         (s-join " " (-flatten (-non-nil args))))))
    (message command)
    (shell-command-to-string command)))

(defun docker-compose--run-async (action &rest args)
  "Execute docker ACTION passing arguments ARGS."
  (let ((command (format "docker-compose %s %s %s"
                         (s-join " " docker-compose-arguments)
                         action
                         (s-join " " (-flatten (-non-nil args))))))
    (message command)
    (async-shell-command command (format "*docker-compose %s*" action))))

(defun docker-compose-parse (line)
  "Convert a LINE from \"docker-compose ps\" to a `tabulated-list-entries' entry."
  (let ((data (s-split " \\{3,\\}" line)))
    (list (car data) (apply #'vector data))))

(defun docker-compose-entries ()
  "Return the docker compose data for `tabulated-list-entries'."
  (let* ((data (docker-compose--run "ps"))
         (lines (-slice (s-split "\n" data t) 2)))
    (-map #'docker-compose-parse lines)))

(defun docker-compose-refresh ()
  "Refresh the docker-compose entries."
  (setq tabulated-list-entries (docker-compose-entries)))

(defun docker-compose-services ()
  "Return the list of services."
  (s-split "\n" (docker-compose--run "config" "--services") t))

(defun docker-compose-read-services-names ()
  "Read the services names."
  (read-string (format "Services (%s or RET): " (s-join "," (docker-compose-services)))))

(defun docker-compose-read-service-name ()
  "Read one service name."
  (completing-read "Service: " (docker-compose-services)))

(defun docker-compose-read-log-level (&rest _ignore)
  "Read the docker-compose log level."
  (completing-read "Level: " '(DEBUG INFO WARNING ERROR CRITICAL)))

(defun docker-compose-read-directory (&rest _ignore)
  "Wrapper around `read-directory-name'."
  (read-directory-name "Directory: "))

(defun docker-compose-read-compose-file (&rest _ignore)
  "Wrapper around `read-file-name'."
  (read-file-name "Compose file: " nil nil t nil (apply-partially 'string-match ".*\\.yml")))

;;;###autoload
(defun docker-compose-build (services args)
  "Run \"docker-compose build ARGS SERVICES\"."
  (interactive (list (docker-compose-read-services-names) (docker-compose-build-arguments)))
  (docker-compose--run-async "build" args services))

;;;###autoload
(defun docker-compose-create (services args)
  "Run \"docker-compose create ARGS SERVICES\"."
  (interactive (list (docker-compose-read-services-names) (docker-compose-create-arguments)))
  (docker-compose--run-async "create" args services))

;;;###autoload
(defun docker-compose-down (services args)
  "Run \"docker-compose down ARGS SERVICES\"."
  (interactive (list (docker-compose-read-services-names) (docker-compose-down-arguments)))
  (docker-compose--run-async "down" args services))

;;;###autoload
(defun docker-compose-exec (service command args)
  "Run \"docker-compose exec ARGS SERVICE COMMAND\"."
  (interactive (list (docker-compose-read-service-name) (read-string "Command: ") (docker-compose-exec-arguments)))
  (docker-compose--run-async "exec" args service command))

;;;###autoload
(defun docker-compose-logs (services args)
  "Run \"docker-compose logs ARGS SERVICES\"."
  (interactive (list (docker-compose-read-services-names) (docker-compose-logs-arguments)))
  (docker-compose--run-async "logs" args services))

;;;###autoload
(defun docker-compose-pull (services args)
  "Run \"docker-compose pull ARGS SERVICES\"."
  (interactive (list (docker-compose-read-services-names) (docker-compose-pull-arguments)))
  (docker-compose--run "pull" args services))

;;;###autoload
(defun docker-compose-push (services args)
  "Run \"docker-compose push ARGS SERVICES\"."
  (interactive (list (docker-compose-read-services-names) (docker-compose-push-arguments)))
  (docker-compose--run "push" args services))

;;;###autoload
(defun docker-compose-restart (services args)
  "Run \"docker-compose restart ARGS SERVICES\"."
  (interactive (list (docker-compose-read-services-names) (docker-compose-restart-arguments)))
  (docker-compose--run "restart" args services))

;;;###autoload
(defun docker-compose-rm (services args)
  "Run \"docker-compose rm ARGS SERVICES\"."
  (interactive (list (docker-compose-read-services-names) (docker-compose-rm-arguments)))
  (docker-compose--run "rm" args services))

;;;###autoload
(defun docker-compose-run (service command args)
  "Run \"docker-compose run ARGS SERVICE COMMAND\"."
  (interactive (list (docker-compose-read-service-name) (read-string "Command: ") (docker-compose-run-arguments)))
  (docker-compose--run-async "run" args service command))

;;;###autoload
(defun docker-compose-start (services args)
  "Run \"docker-compose start ARGS SERVICES\"."
  (interactive (list (docker-compose-read-services-names) (docker-compose-start-arguments)))
  (docker-compose--run "start" args services))

;;;###autoload
(defun docker-compose-stop (services args)
  "Run \"docker-compose stop ARGS SERVICES\"."
  (interactive (list (docker-compose-read-services-names) (docker-compose-stop-arguments)))
  (docker-compose--run "stop" args services))

;;;###autoload
(defun docker-compose-up (services args)
  "Run \"docker-compose up ARGS SERVICES\"."
  (interactive (list (docker-compose-read-services-names) (docker-compose-up-arguments)))
  (docker-compose--run-async "up" args services))

(defmacro docker-compose--all (command)
  "Return a lambda running COMMAND for all services."
  `(lambda (args)
     (interactive (list (,(intern (format "%s-arguments" command)))))
     (,command nil args)))

(magit-define-popup docker-compose-build-popup
  "Popup for \"docker-compose build\"."
  'docker-compose
  :man-page "docker-compose build"
  :switches '((?c "Compress build context" "--compress")
              (?f "Always remove intermediate containers" "--force-rm")
              (?n "Do not use cache" "--no-cache")
              (?p "Attempt to pull a newer version of the image" "--pull"))
  :options  '((?b "Build argument" "--build-arg ")
              (?m "Memory limit" "--memory "))
  :actions  `((?B "Build" docker-compose-build)
              (?A "All services" ,(docker-compose--all docker-compose-build))))

(magit-define-popup docker-compose-create-popup
  "Popup for \"docker-compose create\"."
  'docker-compose
  :man-page "docker-compose create"
  :switches '((?b "Build" "--build")
              (?f "Force recreate" "--force-recreate")
              (?n "No recreate" "--no-recreate"))
  :actions  `((?C "Create" docker-compose-create)
              (?A "All services" ,(docker-compose--all docker-compose-create))))

(magit-define-popup docker-compose-down-popup
  "Popup for \"docker-compose down\"."
  'docker-compose
  :man-page "docker-compose down"
  :switches '((?o "Remove orphans" "--remove-orphans")
              (?v "Remove volumes" "--volumes"))
  :options  '((?t "Timeout" "--timeout "))
  :actions  `((?W "Down" docker-compose-down)
              (?A "All services" ,(docker-compose--all docker-compose-down))))

(magit-define-popup docker-compose-exec-popup
  "Popup for \"docker-compose exec\"."
  'docker-compose
  :man-page "docker-compose exec"
  :switches '((?T "Disable pseudo-tty" "-T")
              (?d "Detach" "--detach")
              (?p "Privileged" "--privileged"))
  :options  '((?e "Env KEY=VAL" "-e ")
              (?u "User " "--user ")
              (?w "Workdir" "--workdir "))
  :actions  '((?E "Exec" docker-compose-exec)))

(magit-define-popup docker-compose-logs-popup
  "Popup for \"docker-compose logs\"."
  'docker-compose
  :man-page "docker-compose logs"
  :switches '((?f "Follow" "--follow")
              (?n "No color" "--no-color")
              (?t "Timestamps" "--timestamps"))
  :options  '((?T "Tail" "--tail="))
  :actions  `((?L "Logs" docker-compose-logs)
              (?A "All services" ,(docker-compose--all docker-compose-logs))))

(magit-define-popup docker-compose-pull-popup
  "Popup for \"docker-compose pull\"."
  'docker-compose
  :man-page "docker-compose pull"
  :switches '((?d "Include dependencies" "--include-deps")
              (?i "Ignore pull failures" "--ignore-pull-failures")
              (?n "No parallel" "--no-parallel"))
  :actions  `((?F "Pull" docker-compose-pull)
              (?A "All services" ,(docker-compose--all docker-compose-pull))))

(magit-define-popup docker-compose-push-popup
  "Popup for \"docker-compose push\"."
  'docker-compose
  :man-page "docker-compose push"
  :switches '((?i "Ignore push failures" "--ignore-push-failures"))
  :actions  `((?P "Push" docker-compose-push)
              (?A "All services" ,(docker-compose--all docker-compose-push))))

(magit-define-popup docker-compose-restart-popup
  "Popup for \"docker-compose restart\"."
  'docker-compose
  :man-page "docker-compose restart"
  :options  '((?t "Timeout" "--timeout "))
  :actions  `((?T "Restart" docker-compose-restart)
              (?A "All services" ,(docker-compose--all docker-compose-restart))))

(magit-define-popup docker-compose-rm-popup
  "Popup for \"docker-compose rm\"."
  'docker-compose
  :man-page "docker-compose rm"
  :switches '((?f "Force" "--force")
              (?s "Stop" "--stop")
              (?v "Remove anonymous volumes" "-v"))
  :actions  `((?D "Remove" docker-compose-rm)
              (?A "All services" ,(docker-compose--all docker-compose-rm))))

(magit-define-popup docker-compose-run-popup
  "Popup for \"docker-compose run\"."
  'docker-compose
  :man-page "docker-compose run"
  :switches '((?T "Disable pseudo-tty" "-T")
              (?d "Detach" "--detach")
              (?n "No deps" "--no-deps")
              (?r "Remove container when it exits" "--rm")
              (?s "Enable services ports" "--service-ports"))
  :options  '((?E "Entrypoint" "--entrypoint ")
              (?e "Env KEY=VAL" "-e ")
              (?l "Label" "--label ")
              (?n "Name" "--name ")
              (?u "User " "--user ")
              (?w "Workdir" "--workdir "))
  :actions  '((?R "Run" docker-compose-run)))

(magit-define-popup docker-compose-start-popup
  "Popup for \"docker-compose start\"."
  'docker-compose
  :man-page "docker-compose start"
  :actions  `((?S "Start" docker-compose-start)
              (?A "All services" ,(docker-compose--all docker-compose-start))))

(magit-define-popup docker-compose-stop-popup
  "Popup for \"docker-compose stop\"."
  'docker-compose
  :man-page "docker-compose stop"
  :options  '((?t "Timeout" "--timeout "))
  :actions  `((?O "Stop" docker-compose-stop)
              (?A "All services" ,(docker-compose--all docker-compose-stop))))

(magit-define-popup docker-compose-up-popup
  "Popup for \"docker-compose up\"."
  'docker-compose
  :man-page "docker-compose up"
  :switches '((?b "Build" "--build")
              (?d "Detach" "--detach")
              (?f "Force recreate" "--force-recreate")
              (?n "No deps" "--no-deps")
              (?r "Remove orphans" "--remove-orphans"))
  :options  '((?c "Scale" "--scale ")
              (?t "Timeout" "--timeout "))
  :actions  `((?U "Up" docker-compose-up)
              (?A "All services" ,(docker-compose--all docker-compose-up))))

;;;###autoload (autoload 'docker-compose "docker-compose" nil t)
(magit-define-popup docker-compose
  "Popup for docker-compose."
  'docker-compose
  :man-page "docker-compose"
  :switches '((?a "No ANSI" "--no-ansi")
              (?c "Compatibility" "--compatibility")
              (?v "Verbose" "--verbose"))
  :options  `((?d "Project directory" "--project-directory " docker-compose-read-directory)
              (?f "Compose file" "--file " docker-compose-read-compose-file)
              (?h "Host" "--host ")
              (?l "Log level" "--log-level " docker-compose-read-log-level)
              (?p "Project name" "--project-name "))
  :actions  `("Docker-compose"
              (?B "Build"      ,(docker-utils-set-then-call 'docker-compose-arguments 'docker-compose-build-popup))
              (?C "Create"     ,(docker-utils-set-then-call 'docker-compose-arguments 'docker-compose-create-popup))
              (?D "Remove"     ,(docker-utils-set-then-call 'docker-compose-arguments 'docker-compose-rm-popup))
              (?E "Exec"       ,(docker-utils-set-then-call 'docker-compose-arguments 'docker-compose-exec-popup))
              (?F "Pull"       ,(docker-utils-set-then-call 'docker-compose-arguments 'docker-compose-pull-popup))
              (?L "Logs"       ,(docker-utils-set-then-call 'docker-compose-arguments 'docker-compose-logs-popup))
              (?O "Stop"       ,(docker-utils-set-then-call 'docker-compose-arguments 'docker-compose-stop-popup))
              (?P "Push"       ,(docker-utils-set-then-call 'docker-compose-arguments 'docker-compose-push-popup))
              (?R "Run"        ,(docker-utils-set-then-call 'docker-compose-arguments 'docker-compose-run-popup))
              (?S "Start"      ,(docker-utils-set-then-call 'docker-compose-arguments 'docker-compose-start-popup))
              (?T "Restart"    ,(docker-utils-set-then-call 'docker-compose-arguments 'docker-compose-restart-popup))
              (?U "Up"         ,(docker-utils-set-then-call 'docker-compose-arguments 'docker-compose-up-popup))
              (?W "Down"       ,(docker-utils-set-then-call 'docker-compose-arguments 'docker-compose-down-popup))))

(provide 'docker-compose)

;;; docker-compose.el ends here
