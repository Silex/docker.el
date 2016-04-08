;;; docker-machine.el --- Emacs interface to docker-machine

;; Author: Ben Swift <ben@benswift.me>

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

(require 'docker-process)
(require 'docker-utils)
(require 'tle)

(require 'eieio)
(require 'magit-popup)

(defun docker-machine-command (action &rest args)
  "Execute docker-machine ACTION passing arguments ARGS.

This is called `docker-machine-command' because the name
`docker-machine' is used for the data object"
  (let ((command (format "docker-machine %s %s" action (s-join " " (-non-nil args)))))
    (message command)
    (shell-command-to-string command)))

(defclass docker-machine ()
  ((name          :initarg :name         :initform nil)
   (active        :initarg :active       :initform nil)
   (driver        :initarg :driver       :initform nil)
   (state         :initarg :state        :initform nil)
   (url           :initarg :url          :initform nil)
   (swarm         :initarg :swarm        :initform nil)
   (docker        :initarg :docker       :initform nil)
   (errors        :initarg :errors       :initform nil)))

(defmethod docker-machine-name ((this docker-machine))
  "Return the machine name."
  (oref this :name))

(defmethod docker-machine-to-tabulated-list ((this docker-machine))
  "Convert `docker-machine' to tabulated list."
  (list (oref this :name)
        `[,(oref this :name)
          ,(oref this :active)
          ,(oref this :driver)
          ,(oref this :state)
          ,(oref this :url)
          ,(oref this :swarm)
          ,(oref this :docker)
          ,(oref this :errors)]))

(defun make-docker-machine (name active driver state url swarm docker errors &rest unused)
  "Helper to create a `eieio` docker machine object."
  (docker-machine name :name name :active active :driver driver :state state :url url :swarm swarm :docker docker :errors errors))

(defun docker-machine-parse (line)
  "Convert LINE from 'docker-machine ls' to `docker-machine'."
  (apply #'make-docker-machine (s-split "\t" line)))

(defun docker-machine-names ()
  "Return the list of machine names."
  (--map (docker-machine-name it) (docker-get-machines)))

(defun docker-machine-active ()
  "Print which machine is active."
  (docker-machine-command "active"))

(defun docker-read-machine-name (prompt)
  "Read an machine name."
  (completing-read prompt (docker-machine-names)))

(defun docker-machine-config (name)
  "Print the connection config for machine."
  (interactive (list (docker-read-machine-name "Config for machine: ")))
  (docker-machine-command "config" name))

(defun docker-machine-inspect (name)
  "Inspect information about a machine."
  (interactive (list (docker-read-machine-name "Inspect machine: ")))
  (docker-machine-command "inspect" name))

(defun docker-machine-ip (name)
  "Get the IP address of a machine."
  (interactive (list (docker-read-machine-name "IP for machine: ")))
  (docker-machine-command "ip" name))

(defun docker-machine-status (name)
  "Get the status of a machine."
  (interactive (list (docker-read-machine-name "Status of machine: ")))
  (docker-machine-command "status" name))

(defun docker-machine-upgrade (name)
  "Upgrade a machine to the latest version of Docker."
  (interactive (list (docker-read-machine-name "Upgrade machine: ")))
  (docker-machine-command "upgrade" name))

(defun docker-machine-kill (name)
  "Kill a machine."
  (interactive (list (docker-read-machine-name "Kill machine: ")))
  (docker-machine-command "kill" name))

(defun docker-machine-start (name)
  "Start a machine."
  (interactive (list (docker-read-machine-name "Start machine: ")))
  (docker-machine-command "start" name))

(defun docker-machine-env-export (line)
  (let ((split-string (s-split "=" (s-chop-prefix "export " line))))
    (setenv (car split-string)
            (read (cdr split-string)))))

(defun docker-machine-env (name)
  "Parse and set environment variables from 'docker-machine env' output"
  (interactive (list (docker-read-machine-name "Set up environment for machine: ")))
  (--each-while
      (s-lines (docker-machine-command "env" name))
      (s-prefix? "export" it)
    (docker-machine-env-export it)))

(defun docker-machine-stop (name)
  "Stop a machine."
  (interactive (list (docker-read-machine-name "Stop machine: ") current-prefix-arg))
  (docker-machine-command "stop" name))

(defun docker-machine-restart (name)
  "Restart a machine."
  (interactive (list (docker-read-machine-name "Restart machine: ") current-prefix-arg))
  (docker-machine-command "restart" name))

(defun docker-machine-rm (name &optional force)
  "Destroy or uncommand an machine."
  (interactive (list (docker-read-machine-name "Delete machine: ") current-prefix-arg))
  (docker-machine-command "rm" (when force "--force") name))

(defun docker-get-machines (&optional quiet filters)
  "Get machines as eieio objects."
  (let* ((data (docker-get-machines-raw quiet filters))
         (lines (s-split "\n" data t))
         (lines (cdr lines)))
    (-map 'docker-machine-parse lines)))

(defun docker-get-machines-raw (&optional quiet filters)
  "Equivalent of \"docker machines\"."
  (docker-machine-command
   "ls --format \"{{.Name}}\\t{{.Active}}\\t{{.DriverName}}\\t{{.State}}\\t{{.URL}}\\t{{.Swarm}}\\t{{.DockerVersion}}\\t{{.Error}}\""
   (when quiet "-q ")
   (when filters
     (s-join " --filter=" filters))))

(defun docker-machine-selection ()
  "Get the machines selection as a list of ids."
  (tle-selection-ids))

(defun docker-machine-run-command-on-selection (command arguments)
  "Run a docker COMMAND on the machines selection with ARGUMENTS."
  (interactive "sCommand: \nsArguments: ")
  (--each (docker-machine-selection)
    (docker-machine-command command arguments it))
  (tabulated-list-revert))

(defmacro docker-machine-create-selection-functions (&rest functions)
  `(progn ,@(--map
             `(defun ,(intern (format "docker-machine-%s-selection" it)) ()
                ,(format "Run `docker-machine-%s' on the machines selection." it)
                (interactive)
                (docker-machine-run-command-on-selection ,(symbol-name it)
                                                         (s-join " " ,(list (intern (format "docker-machine-%s-arguments" it))))))
             functions)))

(docker-machine-create-selection-functions start stop restart rm)

(docker-utils-define-popup docker-machine-start-popup
  "Popup for starting machines."
  'docker-machine-popups
  :man-page "docker-machine-start"
  :actions  '((?S "Start" docker-machine-start-selection)))

(docker-utils-define-popup docker-machine-env-popup
  "Popup for setting up environment variables."
  'docker-machine-popups
  :man-page "docker-machine-env"
  :actions '((?E "Env" docker-machine-env-selection)))

(docker-utils-define-popup docker-machine-stop-popup
  "Popup for stoping machines."
  'docker-machine-popups
  :man-page "docker-machine-stop"
  :actions '((?O "Stop" docker-machine-stop-selection)))

(docker-utils-define-popup docker-machine-restart-popup
  "Popup for restarting machines."
  'docker-machine-popups
  :man-page "docker-machine-restart"
  :actions '((?R "Restart" docker-machine-restart-selection)))

(docker-utils-define-popup docker-machine-rm-popup
  "Popup for removing machines."
  'docker-machine-popups
  :man-page "docker-machine-rm"
  :switches '((?f "Force" "-f"))
  :actions  '((?D "Remove" docker-machine-rm-selection)))

(defun docker-machine-refresh ()
  "Refresh the machines list."
  (setq tabulated-list-entries (-map 'docker-machine-to-tabulated-list (docker-get-machines))))

(defvar docker-machine-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "S" 'docker-machine-start-popup)
    (define-key map "E" 'docker-machine-env-popup)
    (define-key map "O" 'docker-machine-stop-popup)
    (define-key map "R" 'docker-machine-restart-popup)
    (define-key map "D" 'docker-machine-rm-popup)
    map)
  "Keymap for `docker-machine-mode'.")

;;;###autoload
(defun docker-machine-ls ()
  "List docker machines."
  (interactive)
  (pop-to-buffer "*docker-machine*")
  (docker-machine-mode)
  (docker-machine-refresh)
  (tabulated-list-revert))

(define-derived-mode docker-machine-mode tabulated-list-mode "Machines Menu"
  "Major mode for handling a list of docker machines."
  (setq tabulated-list-format [("Name" 16 t)("Active" 7 t)("Driver" 12 t)("State" 12 t)("URL" 30 t)("Swarm" 10 t)("Docker" 10 t)("Errors" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (add-hook 'tabulated-list-revert-hook 'docker-machine-refresh nil t)
  (tabulated-list-init-header)
  (tle-mode))

(provide 'docker-machine)

;;; docker-machine.el ends here
