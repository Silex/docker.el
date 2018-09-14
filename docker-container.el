;;; docker-container.el --- Emacs interface to docker-container  -*- lexical-binding: t -*-

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

(require 's)
(require 'dash)
(require 'json)
(require 'tablist)
(require 'magit-popup)

(require 'docker-group)
(require 'docker-process)
(require 'docker-utils)

(defgroup docker-container nil
  "Docker container customization group."
  :group 'docker)

(defcustom docker-container-ls-arguments '("--all")
  "Default arguments for `docker-container-ls-popup'."
  :group 'docker-container
  :type '(repeat (string :tag "Argument")))

(defcustom docker-container-shell-file-name shell-file-name
  "Shell to use when entering containers.
For more information see the variable `shell-file-name'."
  :group 'docker-container
  :type 'string)

(defcustom docker-container-default-sort-key '("Image" . nil)
  "Sort key for docker containers.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'docker-container
  :type '(cons (choice (const "Id")
                       (const "Image")
                       (const "Command")
                       (const "Created")
                       (const "Status")
                       (const "Ports")
                       (const "Names"))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defun docker-container-parse (line)
  "Convert a LINE from \"docker container ls\" to a `tabulated-list-entries' entry."
  (condition-case nil
      (let ((data (json-read-from-string line)))
        (setf (aref data 3) (format-time-string "%F %T" (date-to-time (aref data 3))))
        (list (aref data 6) data))
    (json-readtable-error
     (error "Could not read following string as json:\n%s" line))))

(defun docker-container-entries ()
  "Return the docker containers data for `tabulated-list-entries'."
  (let* ((fmt "[{{json .ID}},{{json .Image}},{{json .Command}},{{json .CreatedAt}},{{json .Status}},{{json .Ports}},{{json .Names}}]")
         (data (docker-run "container ls" docker-container-ls-arguments (format "--format=\"%s\"" fmt)))
         (lines (s-split "\n" data t)))
    (-map #'docker-container-parse lines)))

(defun docker-container-refresh ()
  "Refresh the containers list."
  (setq tabulated-list-entries (docker-container-entries)))

(defun docker-container-read-name ()
  "Read an container name."
  (completing-read "Container: " (-map #'car (docker-container-entries))))

;;;###autoload
(defun docker-container-eshell (container)
  "Open `eshell' in CONTAINER."
  (interactive (list (docker-container-read-name)))
  (let* ((container-address (format "docker:%s:/" container))
         (file-prefix (if (file-remote-p default-directory)
                          (with-parsed-tramp-file-name default-directory nil
                            (format "/%s:%s|" method host))
                        "/"))
         (default-directory (format "%s%s" file-prefix container-address))
         (eshell-buffer-name (generate-new-buffer-name (format "*eshell %s*" default-directory))))
    (eshell)))

;;;###autoload
(defun docker-container-find-directory (container directory)
  "Inside CONTAINER open DIRECTORY."
  (interactive
   (let* ((container-name (docker-container-read-name))
          (tramp-filename (read-directory-name "Directory: " (format "/docker:%s:/" container-name))))
     (with-parsed-tramp-file-name tramp-filename nil
       (list host localname))))
  (dired (format "/docker:%s:%s" container directory)))

(defalias 'docker-container-dired 'docker-container-find-directory)

;;;###autoload
(defun docker-container-find-file (container file)
  "Inside CONTAINER open FILE."
  (interactive
   (let* ((container-name (docker-container-read-name))
          (tramp-filename (read-file-name "File: " (format "/docker:%s:/" container-name))))
     (with-parsed-tramp-file-name tramp-filename nil
       (list host localname))))
  (find-file (format "/docker:%s:%s" container file)))

;;;###autoload
(defun docker-container-shell (container)
  "Open `shell' in CONTAINER."
  (interactive (list (docker-container-read-name)))
  (let* ((shell-file-name docker-container-shell-file-name)
         (container-address (format "docker:%s:/" container))
         (file-prefix (if (file-remote-p default-directory)
                          (with-parsed-tramp-file-name default-directory nil
                            (format "/%s:%s|" method host))
                        "/"))
         (default-directory (format "%s%s" file-prefix container-address)))
    (shell (generate-new-buffer (format "*shell %s*" default-directory)))))

;;;###autoload
(defun docker-diff (name)
  "Diff the container named NAME."
  (interactive (list (docker-container-read-name)))
  (docker-utils-with-buffer (format "diff %s" name)
   (insert (docker-run "diff" name))))

;;;###autoload
(defun docker-inspect (name)
  "Inspect the container named NAME."
  (interactive (list (docker-container-read-name)))
  (docker-utils-with-buffer (format "inspect %s" name)
    (insert (docker-run "inspect" name))
    (json-mode)))

;;;###autoload
(defun docker-kill (name &optional signal)
  "Kill the container named NAME using SIGNAL."
  (interactive (list (docker-container-read-name)))
  (docker-run "kill" (when signal (format "-s %s" signal)) name))

;;;###autoload
(defun docker-logs (name &optional follow)
  "Show the logs from container NAME.

If FOLLOW is set, run in `async-shell-command'."
  (interactive (list (docker-container-read-name)))
  (if follow
      (async-shell-command
       (format "%s logs -f %s" docker-command name)
       (generate-new-buffer (format "* docker logs %s *" name)))
    (docker-utils-with-buffer (format "logs %s" name)
      (insert (docker-run "logs" name)))))

;;;###autoload
(defun docker-pause (name)
  "Pause the container named NAME."
  (interactive (list (docker-container-read-name)))
  (docker-run "pause" name))

;;;###autoload
(defun docker-rename (container name)
  "Rename CONTAINER using NAME."
  (interactive (list (docker-container-read-name) (read-string "Name: ")))
  (docker-run "rename" container name))

;;;###autoload
(defun docker-restart (name &optional timeout)
  "Restart the container named NAME.

TIMEOUT is the number of seconds to wait for the container to stop before killing it."
  (interactive (list (docker-container-read-name) current-prefix-arg))
  (docker-run "restart" (when timeout (format "-t %d" timeout)) name))

;;;###autoload
(defun docker-rm (name &optional force link volumes)
  "Remove the container named NAME.

With prefix argument, sets FORCE to true.

Force the removal even if the container is running when FORCE is set.
Remove the specified link and not the underlying container when LINK is set.
Remove the volumes associated with the container when VOLUMES is set."
  (interactive (list (docker-container-read-name) current-prefix-arg))
  (docker-run "rm" (when force "-f") (when link "-l") (when volumes "-v") name))

;;;###autoload
(defun docker-start (name)
  "Start the container named NAME."
  (interactive (list (docker-container-read-name)))
  (docker-run "start" name))

;;;###autoload
(defun docker-stop (name &optional timeout)
  "Stop the container named NAME.

TIMEOUT is the number of seconds to wait for the container to stop before killing it."
  (interactive (list (docker-container-read-name) current-prefix-arg))
  (docker-run "stop" (when timeout (format "-t %d" timeout)) name))

;;;###autoload
(defun docker-unpause (name)
  "Unpause the container named NAME."
  (interactive (list (docker-container-read-name)))
  (docker-run "unpause" name))

(defun docker-container-cp-from-selection (container-path host-path)
  "Run \"docker cp\" from CONTAINER-PATH to HOST-PATH for selected container."
  (interactive "sContainer path: \nFHost path: ")
  (--each (docker-utils-get-marked-items-ids)
    (docker-run "cp" (concat it ":" container-path) host-path)))

(defun docker-container-cp-to-selection (host-path container-path)
  "Run \"docker cp\" from HOST-PATH to CONTAINER-PATH for selected containers."
  (interactive "fHost path: \nsContainer path: ")
  (--each (docker-utils-get-marked-items-ids)
    (docker-run "cp" host-path (concat it ":" container-path))))

(defun docker-container-diff-selection ()
  "Run `docker-diff' on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-utils-with-buffer (format "diff %s" it)
      (insert (docker-run "diff" (docker-container-diff-arguments) it)))))

(defun docker-container-eshell-selection ()
  "Run `docker-container-eshell' on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-container-eshell it)))

(defun docker-container-find-file-selection (path)
  "Run `docker-container-find-file' on the containers selection."
  (interactive "sPath: ")
  (--each (docker-utils-get-marked-items-ids)
    (docker-container-find-file it path)))

(defun docker-container-inspect-selection ()
  "Run `docker-inspect' on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-utils-with-buffer (format "inspect %s" it)
      (insert (docker-run "inspect" (docker-container-inspect-arguments) it))
      (json-mode))))

(defun docker-container-kill-selection ()
  "Run `docker-kill' on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run "kill" (docker-container-kill-arguments) it))
  (tablist-revert))

(defun docker-container-logs-selection ()
  "Run \"docker logs\" on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (async-shell-command
     (format "%s logs %s %s" docker-command (s-join " " (docker-container-logs-arguments)) it)
     (generate-new-buffer (format "* docker logs %s *" it)))))

(defun docker-container-pause-selection ()
  "Run `docker-pause' on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run "pause" (docker-container-pause-arguments) it))
  (tablist-revert))

(defun docker-container-rename-selection ()
  "Rename containers."
  (interactive)
  (docker-utils-select-if-empty)
  (--each (docker-utils-get-marked-items-ids)
    (docker-rename it (read-string (format "New name for %s: " it))))
  (tablist-revert))

(defun docker-container-restart-selection ()
  "Run `docker-restart' on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run "restart" (docker-container-restart-arguments) it))
  (tablist-revert))

(defun docker-container-rm-selection ()
  "Run `docker-rm' on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run "rm" (docker-container-rm-arguments) it))
  (tablist-revert))

(defun docker-container-shell-selection ()
  "Run `docker-container-shell' on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-container-shell it)))

(defun docker-container-start-selection ()
  "Run `docker-start' on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run "start" (docker-container-start-arguments) it))
  (tablist-revert))

(defun docker-container-stop-selection ()
  "Run `docker-stop' on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run "stop" (docker-container-stop-arguments) it))
  (tablist-revert))

(defun docker-container-unpause-selection ()
  "Run `docker-unpause' on the containers selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run "unpause" (docker-container-unpause-arguments) it))
  (tablist-revert))

(magit-define-popup docker-container-cp-popup
  "Popup for copying files from/to containers."
  'docker-container
  :man-page "docker-cp"
  :actions  '((?f "Copy From" docker-container-cp-from-selection)
              (?t "Copy To" docker-container-cp-to-selection))
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-container-diff-popup
  "Popup for showing containers diffs."
  'docker-container
  :man-page "docker-diff"
  :actions  '((?d "Diff" docker-container-diff-selection))
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-container-find-file-popup
  "Popup for opening containers files."
  'docker-container
  :actions  '((?f "Open file" docker-container-find-file-selection))
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-container-inspect-popup
  "Popup for inspecting containers."
  'docker-container
  :man-page "docker-inspect"
  :actions  '((?I "Inspect" docker-container-inspect-selection))
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-container-kill-popup
  "Popup for kill signaling containers"
  'docker-container
  :man-page "docker-kill"
  :options  '((?s "Signal" "-s "))
  :actions  '((?K "Kill" docker-container-kill-selection))
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-container-logs-popup
  "Popup for showing containers logs."
  'docker-container
  :man-page "docker-logs"
  :switches '((?f "Follow" "-f"))
  :actions  '((?L "Logs" docker-container-logs-selection))
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-container-ls-popup
  "Popup for listing containers."
  'docker-container
  :man-page "docker-container-ls"
  :switches  '((?a "All" "--all")
               (?e "Exited containers" "--filter status=exited")
               (?n "Don't truncate" "--no-trunc"))
  :options   '((?f "Filter" "--filter ")
               (?n "Last" "--last "))
  :actions   `((?l "List" ,(docker-utils-set-then-call 'docker-container-ls-arguments 'tablist-revert))))

(magit-define-popup docker-container-pause-popup
  "Popup for pauseing containers."
  'docker-container
  :man-page "docker-pause"
  :actions  '((?P "Pause" docker-container-pause-selection)
              (?U "Unpause" docker-container-unpause-selection))
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-container-restart-popup
  "Popup for restarting containers."
  'docker-container
  :man-page "docker-restart"
  :options '((?t "Timeout" "-t "))
  :actions '((?R "Restart" docker-container-restart-selection))
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-container-rm-popup
  "Popup for removing containers."
  'docker-container
  :man-page "docker-rm"
  :switches '((?f "Force" "-f")
              (?v "Volumes" "-v"))
  :actions  '((?D "Remove" docker-container-rm-selection))
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-container-shell-popup
  "Popup for doing M-x `shell'/`eshell' to containers."
  'docker-container
  :actions  '((?b "Shell" docker-container-shell-selection)
              (?e "Eshell" docker-container-eshell-selection))
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-container-start-popup
  "Popup for starting containers."
  'docker-container
  :man-page "docker-start"
  :actions  '((?S "Start" docker-container-start-selection))
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-container-stop-popup
  "Popup for stoping containers."
  'docker-container
  :man-page "docker-stop"
  :options '((?t "Timeout" "-t "))
  :actions '((?O "Stop" docker-container-stop-selection))
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-container-help-popup
  "Help popup for docker containers."
  'docker-container
  :actions '("Docker containers help"
             (?C "Copy"       docker-container-cp-popup)
             (?D "Remove"     docker-container-rm-popup)
             (?I "Inspect"    docker-container-inspect-popup)
             (?K "Kill"       docker-container-kill-popup)
             (?L "Logs"       docker-container-logs-popup)
             (?O "Stop"       docker-container-stop-popup)
             (?P "Pause"      docker-container-pause-popup)
             (?R "Restart"    docker-container-restart-popup)
             (?S "Start"      docker-container-start-popup)
             (?b "Shell"      docker-container-shell-popup)
             (?d "Diff"       docker-container-diff-popup)
             (?f "Find file"  docker-container-find-file-popup)
             (?l "List"       docker-container-ls-popup)
             (?r "Rename"     docker-container-rename-selection)))

(defvar docker-container-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'docker-container-help-popup)
    (define-key map "C" 'docker-container-cp-popup)
    (define-key map "D" 'docker-container-rm-popup)
    (define-key map "I" 'docker-container-inspect-popup)
    (define-key map "K" 'docker-container-kill-popup)
    (define-key map "L" 'docker-container-logs-popup)
    (define-key map "O" 'docker-container-stop-popup)
    (define-key map "P" 'docker-container-pause-popup)
    (define-key map "R" 'docker-container-restart-popup)
    (define-key map "S" 'docker-container-start-popup)
    (define-key map "b" 'docker-container-shell-popup)
    (define-key map "d" 'docker-container-diff-popup)
    (define-key map "f" 'docker-container-find-file-popup)
    (define-key map "l" 'docker-container-ls-popup)
    (define-key map "r" 'docker-container-rename-selection)
    map)
  "Keymap for `docker-container-mode'.")

;;;###autoload
(defun docker-containers ()
  "List docker containers."
  (interactive)
  (docker-utils-pop-to-buffer "*docker-containers*")
  (docker-container-mode)
  (tablist-revert))

(define-derived-mode docker-container-mode tabulated-list-mode "Containers Menu"
  "Major mode for handling a list of docker containers."
  (setq tabulated-list-format [("Id" 16 t)("Image" 15 t)("Command" 30 t)("Created" 23 t)("Status" 20 t)("Ports" 10 t)("Names" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key docker-container-default-sort-key)
  (add-hook 'tabulated-list-revert-hook 'docker-container-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-container)

;;; docker-container.el ends here
