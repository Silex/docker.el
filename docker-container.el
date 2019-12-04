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
(require 'transient)

(require 'docker-core)
(require 'docker-utils)

(defgroup docker-container nil
  "Docker container customization group."
  :group 'docker)

(defcustom docker-container-shell-file-name "/bin/sh"
  "Shell to use when entering containers."
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

(defun docker-container--read-shell (&optional read-shell-name)
  "Return `docker-container-shell-file-name' or read a shell name if READ-SHELL-NAME is truthy."
  (if read-shell-name (read-shell-command "Shell: ") docker-container-shell-file-name))

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
         (data (docker-run-docker "container ls" (docker-container-ls-arguments) (format "--format=\"%s\"" fmt)))
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
         (file-prefix (let ((prefix (file-remote-p default-directory)))
                        (if prefix
                            (format "%s|" (s-chop-suffix ":" prefix))
                          "/")))
         (default-directory (format "%s%s" file-prefix container-address))
         (eshell-buffer-name (docker-utils-generate-new-buffer-name "eshell" default-directory)))
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
  "Open FILE inside CONTAINER."
  (interactive
   (let* ((container-name (docker-container-read-name))
          (tramp-filename (read-file-name "File: " (format "/docker:%s:/" container-name))))
     (with-parsed-tramp-file-name tramp-filename nil
       (list host localname))))
  (find-file (format "/docker:%s:%s" container file)))

;;;###autoload
(defun docker-container-shell (container &optional read-shell)
  "Open `shell' in CONTAINER.  When READ-SHELL is not nil, ask the user for it."
  (interactive (list
                (docker-container-read-name)
                current-prefix-arg))
  (let* ((shell-file-name (docker-container--read-shell read-shell))
         (container-address (format "docker:%s:/" container))
         (file-prefix (let ((prefix (file-remote-p default-directory)))
                        (if prefix
                            (format "%s|" (s-chop-suffix ":" prefix))
                          "/")))
         (default-directory (format "%s%s" file-prefix container-address)))
    (shell (docker-utils-generate-new-buffer "shell" default-directory))))

(defun docker-container-cp-from-selection (container-path host-path)
  "Run \"docker cp\" from CONTAINER-PATH to HOST-PATH for selected container."
  (interactive "sContainer path: \nFHost path: ")
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run-docker "cp" (concat it ":" container-path) host-path)))

(defun docker-container-cp-to-selection (host-path container-path)
  "Run \"docker cp\" from HOST-PATH to CONTAINER-PATH for selected containers."
  (interactive "fHost path: \nsContainer path: ")
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run-docker "cp" host-path (concat it ":" container-path))))

(defun docker-container-eshell-selection ()
  "Run `docker-container-eshell' on the containers selection."
  (interactive)
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (docker-container-eshell it)))

(defun docker-container-find-directory-selection (path)
  "Run `docker-container-find-directory' for PATH on the containers selection."
  (interactive "sPath: ")
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (docker-container-find-directory it path)))

(defun docker-container-find-file-selection (path)
  "Run `docker-container-find-file' for PATH on the containers selection."
  (interactive "sPath: ")
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (docker-container-find-file it path)))

(defun docker-container-rename-selection ()
  "Rename containers."
  (interactive)
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run-docker "rename" it (read-string (format "Rename \"%s\" to: " it))))
  (tablist-revert))

(defun docker-container-shell-selection (prefix)
  "Run `docker-container-shell' on the containers selection."
  (interactive "P")
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (docker-container-shell it prefix)))

(docker-utils-define-transient-command docker-container-attach ()
  "Transient for attaching to containers."
  :man-page "docker-container-attach"
  ["Arguments"
   ("-n" "No STDIN" "--no-stdin")
   ("-d" "Key sequence for detaching" "--detach-keys=" read-string)]
  [:description docker-utils-generic-actions-heading
   ("a" "Attach" docker-utils-generic-action-with-command)])

(docker-utils-define-transient-command docker-container-cp ()
  "Transient for copying files from/to containers."
  :man-page "docker-container-cp"
  [:description docker-utils-generic-actions-heading
   ("f" "Copy From" docker-container-cp-from-selection)
   ("t" "Copy To" docker-container-cp-to-selection)])

(docker-utils-define-transient-command docker-container-diff ()
  "Transient for showing containers diffs."
  :man-page "docker-container-diff"
  [:description docker-utils-generic-actions-heading
   ("d" "Diff" docker-utils-generic-action-with-buffer)])

(docker-utils-define-transient-command docker-container-open ()
  "Transient for opening containers files."
  [:description docker-utils-generic-actions-heading
   ("d" "Open directory" docker-container-find-directory-selection)
   ("f" "Open file" docker-container-find-file-selection)])

(docker-utils-define-transient-command docker-container-inspect ()
  "Transient for inspecting containers."
  :man-page "docker-container-inspect"
  [:description docker-utils-generic-actions-heading
   ("I" "Inspect" docker-utils-generic-action-with-buffer:json)])

(docker-utils-define-transient-command docker-container-kill ()
  "Transient for kill signaling containers"
  :man-page "docker-container-kill"
  ["Arguments"
   ("-s" "Signal" "-s " read-string)]
  [:description docker-utils-generic-actions-heading
   ("K" "Kill" docker-utils-generic-action)])

(docker-utils-define-transient-command docker-container-logs ()
  "Transient for showing containers logs."
  :man-page "docker-container-logs"
  ["Arguments"
   ("-f" "Follow" "-f")]
  [:description docker-utils-generic-actions-heading
   ("L" "Logs" docker-utils-generic-action-with-command)])

(defun docker-container-ls-arguments ()
  "Return the latest used arguments in the `docker-container-ls' transient."
  (car (alist-get 'docker-container-ls transient-history)))

(define-transient-command docker-container-ls ()
  "Transient for listing containers."
  :man-page "docker-container-ls"
  :value '("--all")
  ["Arguments"
   ("-N" "Last" "--last " transient-read-number-N0)
   ("-a" "All" "--all")
   ("-e" "Exited containers" "--filter status=exited")
   ("-f" "Filter" "--filter " read-string)
   ("-n" "Don't truncate" "--no-trunc")]
  ["Actions"
   ("l" "List" tablist-revert)])

(docker-utils-define-transient-command docker-container-pause ()
  "Transient for pauseing containers."
  :man-page "docker-container-pause"
  [:description docker-utils-generic-actions-heading
   ("P" "Pause" docker-utils-generic-action)
   ("U" "Unpause" docker-utils-generic-action)])

(docker-utils-define-transient-command docker-container-restart ()
  "Transient for restarting containers."
  :man-page "docker-container-restart"
  ["Arguments"
   ("-t" "Timeout" "-t " transient-read-number-N0)]
  [:description docker-utils-generic-actions-heading
   ("R" "Restart" docker-utils-generic-action)])

(docker-utils-define-transient-command docker-container-rm ()
  "Transient for removing containers."
  :man-page "docker-container-rm"
  ["Arguments"
   ("-f" "Force" "-f")
   ("-v" "Volumes" "-v")]
  [:description docker-utils-generic-actions-heading
   ("D" "Remove" docker-utils-generic-action)])

(docker-utils-define-transient-command docker-container-shells ()
  "Transient for doing M-x `shell'/`eshell' to containers."
  [:description docker-utils-generic-actions-heading
   ("b" "Shell" docker-container-shell-selection)
   ("e" "Eshell" docker-container-eshell-selection)])

(docker-utils-define-transient-command docker-container-start ()
  "Transient for starting containers."
  :man-page "docker-container-start"
  [:description docker-utils-generic-actions-heading
   ("S" "Start" docker-utils-generic-action)])

(docker-utils-define-transient-command docker-container-stop ()
  "Transient for stoping containers."
  :man-page "docker-container-stop"
  ["Arguments"
   ("-t" "Timeout" "-t " transient-read-number-N0)]
  [:description docker-utils-generic-actions-heading
   ("O" "Stop" docker-utils-generic-action)])

(define-transient-command docker-container-help ()
  "Help transient for docker containers."
  ["Docker containers help"
   ("C" "Copy"       docker-container-cp)
   ("D" "Remove"     docker-container-rm)
   ("I" "Inspect"    docker-container-inspect)
   ("K" "Kill"       docker-container-kill)
   ("L" "Logs"       docker-container-logs)
   ("O" "Stop"       docker-container-stop)
   ("P" "Pause"      docker-container-pause)
   ("R" "Restart"    docker-container-restart)
   ("S" "Start"      docker-container-start)
   ("a" "Attach"     docker-container-attach)
   ("b" "Shell"      docker-container-shells)
   ("d" "Diff"       docker-container-diff)
   ("f" "Find file"  docker-container-open)
   ("l" "List"       docker-container-ls)
   ("r" "Rename"     docker-container-rename-selection)])

(defvar docker-container-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'docker-container-help)
    (define-key map "C" 'docker-container-cp)
    (define-key map "D" 'docker-container-rm)
    (define-key map "I" 'docker-container-inspect)
    (define-key map "K" 'docker-container-kill)
    (define-key map "L" 'docker-container-logs)
    (define-key map "O" 'docker-container-stop)
    (define-key map "P" 'docker-container-pause)
    (define-key map "R" 'docker-container-restart)
    (define-key map "S" 'docker-container-start)
    (define-key map "a" 'docker-container-attach)
    (define-key map "b" 'docker-container-shells)
    (define-key map "d" 'docker-container-diff)
    (define-key map "f" 'docker-container-open)
    (define-key map "l" 'docker-container-ls)
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
