;;; docker-container.el --- Interface to docker-container  -*- lexical-binding: t -*-

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
(require 'aio)
(require 'dash)
(require 'json)
(require 'tablist)
(require 'transient)

(require 'docker-core)
(require 'docker-faces)
(require 'docker-utils)

(defgroup docker-container nil
  "Docker container customization group."
  :group 'docker)

(defconst docker-container-id-template
  "{{ json .Names }}"
  "This Go template extracts the container id which will be passed to transient commands.")

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
  :type '(cons (string :tag "Column Name"
                       :validate (lambda (widget)
                                   (unless (--any-p (equal (plist-get it :name) (widget-value widget)) docker-container-columns)
                                     (widget-put widget :error "Default Sort Key must match a column name")
                                     widget)))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defcustom docker-container-columns
  '((:name "Id" :width 16 :template "{{ json .ID }}" :sort nil :format nil)
    (:name "Image" :width 15 :template "{{ json .Image }}" :sort nil :format nil)
    (:name "Command" :width 30 :template "{{ json .Command }}" :sort nil :format nil)
    (:name "Created" :width 23 :template "{{ json .CreatedAt }}" :sort nil :format (lambda (x) (format-time-string "%F %T" (date-to-time x))))
    (:name "Status" :width 20 :template "{{ json .Status }}" :sort nil :format nil)
    (:name "Ports" :width 10 :template "{{ json .Ports }}" :sort nil :format nil)
    (:name "Names" :width 10 :template "{{ json .Names }}" :sort nil :format nil))
  "Column specification for docker containers.

The order of entries defines the displayed column order.  'Template' is
the Go template passed to `docker-container-ls' to create the column data.
It should return a string delimited with double quotes.  'Sort function' is
a binary predicate that should return true when the first argument should be
sorted before the second.  'Format function' is a function from string to
string that transforms the displayed values in the column."
  :group 'docker-container
  :set 'docker-utils-columns-setter
  :get 'docker-utils-columns-getter
  :type '(repeat (list :tag "Column"
                       (string :tag "Name")
                       (integer :tag "Width")
                       (string :tag "Template")
                       (sexp :tag "Sort function")
                       (sexp :tag "Format function"))))

(defalias 'docker-container-inspect 'docker-inspect)

(defun docker-container--read-shell (&optional read-shell-name)
  "Return `docker-container-shell-file-name' or read a shell name if READ-SHELL-NAME is truthy."
  (if read-shell-name (read-shell-command "Shell: ") docker-container-shell-file-name))

(defun docker-container-status-face (status)
  "Return the correct face according to STATUS."
  (cond
   ((s-starts-with? "Up" status)
    'docker-face-status-up)
   ((s-starts-with? "Exited" status)
    'docker-face-status-down)
   (t
    'docker-face-status-other)))

(aio-defun docker-container-entries (&rest args)
  "Return the docker containers data for `tabulated-list-entries'."
  (let* ((fmt (docker-utils-make-format-string docker-container-id-template docker-container-columns))
         (data (aio-await (docker-run-docker-async "container" "ls" args (format "--format=\"%s\"" fmt))))
         (lines (s-split "\n" data t)))
    (-map (-partial #'docker-utils-parse docker-container-columns) lines)))

(aio-defun docker-container-entries-propertized (&rest args)
  "Return the propertized docker containers data for `tabulated-list-entries'."
  (let ((entries (aio-await (docker-container-entries args))))
    (-map #'docker-container-propertize-entry entries)))

(defun docker-container-propertize-entry (entry)
  "Propertize ENTRY."
  (let* ((index (--find-index (string-equal "Status" (plist-get it :name)) docker-container-columns))
         (data (cadr entry))
         (status (aref data index)))
    (aset data index (propertize status 'font-lock-face (docker-container-status-face status)))
    entry))

(aio-defun docker-container-update-status-async ()
  "Write the status to `docker-status-strings'."
  (plist-put docker-status-strings :containers "Containers")
  (when docker-show-status
    (let* ((entries (aio-await (docker-container-entries-propertized (docker-container-ls-arguments))))
           (index (--find-index (string-equal "Status" (plist-get it :name)) docker-container-columns))
           (statuses (--map (aref (cadr it) index) entries))
           (faces (--map (get-text-property 0 'font-lock-face it) statuses))
           (all (length faces))
           (up (-count (-partial #'equal 'docker-face-status-up) faces))
           (down (-count (-partial #'equal 'docker-face-status-down) faces))
           (other (- all up down)))
      (plist-put docker-status-strings
                 :containers
                 (format "Containers (%s total, %s up, %s down, %s other)"
                         all
                         (propertize (number-to-string up) 'face 'docker-face-status-up)
                         (propertize (number-to-string down) 'face 'docker-face-status-down)
                         (propertize (number-to-string other) 'face 'docker-face-status-other)))
      (transient--redisplay))))

(add-hook 'docker-open-hook #'docker-container-update-status-async)

(aio-defun docker-container-refresh ()
  "Refresh the containers list."
  (docker-utils-refresh-entries
   (docker-container-entries-propertized (docker-container-ls-arguments))))

(defun docker-container-read-name ()
  "Read an container name."
  (completing-read "Container: " (-map #'car (docker-container-entries))))

(defvar eshell-buffer-name)

;;;###autoload (autoload 'docker-container-eshell "docker-container" nil t)
(defun docker-container-eshell (container)
  "Open `eshell' in CONTAINER."
  (interactive (list (docker-container-read-name)))
  (let* ((container-address (format "docker:%s:/" container))
         (file-prefix (let ((prefix (file-remote-p default-directory)))
                        (if prefix
                            (format "%s|" (s-chop-suffix ":" prefix))
                          "/")))
         (default-directory (format "%s%s" file-prefix container-address))
         (eshell-buffer-name (docker-utils-generate-new-buffer-name "docker" "eshell:" default-directory)))
    (eshell)))

;;;###autoload (autoload 'docker-container-find-directory "docker-container" nil t)
(defun docker-container-find-directory (container directory)
  "Inside CONTAINER open DIRECTORY."
  (interactive
   (let* ((container-name (docker-container-read-name))
          (tramp-filename (read-directory-name "Directory: " (format "/docker:%s:/" container-name))))
     (with-parsed-tramp-file-name tramp-filename nil
       (list host localname))))
  (dired (format "/docker:%s:%s" container directory)))

(defalias 'docker-container-dired 'docker-container-find-directory)

;;;###autoload (autoload 'docker-container-find-file "docker-container" nil t)
(defun docker-container-find-file (container file)
  "Open FILE inside CONTAINER."
  (interactive
   (let* ((container-name (docker-container-read-name))
          (tramp-filename (read-file-name "File: " (format "/docker:%s:/" container-name))))
     (with-parsed-tramp-file-name tramp-filename nil
       (list host localname))))
  (find-file (format "/docker:%s:%s" container file)))

;;;###autoload (autoload 'docker-container-shell "docker-container" nil t)
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
    (shell (docker-utils-generate-new-buffer "docker" "shell:" default-directory))))

;;;###autoload (autoload 'docker-container-shell-env "docker-container" nil t)
(aio-defun docker-container-shell-env (container &optional read-shell)
  "Open `shell' in CONTAINER with the environment variable set
and default directory set to workdir. When READ-SHELL is not
nil, ask the user for it."
  (interactive (list
                (docker-container-read-name)
                current-prefix-arg))
  (let* ((shell-file-name (docker-container--read-shell read-shell))
         (container-address (format "docker:%s:" container))
         (file-prefix (let ((prefix (file-remote-p default-directory)))
                        (if prefix
                            (format "%s|" (s-chop-suffix ":" prefix))
                          "/")))
         (container-config (cdr (assq 'Config (aref (json-read-from-string (aio-await (docker-run-docker-async "inspect" container))) 0))))
         (container-workdir (cdr (assq 'WorkingDir container-config)))
         (container-env (cdr (assq 'Env container-config)))
         (default-directory (format "%s%s%s" file-prefix container-address container-workdir))
         ;; process-environment doesn't work with tramp if you call this function more than one per emacs session
         (tramp-remote-process-environment (append container-env nil)))
    (shell (docker-utils-generate-new-buffer "docker" "shell-env:" default-directory))))

;;;###autoload (autoload 'docker-container-vterm "docker-container" nil t)
(defun docker-container-vterm (container)
  "Open `vterm' in CONTAINER."
  (interactive (list (docker-container-read-name)))
  (require 'vterm nil 'noerror)
  (if (fboundp 'vterm-other-window)
      (let* ((container-address (format "docker:%s:/" container))
             (file-prefix (let ((prefix (file-remote-p default-directory)))
                            (if prefix
                                (format "%s|" (s-chop-suffix ":" prefix))
                              "/")))
             (default-directory (format "%s%s" file-prefix container-address)))
        (vterm-other-window (docker-utils-generate-new-buffer-name "docker" "vterm:" default-directory)))
    (error "The vterm package is not installed")))

(defun docker-container-cp-from-selection (container-path host-path)
  "Run \"docker cp\" from CONTAINER-PATH to HOST-PATH for selected container."
  (interactive "sContainer path: \nFHost path: ")
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run-docker-async "cp" (concat it ":" container-path) host-path)))

(defun docker-container-cp-to-selection (host-path container-path)
  "Run \"docker cp\" from HOST-PATH to CONTAINER-PATH for selected containers."
  (interactive "fHost path: \nsContainer path: ")
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run-docker-async "cp" host-path (concat it ":" container-path))))

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

(aio-defun docker-container-rename-selection ()
  "Rename containers."
  (interactive)
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (aio-await (docker-run-docker-async "rename" it (read-string (format "Rename \"%s\" to: " it)))))
  (tablist-revert))

(defun docker-container-shell-selection (prefix)
  "Run `docker-container-shell' on the containers selection forwarding PREFIX."
  (interactive "P")
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (docker-container-shell it prefix)))

(defun docker-container-shell-env-selection (prefix)
  "Run `docker-container-shell-env' on the containers selection forwarding PREFIX."
  (interactive "P")
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (docker-container-shell-env it prefix)))

(defun docker-container-unpause-selection ()
  "Run `docker-container-unpause' on the containers selection."
  (interactive)
  (docker-utils-ensure-items)
  (docker-generic-action-multiple-ids "unpause" (transient-args transient-current-command)))

(defun docker-container-vterm-selection ()
  "Run `docker-container-vterm' on the containers selection."
  (interactive)
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (docker-container-vterm it)))

(docker-utils-transient-define-prefix docker-container-attach ()
  "Transient for attaching to containers."
  :man-page "docker-container-attach"
  ["Arguments"
   ("n" "No STDIN" "--no-stdin")
   ("d" "Key sequence for detaching" "--detach-keys=" read-string)]
  [:description docker-generic-action-description
   ("a" "Attach" docker-generic-action-with-buffer)])

(docker-utils-transient-define-prefix docker-container-cp ()
  "Transient for copying files from/to containers."
  :man-page "docker-container-cp"
  [:description docker-generic-action-description
   ("f" "Copy From" docker-container-cp-from-selection)
   ("t" "Copy To" docker-container-cp-to-selection)])

(docker-utils-transient-define-prefix docker-container-diff ()
  "Transient for showing containers diffs."
  :man-page "docker-container-diff"
  [:description docker-generic-action-description
   ("d" "Diff" docker-generic-action-with-buffer)])

(docker-utils-transient-define-prefix docker-container-open ()
  "Transient for opening containers files."
  [:description docker-generic-action-description
   ("d" "Open directory" docker-container-find-directory-selection)
   ("f" "Open file" docker-container-find-file-selection)])

(docker-utils-transient-define-prefix docker-container-kill ()
  "Transient for kill signaling containers"
  :man-page "docker-container-kill"
  ["Arguments"
   ("s" "Signal" "-s " read-string)]
  [:description docker-generic-action-description
   ("K" "Kill" docker-generic-action-multiple-ids)])

(docker-utils-transient-define-prefix docker-container-logs ()
  "Transient for showing containers logs."
  :man-page "docker-container-logs"
  ["Arguments"
   ("f" "Follow" "-f")
   ("s" "Since" "--since " read-string)
   ("t" "Tail" "--tail " read-string)
   ("u" "Until" "--until " read-string)]
  [:description docker-generic-action-description
   ("L" "Logs" docker-generic-action-with-buffer)])

(docker-utils-define-transient-arguments docker-container-ls)

(transient-define-prefix docker-container-ls ()
  "Transient for listing containers."
  :man-page "docker-container-ls"
  :value '("--all")
  ["Arguments"
   ("N" "Last" "--last " transient-read-number-N0)
   ("a" "All" "--all")
   ("e" "Exited containers" "--filter status=exited")
   ("f" "Filter" "--filter " read-string)
   ("n" "Don't truncate" "--no-trunc")]
  ["Actions"
   ("l" "List" tablist-revert)])

(docker-utils-transient-define-prefix docker-container-pause ()
  "Transient for pausing containers."
  :man-page "docker-container-pause"
  [:description docker-generic-action-description
   ("P" "Pause" docker-generic-action-multiple-ids)
   ("U" "Unpause" docker-container-unpause-selection)])

(docker-utils-transient-define-prefix docker-container-restart ()
  "Transient for restarting containers."
  :man-page "docker-container-restart"
  ["Arguments"
   ("t" "Timeout" "-t " transient-read-number-N0)]
  [:description docker-generic-action-description
   ("R" "Restart" docker-generic-action-multiple-ids)])

(docker-utils-transient-define-prefix docker-container-rm ()
  "Transient for removing containers."
  :man-page "docker-container-rm"
  ["Arguments"
   ("f" "Force" "-f")
   ("v" "Volumes" "-v")]
  [:description docker-generic-action-description
   ("D" "Remove" docker-generic-action-multiple-ids)])

(docker-utils-transient-define-prefix docker-container-shells ()
  "Transient for using shells on containers."
  [:description docker-generic-action-description
   ("b" "Shell" docker-container-shell-selection)
   ("B" "Shell with env" docker-container-shell-env-selection)
   ("e" "Eshell" docker-container-eshell-selection)
   ("v" "Vterm" docker-container-vterm-selection)])

(docker-utils-transient-define-prefix docker-container-start ()
  "Transient for starting containers."
  :man-page "docker-container-start"
  [:description docker-generic-action-description
   ("S" "Start" docker-generic-action-multiple-ids)])

(docker-utils-transient-define-prefix docker-container-stop ()
  "Transient for stoping containers."
  :man-page "docker-container-stop"
  ["Arguments"
   ("t" "Timeout" "-t " transient-read-number-N0)]
  [:description docker-generic-action-description
   ("O" "Stop" docker-generic-action-multiple-ids)])

(transient-define-prefix docker-container-help ()
  "Help transient for docker containers."
  ["Docker Containers"
   ["Lifecycle"
    ("K" "Kill"       docker-container-kill)
    ("O" "Stop"       docker-container-stop)
    ("P" "Pause"      docker-container-pause)
    ("R" "Restart"    docker-container-restart)
    ("S" "Start"      docker-container-start)]
   ["Admin"
    ("C" "Copy"       docker-container-cp)
    ("D" "Remove"     docker-container-rm)
    ("I" "Inspect"    docker-container-inspect)
    ("L" "Logs"       docker-container-logs)
    ("a" "Attach"     docker-container-attach)
    ("b" "Shell"      docker-container-shells)
    ("d" "Diff"       docker-container-diff)
    ("f" "Find file"  docker-container-open)
    ("l" "List"       docker-container-ls)
    ("r" "Rename"     docker-container-rename-selection)]])

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

;;;###autoload (autoload 'docker-containers "docker-container" nil t)
(defun docker-containers ()
  "List docker containers."
  (interactive)
  (docker-utils-pop-to-buffer "*docker-containers*")
  (docker-container-mode)
  (tablist-revert))

(define-derived-mode docker-container-mode tabulated-list-mode "Containers Menu"
  "Major mode for handling a list of docker containers."
  (setq tabulated-list-format (docker-utils-columns-list-format docker-container-columns))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key docker-container-default-sort-key)
  (add-hook 'tabulated-list-revert-hook 'docker-container-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-container)

;;; docker-container.el ends here
