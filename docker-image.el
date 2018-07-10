;;; docker-image.el --- Emacs interface to docker-image  -*- lexical-binding: t -*-

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

(require 'docker-process)
(require 'docker-utils)
(require 'magit-popup)
(require 'tablist)

(defcustom docker-image-default-sort-key '("Repository" . nil)
  "Sort key for docker images.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'docker
  :type '(cons (choice (const "Repository")
                       (const "Tag")
                       (const "Id")
                       (const "Created")
                       (const "Size"))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defun docker-image-parse (line)
  "Convert a LINE from \"docker images\" to a `tabulated-list-entries' entry."
  (let* ((data (s-split "\t" line))
         (name (format "%s:%s" (nth 0 data) (nth 1 data))))
    (list
     (if (s-contains? "<none>" name) (nth 2 data) name)
     (apply #'vector data))))

(defun docker-image-entries ()
  "Return the docker images data for `tabulated-list-entries'."
  (let* ((fmt "{{.Repository}}\\t{{.Tag}}\\t{{.ID}}\\t{{.CreatedSince}}\\t{{.Size}}")
         (data (docker-run "images" (format "--format=\"%s\"" fmt)))
         (lines (s-split "\n" data t)))
    (-map #'docker-image-parse lines)))

(defun docker-image-refresh ()
  "Refresh the images list."
  (setq tabulated-list-entries (docker-image-entries)))

(defun docker-image-read-name ()
  "Read an image name."
  (completing-read "Image: " (-map #'car (docker-image-entries))))

;;;###autoload
(defun docker-pull (name &optional all)
  "Pull the image named NAME.  If ALL is set, use \"-a\"."
  (interactive (list (docker-image-read-name) current-prefix-arg))
  (docker-run "pull" (when all "-a ") name))

;;;###autoload
(defun docker-push (name)
  "Push the image named NAME."
  (interactive (list (docker-image-read-name)))
  (docker-run "push" name))

;;;###autoload
(defun docker-rmi (name &optional force no-prune)
  "Destroy or untag the image named NAME.

Force removal of the image when FORCE is set.
Do not delete untagged parents when NO-PRUNE is set."
  (interactive (list (docker-image-read-name) current-prefix-arg))
  (docker-run "rmi" (when force "-f") (when no-prune "--no-prune") name))

;;;###autoload
(defun docker-tag (image name)
  "Tag IMAGE using NAME."
  (interactive (list (docker-image-read-name) (read-string "Name: ")))
  (docker-run "tag" image name))

(defun docker-image-rm-selection ()
  "Run \"docker rmi\" on the images selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run "rmi" (docker-image-rm-arguments) it))
  (tablist-revert))

(defun docker-image-pull-selection ()
  "Run \"docker pull\" on the images selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run "pull" (docker-image-pull-arguments) it))
  (tablist-revert))

(defun docker-image-push-selection ()
  "Run \"docker push\" on the images selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run "push" (docker-image-push-arguments) it)))

(defun docker-image-run-selection (command)
  "Run \"docker run\" on the images selection."
  (interactive "sCommand: ")
  (let ((default-directory (if (and docker-run-as-root
                                    (not (file-remote-p default-directory)))
                               "/sudo::"
                             default-directory)))
    (--each (docker-utils-get-marked-items-ids)
      (async-shell-command
       (format "%s run %s %s %s" docker-command (s-join " " (docker-image-run-arguments)) it command)
       (format "*run %s*" it)))))

(defun docker-image-inspect-selection ()
  "Run \"docker inspect\" on the images selection."
  (interactive)
  (--each (docker-utils-get-marked-items-ids)
    (docker-utils-with-buffer (format "inspect %s" it)
      (insert (docker-run "inspect" (docker-image-inspect-arguments) it))
      (json-mode))))

(defun docker-image-tag-selection ()
  "Tag images."
  (interactive)
  (docker-utils-select-if-empty)
  (--each (docker-utils-get-marked-items-ids)
    (docker-tag it (read-string (format "Tag for %s: " it))))
  (tablist-revert))

(magit-define-popup docker-image-rm-popup
  "Popup for removing images."
  'docker-image-popups
  :man-page "docker-rmi"
  :switches '((?f "Force" "-f")
              (?n "Don't prune" "--no-prune"))
  :actions  '((?D "Remove" docker-image-rm-selection))
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-image-pull-popup
  "Popup for pulling images."
  'docker-image-popups
  :man-page "docker-pull"
  :switches '((?a "All" "-a"))
  :actions  '((?F "Pull" docker-image-pull-selection))
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-image-push-popup
  "Popup for pushing images."
  'docker-image-popups
  :man-page "docker-push"
  :actions  '((?P "Push" docker-image-push-selection))
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-image-inspect-popup
  "Popup for inspecting images."
  'docker-image-popups
  :man-page "docker-inspect"
  :actions  '((?I "Inspect" docker-image-inspect-selection))
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-image-run-popup
  "Popup for running images."
  'docker-image-popups
  :man-page "docker-run"
  :switches '((?d "Daemonize" "-d")
              (?i "Interactive" "-i")
              (?t "TTY" "-t")
              (?r "Remove" "--rm")
              (?p "Privileged" "--privileged")
              (?o "Read only" "--read-only")
              (?T "Synchronize time" "-v /etc/localtime:/etc/localtime:ro")
              (?W "Web ports" "-p 80:80 -p 443:443 -p 8080:8080")
              (?D "With display" "-v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY"))
  :options  '((?v "volume" "-v ")
              (?m "name" "--name ")
              (?e "environment" "-e ")
              (?p "port" "-p ")
              (?w "workdir" "-w ")
              (?u "user" "-u ")
              (?n "entrypoint" "--entrypoint "))
  :actions  '((?R "Run images" docker-image-run-selection))
  :default-arguments '("-i" "-t" "--rm")
  :setup-function #'docker-utils-setup-popup)

(magit-define-popup docker-image-help-popup
  "Help popup for docker images."
  :actions '("Docker images help"
             (?D "Remove"  docker-image-rm-popup)
             (?F "Pull"    docker-image-pull-popup)
             (?I "Inspect" docker-image-inspect-popup)
             (?P "Push"    docker-image-push-popup)
             (?R "Run"     docker-image-run-popup)
             (?T "Tag"     docker-image-tag-selection)
             "Switch to other parts"
             (?c "Containers" docker-containers)
             (?m "Machines"   docker-machines)
             (?n "Networks"   docker-networks)
             (?v "Volumes"    docker-volumes)))

(defvar docker-image-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'docker-image-help-popup)
    (define-key map "D" 'docker-image-rm-popup)
    (define-key map "F" 'docker-image-pull-popup)
    (define-key map "I" 'docker-image-inspect-popup)
    (define-key map "P" 'docker-image-push-popup)
    (define-key map "R" 'docker-image-run-popup)
    (define-key map "T" 'docker-image-tag-selection)
    map)
  "Keymap for `docker-image-mode'.")

;;;###autoload
(defun docker-images ()
  "List docker images."
  (interactive)
  (docker-utils-pop-to-buffer "*docker-images*")
  (docker-image-mode)
  (tablist-revert))

(define-derived-mode docker-image-mode tabulated-list-mode "Images Menu"
  "Major mode for handling a list of docker images."
  (setq tabulated-list-format [("Repository" 30 t)("Tag" 20 t)("Id" 16 t)("Created" 25 t)("Size" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key docker-image-default-sort-key)
  (add-hook 'tabulated-list-revert-hook 'docker-image-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-image)

;;; docker-image.el ends here
