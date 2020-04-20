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

(require 's)
(require 'dash)
(require 'json)
(require 'tablist)
(require 'transient)

(require 'docker-core)
(require 'docker-utils)

(defgroup docker-image nil
  "Docker images customization group."
  :group 'docker)

(defcustom docker-image-default-sort-key '("Repository" . nil)
  "Sort key for docker images.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'docker-image
  :type '(cons (choice (const "Repository")
                       (const "Tag")
                       (const "Id")
                       (const "Created")
                       (const "Size"))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defun docker-image-parse (line)
  "Convert a LINE from \"docker image ls\" to a `tabulated-list-entries' entry."
  (condition-case nil
      (let* ((data (json-read-from-string line))
             (name (format "%s:%s" (aref data 0) (aref data 1))))
        (aset data 3 (format-time-string "%F %T" (date-to-time (aref data 3))))
        (list (if (s-contains? "<none>" name) (aref data 2) name) data))
    (json-readtable-error
     (error "Could not read following string as json:\n%s" line))))

(defun docker-image-entries ()
  "Return the docker images data for `tabulated-list-entries'."
  (let* ((fmt "[{{json .Repository}},{{json .Tag}},{{json .ID}},{{json .CreatedAt}},{{json .Size}}]")
         (data (docker-run-docker "image ls" (docker-image-ls-arguments) (format "--format=\"%s\"" fmt)))
         (lines (s-split "\n" data t)))
    (-map #'docker-image-parse lines)))

(defun docker-image-refresh ()
  "Refresh the images list."
  (setq tabulated-list-entries (docker-image-entries)))

(defun docker-image-read-name ()
  "Read an image name."
  (completing-read "Image: " (-map #'car (docker-image-entries))))

(defun docker-image-human-size-predicate (a b)
  "Sort A and B by image size."
  (let* ((a-size (elt (cadr a) 4))
         (b-size (elt (cadr b) 4)))
    (< (docker-utils-human-size-to-bytes a-size) (docker-utils-human-size-to-bytes b-size))))

;;;###autoload
(defun docker-image-pull-one (name &optional all)
  "Pull the image named NAME.  If ALL is set, use \"-a\"."
  (interactive (list (docker-image-read-name) current-prefix-arg))
  (docker-run-docker "pull" (when all "-a ") name))

(defun docker-image-run-selection (command)
  "Run \"docker image run\" with COMMAND on the images selection."
  (interactive "sCommand: ")
  (docker-utils-ensure-items)
  (docker-utils-with-sudo
    (--each (docker-utils-get-marked-items-ids)
      (async-shell-command
       (format "%s container run %s %s %s" docker-command (s-join " " (transient-args 'docker-image-run)) it command)
       (docker-utils-generate-new-buffer "run" it)))))

(defun docker-image-tag-selection ()
  "Tag images."
  (interactive)
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run-docker "tag" it (read-string (format "Tag for %s: " it))))
  (tablist-revert))

(docker-utils-define-transient-command docker-image-inspect ()
  "Transient for inspecting images."
  :man-page "docker-image-inspect"
  [:description docker-utils-generic-actions-heading
   ("I" "Inspect" docker-utils-generic-action-with-buffer:json)])

(defun docker-image-ls-arguments ()
  "Return the latest used arguments in the `docker-image-ls' transient."
  (car (alist-get 'docker-image-ls transient-history)))

(define-transient-command docker-image-ls ()
  "Transient for listing images."
  :man-page "docker-image-ls"
  ["Arguments"
   ("-a" "All" "--all")
   ("-d" "Dangling" "-f dangling=true")
   ("-f" "Filter" "--filter" read-string)
   ("-n" "Don't truncate" "--no-trunc")]
  ["Actions"
   ("l" "List" tablist-revert)])

(define-transient-command docker-image-pull ()
  "Transient for pulling images."
  :man-page "docker-image-pull"
  ["Arguments"
   ("-a" "All" "-a")]
  [:description docker-utils-generic-actions-heading
   ("F" "Pull selection" docker-utils-generic-action)
   ("N" "Pull a new image" docker-image-pull-one)])

(docker-utils-define-transient-command docker-image-push ()
  "Transient for pushing images."
  :man-page "docker-image-push"
  [:description docker-utils-generic-actions-heading
   ("P" "Push" docker-utils-generic-action)])

(docker-utils-define-transient-command docker-image-rm ()
  "Transient for removing images."
  :man-page "docker-image-rm"
  ["Arguments"
   ("-f" "Force" "-f")
   ("-n" "Don't prune" "--no-prune")]
  [:description docker-utils-generic-actions-heading
   ("D" "Remove" docker-utils-generic-action)])

(docker-utils-define-transient-command docker-image-run ()
  "Transient for running images."
  :man-page "docker-image-run"
  :value '("-i" "-t" "--rm")
  ["Arguments"
   ("-D" "With display" "-v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY")
   ("-M" "Mount volume" "--mount=" read-string)
   ("-N" "Network" "--network " read-string)
   ("-P" "Privileged" "--privileged")
   ("-T" "Synchronize time" "-v /etc/localtime:/etc/localtime:ro")
   ("-W" "Web ports" "-p 80:80 -p 443:443 -p 8080:8080")
   ("-d" "Detach" "-d")
   ("-e" "environment" "-e " read-string)
   ("-i" "Interactive" "-i")
   ("-m" "name" "--name " read-string)
   ("-n" "entrypoint" "--entrypoint " read-string)
   ("-o" "Read only" "--read-only")
   ("-p" "port" "-p " read-string)
   ("-r" "Remove container when it exits" "--rm")
   ("-t" "TTY" "-t")
   ("-u" "user" "-u " read-string)
   ("-v" "volume" "-v " read-string)
   ("-w" "workdir" "-w " read-string)]
  [:description docker-utils-generic-actions-heading
   ("R" "Run" docker-image-run-selection)])

(define-transient-command docker-image-help ()
  "Help transient for docker images."
  ["Docker images help"
   ("D" "Remove"  docker-image-rm)
   ("F" "Pull"    docker-image-pull)
   ("I" "Inspect" docker-image-inspect)
   ("P" "Push"    docker-image-push)
   ("R" "Run"     docker-image-run)
   ("T" "Tag"     docker-image-tag-selection)
   ("l" "List"    docker-image-ls)])

(defvar docker-image-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'docker-image-help)
    (define-key map "D" 'docker-image-rm)
    (define-key map "F" 'docker-image-pull)
    (define-key map "I" 'docker-image-inspect)
    (define-key map "P" 'docker-image-push)
    (define-key map "R" 'docker-image-run)
    (define-key map "T" 'docker-image-tag-selection)
    (define-key map "l" 'docker-image-ls)
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
  (setq tabulated-list-format [("Repository" 30 t)("Tag" 20 t)("Id" 16 t)("Created" 23 t)("Size" 10 docker-image-human-size-predicate)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key docker-image-default-sort-key)
  (add-hook 'tabulated-list-revert-hook 'docker-image-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-image)

;;; docker-image.el ends here
