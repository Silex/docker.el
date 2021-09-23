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

(defconst docker-image-id-template
  "{{if (eq \\\"<none>\\\" .Repository .Tag)}}{{ json .ID }}{{else}}\\\"{{ .Repository }}:{{ .Tag }}\\\"{{end}}"
  "This Go template defines what will be passed to transient commands.

The default value uses Repository:Tag unless either is <none>, then it uses Id.")

(defcustom docker-image-default-sort-key '("Repository" . nil)
  "Sort key for docker images.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'docker-image
  :type '(cons (string :tag "Column Name"
                       :validate (lambda (widget)
                                   (unless (--any-p (equal (plist-get it :name) (widget-value widget)) docker-image-column-spec)
                                     (widget-put widget :error "Default Sort Key must match a column name")
                                     widget)))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defcustom docker-image-column-spec
  '((:name "Repository" :width 30 :template "{{json .Repository}}" :sort nil :format nil)
    (:name "Tag" :width 20 :template "{{ json .Tag }}" :sort nil :format nil)
    (:name "Id" :width 16 :template "{{ json .ID }}" :sort nil :format nil)
    (:name "Created" :width 24 :template "{{ json .CreatedAt }}" :sort nil :format (lambda (x) (format-time-string "%F %T" (date-to-time x))))
    (:name "Size" :width 10 :template "{{ json .Size }}" :sort docker-utils-human-size-predicate :format nil))
  "Column specification for docker images.

The order of entries defines the displayed column order.
'Template' is the Go template passed to docker-image-ls to generate the column data."
  :group 'docker-image
  :set 'docker-utils-column-spec-setter
  :get 'docker-utils-column-spec-getter
  :type '(repeat (list :tag "Column"
                       (string :tag "Name")
                       (integer :tag "Width")
                       (string :tag "Template")
                       (sexp :tag "Sort function")
                       (sexp :tag "Format function"))))

(defcustom docker-run-default-args
  '("-i" "-t" "--rm")
  "Default infix args used when docker run is invoked.

Note this can be overriden for specific images using
`docker-image-run-custom-args'."
  :group 'docker-run
  :type '(repeat string))

(defcustom docker-image-run-custom-args
  nil
  "List which can be used to customize the default arguments for docker run.

Its elements should be of the form (REGEX ARGS) where
REGEX is a (string) regular expression and ARGS is a list of strings
corresponding to arguments.

Also note if you do not specify `docker-run-default-args', they will be ignored."
  :type '(repeat (list string (repeat string))))

(defun docker-image-entries ()
  "Return the docker images data for `tabulated-list-entries'."
  (let* ((fmt (docker-utils-make-format-string docker-image-id-template docker-image-column-spec))
         (data (docker-run-docker "image ls" (docker-image-ls-arguments) (format "--format=\"%s\"" fmt)))
         (lines (s-split "\n" data t)))
    (-map (-partial #'docker-utils-parse docker-image-column-spec) lines)))

(defun docker-image-refresh ()
  "Refresh the images list."
  (setq tabulated-list-entries (docker-image-entries)))

(defun docker-image-read-name ()
  "Read an image name."
  (completing-read "Image: " (-map #'car (docker-image-entries))))

;;;###autoload
(defun docker-image-pull-one (name &optional all)
  "Pull the image named NAME.  If ALL is set, use \"-a\"."
  (interactive (list (docker-image-read-name) current-prefix-arg))
  (docker-run-docker "pull" (when all "-a ") name)
  (tablist-revert))

(defun docker-image-run-selection (command)
  "Run \"docker image run\" with COMMAND on the images selection."
  (interactive "sCommand: ")
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run-docker-async "container run" (transient-args 'docker-image-run) it command))
  (tablist-revert))

(defun docker-image-tag-selection ()
  "Tag images."
  (interactive)
  (docker-utils-ensure-items)
  (--each (docker-utils-get-marked-items-ids)
    (docker-run-docker "tag" it (read-string (format "Tag for %s: " it))))
  (tablist-revert))

(defun docker-image-ls-arguments ()
  "Return the latest used arguments in the `docker-image-ls' transient."
  (car (alist-get 'docker-image-ls transient-history)))

(transient-define-prefix docker-image-ls ()
  "Transient for listing images."
  :man-page "docker-image-ls"
  ["Arguments"
   ("a" "All" "--all")
   ("d" "Dangling" "-f dangling=true")
   ("f" "Filter" "--filter" read-string)
   ("n" "Don't truncate" "--no-trunc")]
  ["Actions"
   ("l" "List" tablist-revert)])

(transient-define-prefix docker-image-pull ()
  "Transient for pulling images."
  :man-page "docker-image-pull"
  ["Arguments"
   ("a" "All" "-a")]
  [:description docker-utils-generic-actions-heading
   ("F" "Pull selection" docker-utils-generic-action)
   ("N" "Pull a new image" docker-image-pull-one)])

(docker-utils-transient-define-prefix docker-image-push ()
  "Transient for pushing images."
  :man-page "docker-image-push"
  [:description docker-utils-generic-actions-heading
   ("P" "Push" docker-utils-generic-action)])

(docker-utils-transient-define-prefix docker-image-rm ()
  "Transient for removing images."
  :man-page "docker-image-rm"
  ["Arguments"
   ("-f" "Force" "-f")
   ("-n" "Don't prune" "--no-prune")]
  [:description docker-utils-generic-actions-heading
   ("D" "Remove" docker-utils-generic-action)])

(defclass docker-run-prefix (transient-prefix) nil)

(cl-defmethod transient-init-value ((obj docker-run-prefix))
  (oset obj value
        (let* ((images (tablist-get-marked-items))
               (matched-args (let ((repo-name (caar images)))
                               (if repo-name
                                   (--first (string-match (car it) repo-name)
                                            docker-image-run-custom-args)
                                 nil))))
          (if matched-args
              (cadr matched-args)
            docker-run-default-args))))

(docker-utils-transient-define-prefix docker-image-run ()
  "Transient for running images."
  :man-page "docker-image-run"
  :class 'docker-run-prefix
  ["Arguments"
   ("D" "With display" "-v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY")
   ("M" "Mount volume" "--mount=" read-string)
   ("N" "Network" "--network " read-string)
   ("P" "Privileged" "--privileged")
   ("T" "Synchronize time" "-v /etc/localtime:/etc/localtime:ro")
   ("W" "Web ports" "-p 80:80 -p 443:443 -p 8080:8080")
   ("d" "Detach" "-d")
   ("e" "environment" "-e " read-string)
   ("i" "Interactive" "-i")
   ("m" "name" "--name " read-string)
   ("n" "entrypoint" "--entrypoint " read-string)
   ("o" "Read only" "--read-only")
   ("p" "port" "-p " read-string)
   ("r" "Remove container when it exits" "--rm")
   ("t" "TTY" "-t")
   ("u" "user" "-u " read-string)
   ("v" "volume" "-v " read-string)
   ("w" "workdir" "-w " read-string)]
  [:description docker-utils-generic-actions-heading
   ("R" "Run" docker-image-run-selection)])

(transient-define-prefix docker-image-help ()
  "Help transient for docker images."
  ["Docker images help"
   ("D" "Remove"  docker-image-rm)
   ("F" "Pull"    docker-image-pull)
   ("I" "Inspect" docker-utils-inspect)
   ("P" "Push"    docker-image-push)
   ("R" "Run"     docker-image-run)
   ("T" "Tag"     docker-image-tag-selection)
   ("l" "List"    docker-image-ls)])

(defvar docker-image-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'docker-image-help)
    (define-key map "D" 'docker-image-rm)
    (define-key map "F" 'docker-image-pull)
    (define-key map "I" 'docker-utils-inspect)
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
  (setq tabulated-list-format (docker-utils-column-spec-list-format docker-image-column-spec))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key docker-image-default-sort-key)
  (add-hook 'tabulated-list-revert-hook 'docker-image-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-image)

;;; docker-image.el ends here
