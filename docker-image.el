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
  "{{ json .ID }}"
  "This Go template extracts the image id which will be passed to transient commands.")

;; TODO this can just be given to defcustom rather than as a separate definition
(defconst docker-image-default-columns
  '((:name "Repository" :width 30 :template "{{json .Repository}}")
    (:name "Tag" :width 20 :template "{{ json .Tag }}")
    (:name "Id" :width 16 :template "{{ json .ID }}")
    (:name "Created" :width 24 :template "{{ json .CreatedAt }}")
    (:name "Size" :width 10 :template "{{ json .Size }}"))
  "Default column specs for docker-images.")

;; TODO default sort key may not exist?
(defcustom docker-image-default-sort-key '("Repository" . nil)
  "Sort key for docker images.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'docker-image)
  ;; TODO generate column choices from docker-image-column-order
;;  :type (docker-utils-sort-key-customize-type docker-image-default-column-order))

;; TODO fix description
(defcustom docker-image-column-order docker-image-default-columns
  "Column ordering and width for docker images."
  :group 'docker-image
  :set (lambda (sym xs)
         (let ((res (--map (-interleave '(:name :width :template) it) xs))) ;; add plist symbols
           (set sym res)))
  :get (lambda (sym)
         (--map (list (plist-get it :name) (plist-get it :width) (plist-get it :template)) (symbol-value sym)))
  :type '(repeat (list :tag "Column" (string :tag "Name") (integer :tag "Width") (string :tag "Template"))))

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

(defun docker-image-parse (line)
  "Convert a LINE from \"docker image ls\" to a `tabulated-list-entries' entry."
  (condition-case nil
      (let* ((data (json-read-from-string line)))
             ;; (name (format "%s:%s" (aref data 0) (aref data 1))))
             (list (aref data 0) (seq-drop data 1)))
        ;; (aset data 3 (format-time-string "%F %T" (date-to-time (aref data 3))))
        ;; (let ((ordered (docker-utils-reorder-data docker-image-column-order docker-image-default-column-order data)))
          ;; if name is none, then use data 2? otherwise name
          ;; (list (if (s-contains? "<none>" name) (aref data 2) name)
          ;;       ordered)))
    (json-readtable-error
     (error "Could not read following string as json:\n%s" line))))

(defun docker-utils-make-format-string (id-template column-spec)
  (let* ((templates (--map (plist-get it :template) column-spec))
         (delimited (string-join templates ",")))
    (format "[%s,%s]" id-template delimited)))

(defun docker-image-entries ()
  "Return the docker images data for `tabulated-list-entries'."
  (let* ((fmt (docker-utils-make-format-string docker-image-id-template docker-image-column-order))
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
  (setq tabulated-list-format (docker-utils-column-order-list-format docker-image-column-order))
  ;; Manually set sort function of "Size" column
  (let* ((size-pos (seq-position tabulated-list-format "Size" (lambda (x y) (equal (car x) y))))
         (current-elt (aref tabulated-list-format size-pos)))
    (aset tabulated-list-format size-pos (list "Size" (elt current-elt 1) 'docker-image-human-size-predicate)))

  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key docker-image-default-sort-key)
  (add-hook 'tabulated-list-revert-hook 'docker-image-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-image)

;;; docker-image.el ends here
