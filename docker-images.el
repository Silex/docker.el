;;; docker-images.el --- Emacs interface to docker-images

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
(require 'tle)

(require 'eieio)
(require 'magit-popup)

(defclass docker-image ()
  ((id           :initarg :id           :initform nil)
   (repository   :initarg :repository   :initform nil)
   (tag          :initarg :tag          :initform nil)
   (created      :initarg :created      :initform nil)
   (size         :initarg :size         :initform nil)))

(defmethod docker-image-name ((this docker-image))
  "Return the repository:name image name."
  (format "%s:%s" (oref this :repository) (oref this :tag)))

(defmethod docker-image-to-tabulated-list ((this docker-image))
  "Convert `docker-image' to tabulated list."
  (list (oref this :id)
        `[,(oref this :id)
          ,(oref this :repository)
          ,(oref this :tag)
          ,(oref this :created)
          ,(oref this :size)]))

(defun make-docker-image (repository tag id created size)
  "Helper to create a `eieio` docker image object."
  (docker-image id :id id :repository repository :tag tag :size size :created created))

(defun docker-image-parse (line)
  "Convert LINE from 'docker containers' to `docker-container'."
  (apply #'make-docker-image (s-split " \\{3,\\}" line)))

(defun docker-image-names ()
  "Return the list of image names."
  (--map (docker-image-name it) (docker-get-images)))

(defun docker-read-image-name (prompt)
  "Read an image name."
  (completing-read prompt (docker-image-names)))

(defun docker-pull (name &optional all)
  "Pull an image."
  (interactive (list (docker-read-image-name "Pull image: ") current-prefix-arg))
  (docker "pull" (when all "-a ") name))

(defun docker-push (name)
  "Push an image."
  (interactive (list (docker-read-image-name "Push image: ")))
  (docker "push" name))

(defun docker-rmi (name &optional force no-prune)
  "Destroy or untag an image."
  (interactive (list (docker-read-image-name "Delete image: ") current-prefix-arg))
  (docker "rmi" name (when force "-f") (when no-prune "--no-prune")))

(defun docker-get-images (&optional all quiet filters)
  "Get images as eieio objects."
  (let* ((data (docker-get-images-raw all quiet filters))
         (lines (s-split "\n" data t))
         (lines (cdr lines)))
    (-map 'docker-image-parse lines)))

(defun docker-get-images-raw (&optional all quiet filters)
  "Equivalent of \"docker images\"."
  (docker "images" (when all "-a ") (when quiet "-q ") (when filters (s-join " --filter=" filters))))

(defun docker-images-selection ()
  "Get the images selection as a list of names."
  (let* ((selection (--map (let ((name (format "%s:%s" (aref it 1) (aref it 2))))
                             (if (string-equal name "<none>:<none>")
                                 (aref it 0)
                               name))
                           (tle-selection-entries))))
    (when (null selection)
      (error "No images selected."))
    selection))

(defun docker-images-rmi-selection ()
  "Run `docker-rmi' on the images selection."
  (interactive)
  (let ((args (docker-images-rmi-arguments)))
    (--each (docker-images-selection)
      (docker-rmi it (-contains? args "-f") (-contains? args "--no-prune")))
    (tabulated-list-revert)))

(defun docker-images-pull-selection ()
  "Run `docker-pull' on the images selection."
  (interactive)
  (let ((args (docker-images-pull-arguments)))
    (--each (docker-images-selection)
      (docker-pull it (-contains? args "-a")))
    (tabulated-list-revert)))

(defun docker-images-push-selection ()
  "Run `docker-push' on the images selection."
  (interactive)
  (let ((args (s-join " " (docker-images-rmi-arguments))))
    (--each (docker-images-selection)
      (docker "push" args it))
    (tabulated-list-revert)))

(defun docker-images-run-selection (command)
  "Run `docker-run' on the images selection."
  (interactive "sCommand: ")
  (let ((args (s-join " " (docker-images-run-arguments))))
    (--each (docker-images-selection)
      (async-shell-command (format "docker run %s %s %s" args it command) (format "*run %s*" it)))
    (tabulated-list-revert)))

(magit-define-popup docker-images-rmi-popup
  "Popup for removing images."
  'docker-images-popups
  :man-page "docker-rmi"
  :switches '((?f "Force" "-f")
              (?n "Don't prune" "--no-prune"))
  :actions  '((?D "Remove" docker-images-rmi-selection)))

(magit-define-popup docker-images-pull-popup
  "Popup for pulling images."
  'docker-images-popups
  :man-page "docker-pull"
  :switches '((?a "All" "-a"))
  :actions  '((?F "Pull" docker-images-pull-selection)))

(magit-define-popup docker-images-push-popup
  "Popup for pushing images."
  'docker-images-popups
  :man-page "docker-push"
  :actions  '((?P "Push" docker-images-push-selection)))

;;-v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY=unix$DISPLAY
(magit-define-popup docker-images-run-popup
  "Popup for running images."
  'docker-images-popups
  :man-page "docker-run"
  :switches '((?d "Daemonize" "-d")
              (?i "Interactive" "-i")
              (?t "TTY" "-t")
              (?r "Remove" "--rm"))
  :actions  '((?R "Run images" docker-images-run-selection))
  :default-arguments '("-i" "-t" "--rm"))

(defun docker-images-refresh ()
  (setq tabulated-list-entries (-map 'docker-image-to-tabulated-list (docker-get-images))))

(defvar docker-images-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "D" 'docker-images-rmi-popup)
    (define-key map "F" 'docker-images-pull-popup)
    (define-key map "P" 'docker-images-push-popup)
    (define-key map "R" 'docker-images-run-popup)
    map)
  "Keymap for `docker-images-mode'.")

;;;###autoload
(defun docker-images ()
  "List docker images."
  (interactive)
  (pop-to-buffer "*docker-images*")
  (docker-images-mode)
  (docker-images-refresh)
  (tabulated-list-revert))

(define-derived-mode docker-images-mode tabulated-list-mode "Images Menu"
  "Major mode for handling a list of docker images."
  (setq tabulated-list-format [
                               ("Id" 16 t)
                               ("Repository" 30 t)
                               ("Tag" 20 t)
                               ("Created" 15 t)
                               ("Size" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Repository" nil))
  (add-hook 'tabulated-list-revert-hook 'docker-images-refresh nil t)
  (tabulated-list-init-header)
  (tle-mode))

(provide 'docker-images)

;;; docker-images.el ends here
