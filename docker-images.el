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

(require 'docker)

(require 'eieio)
(require 'magit-popup)

(defclass docker-image ()
  ((id           :initarg :id           :initform nil)
   (repository   :initarg :repository   :initform nil)
   (tag          :initarg :tag          :initform nil)
   (size         :initarg :size         :initform nil)
   (created      :initarg :created      :initform nil)))

(defmethod docker-image-name ((this docker-image))
  "Return the repository:name image name."
  (format "%s:%s" (oref this :repository) (oref this :tag)))

(defmethod docker-image-to-etable ((this docker-image))
  "Convert `docker-image' to ETable."
  `[
   ,(oref this :id)
   ,(oref this :repository)
   ,(oref this :tag)
   ,(oref this :created)
   ,(oref this :size)
   ])

(defun make-docker-image (repository tag id created size)
  "Helper to create a `eieio` docker image object."
  (docker-image id :id id :repository repository :tag tag :size size :created created))

(defun docker-image-parse (line)
  "Convert LINE from 'docker images' to `docker-image'."
  (apply #'make-docker-image (cdr (s-match "^\\([^ ]+\\) +\\([^ ]+\\) +\\([_[:alnum:]]+\\) +\\([_ [:alnum:]]+ ago\\) +\\(.*\\)$" line))))

(defun docker-image-names ()
  "Return the list of image names."
  (--map (docker-image-name it) (docker-get-images)))

(defun docker-read-image-name ()
  "Read an image name."
  (completing-read "Image: " (docker-image-names) nil t))

(defun docker-pull (name &optional all)
  "Pull an image."
  (interactive (list (docker-read-image-name) current-prefix-arg))
  (docker "pull" (when all "-a ") name))

(defun docker-push (name)
  "Push an image."
  (interactive (list (docker-read-image-name)))
  (docker "push" name))

(defun docker-rmi (name &optional force no-prune)
  "Destroy or untag an image."
  (interactive (list (docker-read-image-name) current-prefix-arg))
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
  "Get the images selection as a list of names or SHA1."
  (let* ((table (etable-at-point 'etable))
         (selection (etable-get-selection-model table))
         (model (etable-get-table-model table)))
    (--map (if (equal (etable-get-value-at model it 1) "<none>")
               (etable-get-value-at model it 0)
             (format "%s:%s" (etable-get-value-at model it 1) (etable-get-value-at model it 2)))
           (--filter (etable-selected-index-p selection it)
                     (number-sequence 0 (etable-get-row-count (etable-get-table-model table)))))))

(defun docker-images-rmi-selection ()
  "Run `docker-rmi' on the images selection."
  (interactive)
  (let ((args (docker-images-rmi-arguments)))
    (--each (docker-images-selection)
      (docker-rmi it (-contains? args "-f") (-contains? args "--no-prune"))))
  (kill-this-buffer)
  (docker-images))

(defun docker-images-pull-selection ()
  "Run `docker-pull' on the images selection."
  (interactive)
  (let ((args (docker-images-pull-arguments)))
    (--each (docker-images-selection)
      (docker-pull it (-contains? args "-a"))))
  (kill-this-buffer)
  (docker-images))

(defun docker-images-push-selection ()
  "Run `docker-push' on the images selection."
  (interactive)
  (let ((args (s-join " " (docker-images-rmi-arguments))))
    (--each (docker-images-selection)
      (docker "push" args it)))
  (kill-this-buffer)
  (docker-images))

(defun docker-images-run-selection ()
  "Run `docker-run' on the images selection."
  (interactive)
  (--each (docker-images-selection)
    (docker-run it))
  (kill-this-buffer)
  (docker-images))

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
  :actions  '((?F "Push" docker-images-push-selection)))

;;-v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY=unix$DISPLAY
(magit-define-popup docker-images-run-popup
  "Popup for running images."
  'docker-images-popups
  :man-page "docker"
  :switches '((?i "Interactive" "-i")
              (?t "TTY" "-t")
              (?r "Remove" "--rm"))
  :options  '((?c "Command to run" "--command=" read-shell-command))
  :actions  '((?R "Run images" docker-images-run-selection))
  :default-arguments '("-i" "-t" "--command=bash"))

(defvar docker-images-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "D" 'docker-images-rmi-popup)
    (define-key map "F" 'docker-images-pull-popup)
    (define-key map "P" 'docker-images-push-popup)
    (define-key map "R" 'docker-images-run-popup)
    (define-key map "q" 'kill-this-buffer)
    map)
  "Keymap for `docker-images-mode'.")

(define-minor-mode docker-images-mode
  "Minor mode for `docker-images'."
  nil
  " docker-images"
  docker-images-mode-map)

;;;###autoload
(defun docker-images ()
  "List docker images."
  (interactive)
  (let* ((images (docker-get-images))
         (images-etable (etable-create-table (-map #'docker-image-to-etable images))))
    (with-current-buffer-window "*docker-images*"
                                nil
                                nil
                                (insert "---------- DOCKER IMAGES ----------\n\n")
                                (etable-draw images-etable (point))
                                (docker-images-mode))))

(provide 'docker-images)

;;; docker-images.el ends here
