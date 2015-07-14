;;; docker-containers.el --- Emacs interface to docker-containers

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
(require 'tabulated-list-ext)

(require 'eieio)
(require 'magit-popup)

(defclass docker-container ()
  ((id           :initarg :id           :initform nil)
   (image        :initarg :image        :initform nil)
   (command      :initarg :command      :initform nil)
   (created      :initarg :created      :initform nil)
   (status       :initarg :status       :initform nil)
   (ports        :initarg :ports        :initform nil)
   (names        :initarg :names        :initform nil)))

(defmethod docker-container-name ((this docker-container))
  "Return the container name."
  (oref this :names))

(defmethod docker-container-to-tabulated-list ((this docker-container))
  "Convert `docker-container' to tabulated list."
  (list (oref this :id)
        `[,(oref this :id)
          ,(oref this :image)
          ,(oref this :command)
          ,(oref this :created)
          ,(oref this :status)
          ,(oref this :ports)
          ,(oref this :names)]))

(defun make-docker-container (id image command created status ports names &rest unused)
  "Helper to create a `eieio` docker container object."
  (docker-container id :id id :image image :command command :created created :status status :ports ports :names names))

(defun docker-container-parse (line)
  "Convert LINE from 'docker containers' to `docker-container'."
  (apply #'make-docker-container (s-split " \\{3,15\\}" line)))

(defun docker-container-names ()
  "Return the list of container names."
  (--map (docker-container-name it) (docker-get-containers)))

(defun docker-read-container-name (prompt)
  "Read an container name."
  (completing-read prompt (docker-container-names)))

(defun docker-start (name)
  "Start a container."
  (interactive (list (docker-read-container-name "Start container: ")))
  (docker "start" name))

(defun docker-stop (name &optional timeout)
  "Stop a container."
  (interactive (list (docker-read-container-name "Stop container: ")))
  (docker "stop" name (when timeout timeout)))

(defun docker-restart (name &optional timeout)
  "Restart a container."
  (interactive (list (docker-read-container-name "Restart container: ")))
  (docker "restart" name (when timeout timeout)))

(defun docker-pause (name)
  "Pause a container."
  (interactive (list (docker-read-container-name "Pause container: ")))
  (docker "pause" name))

(defun docker-unpause (name)
  "Unpause a container."
  (interactive (list (docker-read-container-name "Unpause container: ")))
  (docker "unpause" name))

(defun docker-rm (name &optional force)
  "Destroy or uncommand an container."
  (interactive (list (docker-read-container-name "Delete container: ") current-prefix-arg))
  (docker "rm" name (when force "-f")))

(defun docker-get-containers (&optional all quiet filters)
  "Get containers as eieio objects."
  (let* ((data (docker-get-containers-raw all quiet filters))
         (lines (s-split "\n" data t))
         (lines (cdr lines)))
    (-map 'docker-container-parse lines)))

(defun docker-get-containers-raw (&optional all quiet filters)
  "Equivalent of \"docker containers\"."
  (docker "ps" (when all "-a ") (when quiet "-q ") (when filters (s-join " --filter=" filters))))

(defun docker-containers-selection ()
  "Get the containers selection as a list of names."
  (save-excursion
    (goto-char (point-min))
    (let ((selection ()))
      (while (not (eobp))
        (when (eq (char-after) ?*)
          (add-to-list 'selection (tabulated-list-get-id)))
        (forward-line))
      (when (null selection)
        (error "No containers selected."))
      selection)))

(defun docker-containers-start-selection ()
  "Run `docker-start' on the containers selection."
  (interactive)
  (let ((args (s-join " " (docker-containers-rm-arguments))))
    (--each (docker-containers-selection)
      (docker "start" it))
    (tabulated-list-revert)))

(defun docker-containers-stop-selection ()
  "Run `docker-stop' on the containers selection."
  (interactive)
  (let ((args (docker-containers-stop-arguments)))
    (--each (docker-containers-selection)
      (docker-stop it args))
    (tabulated-list-revert)))

(defun docker-containers-restart-selection ()
  "Run `docker-restart' on the containers selection."
  (interactive)
  (let ((args (docker-containers-restart-arguments)))
    (--each (docker-containers-selection)
      (docker-restart it args))
    (tabulated-list-revert)))

(defun docker-containers-pause-selection ()
  "Run `docker-pause' on the containers selection."
  (interactive)
  (let ((args (docker-containers-restart-arguments)))
    (--each (docker-containers-selection)
      (docker-pause it))
    (tabulated-list-revert)))

(defun docker-containers-rm-selection ()
  "Run `docker-rm' on the containers selection."
  (interactive)
  (let ((args (docker-containers-rm-arguments)))
    (--each (docker-containers-selection)
      (docker-rm it (-contains? args "-f")))
    (tabulated-list-revert)))

(magit-define-popup docker-containers-start-popup
  "Popup for starting containers."
  'docker-containers-popups
  :man-page "docker-start"
  :actions  '((?S "Start" docker-containers-start-selection)))

(magit-define-popup docker-containers-stop-popup
  "Popup for stoping containers."
  'docker-containers-popups
  :man-page "docker-stop"
  :options '((?t "Timeout" "--time="))
  :actions  '((?O "Stop" docker-containers-stop-selection)))

(magit-define-popup docker-containers-restart-popup
  "Popup for restarting containers."
  'docker-containers-popups
  :man-page "docker-restart"
  :options '((?t "Timeout" "--time="))
  :actions  '((?R "Restart" docker-containers-restart-selection)))

(magit-define-popup docker-containers-pause-popup
  "Popup for pauseing containers."
  'docker-containers-popups
  :man-page "docker-pause"
  :actions  '((?P "Pause" docker-containers-pause-selection)))

(magit-define-popup docker-containers-unpause-popup
  "Popup for unpauseing containers."
  'docker-containers-popups
  :man-page "docker-unpause"
  :actions  '((?P "Unpause" docker-containers-unpause-selection)))

(magit-define-popup docker-containers-rm-popup
  "Popup for removing containers."
  'docker-containers-popups
  :man-page "docker-rm"
  :switches '((?f "Force" "--force=true"))
  :actions  '((?D "Remove" docker-containers-rm-selection)))

(defun docker-containers-refresh ()
  (setq tabulated-list-entries (-map 'docker-container-to-tabulated-list (docker-get-containers t))))

(defvar docker-containers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "S" 'docker-containers-start-popup)
    (define-key map "O" 'docker-containers-stop-popup)
    (define-key map "R" 'docker-containers-restart-popup)
    (define-key map "P" 'docker-containers-pause-popup)
    (define-key map "U" 'docker-containers-unpause-popup)
    (define-key map "D" 'docker-containers-rm-popup)
    map)
  "Keymap for `docker-containers-mode'.")

;;;###autoload
(defun docker-containers ()
  "List docker containers."
  (interactive)
  (pop-to-buffer "*docker-containers*")
  (docker-containers-mode)
  (docker-containers-refresh)
  (tabulated-list-revert))

(defalias 'docker-ps 'docker-containers)

(define-derived-mode docker-containers-mode tabulated-list-ext-mode "Containers Menu"
  "Major mode for handling a list of docker containers."
  (setq tabulated-list-format [
                               ("Id" 16 t)
                               ("Image" 15 t)
                               ("Command" 30 t)
                               ("Created" 15 t)
                               ("Status" 20 t)
                               ("Ports" 10 t)
                               ("Names" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Image" nil))
  (add-hook 'tabulated-list-revert-hook 'docker-containers-refresh nil t)
  (tabulated-list-init-header))

(provide 'docker-containers)

;;; docker-containers.el ends here
