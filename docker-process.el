;;; docker-process.el --- Docker process  -*- lexical-binding: t -*-

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
(require 'aio)
(require 'dash)

(require 'docker-group)
(require 'docker-utils)

(defcustom docker-run-as-root nil
  "Run docker as root."
  :group 'docker
  :type 'boolean)

(defcustom docker-show-messages t
  "If non-nil `message' docker commands which are run."
  :group 'docker
  :type 'boolean)

(defcustom docker-run-async-with-buffer-function (if (featurep 'vterm)
                                                     'docker-run-async-with-buffer-vterm
                                                   'docker-run-async-with-buffer-shell)
  "Function used to run a program with a live buffer attached to it."
  :group 'docker
  :type 'symbol)


(defmacro docker-with-sudo (&rest body)
  "Ensure `default-directory' is set correctly according to `docker-run-as-root' then execute BODY."
  (declare (indent defun))
  `(let ((default-directory (if (and docker-run-as-root (not (file-remote-p default-directory)))
                                "/sudo::"
                              default-directory)))
     ,@body))

(defun docker-run-start-file-process-shell-command (program &rest args)
  "Execute \"PROGRAM ARGS\" and return the process."
  (docker-with-sudo
    (let* ((process-args (-remove 's-blank? (-flatten args)))
           (command (s-join " " (-insert-at 0 program process-args))))
      (when docker-show-messages (message "Running: %s" command))
      (start-file-process-shell-command command (apply #'docker-utils-generate-new-buffer-name program process-args) command))))

(defun docker-run-async (program &rest args)
  "Execute \"PROGRAM ARGS\" and return a promise with the results."
  (let* ((process (apply #'docker-run-start-file-process-shell-command program args))
         (promise (aio-promise)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process (-partial #'docker-process-sentinel promise))
    promise))

(defun docker-run-async-with-buffer (program &rest args)
  "Execute \"PROGRAM ARGS\" and display output in a new buffer."
   (apply docker-run-async-with-buffer-function program args))

(defun docker-run-async-with-buffer-shell (program &rest args)
  "Execute \"PROGRAM ARGS\" and display output in a new `shell' buffer."
  (let* ((process (apply #'docker-run-start-file-process-shell-command program args))
         (buffer (process-buffer process)))
    (set-process-query-on-exit-flag process nil)
    (with-current-buffer buffer (shell-mode))
    (set-process-filter process 'comint-output-filter)
    (switch-to-buffer-other-window buffer)))

(defun docker-run-async-with-buffer-vterm (program &rest args)
  "Execute \"PROGRAM ARGS\" and display output in a new `vterm' buffer."
  (defvar vterm-kill-buffer-on-exit)
  (defvar vterm-shell)
  (if (fboundp 'vterm-other-window)
      (let* ((process-args (-remove 's-blank? (-flatten args)))
             (vterm-shell (s-join " " (-insert-at 0 program process-args)))
             (vterm-kill-buffer-on-exit nil))
        (vterm-other-window
         (apply #'docker-utils-generate-new-buffer-name program process-args)))
    (error "The vterm package is not installed")))

(defun docker-process-sentinel (promise process event)
  "Sentinel that resolves the PROMISE using PROCESS and EVENT."
  (when (memq (process-status process) '(exit signal))
    (setq event (substring event 0 -1))
    (if (not (string-equal event "finished"))
        (error "Error running: \"%s\" (%s)" (process-name process) event)
      (aio-resolve promise
                   (lambda ()
                     (when docker-show-messages
                       (message "Finished: %s" (process-name process)))
                     (run-with-timer 2 nil (lambda () (message nil)))
                     (with-current-buffer (process-buffer process)
                       (prog1 (buffer-substring-no-properties (point-min) (point-max))
                         (kill-buffer))))))))

(provide 'docker-process)

;;; docker-process.el ends here
