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
(eval-when-compile
  (setq-local byte-compile-warnings '(not docstrings)))

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

(defcustom docker-terminal-backend 'auto
  "Terminal backend used for commands that need a live buffer.
When set to `auto', prefer eat, then vterm, then shell."
  :group 'docker
  :type '(choice (const :tag "Auto (eat > vterm > shell)" auto)
                 (const :tag "Eat" eat)
                 (const :tag "Vterm" vterm)
                 (const :tag "Shell" shell)))

(defcustom docker-run-async-with-buffer-function nil
  "Obsolete; use `docker-terminal-backend' instead."
  :group 'docker
  :type 'symbol)

(make-obsolete-variable 'docker-run-async-with-buffer-function 'docker-terminal-backend "2.5.0")


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

(defun docker-run-async-with-buffer (program interactive &rest args)
  "Execute \"PROGRAM ARGS\" and display output in a new buffer.
INTERACTIVE selects an interactive terminal buffer when non-nil.
Prefer `docker-run-async-with-buffer-interactive' or
`docker-run-async-with-buffer-noninteractive'."
  (if docker-run-async-with-buffer-function
      (apply docker-run-async-with-buffer-function program interactive args)
    (apply #'docker-run-async-with-buffer-dispatch
           (docker--terminal-backend)
           program interactive args)))

(defun docker-run-async-with-buffer-interactive (program &rest args)
  "Execute \"PROGRAM ARGS\" and display output in an interactive buffer."
  (apply #'docker-run-async-with-buffer program t args))

(defun docker-run-async-with-buffer-noninteractive (program &rest args)
  "Execute \"PROGRAM ARGS\" and display output in a non-interactive buffer."
  (apply #'docker-run-async-with-buffer program nil args))

(defun docker--terminal-backend-available-p (backend)
  "Return non-nil when BACKEND is available."
  (pcase backend
    ('eat (fboundp 'eat-other-window))
    ('vterm (fboundp 'vterm-other-window))
    ('shell t)
    (_ nil)))

(defun docker--terminal-backend ()
  "Return the selected backend symbol."
  (pcase docker-terminal-backend
    ('auto (cond
            ((docker--terminal-backend-available-p 'eat) 'eat)
            ((docker--terminal-backend-available-p 'vterm) 'vterm)
            (t 'shell)))
    (_ docker-terminal-backend)))

(defun docker-run-async-with-buffer-dispatch (backend program interactive &rest args)
  "Dispatch PROGRAM to BACKEND and display output in a new buffer."
  (pcase backend
    ('eat (if (docker--terminal-backend-available-p 'eat)
              (apply #'docker-run-async-with-buffer-eat program interactive args)
            (error "The eat package is not installed")))
    ('vterm (if (docker--terminal-backend-available-p 'vterm)
                (apply #'docker-run-async-with-buffer-vterm program interactive args)
              (error "The vterm package is not installed")))
    ('shell (apply #'docker-run-async-with-buffer-shell program interactive args))
    (_ (error "Unsupported docker terminal backend: %s" backend))))

(defun docker-run-async-with-buffer-shell (program &optional interactive &rest args)
  "Execute \"PROGRAM ARGS\" and display output in a new buffer.
If INTERACTIVE is non-nil, use a `shell' buffer for interactive use.
Otherwise, use a non-interactive buffer with ANSI color support."
  (let* ((process (apply #'docker-run-start-file-process-shell-command program args))
         (buffer (process-buffer process)))
    (set-process-query-on-exit-flag process nil)
    (if interactive
        (with-current-buffer buffer (shell-mode))
      (with-current-buffer buffer (special-mode)))
    (set-process-filter process
                        (if interactive
                            'comint-output-filter
                          'docker-process-filter-noninteractive))
    (switch-to-buffer-other-window buffer)))

(defun docker-run-async-with-buffer-vterm (program &optional interactive &rest args)
  "Execute \"PROGRAM ARGS\" and display output in a new `vterm' buffer.
If INTERACTIVE is nil, fall back to shell mode since vterm is interactive."
  (if (not interactive)
      ;; vterm is interactive only, fall back to shell for non-interactive output
      (apply #'docker-run-async-with-buffer-shell program nil args)
    (defvar vterm-kill-buffer-on-exit)
    (defvar vterm-shell)
    (if (fboundp 'vterm-other-window)
        (let* ((process-args (-remove 's-blank? (-flatten args)))
               (vterm-shell (s-join " " (-insert-at 0 program process-args))))
          (vterm-other-window
           (apply #'docker-utils-generate-new-buffer-name program process-args)))
      (error "The vterm package is not installed"))))

(defun docker-run-async-with-buffer-eat (program &optional interactive &rest args)
  "Execute \"PROGRAM ARGS\" and display output in a new `eat' buffer.
If INTERACTIVE is nil, fall back to shell mode since eat is interactive."
  (if (not interactive)
      (apply #'docker-run-async-with-buffer-shell program nil args)
    (defvar eat-buffer-name)
    (if (fboundp 'eat-other-window)
        (let* ((process-args (-remove 's-blank? (-flatten args)))
               (command (s-join " " (-insert-at 0 program process-args)))
               (eat-buffer-name (apply #'docker-utils-generate-new-buffer-name
                                       program process-args)))
          (eat-other-window command))
      (error "The eat package is not installed"))))

(defun docker-process-filter-noninteractive (proc string)
  "Process filter for non-interactive streaming buffers.
Strips carriage returns and applies ANSI color codes."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert (ansi-color-apply (replace-regexp-in-string "\r" "" string)))
          (set-marker (process-mark proc) (point)))
        (when moving (goto-char (process-mark proc)))))))

(defun docker-process-sentinel (promise process event)
  "Sentinel that resolves the PROMISE using PROCESS and EVENT."
  (when (memq (process-status process) '(exit signal))
    (setq event (substring event 0 -1))
    (if (not (string-equal event "finished"))
        (aio-resolve promise
                     (lambda ()
                       (error "Error running: \"%s\" (%s)" (process-name process) event)))
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
