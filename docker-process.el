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
(require 'ansi-color)
(require 's)
(require 'aio)
(require 'dash)

(require 'docker-group)
(require 'docker-utils)

(declare-function ghostel-exec "ghostel")

(defcustom docker-run-as-root nil
  "Obsolete; open docker.el from a sudo directory instead.
See `docker-open-dired-as-root'."
  :group 'docker
  :type 'boolean)

(make-obsolete-variable 'docker-run-as-root
                        "open docker.el from a sudo directory, see `docker-open-dired-as-root'."
                        "2.6.0")

(defcustom docker-show-messages t
  "If non-nil `message' docker commands which are run."
  :group 'docker
  :type 'boolean)

(defcustom docker-terminal-backend 'auto
  "Terminal backend used for commands that need a live buffer.
When set to `auto', prefer eat, then ghostel, then vterm, then shell."
  :group 'docker
  :type '(choice (const :tag "Auto (eat > ghostel > vterm > shell)" auto)
                 (const :tag "Eat" eat)
                 (const :tag "Ghostel" ghostel)
                 (const :tag "Vterm" vterm)
                 (const :tag "Shell" shell)))

(defcustom docker-run-async-with-buffer-function nil
  "Obsolete; use `docker-terminal-backend' instead."
  :group 'docker
  :type 'symbol)

(make-obsolete-variable 'docker-run-async-with-buffer-function 'docker-terminal-backend "2.5.0")

(defmacro docker-with-sudo (&rest body)
  "Set `default-directory' according to `docker-run-as-root', then execute BODY."
  (declare (indent defun))
  `(let ((default-directory (if (and (with-suppressed-warnings ((obsolete docker-run-as-root))
                                       docker-run-as-root)
                                     (not (file-remote-p default-directory)))
                                "/sudo::"
                              default-directory)))
     ,@body))

(defun docker--process-command (program args)
  "Return the shell command line running PROGRAM with ARGS.

ARGS may nest lists and hold blank strings, which are dropped."
  (s-join " " (cons program (-remove #'s-blank? (-flatten args)))))

(defun docker-run-start-file-process-shell-command (program &rest args)
  "Execute \"PROGRAM ARGS\" and return the process."
  (docker-with-sudo
    (let ((command (docker--process-command program args)))
      (when docker-show-messages (message "Running: %s" command))
      (start-file-process-shell-command command (docker-utils-generate-new-buffer-name command) command))))

(defun docker--start-process-with-stderr (program args)
  "Run PROGRAM with ARGS, returning a process with separate stderr."
  (docker-with-sudo
    (let* ((command (docker--process-command program args))
           (directory default-directory)
           (stderr-file (make-nearby-temp-file "docker-stderr-"))
           (buffer (docker-utils-generate-new-buffer command))
           process)
      (unwind-protect
          (progn
            (with-current-buffer buffer (setq default-directory directory))
            (when docker-show-messages (message "Running: %s" command))
            (setq process
                  (start-file-process-shell-command
                   command buffer
                   (format "( %s ) 2> %s" command
                           (shell-quote-argument (file-local-name stderr-file)))))
            (process-put process 'docker-stderr-file stderr-file)
            (process-put process 'docker-command command)
            (process-put process 'docker-directory directory)
            process)
        (unless process
          (unwind-protect (delete-file stderr-file)
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(defun docker-run-async (program &rest args)
  "Execute \"PROGRAM ARGS\" and return a promise with standard output.
On failure, display diagnostics and reject the promise with stderr."
  (let ((process (docker--start-process-with-stderr program args))
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
    ('ghostel (or (fboundp 'ghostel-exec) (fboundp 'ghostel)))
    ('vterm (fboundp 'vterm-other-window))
    ('shell t)
    (_ nil)))

(defun docker--terminal-backend ()
  "Return the selected backend symbol."
  (pcase docker-terminal-backend
    ('auto (cond
            ((docker--terminal-backend-available-p 'eat) 'eat)
            ((docker--terminal-backend-available-p 'ghostel) 'ghostel)
            ((docker--terminal-backend-available-p 'vterm) 'vterm)
            (t 'shell)))
    (_ docker-terminal-backend)))

(defun docker-run-async-with-buffer-dispatch (backend program interactive &rest args)
  "Dispatch PROGRAM to BACKEND and display output in a new buffer."
  (pcase backend
    ('eat (if (docker--terminal-backend-available-p 'eat)
              (apply #'docker-run-async-with-buffer-eat program interactive args)
            (error "The eat package is not installed")))
    ('ghostel (if (docker--terminal-backend-available-p 'ghostel)
                  (apply #'docker-run-async-with-buffer-ghostel program interactive args)
                (error "The ghostel package is not installed")))
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
    (defvar vterm-shell)
    (if (fboundp 'vterm-other-window)
        (let ((vterm-shell (docker--process-command program args)))
          (vterm-other-window (docker-utils-generate-new-buffer-name vterm-shell)))
      (error "The vterm package is not installed"))))

(defun docker-run-async-with-buffer-eat (program &optional interactive &rest args)
  "Execute \"PROGRAM ARGS\" and display output in a new `eat' buffer.
If INTERACTIVE is nil, fall back to shell mode since eat is interactive."
  (if (not interactive)
      (apply #'docker-run-async-with-buffer-shell program nil args)
    (defvar eat-buffer-name)
    (if (fboundp 'eat-other-window)
        (let* ((command (docker--process-command program args))
               (eat-buffer-name (docker-utils-generate-new-buffer-name command)))
          (eat-other-window command))
      (error "The eat package is not installed"))))

(defun docker-run-async-with-buffer-ghostel (program &optional interactive &rest args)
  "Execute \"PROGRAM ARGS\" and display output in a new `ghostel' buffer.
If INTERACTIVE is nil, fall back to shell mode since ghostel is interactive."
  (if (not interactive)
      (apply #'docker-run-async-with-buffer-shell program nil args)
    (unless (fboundp 'ghostel-exec)
      (require 'ghostel nil t))
    (if (fboundp 'ghostel-exec)
        (let* ((command (docker--process-command program args))
               (buffer (docker-utils-generate-new-buffer command)))
          ;; `ghostel-exec' shell-quotes the program and each argument separately,
          ;; so the command goes through a shell like it does in the other backends.
          ;; Display first so the terminal is sized to the window.
          (switch-to-buffer-other-window buffer)
          ;; Like `start-file-process-shell-command', use the shell of the host
          ;; `default-directory' is on.
          (with-connection-local-variables
           (ghostel-exec buffer shell-file-name (list shell-command-switch command))))
      (error "The ghostel package is not installed"))))

(defun docker-process-filter-noninteractive (proc string)
  "Insert STRING from PROC into the buffer PROC writes to.

Carriage returns are stripped and ANSI color codes are applied, which is what
a non-interactive streaming buffer needs."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert (ansi-color-apply (replace-regexp-in-string "\r" "" string)))
          (set-marker (process-mark proc) (point)))
        (when moving (goto-char (process-mark proc)))))))

(defun docker--resolve-promise (promise value-function)
  "Resolve PROMISE with VALUE-FUNCTION, whatever `timer-list' is bound to.

`aio-resolve' runs the callbacks of PROMISE from a timer.  Tramp let-binds
`timer-list' while it talks to a connection, and a sentinel can run inside
that binding: the timer it starts there is dropped when the binding is
unwound, and the callbacks never run.  So the timer is started in the
top-level `timer-list'."
  (let ((timer-list (default-toplevel-value 'timer-list)))
    (aio-resolve promise value-function)
    (set-default-toplevel-value 'timer-list timer-list)))

(defun docker-process-sentinel (promise process _event)
  "Resolve PROMISE from PROCESS, retaining diagnostics on failure."
  (when (and (memq (process-status process) '(exit signal))
             (not (process-get process 'docker-completed)))
    (process-put process 'docker-completed t)
    (let* ((buffer (process-buffer process))
           (command (or (process-get process 'docker-command)
                        (process-name process)))
           (directory (or (process-get process 'docker-directory)
                          default-directory))
           (code (process-exit-status process))
           (stderr-file (process-get process 'docker-stderr-file))
           (stdout (if (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (buffer-substring-no-properties (point-min) (point-max)))
                     ""))
           (stderr "")
           (capture-error (unless (buffer-live-p buffer) "Output buffer was killed")))
      (when stderr-file
        (condition-case err
            (unwind-protect
                (setq stderr (with-temp-buffer
                               (insert-file-contents stderr-file)
                               (buffer-string)))
              (delete-file stderr-file))
          (error (setq capture-error (error-message-string err)))))
      (if (and (eq (process-status process) 'exit) (= code 0)
               (not capture-error))
          (progn
            (when docker-show-messages
              (if (s-blank? stderr)
                  (progn (message "Finished: %s" command)
                         (run-with-timer 2 nil (lambda () (message nil))))
                (message "Finished: %s\n%s" command (s-trim stderr))))
            (kill-buffer buffer)
            (docker--resolve-promise promise (lambda () stdout)))
        (unless (buffer-live-p buffer)
          (setq buffer (docker-utils-generate-new-buffer command))
          (set-process-buffer process buffer))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Command: %s\nDirectory: %s\nExit: %s (%s)\n\n"
                            command directory (process-status process) code))
            (when capture-error
              (insert (format "Capture error: %s\n\n" capture-error)))
            (insert "Standard output:\n" stdout "\nStandard error:\n" stderr)
            (setq default-directory directory)
            (special-mode)))
        (condition-case err
            (display-buffer buffer docker-pop-to-buffer-action)
          (error (message "Cannot display Docker output: %s"
                          (error-message-string err))))
        (let ((diagnostic (format "Error running: \"%s\" (exit %s). %s See %s"
                                  command code
                                  (or capture-error (s-trim stderr))
                                  (buffer-name buffer))))
          (docker--resolve-promise promise (lambda () (error "%s" diagnostic))))))))

(provide 'docker-process)

;;; docker-process.el ends here
