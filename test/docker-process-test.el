;;; docker-process-test.el --- Tests for docker-process  -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the terminal backend selection and dispatch in `docker-process'.

;;; Code:
(require 'ert)
(require 'docker-process)

(defmacro docker-process-test-with-backends (available &rest body)
  "Evaluate BODY with the terminal backends in AVAILABLE defined.

AVAILABLE is a list of backend symbols among `eat', `ghostel' and `vterm'.
The backends left out keep whatever definition the running Emacs has, which
in batch is none."
  (declare (indent 1))
  `(cl-letf ,(--map (list (list 'symbol-function (list 'quote it)) '#'ignore)
                    (--map (pcase it
                             ('eat 'eat-other-window)
                             ('ghostel 'ghostel-exec)
                             ('vterm 'vterm-other-window))
                           (cadr available)))
     ,@body))

(ert-deftest docker-process-test-backend-available-p ()
  (docker-process-test-with-backends '(eat ghostel vterm)
    (should (docker--terminal-backend-available-p 'eat))
    (should (docker--terminal-backend-available-p 'ghostel))
    (should (docker--terminal-backend-available-p 'vterm))))

(ert-deftest docker-process-test-shell-backend-is-always-available ()
  (should (docker--terminal-backend-available-p 'shell)))

(ert-deftest docker-process-test-unknown-backend-is-never-available ()
  (should-not (docker--terminal-backend-available-p 'quux)))

(ert-deftest docker-process-test-missing-backends-are-unavailable ()
  (should-not (docker--terminal-backend-available-p 'eat))
  (should-not (docker--terminal-backend-available-p 'ghostel))
  (should-not (docker--terminal-backend-available-p 'vterm)))

(ert-deftest docker-process-test-auto-prefers-eat ()
  (let ((docker-terminal-backend 'auto))
    (docker-process-test-with-backends '(eat ghostel vterm)
      (should (equal (docker--terminal-backend) 'eat)))))

(ert-deftest docker-process-test-auto-falls-back-to-ghostel ()
  (let ((docker-terminal-backend 'auto))
    (docker-process-test-with-backends '(ghostel vterm)
      (should (equal (docker--terminal-backend) 'ghostel)))))

(ert-deftest docker-process-test-auto-falls-back-to-vterm ()
  (let ((docker-terminal-backend 'auto))
    (docker-process-test-with-backends '(vterm)
      (should (equal (docker--terminal-backend) 'vterm)))))

(ert-deftest docker-process-test-auto-falls-back-to-shell ()
  (let ((docker-terminal-backend 'auto))
    (should (equal (docker--terminal-backend) 'shell))))

(ert-deftest docker-process-test-explicit-backend-is-returned-as-is ()
  (dolist (backend '(eat ghostel vterm shell))
    (let ((docker-terminal-backend backend))
      (should (equal (docker--terminal-backend) backend)))))

(ert-deftest docker-process-test-dispatch-routes-to-the-backend ()
  (let (called)
    (cl-letf (((symbol-function 'docker-run-async-with-buffer-shell)
               (lambda (&rest args) (setq called (cons 'shell args)))))
      (docker-run-async-with-buffer-dispatch 'shell "docker" t "ps")
      (should (equal called '(shell "docker" t "ps"))))))

(ert-deftest docker-process-test-dispatch-rejects-an-unknown-backend ()
  (should-error (docker-run-async-with-buffer-dispatch 'quux "docker" t)
                :type 'error))

(ert-deftest docker-process-test-dispatch-reports-a-missing-backend ()
  (dolist (backend '(eat ghostel vterm))
    (should (equal (cadr (should-error (docker-run-async-with-buffer-dispatch backend "docker" t)))
                   (format "The %s package is not installed" backend)))))

(ert-deftest docker-process-test-process-command-flattens-and-drops-blanks ()
  (should (equal (docker--process-command "docker" '("run" ("-p 80:80" "") "" "alpine"))
                 "docker run -p 80:80 alpine")))

(ert-deftest docker-process-test-vterm-backend-builds-one-command ()
  (defvar vterm-shell)
  (let (captured)
    (cl-letf (((symbol-function 'vterm-other-window)
               (lambda (name) (setq captured (cons vterm-shell name)))))
      (docker-run-async-with-buffer-vterm "docker" t "run" '("-p 80:80" "") "alpine"))
    (should (equal captured '("docker run -p 80:80 alpine" . "* docker run -p 80:80 alpine *")))))

(ert-deftest docker-process-test-eat-backend-builds-one-command ()
  (let (command)
    (cl-letf (((symbol-function 'eat-other-window) (lambda (arg) (setq command arg))))
      (docker-run-async-with-buffer-eat "docker" t "run" '("-p 80:80" "") "alpine")
      (should (equal command "docker run -p 80:80 alpine")))))

(ert-deftest docker-process-test-ghostel-backend-builds-one-command ()
  (let ((shell-command-switch "-lc")
        args)
    (cl-letf (((symbol-function 'ghostel-exec) (lambda (&rest rest) (setq args rest)))
              ((symbol-function 'switch-to-buffer-other-window) #'ignore))
      (docker-run-async-with-buffer-ghostel "docker compose" t "up" '("-d" "") "web"))
    (should (equal (cdr args)
                   (list shell-file-name (list "-lc" "docker compose up -d web"))))
    (should (equal (buffer-name (car args)) "* docker compose up -d web *"))
    (kill-buffer (car args))))

(ert-deftest docker-process-test-ghostel-is-probed-on-its-own-entry-points ()
  (cl-letf (((symbol-function 'ghostel-exec) #'ignore))
    (should (docker--terminal-backend-available-p 'ghostel)))
  (cl-letf (((symbol-function 'ghostel) #'ignore))
    (should (docker--terminal-backend-available-p 'ghostel))))

(ert-deftest docker-process-test-ghostel-backend-leaves-quoting-to-the-shell ()
  (let (args)
    (cl-letf (((symbol-function 'ghostel-exec) (lambda (&rest rest) (setq args rest)))
              ((symbol-function 'switch-to-buffer-other-window) #'ignore))
      (docker-run-async-with-buffer-ghostel "docker" t "container" "run" '("-v /a:/b") "alpine" "sh -c 'echo hi'"))
    (should (equal (nth 2 args) (list "-c" "docker container run -v /a:/b alpine sh -c 'echo hi'")))
    (kill-buffer (car args))))

(ert-deftest docker-process-test-ghostel-backend-uses-the-remote-shell ()
  (let ((connection-local-profile-alist nil)
        (connection-local-criteria-alist nil)
        (default-directory "/ssh:docker-test-host:/tmp/")
        args)
    (connection-local-set-profile-variables
     'docker-process-test-remote-shell
     '((shell-file-name . "/bin/remote-sh") (shell-command-switch . "-rc")))
    (connection-local-set-profiles '(:machine "docker-test-host") 'docker-process-test-remote-shell)
    (cl-letf (((symbol-function 'ghostel-exec) (lambda (&rest rest) (setq args rest)))
              ((symbol-function 'switch-to-buffer-other-window) #'ignore))
      (docker-run-async-with-buffer-ghostel "docker" t "ps"))
    (should (equal (cdr args) (list "/bin/remote-sh" (list "-rc" "docker ps"))))
    (kill-buffer (car args))))

(ert-deftest docker-process-test-noninteractive-backends-fall-back-to-shell ()
  (dolist (backend '(docker-run-async-with-buffer-eat
                     docker-run-async-with-buffer-ghostel
                     docker-run-async-with-buffer-vterm))
    (let (called)
      (cl-letf (((symbol-function 'docker-run-async-with-buffer-shell)
                 (lambda (&rest args) (setq called args))))
        (funcall backend "docker" nil "ps")
        (should (equal called '("docker" nil "ps")))))))

(ert-deftest docker-process-test-with-sudo-keeps-a-remote-directory ()
  (let ((docker-run-as-root t)
        (default-directory "/ssh:host:/srv/"))
    (should (equal (docker-with-sudo default-directory) "/ssh:host:/srv/"))))

(ert-deftest docker-process-test-with-sudo-switches-to-sudo-locally ()
  (let ((docker-run-as-root t)
        (default-directory "/tmp/"))
    (should (equal (docker-with-sudo default-directory) "/sudo::"))))

(ert-deftest docker-process-test-run-as-root-is-obsolete ()
  (should (get 'docker-run-as-root 'byte-obsolete-variable)))

(ert-deftest docker-process-test-filter-strips-carriage-returns ()
  (let ((process (make-pipe-process :name "docker-process-test" :noquery t)))
    (unwind-protect
        (with-current-buffer (get-buffer-create "*docker-process-test*")
          (set-process-buffer process (current-buffer))
          (set-marker (process-mark process) (point-max))
          (docker-process-filter-noninteractive process "one\r\ntwo\r\n")
          (should (equal (buffer-string) "one\ntwo\n")))
      (delete-process process)
      (kill-buffer "*docker-process-test*"))))

(ert-deftest docker-process-test-filter-applies-ansi-color ()
  (let ((process (make-pipe-process :name "docker-process-test" :noquery t)))
    (unwind-protect
        (with-current-buffer (get-buffer-create "*docker-process-test*")
          (set-process-buffer process (current-buffer))
          (set-marker (process-mark process) (point-max))
          (docker-process-filter-noninteractive process "\e[31mred\e[0m")
          (should (equal (buffer-string) "red")))
      (delete-process process)
      (kill-buffer "*docker-process-test*"))))

(ert-deftest docker-process-test-resolve-promise-survives-a-rebound-timer-list ()
  (let ((promise (aio-promise))
        resolved)
    (aio-listen promise (lambda (value-function) (setq resolved (funcall value-function))))
    (let (timer-list)
      (docker--resolve-promise promise (lambda () 'done)))
    (dotimes (_ 50)
      (unless resolved (accept-process-output nil 0.1)))
    (should (eq resolved 'done))))

(defun docker-process-test-await (promise)
  "Return PROMISE's value function, allowing at most ten seconds."
  (let (value-function)
    (aio-listen promise (lambda (value) (setq value-function value)))
    (let ((deadline (+ (float-time) 10)))
      (while (and (not value-function) (< (float-time) deadline))
        (accept-process-output nil 0.02)))
    (should value-function)
    value-function))

(defmacro docker-process-test-capture (command &rest body)
  "Run COMMAND safely, exposing process, promise and display to BODY."
  (declare (indent 1))
  `(let* ((docker-show-messages nil)
          (start (symbol-function 'start-file-process-shell-command))
          process displayed promise)
     (unwind-protect
         (cl-letf (((symbol-function 'start-file-process-shell-command)
                    (lambda (&rest args)
                      (setq process (apply start args))))
                   ((symbol-function 'display-buffer)
                    (lambda (buffer &rest _) (setq displayed buffer))))
           (setq promise (docker-run-async "sh" "-c"
                                          (shell-quote-argument ,command)))
           ,@body)
       (when (and process (process-live-p process)) (delete-process process))
       (when (and process (buffer-live-p (process-buffer process)))
         (kill-buffer (process-buffer process)))
       (when (and displayed (buffer-live-p displayed)) (kill-buffer displayed)))))

(ert-deftest docker-process-test-success-excludes-stderr ()
  (docker-process-test-capture "printf 'WARN unset variable\n' >&2; printf 'web\ndb\n'"
    (should (equal (funcall (docker-process-test-await promise)) "web\ndb\n"))
    (should-not displayed)
    (should-not (buffer-live-p (process-buffer process)))
    (should (process-get process 'docker-stderr-file))
    (should-not (file-exists-p (process-get process 'docker-stderr-file)))))

(ert-deftest docker-process-test-failure-has-persistent-diagnostics ()
  (docker-process-test-capture "printf 'partial\n'; printf 'invalid depends_on\n' >&2; exit 7"
    (let ((err (should-error (funcall (docker-process-test-await promise)))))
      (should (string-match-p "invalid depends_on" (error-message-string err))))
    (should (eq displayed (process-buffer process)))
    (with-current-buffer displayed
      (should (eq major-mode 'special-mode))
      (should (string-match-p "Directory:" (buffer-string)))
      (should (string-match-p "Exit: exit (7)" (buffer-string)))
      (should (string-match-p "partial" (buffer-string)))
      (should (string-match-p "invalid depends_on" (buffer-string))))
    (should-not (file-exists-p (process-get process 'docker-stderr-file)))))

(ert-deftest docker-process-test-complete-trailing-stderr ()
  (docker-process-test-capture
      "i=0; while [ $i -lt 3000 ]; do printf '0123456789\n' >&2; i=$((i+1)); done; printf 'FINAL_MARKER\n' >&2; exit 2"
    (should-error (funcall (docker-process-test-await promise)))
    (with-current-buffer displayed
      (should (string-suffix-p "FINAL_MARKER\n" (buffer-string)))
      (goto-char (point-min))
      (should (search-forward "Standard error:\n" nil t))
      (should (equal (buffer-substring-no-properties (point) (point-max))
                     (concat (apply #'concat (make-list 3000 "0123456789\n"))
                             "FINAL_MARKER\n"))))))

(ert-deftest docker-process-test-signal-cleans-stderr ()
  (docker-process-test-capture "exec sleep 10"
    (delete-process process)
    (should-error (funcall (docker-process-test-await promise)))
    (should (buffer-live-p displayed))
    (should-not (file-exists-p (process-get process 'docker-stderr-file)))))

(ert-deftest docker-process-test-killed-output-still-rejects ()
  (docker-process-test-capture "sleep 0.1; exit 7"
    (kill-buffer (process-buffer process))
    (should-error (funcall (docker-process-test-await promise)))
    (should (buffer-live-p displayed))
    (should (eq displayed (process-buffer process)))
    (should-not (file-exists-p (process-get process 'docker-stderr-file)))))

(ert-deftest docker-process-test-start-error-cleans-stderr ()
  (let ((make-temp (symbol-function 'make-nearby-temp-file))
        tempfile created-buffer)
    (cl-letf (((symbol-function 'make-nearby-temp-file)
               (lambda (&rest args) (setq tempfile (apply make-temp args))))
              ((symbol-function 'start-file-process-shell-command)
               (lambda (_name buffer _command)
                 (setq created-buffer buffer)
                 (error "fixture startup failure"))))
      (should-error (docker-run-async "fixture")))
    (should tempfile)
    (should-not (file-exists-p tempfile))
    (should-not (buffer-live-p created-buffer))))

(ert-deftest docker-process-test-warning-is-reported-when-enabled ()
  (docker-process-test-capture "printf 'warning payload\n' >&2; printf 'web\n'"
    (let ((docker-show-messages t)
          messages)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (and format-string (apply #'format format-string args))
                         messages))))
        (should (equal (funcall (docker-process-test-await promise)) "web\n")))
      (should (cl-some (lambda (text)
                         (and text (string-match-p "warning payload" text)))
                       messages)))))

(ert-deftest docker-process-test-sentinel-is-idempotent ()
  (docker-process-test-capture "printf 'failure payload\n' >&2; exit 7"
    (should-error (funcall (docker-process-test-await promise)))
    (let ((original (with-current-buffer displayed (buffer-string))))
      (docker-process-sentinel promise process "exited again\n")
      (should (equal original (with-current-buffer displayed (buffer-string)))))))

(provide 'docker-process-test)

;;; docker-process-test.el ends here
