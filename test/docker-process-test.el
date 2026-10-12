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

(defmacro docker-process-test-with-diagnostics (&rest body)
  "Evaluate BODY with a diagnostics buffer of its own.

BODY sees `displayed', the buffers `display-buffer' was called with, and
`processes', the processes it started.  Both are most recent first."
  (declare (indent 0))
  `(let* ((docker-show-messages nil)
          (start (symbol-function 'docker--start-shell-command))
          displayed processes)
     (unwind-protect
         (cl-letf (((symbol-function 'docker--start-shell-command)
                    (lambda (&rest args) (car (push (apply start args) processes))))
                   ((symbol-function 'display-buffer)
                    (lambda (buffer &rest _) (push buffer displayed) nil)))
           ,@body)
       (--each processes
         (set-process-sentinel it #'ignore)
         (delete-process it))
       (when (get-buffer docker-diagnostics-buffer-name)
         (kill-buffer docker-diagnostics-buffer-name)))))

(defun docker-process-test-sh (script &rest args)
  "Return the promise of `docker-run-async' running the shell SCRIPT.
ARGS follow SCRIPT on the command line."
  (apply #'docker-run-async "sh" "-c" (shell-quote-argument script) args))

(defun docker-process-test-diagnostics ()
  "Return the text of the diagnostics buffer, or nil without one."
  (-when-let (buffer (get-buffer docker-diagnostics-buffer-name))
    (with-current-buffer buffer (buffer-string))))

(ert-deftest docker-process-test-success-excludes-stderr ()
  (docker-process-test-with-diagnostics
    (let ((promise (docker-process-test-sh "printf 'WARN unset variable\n' >&2; printf 'web\ndb\n'")))
      (should (equal (funcall (docker-process-test-await promise)) "web\ndb\n")))
    (should-not displayed)
    (should-not (buffer-live-p (process-buffer (car processes))))
    (should (string-suffix-p "Status: finished\n\nStandard error:\nWARN unset variable\n\n"
                             (docker-process-test-diagnostics)))))

(ert-deftest docker-process-test-success-keeps-stdout-as-it-is ()
  (docker-process-test-with-diagnostics
    (let ((promise (docker-process-test-sh "printf 'no newline'")))
      (should (equal (funcall (docker-process-test-await promise)) "no newline")))
    (should-not (docker-process-test-diagnostics))))

(ert-deftest docker-process-test-success-mentions-warnings ()
  (docker-process-test-with-diagnostics
    (let ((docker-show-messages t)
          messages)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (and format-string (apply #'format format-string args)) messages))))
        (let ((promise (docker-process-test-sh "printf 'warning payload\n' >&2; printf 'web\n'")))
          (should (equal (funcall (docker-process-test-await promise)) "web\n"))))
      (should (--some (and it (string-suffix-p "(warnings in *docker-diagnostics*)" it)) messages))
      (should-not (--some (and it (string-match-p "warning payload" it)) messages)))))

(ert-deftest docker-process-test-failure-is-logged-and-displayed ()
  (docker-process-test-with-diagnostics
    (let* ((default-directory temporary-file-directory)
           (promise (docker-process-test-sh "printf 'partial\n'; printf 'invalid depends_on\n' >&2; exit 7"))
           (err (should-error (funcall (docker-process-test-await promise)))))
      (should (string-match-p "(exited abnormally with code 7): invalid depends_on\\'"
                              (error-message-string err)))
      (should (equal displayed (list docker-diagnostics-buffer-name)))
      (with-current-buffer docker-diagnostics-buffer-name
        (should (eq major-mode 'special-mode))
        (should (string-prefix-p "Command: sh -c " (buffer-string)))
        (should (string-suffix-p (concat "\nDirectory: " temporary-file-directory "\n"
                                         "Status: exited abnormally with code 7\n\n"
                                         "Standard output:\npartial\n\n"
                                         "Standard error:\ninvalid depends_on\n\n")
                                 (buffer-string)))))))

(ert-deftest docker-process-test-failure-reports-the-last-stderr-line ()
  (docker-process-test-with-diagnostics
    (let* ((promise (docker-process-test-sh "yes 0123456789 | head -n 3000 >&2; printf 'FINAL_MARKER\n' >&2; exit 2"))
           (err (should-error (funcall (docker-process-test-await promise)))))
      (should (equal (error-message-string err)
                     (format "Error running: \"%s\" (exited abnormally with code 2): FINAL_MARKER"
                             (process-get (car processes) 'docker-command))))
      (should (string-suffix-p (concat "Standard error:\n"
                                       (apply #'concat (make-list 3000 "0123456789\n"))
                                       "FINAL_MARKER\n\n")
                               (docker-process-test-diagnostics))))))

(ert-deftest docker-process-test-failures-share-one-buffer ()
  (docker-process-test-with-diagnostics
    (let ((promises (--map (docker-process-test-sh (format "printf 'failure %d\n' >&2; exit 1" it)) '(0 1 2))))
      (--each promises (should-error (funcall (docker-process-test-await it)))))
    (should (equal (-uniq displayed) (list docker-diagnostics-buffer-name)))
    (dotimes (i 3)
      (should (string-match-p (format "Standard error:\nfailure %d\n" i) (docker-process-test-diagnostics))))))

(ert-deftest docker-process-test-signal-is-reported-as-one ()
  (docker-process-test-with-diagnostics
    (let ((promise (docker-process-test-sh "exec sleep 10")))
      (delete-process (car processes))
      (let ((err (should-error (funcall (docker-process-test-await promise)))))
        (should (string-match-p "(killed\\(: 9\\)?)\\'" (error-message-string err)))))
    (should (string-match-p "\nStatus: killed" (docker-process-test-diagnostics)))))

(ert-deftest docker-process-test-killed-output-still-rejects ()
  (docker-process-test-with-diagnostics
    (let ((promise (docker-process-test-sh "sleep 0.1; exit 7")))
      (kill-buffer (process-buffer (car processes)))
      (should-error (funcall (docker-process-test-await promise))))
    (should (equal displayed (list docker-diagnostics-buffer-name)))))

(ert-deftest docker-process-test-sentinel-is-idempotent ()
  (docker-process-test-with-diagnostics
    (let ((promise (docker-process-test-sh "printf 'failure payload\n' >&2; exit 7")))
      (should-error (funcall (docker-process-test-await promise)))
      (let ((logged (docker-process-test-diagnostics)))
        (docker-process-sentinel promise (car processes) "exited again\n")
        (should (equal (docker-process-test-diagnostics) logged))))))

(ert-deftest docker-process-test-redirected-stderr-keeps-its-place ()
  (docker-process-test-with-diagnostics
    (let* ((script "printf 'out-1\n'; printf 'err-1\n' >&2; printf 'out-2\n'; printf 'err-2\n' >&2")
           (promise (docker-process-test-sh script "2>&1")))
      (should (equal (funcall (docker-process-test-await promise)) "out-1\nerr-1\nout-2\nerr-2\n")))
    (should-not (docker-process-test-diagnostics))))

(ert-deftest docker-process-test-command-reaches-the-user-shell-untouched ()
  (let ((docker-show-messages nil)
        (default-directory temporary-file-directory)
        (shell-file-name "/usr/bin/fish")
        (shell-command-switch "-c")
        (process (make-pipe-process :name "docker-process-test" :noquery t))
        started)
    (unwind-protect
        (cl-letf (((symbol-function 'start-file-process)
                   (lambda (_name _buffer &rest command) (setq started command) process)))
          (docker--start-shell-command "docker ps --format='{{.ID}}'" t)
          (should (equal started (list "sh" "-c" docker--stderr-script
                                       (process-get process 'docker-stderr-token)
                                       "/usr/bin/fish" "-c" "docker ps --format='{{.ID}}'"))))
      (delete-process process))))

(ert-deftest docker-process-test-streams-stay-mixed-without-a-posix-shell ()
  (docker-process-test-with-diagnostics
    (let* ((exec-path nil)
           (promise (docker-process-test-sh "printf 'out\n'; printf 'err\n' >&2")))
      (should-not (process-get (car processes) 'docker-stderr-token))
      (should (equal (funcall (docker-process-test-await promise)) "out\nerr\n")))))

(ert-deftest docker-process-test-remote-commands-overlap ()
  "Several commands in flight on one tramp connection all resolve."
  (let* ((tramp-methods `(("mock"
                           (tramp-login-program ,tramp-default-remote-shell)
                           (tramp-login-args (("-i")))
                           (tramp-direct-async ("-c"))
                           (tramp-remote-shell ,tramp-default-remote-shell)
                           (tramp-remote-shell-args ("-c"))
                           (tramp-connection-timeout 10))))
         (tramp-default-host-alist `(("\\`mock\\'" nil ,(system-name))))
         (tramp-persistency-file-name nil)
         (tramp-verbose 0)
         (default-directory (concat "/mock::" temporary-file-directory)))
    (unwind-protect
        (docker-process-test-with-diagnostics
          (let* ((script "printf 'out-%d\n'; printf 'err-%d\n' >&2")
                 (promises (--map (docker-process-test-sh (format script it it)) '(0 1 2 3 4))))
            (--each-indexed promises
              (should (equal (funcall (docker-process-test-await it)) (format "out-%d\n" it-index)))))
          (should-not displayed)
          (dotimes (i 5)
            (should (string-match-p (format "Standard error:\nerr-%d\n" i) (docker-process-test-diagnostics)))))
      (tramp-cleanup-connection (tramp-dissect-file-name default-directory) t t))))

(provide 'docker-process-test)

;;; docker-process-test.el ends here
