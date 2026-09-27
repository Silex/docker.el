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

(provide 'docker-process-test)

;;; docker-process-test.el ends here
