;;; docker-container-test.el --- Tests for docker-container  -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the tramp paths the container entry points build.

;;; Code:
(require 'ert)
(require 'docker-container)

(defmacro docker-container-test-capture-directory (terminal &rest body)
  "Evaluate BODY with TERMINAL stubbed and return the `default-directory' it saw."
  (declare (indent 1))
  `(let (captured)
     (cl-letf (((symbol-function 'docker-container-assert-tramp-docker) #'ignore)
               ((symbol-function ,terminal)
                (lambda (&rest args)
                  (setq captured default-directory)
                  (when (bufferp (car args)) (kill-buffer (car args))))))
       ,@body)
     captured))

(ert-deftest docker-container-test-eshell-directory ()
  (let ((default-directory "/tmp/"))
    (should (equal (docker-container-test-capture-directory 'eshell
                     (docker-container-eshell "web"))
                   "/docker:web:/"))))

(ert-deftest docker-container-test-eshell-directory-from-a-remote-host ()
  (let ((default-directory "/ssh:host:/srv/"))
    (should (equal (docker-container-test-capture-directory 'eshell
                     (docker-container-eshell "web"))
                   "/ssh:host|docker:web:/"))))

(ert-deftest docker-container-test-default-directory-keeps-every-hop ()
  (let ((tramp-default-proxies-alist nil))
    (should (equal (docker-container--default-directory "web" nil "/ssh:myhost|sudo:myhost:/srv/")
                   "/ssh:myhost|sudo:root@myhost|docker:web:/"))
    (should (equal (docker-container--default-directory "web" nil "/sudo::/srv/")
                   (format "/sudo:root@%s|docker:web:/" (system-name))))))

(ert-deftest docker-container-test-find-file-and-directory-keep-the-host ()
  (let ((default-directory "/ssh:host:/srv/"))
    (dolist (entry '((docker-container-find-file find-file "/etc/hosts")
                     (docker-container-find-directory dired "/etc/")))
      (let (opened)
        (cl-letf (((symbol-function 'docker-container-assert-tramp-docker) #'ignore)
                  ((symbol-function (nth 1 entry)) (lambda (name) (setq opened name))))
          (funcall (nth 0 entry) "web" (nth 2 entry)))
        (should (equal opened (concat "/ssh:host|docker:web:" (nth 2 entry))))))))

(ert-deftest docker-container-test-eshell-honours-the-tramp-method ()
  (let ((default-directory "/tmp/")
        (docker-container-tramp-method "podman"))
    (should (equal (docker-container-test-capture-directory 'eshell
                     (docker-container-eshell "web"))
                   "/podman:web:/"))))

(ert-deftest docker-container-test-eshell-buffer-name ()
  (let ((default-directory "/tmp/")
        captured)
    (cl-letf (((symbol-function 'docker-container-assert-tramp-docker) #'ignore)
              ((symbol-function 'eshell)
               (lambda (&rest _) (setq captured eshell-buffer-name))))
      (docker-container-eshell "web"))
    (should (equal captured "* docker eshell: /docker:web:/ *"))))

(ert-deftest docker-container-test-default-directory-without-a-workdir ()
  (let ((default-directory "/tmp/"))
    (should (equal (docker-container--default-directory "web") "/docker:web:/"))
    (should (equal (docker-container--default-directory "web" nil) "/docker:web:/"))
    (should (equal (docker-container--default-directory "web" "") "/docker:web:/"))))

(ert-deftest docker-container-test-shell-directory ()
  (let ((default-directory "/tmp/"))
    (should (equal (docker-container-test-capture-directory 'shell
                     (docker-container-shell "web"))
                   "/docker:web:/"))))

(ert-deftest docker-container-test-shell-directory-from-a-remote-host ()
  (let ((default-directory "/ssh:host:/srv/"))
    (should (equal (docker-container-test-capture-directory 'shell
                     (docker-container-shell "web"))
                   "/ssh:host|docker:web:/"))))

(ert-deftest docker-container-test-shell-buffer-name ()
  (let ((default-directory "/tmp/")
        captured)
    (cl-letf (((symbol-function 'docker-container-assert-tramp-docker) #'ignore)
              ((symbol-function 'shell)
               (lambda (buffer) (setq captured (buffer-name buffer)))))
      (docker-container-shell "web"))
    (should (equal captured "* docker shell: /docker:web:/ *"))
    (kill-buffer captured)))

(ert-deftest docker-container-test-shell-reads-the-shell-with-a-prefix-argument ()
  (let ((default-directory "/tmp/")
        captured)
    (cl-letf (((symbol-function 'docker-container-assert-tramp-docker) #'ignore)
              ((symbol-function 'read-shell-command) (lambda (&rest _) "/bin/bash"))
              ((symbol-function 'shell)
               (lambda (buffer) (setq captured shell-file-name) (kill-buffer buffer))))
      (docker-container-shell "web" t))
    (should (equal captured "/bin/bash"))))

(ert-deftest docker-container-test-vterm-directory ()
  (let ((default-directory "/tmp/"))
    (should (equal (docker-container-test-capture-directory 'vterm-other-window
                     (docker-container-vterm "web"))
                   "/docker:web:/"))))

(ert-deftest docker-container-test-vterm-directory-from-a-remote-host ()
  (let ((default-directory "/ssh:host:/srv/"))
    (should (equal (docker-container-test-capture-directory 'vterm-other-window
                     (docker-container-vterm "web"))
                   "/ssh:host|docker:web:/"))))

(ert-deftest docker-container-test-eat-directory ()
  (let ((default-directory "/tmp/"))
    (should (equal (docker-container-test-capture-directory 'eat-other-window
                     (docker-container-eat "web"))
                   "/docker:web:/"))))

(ert-deftest docker-container-test-ghostel-directory ()
  (let ((default-directory "/tmp/"))
    (should (equal (docker-container-test-capture-directory 'ghostel-create
                     (docker-container-ghostel "web"))
                   "/docker:web:/"))))

(ert-deftest docker-container-test-ghostel-leaves-the-display-prefixes-alone ()
  (let ((default-directory "/tmp/"))
    (dolist (command '(docker-container-ghostel docker-container-ghostel-env))
      (let (captured)
        (cl-letf (((symbol-function 'docker-container-assert-tramp-docker) #'ignore)
                  ((symbol-function 'docker-run-docker-async)
                   (lambda (&rest _)
                     (docker-container-test-resolved
                      "[{\"Config\":{\"WorkingDir\":\"/app\",\"Env\":[\"A=1\"]}}]")))
                  ((symbol-function 'ghostel-create)
                   (lambda (_name display)
                     (setq captured (list display-buffer-overriding-action display)))))
          (let ((result (funcall command "web")))
            (when (aio-promise-p result) (aio-wait-for result))))
        (should (equal captured (list display-buffer-overriding-action
                                      '((display-buffer-pop-up-window)))))))))

(ert-deftest docker-container-test-terminals-report-a-missing-package ()
  (dolist (entry '((docker-container-vterm . "The vterm package is not installed")
                   (docker-container-eat . "The eat package is not installed")
                   (docker-container-ghostel
                    . "The ghostel package (0.52.0 or later) is not installed")))
    (cl-letf (((symbol-function 'docker-container-assert-tramp-docker) #'ignore))
      (should (equal (cadr (should-error (funcall (car entry) "web")))
                     (cdr entry))))))

(ert-deftest docker-container-test-shell-entry-points-assert-tramp-support ()
  (dolist (entry '((docker-container-eshell . eshell)
                   (docker-container-shell . shell)
                   (docker-container-vterm . vterm-other-window)
                   (docker-container-eat . eat-other-window)
                   (docker-container-ghostel . ghostel-create)))
    (let (asserted)
      (cl-letf (((symbol-function 'docker-container-assert-tramp-docker)
                 (lambda () (setq asserted t)))
                ((symbol-function (cdr entry))
                 (lambda (&rest args) (when (bufferp (car args)) (kill-buffer (car args))))))
        (funcall (car entry) "web"))
      (should asserted))))

(defun docker-container-test-resolved (value)
  "Return a promise already resolved with VALUE."
  (let ((promise (aio-promise)))
    (aio-resolve promise (lambda () value))
    promise))

(ert-deftest docker-container-test-env-entry-points-check-the-terminal-first ()
  (dolist (entry '((docker-container-vterm-env . "The vterm package is not installed")
                   (docker-container-eat-env . "The eat package is not installed")
                   (docker-container-ghostel-env
                    . "The ghostel package (0.52.0 or later) is not installed")))
    (let (inspected)
      (cl-letf (((symbol-function 'docker-container-assert-tramp-docker) #'ignore)
                ((symbol-function 'docker-run-docker-async)
                 (lambda (&rest _)
                   (setq inspected t)
                   (docker-container-test-resolved
                    "[{\"Config\":{\"WorkingDir\":\"/app\",\"Env\":[\"A=1\"]}}]"))))
        (should (equal (cadr (should-error (aio-wait-for (funcall (car entry) "web"))))
                       (cdr entry)))
        (should-not inspected)))))

(ert-deftest docker-container-test-terminal-buffer-names ()
  (let ((default-directory "/tmp/"))
    (dolist (entry `((docker-container-eat eat-other-window
                                           ,(lambda (_) (symbol-value 'eat-buffer-name))
                                           "* docker eat: /docker:web:/ *")
                     (docker-container-ghostel ghostel-create car
                                               "* docker ghostel: /docker:web:/ *")))
      (let (captured)
        (cl-letf (((symbol-function 'docker-container-assert-tramp-docker) #'ignore)
                  ((symbol-function (nth 1 entry))
                   (lambda (&rest args) (setq captured (funcall (nth 2 entry) args)))))
          (funcall (car entry) "web"))
        (should (equal captured (nth 3 entry)))))))

(ert-deftest docker-container-test-env-terminal-buffer-names ()
  (let ((default-directory "/tmp/"))
    (dolist (entry `((docker-container-eat-env eat-other-window
                                               ,(lambda (_) (symbol-value 'eat-buffer-name))
                                               "* docker eat-env: /docker:web:/app *")
                     (docker-container-ghostel-env ghostel-create car
                                                   "* docker ghostel-env: /docker:web:/app *")))
      (let (captured)
        (cl-letf (((symbol-function 'docker-container-assert-tramp-docker) #'ignore)
                  ((symbol-function 'docker-run-docker-async)
                   (lambda (&rest _)
                     (docker-container-test-resolved
                      "[{\"Config\":{\"WorkingDir\":\"/app\",\"Env\":[\"A=1\"]}}]")))
                  ((symbol-function (nth 1 entry))
                   (lambda (&rest args) (setq captured (funcall (nth 2 entry) args)))))
          (aio-wait-for (funcall (car entry) "web")))
        (should (equal captured (nth 3 entry)))))))

(ert-deftest docker-container-test-env-context ()
  (let ((default-directory "/ssh:host:/srv/"))
    (cl-letf (((symbol-function 'docker-run-docker-async)
               (lambda (&rest _)
                 (docker-container-test-resolved
                  "[{\"Config\":{\"WorkingDir\":\"/app\",\"Env\":[\"A=1\",\"B=2\"]}}]"))))
      (should (equal (aio-wait-for (docker-container--env-context "web"))
                     '("/ssh:host|docker:web:/app" "A=1" "B=2"))))))

(ert-deftest docker-container-test-env-entry-points-keep-the-caller-host ()
  (dolist (entry '((docker-container-shell-env . shell)
                   (docker-container-vterm-env . vterm-other-window)
                   (docker-container-eat-env . eat-other-window)
                   (docker-container-ghostel-env . ghostel-create)))
    (let (captured promise)
      (with-temp-buffer
        (setq default-directory "/tmp/")
        (cl-letf (((symbol-function 'docker-container-assert-tramp-docker) #'ignore)
                  ((symbol-function 'docker-run-docker-async)
                   (lambda (&rest _)
                     (docker-container-test-resolved
                      "[{\"Config\":{\"WorkingDir\":\"/app\",\"Env\":[\"A=1\"]}}]")))
                  ((symbol-function (cdr entry))
                   (lambda (&rest args)
                     (setq captured default-directory)
                     (when (bufferp (car args)) (kill-buffer (car args))))))
          ;; The caller's binding has ended by the time the await resumes.
          (let ((default-directory "/ssh:host:/srv/"))
            (setq promise (funcall (car entry) "web")))
          (aio-wait-for promise)))
      (should (equal captured "/ssh:host|docker:web:/app")))))

(ert-deftest docker-container-test-shell-command-runs-in-an-interactive-buffer ()
  (let ((docker-container-exec-default-args '("-i" "-t"))
        (docker-container-exec-custom-args nil)
        ran)
    (cl-letf (((symbol-function 'read-shell-command)
               (lambda (_prompt default) (concat default " ls")))
              ((symbol-function 'docker-run-async-with-buffer-interactive)
               (lambda (&rest args) (setq ran args))))
      (docker-container-shell-command "web"))
    (should (equal ran '("docker exec -i -t web ls")))))

(ert-deftest docker-container-test-shell-command-uses-the-container-exec-args ()
  (let ((docker-container-exec-default-args '("-i" "-t"))
        (docker-container-exec-custom-args '(("^we" ("-u" "root"))))
        default-command)
    (cl-letf (((symbol-function 'read-shell-command)
               (lambda (_prompt default) (setq default-command default)))
              ((symbol-function 'docker-run-async-with-buffer-interactive) #'ignore))
      (docker-container-shell-command "web"))
    (should (equal default-command "docker exec -u root web"))))

(ert-deftest docker-container-test-env-commands-are-obsolete ()
  (dolist (command '(docker-container-shell-env docker-container-vterm-env
                     docker-container-eat-env docker-container-ghostel-env
                     docker-container-shell-env-selection docker-container-vterm-env-selection
                     docker-container-eat-env-selection docker-container-ghostel-env-selection))
    (should (equal (nth 2 (get command 'byte-obsolete-info)) "2.6.0"))))

(ert-deftest docker-container-test-dired-alias-is-obsolete ()
  (should (eq (indirect-function 'docker-container-dired)
              (indirect-function 'docker-container-find-directory)))
  (should (get 'docker-container-dired 'byte-obsolete-info)))

(ert-deftest docker-container-test-selections-build-each-directory-from-the-list ()
  (dolist (entry '((docker-container-eshell-selection . eshell)
                   (docker-container-shell-selection . shell)
                   (docker-container-vterm-selection . vterm-other-window)
                   (docker-container-eat-selection . eat-other-window)
                   (docker-container-ghostel-selection . ghostel-create)))
    (let (directories buffers)
      (unwind-protect
          (with-temp-buffer
            (setq default-directory "/tmp/")
            (cl-letf (((symbol-function 'docker-container-assert-tramp-docker) #'ignore)
                      ((symbol-function 'docker-utils-ensure-items) #'ignore)
                      ((symbol-function 'docker-utils-get-marked-items-ids)
                       (lambda () '("web" "db")))
                      ;; Like the real terminals, make the new buffer current.
                      ((symbol-function (cdr entry))
                       (lambda (&rest args)
                         (let ((directory default-directory)
                               (buffer (if (bufferp (car args)) (car args) (generate-new-buffer "terminal"))))
                           (push directory directories)
                           (push buffer buffers)
                           (set-buffer buffer)
                           (setq default-directory directory)))))
              (if (eq (car entry) 'docker-container-shell-selection)
                  (funcall (car entry) nil)
                (funcall (car entry)))))
        ;; Killing a buffer dissects its tramp directory, and Emacs 28 has no
        ;; docker method.
        (dolist (buffer buffers)
          (with-current-buffer buffer (setq default-directory "/tmp/"))
          (kill-buffer buffer)))
      (should (equal (nreverse directories) '("/docker:web:/" "/docker:db:/"))))))

(ert-deftest docker-container-test-status-face ()
  (should (equal (docker-container-status-face "Up 3 hours") 'docker-face-status-up))
  (should (equal (docker-container-status-face "Exited (0) 3 hours ago") 'docker-face-status-down))
  (should (equal (docker-container-status-face "Created") 'docker-face-status-other)))

(ert-deftest docker-container-test-propertize-entry ()
  (let* ((docker-container-columns '((:name "Names") (:name "Status")))
         (entry (docker-container-propertize-entry (list "web" (vector "web" "Up 3 hours")))))
    (should (equal (get-text-property 0 'font-lock-face (aref (cadr entry) 1))
                   'docker-face-status-up))))

(ert-deftest docker-container-test-read-shell ()
  (let ((docker-container-shell-file-name "/bin/sh"))
    (should (equal (docker-container--read-shell) "/bin/sh"))
    (cl-letf (((symbol-function 'read-shell-command) (lambda (&rest _) "/bin/bash")))
      (should (equal (docker-container--read-shell t) "/bin/bash")))))

(provide 'docker-container-test)

;;; docker-container-test.el ends here
