;;; docker-compose-test.el --- Tests for docker-compose  -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the action helpers in `docker-compose'.

;;; Code:
(require 'ert)
(require 'docker-compose)

(defun docker-compose-test-resolved (value)
  "Return a promise already resolved with VALUE."
  (let ((promise (aio-promise)))
    (aio-resolve promise (lambda () value))
    promise))

(ert-deftest docker-compose-test-run-action-uses-the-given-services ()
  (let (ran prompted)
    (cl-letf (((symbol-function 'docker-compose-read-services-names)
               (lambda () (setq prompted t) (docker-compose-test-resolved '("db"))))
              ((symbol-function 'docker-compose-run-docker-compose-async-with-buffer)
               (lambda (&rest args) (setq ran args))))
      (aio-wait-for (docker-compose-run-action-for-services "up" '("-d") '("web"))))
    (should (equal ran '("up" ("-d") ("web"))))
    (should-not prompted)))

(ert-deftest docker-compose-test-run-action-reads-the-services-when-absent ()
  (let (ran)
    (cl-letf (((symbol-function 'docker-compose-read-services-names)
               (lambda () (docker-compose-test-resolved '("db"))))
              ((symbol-function 'docker-compose-run-docker-compose-async-with-buffer)
               (lambda (&rest args) (setq ran args))))
      (aio-wait-for (docker-compose-run-action-for-services "up" '("-d") nil)))
    (should (equal ran '("up" ("-d") ("db"))))))

(ert-deftest docker-compose-test-run-action-with-command-uses-the-given-service ()
  (let (ran prompted)
    (cl-letf (((symbol-function 'docker-compose-read-service-name)
               (lambda () (setq prompted t) (docker-compose-test-resolved "db")))
              ((symbol-function 'docker-compose-run-docker-compose-async-with-buffer)
               (lambda (&rest args) (setq ran args))))
      (aio-wait-for (docker-compose-run-action-with-command "run" nil "web" "ls")))
    (should (equal ran '("run" nil "web" "ls")))
    (should-not prompted)))

(ert-deftest docker-compose-test-one-service-alias-is-obsolete ()
  (should (eq (indirect-function 'docker-compose-run-action-for-one-service)
              (indirect-function 'docker-compose-run-action-for-services)))
  (should (get 'docker-compose-run-action-for-one-service 'byte-obsolete-info)))

(provide 'docker-compose-test)

;;; docker-compose-test.el ends here
