;;; docker-resources-test.el --- Tests for the resource entry helpers  -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the dangling and active markers each resource puts on its entries.

;;; Code:
(require 'ert)
(require 'docker-context)
(require 'docker-image)
(require 'docker-network)
(require 'docker-volume)

(defun docker-resources-test-entry ()
  "Return a fresh unpropertized entry."
  (list "abcdef" (vector "alpine" "latest")))

(ert-deftest docker-resources-test-set-dangling-marks-the-id ()
  (dolist (entry '((docker-image-entry-set-dangling . docker-image-dangling)
                   (docker-network-entry-set-dangling . docker-network-dangling)
                   (docker-volume-entry-set-dangling . docker-volume-dangling)))
    (let ((marked (funcall (car entry) (docker-resources-test-entry))))
      (should (get-text-property 0 (cdr entry) (car marked))))))

(ert-deftest docker-resources-test-set-dangling-fontifies-the-columns ()
  (dolist (setter '(docker-image-entry-set-dangling
                    docker-network-entry-set-dangling
                    docker-volume-entry-set-dangling))
    (let ((marked (funcall setter (docker-resources-test-entry))))
      (should (equal (get-text-property 0 'font-lock-face (aref (cadr marked) 0))
                     'docker-face-dangling))
      (should (equal (get-text-property 0 'font-lock-face (aref (cadr marked) 1))
                     'docker-face-dangling)))))

(ert-deftest docker-resources-test-dangling-p ()
  (dolist (entry '((docker-image-entry-set-dangling . docker-image-dangling-p)
                   (docker-network-entry-set-dangling . docker-network-dangling-p)
                   (docker-volume-entry-set-dangling . docker-volume-dangling-p)))
    (should (funcall (cdr entry) (car (funcall (car entry) (docker-resources-test-entry)))))
    (should-not (funcall (cdr entry) "abcdef"))))

(ert-deftest docker-resources-test-context-set-active ()
  (let ((marked (docker-context-entry-set-active (docker-resources-test-entry))))
    (should (get-text-property 0 'docker-context-active (car marked)))
    (should (equal (get-text-property 0 'font-lock-face (aref (cadr marked) 0))
                   'docker-face-active))))

(provide 'docker-resources-test)

;;; docker-resources-test.el ends here
