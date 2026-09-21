;;; docker-core-test.el --- Tests for docker-core  -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the generic helpers in `docker-core'.

;;; Code:
(require 'ert)
(require 'docker-core)

(defun docker-core-test-requires-p (library feature)
  "Return non-nil when LIBRARY pulled in FEATURE when it was loaded."
  (--any? (and (stringp (car it))
               (equal (file-name-base (car it)) library)
               (member (cons 'require feature) (cdr it)))
          load-history))

(ert-deftest docker-core-test-ansi-color-is-required ()
  "Both callers of `ansi-color-apply' must pull the feature in themselves.

Nothing else in the dependency tree loads it, and the buffers the two call
sites write to are `special-mode', so comint never loads it either."
  (should (docker-core-test-requires-p "docker-core" 'ansi-color))
  (should (docker-core-test-requires-p "docker-process" 'ansi-color)))

(ert-deftest docker-core-test-get-transient-action ()
  (let ((transient-current-command 'docker-container-rm))
    (should (equal (docker-get-transient-action) "container rm")))
  (let ((transient-current-command 'docker-image-ls))
    (should (equal (docker-get-transient-action) "image ls"))))

(provide 'docker-core-test)

;;; docker-core-test.el ends here
