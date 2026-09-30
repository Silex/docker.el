;;; docker-test-helpers.el --- Helpers shared by the docker.el tests  -*- lexical-binding: t -*-

;;; Commentary:

;; Helpers used by more than one test file.

;;; Code:
(require 'ert)
(require 'tramp)

(defun docker-test-should-be-behind-hop (file hop name)
  "Check that FILE is the tramp file NAME reached through the ad-hoc HOP.

HOP is a single hop such as \"/ssh:myhost|\" and NAME a file name such as
\"/sudo:root@myhost:/srv/\".  Tramp 2.6.0, in Emacs 29.1, leaves ad-hoc hops
out of file names and routes through them with `tramp-default-proxies-alist',
so there FILE must be NAME and HOP must be registered as a proxy."
  (if (string-prefix-p "2.6.0." tramp-version)
      (progn
        (should (equal file name))
        (should (member (concat (substring hop 0 -1) ":") (mapcar #'caddr tramp-default-proxies-alist))))
    (should (equal file (concat hop (substring name 1))))))

(provide 'docker-test-helpers)

;;; docker-test-helpers.el ends here
