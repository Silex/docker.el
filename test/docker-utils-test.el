;;; docker-utils-test.el --- Tests for docker-utils  -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the helpers in `docker-utils'.

;;; Code:
(require 'ert)
(require 'docker-utils)

(ert-deftest docker-utils-test-generate-new-buffer-name ()
  (should (equal (docker-utils-generate-new-buffer-name "docker" "shell:" "/docker:web:/")
                 "* docker shell: /docker:web:/ *")))

(ert-deftest docker-utils-test-generate-new-buffer-name-without-args ()
  (should (equal (docker-utils-generate-new-buffer-name "docker exec web bash")
                 "* docker exec web bash *")))

(ert-deftest docker-utils-test-generate-new-buffer ()
  (let ((buffer (docker-utils-generate-new-buffer "docker" "logs:")))
    (unwind-protect
        (should (equal (buffer-name buffer) "* docker logs: *"))
      (kill-buffer buffer))))

(ert-deftest docker-utils-test-mark-dangling-skips-lines-without-an-id ()
  (let (ids marked)
    (with-temp-buffer
      (insert "header\n")
      (insert (propertize "abcdef\n" 'tabulated-list-id
                          (propertize "abcdef" 'docker-utils-test-dangling t)))
      (cl-letf (((symbol-function 'tablist-unmark-all-marks) #'ignore)
                ((symbol-function 'tablist-put-mark)
                 (lambda () (push (line-number-at-pos) marked))))
        ;; The predicate reads a text property of the id, like the real ones do,
        ;; so it signals when it is handed the nil id of the header line.
        (docker-utils-mark-dangling
         (lambda (id)
           (push id ids)
           (get-text-property 0 'docker-utils-test-dangling id)))))
    (should (equal ids '("abcdef")))
    (should (equal marked '(2)))))

(ert-deftest docker-utils-test-unit-multiplier ()
  (should (equal (docker-utils-unit-multiplier nil) 1))
  (should (equal (docker-utils-unit-multiplier "B") 1))
  (should (equal (docker-utils-unit-multiplier "kB") 1024))
  (should (equal (docker-utils-unit-multiplier "MB") (* 1024 1024)))
  (should (equal (docker-utils-unit-multiplier "GB") (* 1024 1024 1024)))
  (should (equal (docker-utils-unit-multiplier "quux") 1)))

(ert-deftest docker-utils-test-human-size-to-bytes ()
  (should (equal (docker-utils-human-size-to-bytes "42") 42))
  (should (equal (docker-utils-human-size-to-bytes "42B") 42))
  (should (equal (docker-utils-human-size-to-bytes "1.5kB") (* 1.5 1024)))
  (should-error (docker-utils-human-size-to-bytes "not a size")))

(ert-deftest docker-utils-test-human-size-predicate ()
  (should (docker-utils-human-size-predicate "1kB" "1MB"))
  (should-not (docker-utils-human-size-predicate "1MB" "1kB")))

(ert-deftest docker-utils-test-compute-args-without-match ()
  (cl-letf (((symbol-function 'tablist-get-marked-items)
             (lambda () '(("web" ["web"])))))
    (should (equal (docker-utils-compute-args '("-i" "-t") '(("^db" ("-i"))))
                   '("-i" "-t")))))

(ert-deftest docker-utils-test-compute-args-with-match ()
  (cl-letf (((symbol-function 'tablist-get-marked-items)
             (lambda () '(("web" ["web"])))))
    (should (equal (docker-utils-compute-args '("-i" "-t") '(("^we" ("-u" "root"))))
                   '("-u" "root")))))

(ert-deftest docker-utils-test-compute-args-without-marked-item ()
  (cl-letf (((symbol-function 'tablist-get-marked-items) (lambda () nil)))
    (should (equal (docker-utils-compute-args '("-i" "-t") '(("^we" ("-u" "root"))))
                   '("-i" "-t")))))

(ert-deftest docker-utils-test-compute-args-with-an-explicit-name ()
  (cl-letf (((symbol-function 'tablist-get-marked-items)
             (lambda () (error "The marked items should not be read"))))
    (should (equal (docker-utils-compute-args '("-i" "-t") '(("^we" ("-u" "root"))) "web")
                   '("-u" "root")))))

(ert-deftest docker-utils-test-make-format-string ()
  (should (equal (docker-utils-make-format-string
                  "{{ json .Names }}"
                  '((:name "Id" :template "{{ json .ID }}")
                    (:name "Image" :template "{{ json .Image }}")))
                 "[{{ json .Names }},{{ json .ID }},{{ json .Image }}]")))

(ert-deftest docker-utils-test-parse ()
  (should (equal (docker-utils-parse
                  '((:name "Id") (:name "Image"))
                  "[\"web\",\"abcdef\",\"alpine\"]")
                 '("web" ["abcdef" "alpine"]))))

(ert-deftest docker-utils-test-parse-applies-the-format-function ()
  (should (equal (docker-utils-parse
                  '((:name "Id") (:name "Image" :format upcase))
                  "[\"web\",\"abcdef\",\"alpine\"]")
                 '("web" ["abcdef" "ALPINE"]))))

(ert-deftest docker-utils-test-parse-rejects-invalid-json ()
  (should-error (docker-utils-parse '((:name "Id")) "not json")))

(ert-deftest docker-utils-test-columns-list-format ()
  (should (equal (docker-utils-columns-list-format
                  '((:name "Id" :width 16) (:name "Image" :width 20)))
                 [("Id" 16 t) ("Image" 20 t)])))

(ert-deftest docker-utils-test-columns-setter-converts-a-list ()
  (let ((symbol (make-symbol "docker-utils-test-columns")))
    (docker-utils-columns-setter symbol '(("Id" 16 "{{ json .ID }}" nil nil)))
    (should (equal (symbol-value symbol)
                   '((:name "Id" :width 16 :template "{{ json .ID }}" :sort nil :format nil))))))

(ert-deftest docker-utils-test-columns-setter-keeps-a-plist ()
  (let ((symbol (make-symbol "docker-utils-test-columns"))
        (value '((:name "Id" :width 16 :template "{{ json .ID }}" :sort nil :format nil))))
    (docker-utils-columns-setter symbol value)
    (should (equal (symbol-value symbol) value))))

(ert-deftest docker-utils-test-columns-getter ()
  (let ((symbol (make-symbol "docker-utils-test-columns")))
    (set symbol '((:name "Id" :width 16 :template "{{ json .ID }}" :sort nil :format nil)))
    (should (equal (docker-utils-columns-getter symbol)
                   '(("Id" 16 "{{ json .ID }}" nil nil))))))

(ert-deftest docker-utils-test-pop-to-buffer-names ()
  (let ((tramp-default-proxies-alist nil))
    (dolist (entry `(("/tmp/" . "*docker-containers*")
                     ("/ssh:myhost:/srv/" . "*docker-containers* - myhost")
                     ("/ssh:myhost|sudo:myhost:/srv/" . "*docker-containers* - root@myhost")
                     ("/sudo::/srv/" . ,(format "*docker-containers* - root@%s" (system-name)))))
      (let ((default-directory (car entry))
            name)
        (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) (setq name buffer))))
          (docker-utils-pop-to-buffer "*docker-containers*"))
        (should (equal name (cdr entry)))))))

(ert-deftest docker-utils-test-pop-to-buffer-action-is-customizable ()
  (should (custom-variable-p 'docker-pop-to-buffer-action)))

(ert-deftest docker-utils-test-sudo-directory ()
  (let ((tramp-default-proxies-alist nil))
    (should (equal (docker-utils-sudo-directory "/srv/project/") "/sudo::/srv/project/"))
    (should (equal (docker-utils-sudo-directory "/ssh:myhost:/srv/project/")
                   "/ssh:myhost|sudo:root@myhost:/srv/project/"))
    (should (equal (docker-utils-sudo-directory "/sudo::/srv/project/") "/sudo::/srv/project/"))
    (should (equal (docker-utils-sudo-directory "/ssh:myhost|sudo:myhost:/srv/project/")
                   "/ssh:myhost|sudo:myhost:/srv/project/"))))

(provide 'docker-utils-test)

;;; docker-utils-test.el ends here
