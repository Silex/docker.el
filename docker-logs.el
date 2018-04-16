;;; docker-logs.el --- Docker logs specific functionality
(require 'compile)

(define-compilation-mode docker-logs-mode "docker logs"
  "Docker logs mode"
  (set (make-local-variable 'compilation-highlight-regexp) nil)
  (set (make-local-variable 'compilation-error-regexp-alist) nil)
  (set (make-local-variable 'compilation-error-regexp-alist-alist) nil))


(defun docker-logs-show (id)
  "Run docker logs in compilation mode."
  (let ((docker-logs-command (format "docker logs %s" id)))
    (compilation-start docker-logs-command #'docker-logs-mode
                       `(lambda (mode-name) ,(concat "*" docker-logs-command "*")))))

(provide 'docker-logs)
