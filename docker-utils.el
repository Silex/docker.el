;;; docker-utils.el --- Random utilities  -*- lexical-binding: t -*-

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 's)
(require 'dash)
(require 'tramp)
(require 'tablist)
(require 'json-mode)
(require 'transient)

(require 'docker-core)

(defun docker-utils-get-marked-items-ids ()
  "Get the id part of `tablist-get-marked-items'."
  (-map #'car (tablist-get-marked-items)))

(defun docker-utils-ensure-items ()
  (when (null (docker-utils-get-marked-items-ids))
    (user-error "This action cannot be used en an empty list")))

(defmacro docker-utils-with-buffer (name &rest body)
  "Wrapper around `with-current-buffer'.
Execute BODY in a buffer named with the help of NAME."
  (declare (indent defun))
  `(with-current-buffer (docker-generate-new-buffer ,name)
     (setq buffer-read-only nil)
     (erase-buffer)
     ,@body
     (setq buffer-read-only t)
     (goto-char (point-min))
     (pop-to-buffer (current-buffer))))

(defmacro docker-utils-transient-define-prefix (name arglist &rest args)
  `(transient-define-prefix ,name ,arglist
     ,@args
     (interactive)
     (docker-utils-ensure-items)
     (transient-setup ',name)))

(defun docker-utils-get-transient-action ()
  (s-replace "-" " " (s-chop-prefix "docker-" (symbol-name transient-current-command))))

(defun docker-utils-generic-actions-heading ()
  (let ((items (s-join ", " (docker-utils-get-marked-items-ids))))
    (format "%s %s"
            (propertize "Actions on" 'face 'transient-heading)
            (propertize items        'face 'transient-value))))

(defun docker-utils-generic-action (action args)
  (interactive (list (docker-utils-get-transient-action)
                     (transient-args transient-current-command)))
  (--each (docker-utils-get-marked-items-ids)
    (docker-run-docker action args it))
  (tablist-revert))

(defun docker-utils-generic-action-async (action args)
  (interactive (list (docker-utils-get-transient-action)
                     (transient-args transient-current-command)))
  (--each (docker-utils-get-marked-items-ids)
    (docker-run-docker-async action args it))
  (tablist-revert))

(defun docker-utils-generic-action-with-buffer (action args)
  (interactive (list (docker-utils-get-transient-action)
                     (transient-args transient-current-command)))
  (--each (docker-utils-get-marked-items-ids)
    (docker-utils-with-buffer (format "%s %s" action it)
      (insert (docker-run-docker action args it))))
  (tablist-revert))

(defun docker-utils-generic-action-with-buffer:json (action args)
  (interactive (list (docker-utils-get-transient-action)
                     (transient-args transient-current-command)))
  (--each (docker-utils-get-marked-items-ids)
    (docker-utils-with-buffer (format "%s %s" action it)
      (insert (docker-run-docker action args it))
      (json-mode)))
  (tablist-revert))

(defun docker-utils-pop-to-buffer (name)
  "Like `pop-to-buffer', but suffix NAME with the host if on a remote host."
  (pop-to-buffer
   (if (file-remote-p default-directory)
       (with-parsed-tramp-file-name default-directory nil (concat name " - " host))
     name)))

(defun docker-utils-unit-multiplier (str)
  "Return the correct multiplier for STR."
  (expt 1024 (-elem-index (upcase str) '("B" "KB" "MB" "GB" "TB" "PB" "EB"))))

(defun docker-utils-human-size-to-bytes (str)
  "Parse STR and return size in bytes."
  (let* ((parts (s-match "^\\([0-9\\.]+\\)\\([A-Z]+\\)?$" str))
         (value (string-to-number (-second-item parts)))
         (multiplier (docker-utils-unit-multiplier (-third-item parts))))
    (* value multiplier)))

(provide 'docker-utils)

;;; docker-utils.el ends here
