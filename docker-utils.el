;;; docker-utils.el --- Random utilities

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

(require 'magit-popup)

(defun docker-utils-get-marked-items ()
  "Get the marked items data."
  (save-excursion
    (goto-char (point-min))
    (let ((selection ()))
      (while (not (eobp))
        (when (not (null (tablist-get-mark-state)))
          (add-to-list 'selection (cons (tabulated-list-get-id) (tabulated-list-get-entry)) t))
        (forward-line))
      selection)))

(defmacro docker-utils-define-popup (name doc &rest args)
  "Wrapper around `docker-utils-define-popup'."
  `(progn
     (magit-define-popup ,name ,doc ,@args)
     (add-function :before (symbol-function ',name) #'docker-utils-select-if-empty)))

(defun docker-utils-select-if-empty (&optional arg)
  "Select current row is selection is empty."
  (save-excursion
    (when (null (docker-utils-get-marked-items))
      (tablist-put-mark))))

(put 'docker-utils-define-popup 'lisp-indent-function 'defun)

(defun docker--recreate-buffer-pop-to (buffer-name)
  (let ((buffer (get-buffer buffer-name)))
    (if buffer
      (kill-buffer buffer)))
  (pop-to-buffer buffer-name))

(provide 'docker-utils)

;;; docker-utils.el ends here
