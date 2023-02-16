;;; docker-context.el --- Interface to docker-context  -*- lexical-binding: t -*-

;; Author: Pablo González Carrizo <unmonoqueteclea@gmail.com>

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

(require 'aio)
(require 'docker-core)

(aio-defun docker-context-read-contexts (prompt &rest _args)
  "Read all the available docker contexts forwarding PROMPT."
  (completing-read
   prompt
   (split-string
    (aio-await (docker-run-docker-async (concat "context" " ls"  " -q")))
    "\n"
    t)))

;;;###autoload (autoload 'docker-contexts "docker-contexts" nil t)
(aio-defun docker-contexts ()
  "List availables docker contexts and let user change it"
  (interactive)
  (let ((context
	 (aio-await(docker-context-read-contexts "Select the context to use"))))
    (message
     (aio-await
      (docker-run-docker-async
       (concat "context" " use " context))))))

(provide 'docker-context)

;;; docker-context.el ends here
