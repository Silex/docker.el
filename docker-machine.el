;;; docker-machine.el --- Emacs interface to docker-machine  -*- lexical-binding: t -*-

;; Author: Ben Swift <ben@benswift.me>

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
(require 'tablist)
(require 'transient)

(require 'docker-core)
(require 'docker-faces)
(require 'docker-utils)

(defgroup docker-machine nil
  "Docker machine customization group."
  :group 'docker)

(defcustom docker-machine-command "docker-machine"
  "The docker-machine binary."
  :group 'docker-machine
  :type 'string)

(defconst docker-machine-id-template
  "{{ .Name }}"
  "This Go template describes the id of rows; this will be passed to transients.

docker-machine doesn't support the 'json' template function!")

(defcustom docker-machine-default-sort-key '("Name" . nil)
  "Sort key for docker machines.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'docker-machine
  :type '(cons (string :tag "Column Name"
                       :validate (lambda (widget)
                                   (unless (--any-p (equal (plist-get it :name) (widget-value widget)) docker-volume-columns)
                                     (widget-put widget :error "Default Sort Key must match a column name")
                                     widget)))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defcustom docker-machine-columns
  '((:name "Name" :width 16 :template "{{ .Name}}" :sort nil :format nil)
    (:name "Active" :width 7 :template "{{ .Active}}" :sort nil :format nil)
    (:name "Driver" :width 12 :template "{{ .DriverName}}" :sort nil :format nil)
    (:name "State" :width 12 :template "{{ .State}}" :sort nil :format (lambda (x) (propertize x 'font-lock-face (docker-machine-status-face x))))
    (:name "URL" :width 30 :template "{{ .URL}}" :sort nil :format nil)
    (:name "Swarm" :width 10 :template "{{ .Swarm}}" :sort nil :format nil)
    (:name "Docker" :width 10 :template "{{ .DockerVersion}}" :sort nil :format nil)
    (:name "Errors" :width 10 :template "{{ .Error}}" :sort nil :format nil))
  "Column specification for docker machines.

The order of entries defines the displayed column order.
'Template' is the Go template passed to docker-machine-ls to create the column
data.   It should return a string delimited with double quotes.
'Sort function' is a binary predicate that should return true when the first
argument should be sorted before the second.
'Format function' is a function from string to string that transforms the
displayed values in the column."
  :group 'docker-machine
  :set 'docker-utils-columns-setter
  :get 'docker-utils-columns-getter
  :type '(repeat (list :tag "Column"
                       (string :tag "Name")
                       (integer :tag "Width")
                       (string :tag "Template")
                       (sexp :tag "Sort function")
                       (sexp :tag "Format function"))))

(defun docker-machine-parse (line)
  "Convert a LINE from \"docker machine ls\" to a `tabulated-list-entries' entry."
  (let* ((data (apply #'vector (s-split "\t" line)))
         (status (aref data 3)))
    (aset data 3 (propertize status 'font-lock-face (docker-machine-status-face status)))
    (list (aref data 0) data)))

(defun docker-machine-status-face (status)
  "Return the correct face according to STATUS."
  (cond
   ((s-equals? status "Running")
    'docker-face-status-up)
   ((s-equals? status "Stopped")
    'docker-face-status-down)
   (t
    'docker-face-status-other)))

(defun docker-machine-entries ()
  "Return the docker machines data for `tabulated-list-entries'."
  (let* ((templates (--map (plist-get it :template) docker-machine-columns))
         (fmt (format "%s\\t%s" docker-machine-id-template (string-join templates "\\t")))
         (data (docker-machine-run-docker-machine "ls" (docker-machine-ls-arguments) (format "--format=\"%s\"" fmt)))
         (lines (s-split "\n" data t)))
    (-map #'docker-machine-parse lines)))

(defun docker-machine-refresh ()
  "Refresh the machines list."
  (setq tabulated-list-entries (docker-machine-entries)))

(defun docker-machine-read-name ()
  "Read a machine name."
  (completing-read "Machine: " (-map #'car (docker-machine-entries))))

(defun docker-machine-run-docker-machine (action &rest args)
  "Execute \"`docker-machine-command' ACTION ARGS\"."
  (let ((command (format "%s %s %s" docker-machine-command action (s-join " " (-flatten (-non-nil args))))))
    (message command)
    (docker-shell-command-to-string command)))

(defun docker-machine-get-transient-action ()
  (s-replace "-" " " (s-chop-prefix "docker-machine" (symbol-name transient-current-command))))

(defun docker-machine-generic-action (action args)
  (interactive (list (docker-machine-get-transient-action)
                     (transient-args transient-current-command)))
  (--each (docker-utils-get-marked-items-ids)
    (docker-machine-run-docker-machine action args it))
  (tablist-revert))

;;;###autoload
(defun docker-machine-create (name driver)
  "Create a machine NAME using DRIVER."
  (interactive "sName: \nsDriver: ")
  (docker-machine-run-docker-machine "create" name "-d" driver)
  (tablist-revert))

;;;###autoload
(defun docker-machine-env-one (name)
  "Parse and set environment variables from \"docker-machine env NAME\" output."
  (interactive (list (docker-machine-read-name)))
  (--each-while
      (s-lines (docker-machine-run-docker-machine "env" name))
      (s-prefix? "export" it)
    (docker-machine-env-export it)))

(defun docker-machine-env-export (line)
  "Export the env for LINE."
  (let ((index (s-index-of "=" line)))
    (unless index
      (error (format "Cannot find separator in %s" line)))
    (setenv (substring line (length "export ") index) (substring line (+ 2 index) -1))))

(defun docker-machine-env-selection ()
  "Run \"docker-machine env\" on selected machine."
  (interactive)
  (docker-utils-ensure-items)
  (let ((marked (docker-utils-get-marked-items-ids)))
    (when (/= (length marked) 1)
      (error "Can only set environment vars for one machine at a time"))
    (docker-machine-env-one (car marked))
    (tablist-revert)))

(docker-utils-transient-define-prefix docker-machine-env ()
  "Transient for setting up environment variables."
  :man-page "docker-machine-env"
  [:description docker-utils-generic-actions-heading
   ("E" "Env" docker-machine-env-selection)])

(defun docker-machine-ls-arguments ()
  "Return the latest used arguments in the `docker-machine-ls' transient."
  (car (alist-get 'docker-machine-ls transient-history)))

(transient-define-prefix docker-machine-ls ()
  "Transient for listing machines."
  :man-page "docker-machine-ls"
  ["Arguments"
   ("f" "Filter" "--filter " read-string)
   ("t" "Timeout" "--timeout " transient-read-number-N0)]
  ["Actions"
   ("l" "List" tablist-revert)])

(docker-utils-transient-define-prefix docker-machine-restart ()
  "Transient for restarting machines."
  :man-page "docker-machine-restart"
  [:description docker-utils-generic-actions-heading
   ("R" "Restart" docker-machine-generic-action)])

(docker-utils-transient-define-prefix docker-machine-rm ()
  "Transient for removing machines."
  :man-page "docker-machine-rm"
  :value '("-y")
  ["Arguments"
   ("f" "Force" "-f")
   ("y" "Automatic yes" "-y")]
  [:description docker-utils-generic-actions-heading
   ("D" "Remove" docker-machine-generic-action)])

(docker-utils-transient-define-prefix docker-machine-start ()
  "Transient for starting machines."
  :man-page "docker-machine-start"
  [:description docker-utils-generic-actions-heading
   ("S" "Start" docker-machine-generic-action)])

(docker-utils-transient-define-prefix docker-machine-stop ()
  "Transient for stoping machines."
  :man-page "docker-machine-stop"
  [:description docker-utils-generic-actions-heading
   ("O" "Stop" docker-machine-generic-action)])

(transient-define-prefix docker-machine-help ()
  "Help transient for docker machine."
  ["Docker machines help"
   ("C" "Create"     docker-machine-create)
   ("D" "Remove"     docker-machine-rm)
   ("E" "Env"        docker-machine-env-selection)
   ("O" "Stop"       docker-machine-stop)
   ("R" "Restart"    docker-machine-restart)
   ("S" "Start"      docker-machine-start)
   ("l" "List"       docker-machine-ls)])

(defvar docker-machine-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'docker-machine-help)
    (define-key map "C" 'docker-machine-create)
    (define-key map "D" 'docker-machine-rm)
    (define-key map "E" 'docker-machine-env-selection)
    (define-key map "O" 'docker-machine-stop)
    (define-key map "R" 'docker-machine-restart)
    (define-key map "S" 'docker-machine-start)
    (define-key map "l" 'docker-machine-ls)
    map)
  "Keymap for `docker-machine-mode'.")

;;;###autoload
(defun docker-machines ()
  "List docker machines."
  (interactive)
  (docker-utils-pop-to-buffer "*docker-machines*")
  (docker-machine-mode)
  (tablist-revert))

(define-derived-mode docker-machine-mode tabulated-list-mode "Machines Menu"
  "Major mode for handling a list of docker machines."
  (setq tabulated-list-format (docker-utils-columns-list-format docker-machine-columns))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key docker-machine-default-sort-key)
  (add-hook 'tabulated-list-revert-hook 'docker-machine-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-machine)

;;; docker-machine.el ends here
