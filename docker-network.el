;;; docker-network.el --- Emacs interface to docker-network  -*- lexical-binding: t -*-

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
(require 'json)
(require 'tablist)
(require 'transient)

(require 'docker-core)
(require 'docker-utils)

(defgroup docker-network nil
  "Docker network customization group."
  :group 'docker)

(defconst docker-network-id-template
  "{{ json .ID }}"
  "This Go template extracts the id which will be passed to transient commands.")

(defconst docker-network-default-columns
  '((:name "Network ID" :width 20 :template "{{json .ID}}" :sort nil :format nil)
    (:name "Name" :width 50 :template "{{json .Name}}" :sort nil :format nil)
    (:name "Driver" :width 10 :template "{{json .Driver}}" :sort nil :format nil)
    (:name "Scope" :width 10 :template "{{json .Scope}}" :sort nil :format nil))
  "Column spec for docker-network-entries.")

(defcustom docker-network-default-sort-key '("Name" . nil)
  "Sort key for docker networks.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'docker-network
  :type '(cons (string :tag "Column Name")
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defcustom docker-network-column-order docker-network-default-columns
  "Column specification for docker networks.

The order of entries defines the displayed column order.
'Template' is the Go template passed to docker-network-ls to generate the column data."
  :group 'docker-network
  ;; add plist symbols
  :set (lambda (sym xs)
         (let ((res (--map (-interleave '(:name :width :template :sort :format) it)
                           xs)))
           (set sym res)))
  ;; removes plist symbols
  :get (lambda (sym)
         (--map
          (-map (-partial #'plist-get it) '(:name :width :template :sort :format))
          (symbol-value sym)))
  :type '(repeat (list :tag "Column"
                       (string :tag "Name")
                       (integer :tag "Width")
                       (string :tag "Template")
                       (sexp :tag "Sort function")
                       (sexp :tag "Format function"))))

(defun docker-network-parse (column-specs line)
  "Convert a LINE from \"docker network ls\" to a `tabulated-list-entries' entry."
  (condition-case nil
      (let* ((data (json-read-from-string line)))
        ;; apply format function, if any
        (--each-indexed
            column-specs
          (let ((fmt-fn (plist-get it :format))
                (data-index (+ it-index 1)))
            (when fmt-fn (aset data data-index (apply fmt-fn (list (aref data data-index)))))))

        (list (aref data 0) (seq-drop data 1)))
    (json-readtable-error
     (error "Could not read following string as json:\n%s" line))))

(defun docker-network-entries ()
  "Return the docker networks data for `tabulated-list-entries'."
  (let* ((fmt (docker-utils-make-format-string docker-network-id-template docker-network-column-order))
         (data (docker-run-docker "network ls" (docker-network-ls-arguments) (format "--format=\"%s\"" fmt)))
         (lines (s-split "\n" data t)))
    (-map (-partial #'docker-network-parse docker-network-column-order) lines)))

(defun docker-network-refresh ()
  "Refresh the networks list."
  (setq tabulated-list-entries (docker-network-entries)))

(defun docker-network-read-name ()
  "Read a network name."
  (completing-read "Network: " (-map #'car (docker-network-entries))))

(defun docker-network-ls-arguments ()
  "Return the latest used arguments in the `docker-network-ls' transient."
  (car (alist-get 'docker-network-ls transient-history)))

(transient-define-prefix docker-network-ls ()
  "Transient for listing networks."
  :man-page "docker-network-ls"
  ["Arguments"
   ("f" "Filter" "--filter " read-string)
   ("n" "Don't truncate" "--no-trunc")]
  ["Actions"
   ("l" "List" tablist-revert)])

(docker-utils-transient-define-prefix docker-network-rm ()
  "Transient for removing networks."
  :man-page "docker-network-rm"
  [:description docker-utils-generic-actions-heading
   ("D" "Remove" docker-utils-generic-action)])

(transient-define-prefix docker-network-help ()
  "Help transient for docker networks."
  ["Docker networks help"
   ("D" "Remove"     docker-network-rm)
   ("I" "Inspect"    docker-utils-inspect)
   ("l" "List"       docker-network-ls)])

(defvar docker-network-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'docker-network-help)
    (define-key map "D" 'docker-network-rm)
    (define-key map "l" 'docker-network-ls)
    (define-key map "I" 'docker-utils-inspect)
    map)
  "Keymap for `docker-network-mode'.")

;;;###autoload
(defun docker-networks ()
  "List docker networks."
  (interactive)
  (docker-utils-pop-to-buffer "*docker-networks*")
  (docker-network-mode)
  (tablist-revert))

(define-derived-mode docker-network-mode tabulated-list-mode "Networks Menu"
  "Major mode for handling a list of docker networks."
  (setq tabulated-list-format (docker-utils-column-order-list-format docker-network-column-order))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key docker-network-default-sort-key)
  (add-hook 'tabulated-list-revert-hook 'docker-network-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-network)

;;; docker-network.el ends here
