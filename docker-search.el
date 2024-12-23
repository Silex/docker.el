;;; docker-search.el --- Emacs interface to docker-search  -*- lexical-binding: t -*-

;; Author: Josh Bax <joshbax189@gmail.com>

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
(require 'aio)

(require 'docker-core)
(require 'docker-utils)

(defgroup docker-search nil
  "Docker search customization group."
  :group 'docker)

(defconst docker-search-id-template
  "{{ json .Name }}"
  "This Go template defines what will be passed to transient commands.")

(defcustom docker-search-default-sort-key '("Stars" . nil)
  "Sort key for docker search results.

This should be a cons cell (NAME . FLIP) where
NAME is a string matching one of the column names
and FLIP is a boolean to specify the sort order."
  :group 'docker-search
  :type '(cons (string :tag "Column Name"
                       :validate (lambda (widget)
                                   (unless (--any-p (equal (plist-get it :name) (widget-value widget)) docker-search-columns)
                                     (widget-put widget :error "Default Sort Key must match a column name")
                                     widget)))
               (choice (const :tag "Ascending" nil)
                       (const :tag "Descending" t))))

(defcustom docker-search-columns
  '((:name "Name" :width 30 :template "{{json .Name}}" :sort nil :format nil)
    (:name "Description" :width 80 :template "{{ json .Description }}" :sort nil :format nil)
    (:name "Stars" :width 10 :template "{{ json .StarCount }}" :sort docker-utils-numeric-sort :format nil)
    (:name "Official" :width 10 :template "{{ json .IsOfficial }}" :sort nil :format nil)
    (:name "Automated" :width 10 :template "{{ json .IsAutomated }}" :sort nil :format nil))
  "Column specification for docker search.

The order of entries defines the displayed column order.
'Template' is the Go template passed to docker-search-ls to create the column
data.   It should return a string delimited with double quotes.
'Sort function' is a binary predicate that should return true when the first
argument should be sorted before the second.
'Format function' is a function from string to string that transforms the
displayed values in the column."
  :group 'docker-search
  :set 'docker-utils-columns-setter
  :get 'docker-utils-columns-getter
  :type '(repeat (list :tag "Column"
                       (string :tag "Name")
                       (integer :tag "Width")
                       (string :tag "Template")
                       (sexp :tag "Sort function")
                       (sexp :tag "Format function"))))

(defvar docker-search-string
  ""
  "Last search string used with docker-search.")
(make-variable-buffer-local 'docker-search-string)

(aio-defun docker-search-entries (&optional args)
  "Return the docker search data for `tabulated-list-entries'."
  (let* ((fmt (docker-utils-make-format-string docker-search-id-template docker-search-columns))
         (data (aio-await (docker-run-docker-async "search" "--no-trunc" args (format "--format='%s'" fmt) docker-search-string)))
         (lines (s-split "\n" data t)))
    (when (eq 0 (length lines))
      (error "No results"))
      (setq tabulated-list-entries (-map (-partial #'docker-utils-parse docker-search-columns) lines))
      ;; necessary, as the callback may complete after this is called in the hook
      (tabulated-list-print)))

(defun docker-search-refresh ()
  "Refresh the search results."
  (docker-search-entries (docker-search-arguments)))

(defun docker-search-arguments ()
  "Return the latest used arguments in the `docker-search-ls' transient."
  (car (alist-get 'docker-search transient-history)))

;;;###autoload (autoload 'docker-search "docker-search" nil t)
(transient-define-prefix docker-search ()
  "Transient for searches."
  :man-page "docker-search"
  ["Arguments"
   ("a" "Automated" "--filter=is-automated=true")
   ("l" "Limit Results" "--limit=" read-string)
   ("o" "Official" "--filter=is-official=true")
   ("s" "Min Stars" "--filter=stars=" read-string)]
  ["Actions"
   ("S" "Search" docker-search-submit)])

(defun docker-search-browse (repository)
  "Open Docker Hub REPOSITORY with web browser."
  (interactive `(,(read-string "Repository: " (tabulated-list-get-id))))
  (let ((user-repo (s-split "/" repository)))
    (if (eq 1 (length user-repo))
        ;; special format for official ones
        (browse-url (format "https://hub.docker.com/_/%s" repository))
        (browse-url (format "https://hub.docker.com/r/%s/%s" (car user-repo) (cadr user-repo))))))

(defun docker-search-show-description ()
  "Echo description of entry at point."
  (interactive)
  (message (elt (tabulated-list-get-entry) 1)))

(aio-defun docker-search-pull-action ()
  "Run 'docker pull' on search selection displaying progress in a new buffer.

The transient may specify a tag, if multiple images are marked for pulling,
the same tag will be applied to each."
  (interactive)
  (-let* ((((tag) args) (--separate (string-match-p ":.*" it) (transient-args transient-current-command)))
          (promises (--map (docker-run-docker-async "pull" "-q" args (concat it tag)) (docker-utils-get-marked-items-ids))))
    (aio-await (aio-all promises)))
  (message "docker-pull complete"))

(transient-define-prefix docker-search-pull ()
  "Transient for pulling Docker Hub images."
  :man-page "docker-image-pull"
  ["Arguments"
   ("a" "Download all tags" "-a")
   ("t" "Download a specific tag" ":" read-string)]
  [:description docker-utils-generic-actions-heading
   ("F" "Pull selection" docker-search-pull-action)])

(transient-define-prefix docker-search-help ()
  "Transient for searches."
  :man-page "docker-search"
  ["Docker Search"
   ("F" "Pull Image" docker-search-pull)
   ("S" "New Search" docker-search)
   ("g" "Revert" tablist-revert)
   ("o" "Open in Browser" docker-search-browse)
   ("v" "Show description" docker-search-show-description)])

(defvar docker-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'docker-search-help)
    (define-key map "F" 'docker-search-pull)
    (define-key map "S" 'docker-search)
    (define-key map "g" 'tablist-revert)
    (define-key map "o" 'docker-search-browse)
    (define-key map "v" 'docker-search-show-description)
    map)
  "Keymap for `docker-search-mode'.")

(defun docker-search-submit (search)
  "Run docker-search on string SEARCH and display results in an interactive buffer."
  (interactive `(,(read-string "Search: " docker-search-string)))
  (let ((search-buffer (format "*docker-search %s*" search)))
    (docker-utils-pop-to-buffer search-buffer)
    (docker-search-mode)
    (with-current-buffer search-buffer
      (setq docker-search-string search)
      (tablist-revert))))

(define-derived-mode docker-search-mode tabulated-list-mode "Search Menu"
  "Major mode for handling docker search results."
  (setq tabulated-list-format (docker-utils-columns-list-format docker-search-columns))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key docker-search-default-sort-key)
  (add-hook 'tabulated-list-revert-hook 'docker-search-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'docker-search)

;;; docker-search.el ends here
