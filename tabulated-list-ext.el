(require 'dash)

(defun tabulated-list-mark (&optional how-many)
  "Mark move to the next line."
  (interactive "p")
  (--dotimes how-many (tabulated-list-put-tag "*" t)))

(defun tabulated-list-unmark (&optional how-many)
  "Unmark and move to the next line."
  (interactive "p")
  (--dotimes how-many (tabulated-list-put-tag "" t)))

(defun tabulated-list-toggle-marks ()
  "Toggle marks."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (setq cmd (char-after))
      (tabulated-list-put-tag (if (eq cmd ?*) "" "*") t))))

(defun tabulated-list-unmark-all ()
  "Unmark all."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (tabulated-list-put-tag "" t))))

(defvar tabulated-list-ext-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'tabulated-list-mark)
    (define-key map "u" 'tabulated-list-unmark)
    (define-key map "t" 'tabulated-list-toggle-marks)
    (define-key map "U" 'tabulated-list-unmark-all)
    map)
  "Keymap for `tabulated-list-ext-mode'.")

(define-derived-mode tabulated-list-ext-mode tabulated-list-mode "Extended Tabulated List"
  "Extended Tabulated List")

(provide 'tabulated-list-ext)
