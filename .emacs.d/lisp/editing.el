;;; editing.el --- Functions for general text editing

;;; Commentary:

;; These functions are emulations of vim commands

;;; Code:

(defun duplicate-line()
  "Duplicate the line at the point."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(defun add-line-above ()
  "Open a new blank line above the point."
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-for-tab-command))

(defun add-line-below ()
  "Open a new blank line below the point."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(provide 'editing)
;;; editing ends here
