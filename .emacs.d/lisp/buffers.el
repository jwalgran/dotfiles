;;; buffers.el --- Functions for navigating Emacs buffers

;;; Commentary:

;; Helpers for opening a split above or below the current window and
;; prompting to select a buffer to load in the new window.

;;; Code:

(defun visit-buffer-below ()
  "Split the window horizontally and prompt for a buffer to load."
  (interactive)
  (split-window-below)
  (other-window 1)
  (ido-switch-buffer))

(defun visit-buffer-right ()
  "Split the window vertically and prompt for a buffer to load."
  (interactive)
  (split-window-right)
  (other-window 1)
  (ido-switch-buffer))

(provide 'buffers)
;;; buffers ends here
