;;; whitespace.el --- Functions for manipulating whitespace

;;; Commentary:

;; These functions are focused on bulk whitespace cleanup operations

;;; Code:

(defun untabify-buffer ()
  "Call the 'untabify' function with the entire content of the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Call the 'indent-region' function on the entire content of the current buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "'untabify' and 'delete-trailing-whitespace' on the current buffer."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace))

(provide 'whitespace)
;;; whitespace ends here
