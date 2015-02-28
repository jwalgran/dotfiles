;;; movement.el --- Functions for moving the point

;;; Commentary:

;; Helpers to speed up navigation within buffers.

;;; Code:

;; Move more quickly (http://whattheemacsd.com/)
(defun fast-next()
  "Move the point down 5 lines."
  (interactive)
  (ignore-errors (next-line 5)))

(defun fast-previous()
  "Move the point up 5 lines."
  (interactive)
  (ignore-errors (previous-line 5)))

(defun fast-forward()
  "Move the point forward 5 characters."
  (interactive)
  (ignore-errors (forward-char 5)))

(defun fast-back()
  "Move the point backward 5 characters."
  (interactive)
  (ignore-errors (backward-char 5)))


;; https://github.com/ahinz/emacs-config/blob/7e025076097f045aea2a0aedd0523ee996753346/.emacs.d/ah-config.el#L63
(defun smart-start-of-line ()
  "Jump between the true start of the line and the first char of the line."
  (interactive)
  (if (eq (point) (point-at-bol))
      (back-to-indentation)
    (move-beginning-of-line nil)))


(provide 'movement)
;;; movement ends here
