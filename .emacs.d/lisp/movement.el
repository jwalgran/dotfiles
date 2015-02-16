;; Move more quickly (http://whattheemacsd.com/)
(defun fast-next()
  (interactive)
  (ignore-errors (next-line 5)))

(defun fast-previous()
  (interactive)
  (ignore-errors (previous-line 5)))

(defun fast-forward()
  (interactive)
  (ignore-errors (forward-char 5)))

(defun fast-back()
  (interactive)
  (ignore-errors (backward-char 5)))


;; https://github.com/ahinz/emacs-config/blob/7e025076097f045aea2a0aedd0523ee996753346/.emacs.d/ah-config.el#L63
(defun smart-start-of-line ()
  (interactive)
  (if (eq (point) (point-at-bol))
      (back-to-indentation)
    (move-beginning-of-line nil)))


(provide 'movement)
