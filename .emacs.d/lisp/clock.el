;;; clock.el --- Functions for inserting dates and times

;;; Commentary:

;; Useful in notetaking

;;; Code:

(defun insert-date (prefix)
  "Insert the current date.  With PREFIX argument, use ISO format.
With two PREFIX arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%Y-%m-%d %A")
                 ((equal prefix '(16)) "%A, %B %d %Y")))
        (system-time-locale "en_US"))
    (insert (format-time-string format))))

(global-set-key (kbd "C-c d") 'insert-date)

(provide 'clock)
;;; clock ends here
