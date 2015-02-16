(defun visit-buffer-below ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (ido-switch-buffer))

(defun visit-buffer-right ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (ido-switch-buffer))

(provide 'buffers)
