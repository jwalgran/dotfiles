;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ido / imenu

(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)
(set-default 'imenu-auto-rescan t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; js2

(use-package js2-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    ;; tweak js2 intentation
    ;; http://feeding.cloud.geek.nz/posts/proper-indentation-of-javascript-files/
    (custom-set-variables
     '(js2-basic-offset 4)
     '(js2-bounce-indent-p t))

    ;; React JSX files are .js with special syntax sugar
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; js2-refactor

(use-package js2-refactor
  :init
  (progn
    (js2r-add-keybindings-with-prefix "C-c C-r")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; emmet (zen coding)

(use-package emmet-mode
  :defer t
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)    ;; Auto-start on any markup modes
    (add-hook 'css-mode-hook  'emmet-mode))) ;; enable Emmet's css


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Magit

(use-package magit
  :demand t
  :init
  (progn
    ;; full screen magit-status (http://whattheemacsd.com/)
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer"
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))

    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

  :bind ("C-c g" . magit-status))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Helm

(use-package helm
  :demand t
  :bind (("C-c h" . helm-mini)
         ("C-x C-b" . helm-buffers-list)))


(provide 'modes)
