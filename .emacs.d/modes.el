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
  :defer t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    ;; tweak js2 intentation
    ;; http://feeding.cloud.geek.nz/posts/proper-indentation-of-javascript-files/
    (custom-set-variables
     '(js2-basic-offset 4)
     '(js2-bounce-indent-p t))

    ;; convert the word "function" to "λ"
    ;; https://github.com/ahinz/emacs-config/blob/e04cda76030f7adaacfff0706a267a7c3d71c010/.emacs.d/ah-modes.el#L183
    (font-lock-add-keywords
     'js2-mode `(("\\(function\\) *("
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1) ?λ)
                            nil)))))


    ;; React JSX files are .js with special syntax sugar
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; js2-refactor

(use-package js2-refactor
  :defer t
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
  :defer t
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
      (jump-to-register :magit-fullscreen)))
  :config (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
  :bind ("C-c g" . magit-status))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Helm

(use-package helm
  :demand t
  :bind (("C-c h" . helm-mini)
         ("C-x C-b" . helm-buffers-list)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Paredit

(use-package paredit
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           'enable-paredit-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; key-chord

(use-package key-chord
  :init
  (progn
    (key-chord-define-global "jk" 'ace-jump-word-mode)
    (key-chord-define-global "jj" 'ace-jump-char-mode)
    (key-chord-define-global "jl" 'ace-jump-line-mode)
    (key-chord-define-global "qq" 'projectile-find-file)
    (key-chord-define-global "qw" 'mc/edit-lines)
    (key-chord-define-global ",." 'undo)
    (key-chord-define-global "cv" 'reindent-then-newline-and-indent)
    (key-chord-define-global "4r" "$")
    (key-chord-define-global "xc" 'hippie-expand)
    (key-chord-define-global "zx" 'fiplr-find-file)
    (key-chord-mode +1)))


(provide 'modes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multiple-cursors

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; markdown

(use-package markdown
  :init
  (progn
    (defun markdown-preview-file ()
      "run Marked on the current file and revert the buffer"
      (interactive)
      (shell-command
       (format "open -a \"/Applications/Marked 2.app\" %s"
               (shell-quote-argument (buffer-file-name))))))
  :bind (("C-c m" . markdown-preview-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; flycheck

(use-package flycheck
  :config
  (progn
    (add-hook 'after-init-hook 'global-flycheck-mode)))
