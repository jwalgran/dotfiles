;;; config.el --- General Emacs setup

;;; Commentary:

;; Set up perfered defaults and general editing functionality.

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Memory

; https://github.com/lewang/flx#gc-optimization
(setq gc-cons-threshold 20000000)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Styling

(set-face-background 'mode-line "black")
(set-face-attribute 'mode-line nil :box "#222" :underline nil :overline nil)

(set-face-background 'modeline-inactive "black")
(set-face-attribute 'modeline-inactive nil :box "#222" :underline nil :overline nil)

;; nil uses the default fringe values
(fringe-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Scratch Buffer

(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq initial-major-mode 'text-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Minibuffer

(defalias 'yes-or-no-p 'y-or-n-p) ;; Single letter yes/no

(use-package smex
  :demand t
  :bind ("M-x" . smex)
  :init
  (progn
    (setq smex-save-file (concat user-emacs-directory ".smex-items"))
    (smex-initialize)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Modeline

(which-function-mode)
;;;; Uncomment this block to show the function name in the header
;;
;;(setq-default header-line-format
;;              '((which-func-mode ("" which-func-format " "))))
;;(setq mode-line-misc-info
;;            We remove Which Function Mode from the mode line, because it's mostly
;;            invisible here anyway.
;;            (assq-delete-all 'which-func-mode mode-line-misc-info))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Frame

(use-package maxframe
  :demand t
  :init
  (progn
    (add-hook 'window-setup-hook 'maximize-frame t)))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; File Handling

(global-auto-revert-mode t)
(setq make-backup-files nil)
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)

;;---- http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/

(defun create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'create-non-existent-directory)

;;------------------------------------------------

(use-package dired
  :defer t
  :init
  (progn
    (use-package dired-details+
      :ensure dired-details+)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; File Finding

(use-package fiplr
  :defer t
  :bind ("C-," . fiplr-find-file))

(use-package projectile
  :init
  (progn
    (projectile-global-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Appearance

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(when (window-system)
  (set-frame-font "Source Code Pro")
  (set-face-attribute 'default nil :font "Source Code Pro" :height 140)
  (set-face-font 'default "Source Code Pro"))

(use-package color-theme-sanityinc-tomorrow
  :init (load-theme 'sanityinc-tomorrow-bright t)
  :config (set-face-attribute 'mode-line nil
                              :box nil))

(set 'inhibit-startup-screen t)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Spell Check

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Whitespace

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)

(use-package whitespace
  :init
  (progn
    (add-hook 'before-save-hook 'cleanup-buffer 'local)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Syntax Hilighting

(use-package font-lock
  :init
  (progn
    ;; turn on maximum syntax highlighting
    (setq font-lock-maximum-decoration t)
    (global-font-lock-mode t)))


(use-package rainbow-delimiters
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Navigation Within Buffers

(use-package ace-jump-mode
  :defer t
  :bind ("C-." . ace-jump-mode))

;; Cursor movement within CamelCaseWords
(global-subword-mode 1)

(use-package movement
  :bind (("C-s-n" . fast-next)
         ("C-s-p" . fast-previous)
         ("C-s-f" . fast-forward)
         ("C-s-b" . fast-backward)
         ("C-a"   . smart-start-of-line)))

(use-package paragraphs
  :bind (("M-n" . forward-paragraph)
         ("M-p" . backward-paragraph)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Editing

(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))

(use-package editing
  :bind (("C-c C-d" . duplicate-line)
         ("C-c O"   . add-line-above)
         ("C-c o"   . add-line-below)))

(use-package change-inner
  :defer t
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package clock
  :bind ("C-c d" . insert-date))

(use-package smartparens
  :init
  (progn
    (require 'smartparens-config)))

(show-paren-mode 1)

(use-package yasnippet
  :init
  (progn
    (add-hook 'term-mode-hook (lambda()
                                (setq yas-dont-activate t)))
    (yas-global-mode 1)
    (yas-reload-all)))

(delete-selection-mode 1)

(use-package fill
  :bind (("C-c f" . fill-paragraph)))

(global-set-key "\C-q" 'backward-kill-word)

(defun endless/comment-line-or-region (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))

(global-set-key (kbd "C-;") 'endless/comment-line-or-region)

;; http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Buffers

(use-package buffers
  :bind (("C-x n" . visit-buffer-below)
         ("C-x C-n" . visit-buffer-right)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Searching

(global-set-key (kbd "C-'") 'rgrep)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Shell


;; exec-path-from-shell properly sets up the environment for GUI emacs
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Terminals

(use-package term
  :demand t
  :init
  (progn
    ;; https://github.com/ahinz/emacs-config/blob/7e025076097f045aea2a0aedd0523ee996753346/.emacs.d/ah-modes.el#L268
    (defun open-named-term (new-buffer-name cmd &rest switches)
      (setq term-ansi-buffer-name (generate-new-buffer-name new-buffer-name))
      (setq term-ansi-buffer-name (apply 'make-term term-ansi-buffer-name cmd nil switches))
      (set-buffer term-ansi-buffer-name)
      (term-mode)
      (term-char-mode)
      (term-set-escape-char ?\C-x)
      (switch-to-buffer term-ansi-buffer-name))

    ;; https://github.com/ahinz/emacs-config/blob/7e025076097f045aea2a0aedd0523ee996753346/.emacs.d/ah-modes.el#L268
    (defun open-term (name)
      (interactive "sName: ")
      (open-named-term name "/bin/bash"))

    (defun visit-ansi-term ()
      "If the current buffer is:
         1) a running ansi-term named *ansi-term*, rename it.
         2) a stopped ansi-term, kill it and create a new one.
         3) a non ansi-term, go to an already running ansi-term
            or start a new one while killing a defunt one"
      (interactive)
      (let ((is-term (string= "term-mode" major-mode))
            (is-running (term-check-proc (buffer-name)))
            (term-cmd "/bin/bash")
            (anon-term (get-buffer "*ansi-term*")))
        (if is-term
            (if is-running
                (if (string= "*ansi-term*" (buffer-name))
                    (call-interactively 'rename-buffer)
                  (if anon-term
                      (switch-to-buffer "*ansi-term*")
                    (ansi-term term-cmd)))
              (kill-buffer (buffer-name))
              (ansi-term term-cmd))
          (if anon-term
              (if (term-check-proc "*ansi-term*")
                  (switch-to-buffer "*ansi-term*")
                (kill-buffer "*ansi-term*")
                (ansi-term term-cmd))
            (ansi-term term-cmd)))))

    ;; Make ansi-term buffers close when you kill the shell process
    (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
      (if (memq (process-status proc) '(signal exit))
          (let ((buffer (process-buffer proc)))
            ad-do-it
            (kill-buffer buffer))
        ad-do-it))
    (ad-activate 'term-sentinel))

  :bind (("C-x a" . visit-ansi-term)
         ("C-x C-a" . open-term)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tags

;; http://stackoverflow.com/a/24899166
(defun find-tag-no-prompt ()
  "Jump to the tag at point without prompting."
  (interactive)
  (find-tag (find-tag-default)))
;; don't prompt when finding a tag
(global-set-key (kbd "M-.") 'find-tag-no-prompt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Other

;; https://sites.google.com/site/steveyegge2/effective-emacs
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'config)
;;; config.el ends here
