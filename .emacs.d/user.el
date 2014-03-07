;; This is where your customizations should live

;; env PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it

;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 20) (height . 20)))


;; Place downloaded elisp files in this directory. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
;; Uncomment this to increase font size
;; (set-face-attribute 'default nil :height 140)
(load-theme 'tomorrow-night-bright t)

;; Fonts
(when (window-system)
  (set-frame-font "Source Code Pro")
  (set-face-attribute 'default nil :font "Source Code Pro" :height 140)
  (set-face-font 'default "Source Code Pro"))

;; Flyspell often slows down editing so it's turned off
(remove-hook 'text-mode-hook 'turn-on-flyspell)

;; Clojure
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(setq nrepl-history-file "~/.emacs.d/nrepl-history")
(setq nrepl-popup-stacktraces t)
(setq nrepl-popup-stacktraces-in-repl t)
(add-hook 'nrepl-connected-hook
          (defun pnh-clojure-mode-eldoc-hook ()
            (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
            (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
            (nrepl-enable-on-existing-clojure-buffers)))
(add-hook 'nrepl-mode-hook 'subword-mode)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)

;; hippie expand - don't try to complete with file names
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name hippie-expand-try-functions-list))
(setq hippie-expand-try-functions-list (delete 'try-complete-file-name-partially hippie-expand-try-functions-list))

(setq ido-use-filename-at-point nil)

;; Save here instead of littering current directory with emacs backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; Jabber
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jabber-auto-reconnect t)
 '(jabber-avatar-verbose nil)
 '(jabber-vcard-avatars-retrieve nil)
 '(jabber-chat-buffer-format "*-jabber-%n-*")
 '(jabber-history-enabled t)
 '(jabber-mode-line-mode t)
 '(jabber-roster-buffer "*-jabber-*")
 '(jabber-roster-line-format " %c %-25n %u %-8s")
 '(jabber-show-offline-contacts t))

;; Tramp

;; Allow sudo editing
;; http://stackoverflow.com/a/4725727
;; TODO: Fix, not working with ssh aliases
;;(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'"
                                        ;"/ssh:%h:"))))

;; linum

;; Add a space and a vertical bar after the line numbers
(setq linum-format  "%4d \u2502 ")

;; Set chrome as the default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; js2
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; tweak js2 intentation
;; http://feeding.cloud.geek.nz/posts/proper-indentation-of-javascript-files/
(custom-set-variables
 '(js2-basic-offset 4)
 '(js2-bounce-indent-p t)
 )

;; Duplicate line
;; http://stackoverflow.com/a/88828
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )
(global-set-key (kbd "C-d") 'duplicate-line)

;; Reload buffers from disk
;; http://stackoverflow.com/a/1481706
(global-auto-revert-mode t)

;; expand region
(global-set-key (kbd "C-=") 'er/expand-region)
(delete-selection-mode 1)

;; emmet (zen coding)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css

;; custom bindings
(defun ansi-term-default ()
  (interactive)
  (ansi-term "/bin/bash"))
(global-set-key (kbd "C-x a") 'ansi-term-default)

;; custom movement
;; Move more quickly (http://whattheemacsd.com/)
(global-set-key (kbd "C-s-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-s-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-s-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-s-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))


;; move lines (whattheemacsd.com)
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key (kbd "<C-s-up>") 'move-line-up)
(global-set-key (kbd "<C-s-down>") 'move-line-down)

;; Visit a buffer in a lower split
(defun visit-buffer-below ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (ido-switch-buffer)
)
(global-set-key (kbd "C-x n") 'visit-buffer-below)

;; Visit a buffer in a right split
(defun visit-buffer-right ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (ido-switch-buffer)
)
(global-set-key (kbd "C-x C-n") 'visit-buffer-right)

;; bind ace-jump-mode
(global-set-key (kbd "C-x C-a") 'ace-jump-mode)

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

;; Functions to open lines above and below with indenting (vim-ish)
(defun add-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-for-tab-command))

(defun add-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(global-set-key (kbd "C-c O") 'add-line-above)
(global-set-key (kbd "C-c o") 'add-line-below)

;; Insert Date
;;
(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%A, %B %d %Y")
                 ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "en_US"))
    (insert (format-time-string format))))

(global-set-key (kbd "C-c d") 'insert-date)

;; Don't show messages that will never be read

;; http://bzg.fr/emacs-strip-tease.html
(setq initial-scratch-message "")
(setq inhibit-startup-message t)

;; dired-details+
(require 'dired-details+)

;; helm
(global-set-key (kbd "C-c h") 'helm-mini)

