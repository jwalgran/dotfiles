;;; modes.el --- Mode-specific Emacs setup

;;; Commentary:

;; Configure installed packages

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ido / imenu

(ido-mode t)
(ido-ubiquitous-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)
(set-default 'imenu-auto-rescan t)

(require 'ido-vertical-mode)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(require 'flx-ido)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
;;;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)


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
     '(js2-bounce-indent-p t)
     '(js2-auto-indent-flag f))

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
  :bind ("C-c g" . magit-status))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Helm

(use-package helm
  :demand t
  :bind (("C-x C-b" . helm-mini)
         ("C-c h w" . helm-swoop))
  :init
  (progn
    ;; config from http://www.reddit.com/r/emacs/comments/2z7nbv/lean_helm_window/
    (setq helm-display-header-line nil) ;; t by default
    (setq helm-autoresize-max-height 30)
    (setq helm-autoresize-min-height 30)
    (setq helm-split-window-in-side-p t)
    (defun helm-toggle-header-line ()
      (if (= (length helm-sources) 1)
          (set-face-attribute 'helm-source-header nil :height 0.1)
        (set-face-attribute 'helm-source-header nil :height 1.0)))

    (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)

    (semantic-mode 1)
    (require 'helm-config)
    (global-set-key (kbd "C-c h") 'helm-command-prefix)))





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
    (key-chord-define-global "jk" 'avy-goto-word-or-subword-1)
    (key-chord-define-global "jj" 'avy-goto-char)
    (key-chord-define-global "jl" 'avy-goto-line)
    (key-chord-define-global "fj" 'helm-buffers-list)
    (key-chord-define-global "fk" 'ido-find-file)
    (key-chord-define-global "qq" 'projectile-find-file)
    (key-chord-define-global "qw" 'mc/edit-lines)
    (key-chord-define-global ",." 'undo)
    (key-chord-define-global "cv" 'reindent-then-newline-and-indent)
    (key-chord-define-global "4r" "$")
    (key-chord-define-global "xc" 'hippie-expand)
    (key-chord-define-global "zx" 'fiplr-find-file)
    (key-chord-define-global "dk" 'smex)
    (key-chord-mode +1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multiple-cursors
;; Reference: http://endlessparentheses.com/multiple-cursors-keybinds.html

(use-package multiple-cursors
  :init
  (progn
    (define-prefix-command 'endless/mc-map)

    (define-key ctl-x-map "m" 'endless/mc-map)

    (define-key endless/mc-map "i" #'mc/insert-numbers)
    (define-key endless/mc-map "h" #'mc-hide-unmatched-lines-mode)
    (define-key endless/mc-map "a" #'mc/mark-all-like-this)

    (define-key endless/mc-map "d"
      #'mc/mark-all-symbols-like-this-in-defun)
    (define-key endless/mc-map "r" #'mc/reverse-regions)
    (define-key endless/mc-map "s" #'mc/sort-regions)
    (define-key endless/mc-map "l" #'mc/edit-lines)
    (define-key endless/mc-map "\C-a"
      #'mc/edit-beginnings-of-lines)
    (define-key endless/mc-map "\C-e"
      #'mc/edit-ends-of-lines))
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-S-c C->"   . mc/mark-all-like-this-dwim)))

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

(defun flycheck-list-errors-only-when-errors ()
  (interactive)
  (if flycheck-current-errors
      (flycheck-list-errors)
    (-when-let (buffer (get-buffer flycheck-error-list-buffer))
      (dolist (window (get-buffer-window-list buffer))
        (quit-window nil window)))))

(use-package flycheck
  :config
  (progn
    (setq flycheck-disabled-checkers '(html-tidy))
    (add-hook 'after-init-hook 'global-flycheck-mode)
    (setq flycheck-ruby-rubocop-executable "~/.rbenv/shims/rubocop")))

;;;; Add this to run flycheck on every save
;;(add-hook 'before-save-hook 'flycheck-list-errors-only-when-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; undo-tree

(use-package undo-tree
  :init
  (progn
    (global-undo-tree-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; browse-kill-ring

(use-package browse-kill-ring
  :bind
  (("C-M-y" . browse-kill-ring)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sr-speedbar

(use-package sr-speedbar
  :init
  (progn
    (setq speedbar-use-images nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bbyac

(use-package bbyac
  :init
  (progn
    (bbyac-global-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; projectile

(use-package projectile
  :init
  (progn
    (setq magit-repository-directories
          (mapcar
           (lambda (dir)
             (substring dir 0 -1))
           (cl-remove-if-not
            (lambda (project)
              (unless (file-remote-p project)
                (file-directory-p (concat project "/.git/"))))
            (projectile-relevant-known-projects))))
    (setq magit-repo-dirs-depth 1)
    (setq magit-completing-read-function 'magit-ido-completing-read)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; scripty-client

(use-package scripty-client
  :bind
  (("C-0" . scripty)
   ("C-9" . scripty/rerun)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; clojure-mode

(use-package clojure-mode
  :init
  (progn
    (add-hook 'clojure-mode-hook #'paredit-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; clj-refactor

;; TODO: This is a heavy package. Wait until you are writing a
;; bunch of Clojure to use it.

;; (use-package clj-refactor
;;   :init
;;   (progn
;;     (add-hook 'clojure-mode-hook
;;               (lambda ()
;;                 (clj-refactor-mode 1)
;;                 (cljr-add-keybindings-with-prefix "C-c C-n")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; quickrun

(use-package quickrun
  :init
  (progn
    (quickrun-add-command "jrnl"
                          '((:command . "jrnl")
                            (:exec . ("%c < %s"))
                            (:default-directory . "/tmp")))

    (quickrun-add-command "markdown/doku"
                          '((:command . "pandoc")
                            (:exec . "%c --from=markdown --to=dokuwiki %o %s %a")
                            (:default-directory . "/tmp"))
                          :mode 'markdown-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tramp / vagrant-tramp

(use-package tramp
  :init
  (progn
    (vagrant-tramp-enable)))


(provide 'modes)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org

(setq org-src-fontify-natively t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; monroe

;; https://github.com/sanel/monroe

(use-package monroe
  :init
  (progn
    (add-hook 'clojure-mode-hook 'clojure-enable-monroe)))


;;; modes ends here
