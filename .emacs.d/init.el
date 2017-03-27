;;; init.el --- Main Emacs config

;;; Commentary:

;; Built on these foundational tools:

;;  Cask: Manage the config as an elisp project
;;  https://github.com/cask/cask

;;  use-package: isolate package configuration
;;  https://github.com/jwiegley/use-package

;;  pallet: Keep track of installed packages
;;  https://github.com/rdallasgray/pallet

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(js2-auto-indent-flag f)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

(require 'use-package)

(use-package use-package
  :config (setq use-package-verbose t)
  :bind ("C-c C-k" . describe-personal-keybindings))

(add-to-list 'load-path "~/.emacs.d/lisp")
(load "config")
(load "modes")

(provide 'init)
;;; init.el ends here
