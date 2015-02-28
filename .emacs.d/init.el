;;; init.el --- Main Emacs config

;;; Commentary:

;; Built on these foudational tools:

;;  Cask: Manage the config as an elisp project
;;  https://github.com/cask/cask

;;  use-package: isolate package configuration
;;  https://github.com/jwiegley/use-package

;;  pallet: Keep track of installed packages
;;  https://github.com/rdallasgray/pallet

;;; Code:

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
