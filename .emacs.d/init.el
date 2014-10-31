(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

(require 'use-package)

(use-package use-package
  :config (setq use-package-verbose t)
  :bind ("C-c C-k" . describe-personal-keybindings))

(add-to-list 'load-path "~/.emacs.d")
(require 'config)
(require 'modes)
