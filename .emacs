;; Increase gc-cons-threshold during startup
(setq gc-cons-threshold most-positive-fixnum)

;; (defun efs/display-startup-time ()
;;   (message "Emacs loaded in %s with %d garbage collections."
;; 	   (format "%.2f seconds"
;; 		   (float-time
;; 		    (time-subtract after-init-time before-init-time)))
;; 	   gcs-done))

;; (add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; prevent package.el from loading packages prior to their init-file loading.
(setq package-enable-at-startup nil)

;; Package install
(require 'package)
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Load custom files with load-file
(load-file "/home/ds/.emacs.rc/org-mode-rc.el")
(load-file "/home/ds/.emacs.rc/emacs-misc-rc.el")
(load-file "/home/ds/.emacs.rc/emacs-git-rc.el")
(load-file "/home/ds/.emacs.rc/emacs-format-rc.el")

;; Font settings
(set-face-attribute 'default nil
                    :font "FiraCode Nerd Font-10.6")
(set-fontset-font t 'unicode "Firacode Nerd Font" nil 'prepend)

;; Basic configuration
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)

;; Backup directory
(setq backup-directory-alist '(("." . "/home/ds/.emacs_show")))

;; Smex setup
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Ido mode
(ido-mode 1)
(ido-everywhere 1)

;; Keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "M-<left>") 'other-window)
(global-set-key (kbd "M-<right>") 'other-window)

;; Theme
(use-package gruber-darker-theme
  :config (load-theme 'gruber-darker t))

;; Mode enable
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; NASM mode
(use-package nasm-mode
  :mode "\\.asm\\'")

;; Other modes, deferred loading
(use-package scala-mode :defer t)
(use-package d-mode :defer t)
(use-package yaml-mode :defer t)
(use-package glsl-mode :defer t)
(use-package tuareg :defer t)
(use-package lua-mode :defer t)
(use-package less-css-mode :defer t)
(use-package graphviz-dot-mode :defer t)
(use-package clojure-mode :defer t)
(use-package cmake-mode :defer t)
(use-package rust-mode :defer t)
(use-package nim-mode :defer t)
(use-package jinja2-mode :defer t)
(use-package markdown-mode :defer t)
(use-package purescript-mode :defer t)
(use-package nix-mode :defer t)
(use-package dockerfile-mode :defer t)
(use-package toml-mode :defer t)
(use-package nginx-mode :defer t)
(use-package kotlin-mode :defer t)
(use-package go-mode :defer t)
(use-package php-mode :defer t)
(use-package racket-mode :defer t)
(use-package qml-mode :defer t)
(use-package ag :defer t)
(use-package hindent :defer t)
(use-package elpy :defer t)
(use-package typescript-mode :defer t)
(use-package rfc-mode :defer t)

;; Tramp auto-save directory
(setq tramp-auto-save-directory "/tmp")

;; Confirm before exiting Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Reduce gc-cons-threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 40 1024 1024))))

(custom-set-variables
 '(custom-safe-themes
   '("bddf21b7face8adffc42c32a8223c3cc83b5c1bbd4ce49a5743ce528ca4da2b6" default))
 '(package-selected-packages
   '(gnu-elpa-keyring-update lsp-mode haskell-mode lsp-java magit java-imports android-env ## android-mode nasm-mode ggtags nix-mode move-text nim-mode lua-mode purescript-mode popup unicode-fonts yaml-mode nginx-mode racket-mode autothemer async glsl-mode command-log-mode all-the-icons ivy smex ag tuareg dockerfile-mode toml-mode elpy cmake-mode visual-fill-column kotlin-mode clojure-mode jinja2-mode graphviz-dot-mode csharp-mode rfc-mode org-bullets avy go-mode rust-mode typescript-mode php-mode d-mode use-package rainbow-delimiters multiple-cursors hindent scala-mode qml-mode paredit sqlite3 gruber-darker-theme))
 '(warning-suppress-log-types '((use-package) (use-package))))

(custom-set-faces)
