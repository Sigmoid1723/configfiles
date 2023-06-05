;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1024 1024))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Package install
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
(setq package-archives
	     '(("melpa" . "https://melpa.org/packages/")
	       ("org" . "https://orgmode.org/elpa/")
	       ("elpa" . "https://elpa.gnu.org/packages")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; load all the files
(load "~/.emacs.rc/org-mode-rc.el")
(load "~/.emacs.rc/emacs-misc-rc.el")
(load "~/.emacs.rc/emacs-git-rc.el")
(load "~/.emacs.rc/emacs-format-rc.el")

;; font character and symbols to show in term mode
(set-face-attribute 'default nil
                    ;; :font "FiraCode Nerd Font-12")
                    :font "Iosevka-14")

;; basic config
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)
(scroll-bar-mode 0)

;; add language hook eglot
;; (use-package eglot)
(add-hook 'c-mode 'eglot-ensure)

;; sqlite3
(require 'sqlite3)

(add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; line no
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; ido
(ido-mode 1)
(ido-everywhere 1)

;; backup emacs
(setq backup-directory-alist '(("." . "~/.emacs_show")))

;; keybindings.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; window movement
(global-set-key (kbd "M-<left>") 'other-window)
(global-set-key (kbd "M-<right>") 'other-window)

;;theme
(load-theme 'gruber-darker t)

;; Packages that don't require
(use-package scala-mode
  :commands scala-mode
  ;; :defer 2
)
(use-package d-mode
  :commands d-mode
  ;; :defer 2
)
(use-package yaml-mode
  :commands yaml-mode
  ;; :defer 2
)
(use-package glsl-mode
  :commands glsl-mode
  ;; :defer 2
)
(use-package tuareg
  :commands tuareg
  ;; :defer 2
)
(use-package lua-mode
  :commands lua-mode
  ;; :defer 2
)
(use-package less-css-mode
  :commands less-css-mode
  ;; :defer 2
)
(use-package graphviz-dot-mode
  :commands graphviz-dot-mode
  ;; :defer 2
)
(use-package clojure-mode
  :commands clojure-mode
  ;; :defer 2
)
(use-package cmake-mode
  :commands cmake-mode
  ;; :defer 2
)
(use-package rust-mode
  :commands rust-mode
  ;; :defer 2
)
;; (use-package csharp-mode
;;   :commands csharp-mode
;;   ;; :defer 2
;;)
(use-package nim-mode
  :commands nim-mode
  ;; :defer 2
)
(use-package jinja2-mode
  :commands jinja2-mode
  ;; :defer 2
)
(use-package markdown-mode
  :commands markdown-mode
  ;; :defer 2
)
(use-package purescript-mode
  :commands purescript-mode
  ;; :defer 2
)
(use-package nix-mode
  :commands nix-mode
  ;; :defer 2
)
(use-package dockerfile-mode
  :commands dockerfile-mode
  ;; :defer 2
)
;; (use-package love-minor-mode
;;   :commands love-minor-mode
;;:defer 2
;;)
(use-package toml-mode
  :commands toml-mode
  ;; :defer 2
)
(use-package nginx-mode
  :commands nginx-mode
  ;; :defer 2
)
(use-package kotlin-mode
  :commands kotlin-mode
  ;; :defer 2
)
(use-package go-mode
  :commands go-mode
  ;; :defer 2
)
(use-package php-mode
  :commands php-mode
  ;; :defer 2
)
(use-package racket-mode
  :commands racket-mode
  ;; :defer 2
)
(use-package qml-mode
  :commands qml-mode
  ;; :defer 2
)
(use-package ag
  :commands ag
  ;; :defer 2
)
(use-package hindent
  :commands hindent
  ;; :defer 2
)
(use-package elpy
  :commands elpy
  ;; :defer 2
)
(use-package typescript-mode
  :commands typescript-mode
  ;; :defer 2
)
(use-package rfc-mode
  :commands rfc-mode
  ;; :defer 2
)
;; (use-package sml-mode
;;  :commands sml-mode
;;:defer 2
;;)

(use-package compile
  ;; :defer 2
)

;;; tramp
;;; http://stackoverflow.com/questions/13794433/how-to-disable-autosave-for-tramp-buffers-in-emacs
  (setq tramp-auto-save-directory "/tmp")

  ;; confirm before exiting emacs
  (setq confirm-kill-emacs 'y-or-n-p)

  ;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 40 1024 1024))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bddf21b7face8adffc42c32a8223c3cc83b5c1bbd4ce49a5743ce528ca4da2b6" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
