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

;; add language hook eglot
;; (use-package eglot)
(add-hook 'c-mode 'eglot-ensure)

;; sqlite3
(require 'sqlite3)

;; font character and symbols to show in term mode
(set-face-attribute 'default nil
                    ;; :font "FiraCode Nerd Font-12")
                    :font "Iosevka-14")


(add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; basic config
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)
(scroll-bar-mode 0)

;; line no
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ido
(ido-mode 1)
(ido-everywhere 1)

;; backup emacs
(setq backup-directory-alist '(("." . "~/.emacs_show")))

;; ;; To saves which command im typing(optional)
;; (use-package command-log-mode
;;   :commands command-log-mode)

;; keybindings.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; Git
(use-package magit
  ;; :defer 0

  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
 :after magit)

(setq auth-sources '("~/.authinfo"))

;; Multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
	 ("C-\"" . mc/skip-to-next-like-this)
	 ("C-:" . mc/skip-to-previous-like-this)
	 ("C-M-j" . mc/mark-all-dwim)
         ))

;; move text up down
(use-package move-text
  :ensure t
  :bind(("M-p" . move-text-up)
	("M-n" . move-text-down)))

;; duplicate line (very usefull)
(defun rc/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-,") 'rc/duplicate-line)
(global-set-key (kbd "C-.") 'duplicate-dwim)

;; Theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("f74e8d46790f3e07fbb4a2c5dafe2ade0d8f5abc9c203cd1c29c7d5110a85230" "bddf21b7face8adffc42c32a8223c3cc83b5c1bbd4ce49a5743ce528ca4da2b6" default))
 '(display-line-numbers-type 'relative)
 '(package-selected-packages
   '(helm-gtags ggtags helm-ls-git helm-git-grep helm-cmd-t helm csharp-mode sml-mode rfc-mode typescript-mode elpy hindent ag qml-mode racket-mode php-mode go-mode kotlin-mode nginx-mode toml-mode love-minor-mode dockerfile-mode nix-mode purescript-mode jinja2-mode nim-mode rust-mode cmake-mode clojure-mode graphviz-dot-mode lua-mode tuareg glsl-mode yaml-mode d-mode scala-mode paredit yasnippet gruvbox-theme move-text unicode-fonts doom-themes command-log-mode all-the-icons ivy smex forge visual-fill-column org-bullets avy zenburn-theme use-package rainbow-delimiters multiple-cursors sqlite3 gruber-darker-theme))
 '(whitespace-style
   '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-2 ((t (:foreground "pink"))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; yasnippet
(use-package yasnippet
  :after interactive
  :ensure t
  :custom
  (setq yas/triggers-in-field nil)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(yas-global-mode 1)

;;; helm
(use-package helm
  ;; :defer 0

  :commands helm-cmd-t helm-git-grep helm-ls-git
  :custom (setq helm-ff-transformer-show-only-basename nil)
  (global-set-key (kbd "C-c h t") 'helm-cmd-t)
  (global-set-key (kbd "C-c h g g") 'helm-git-grep)
  (global-set-key (kbd "C-c h g l") 'helm-ls-git-ls)
  (global-set-key (kbd "C-c h f") 'helm-find)
  (global-set-key (kbd "C-c h a") 'helm-org-agenda-files-headings)
  (global-set-key (kbd "C-c h r") 'helm-recentf))

;; window movement
(global-set-key (kbd "M-<left>") 'other-window)
(global-set-key (kbd "M-<right>") 'other-window)

;; kill autoload buffers
(defun rc/kill-autoloads-buffers ()
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (string-match-p "-autoloads.el" name)
        (kill-buffer buffer)
        (message "Killed autoloads buffer %s" name)))))

;; Company mode(for autofilling)
(use-package company
  :ensure t
  ;; :defer 0

  :config (global-company-mode 1))

;; Packages that don't require
(use-package scala-mode
  :commands scala-mode
  ;; :defer 0
)
(use-package d-mode
  :commands d-mode
  ;; :defer 0
)
(use-package yaml-mode
  :commands yaml-mode
  ;; :defer 0
)
(use-package glsl-mode
  :commands glsl-mode
  ;; :defer 0
)
(use-package tuareg
  :commands tuareg
  ;; :defer 0
)
(use-package lua-mode
  :commands lua-mode
  ;; :defer 0
)
(use-package less-css-mode
  :commands less-css-mode
  ;; :defer 0
)
(use-package graphviz-dot-mode
  :commands graphviz-dot-mode
  ;; :defer 0
)
(use-package clojure-mode
  :commands clojure-mode
  ;; :defer 0
)
(use-package cmake-mode
  :commands cmake-mode
  ;; :defer 0
)
(use-package rust-mode
  :commands rust-mode
  ;; :defer 0
)
;; (use-package csharp-mode
;;   :commands csharp-mode
;;   ;; :defer 0
;;)
(use-package nim-mode
  :commands nim-mode
  ;; :defer 0
)
(use-package jinja2-mode
  :commands jinja2-mode
  ;; :defer 0
)
(use-package markdown-mode
  :commands markdown-mode
  ;; :defer 0
)
(use-package purescript-mode
  :commands purescript-mode
  ;; :defer 0
)
(use-package nix-mode
  :commands nix-mode
  ;; :defer 0
)
(use-package dockerfile-mode
  :commands dockerfile-mode
  ;; :defer 0
)
;; (use-package love-minor-mode
;;   :commands love-minor-mode
;;;; :defer 0
;;)
(use-package toml-mode
  :commands toml-mode
  ;; :defer 0
)
(use-package nginx-mode
  :commands nginx-mode
  ;; :defer 0
)
(use-package kotlin-mode
  :commands kotlin-mode
  ;; :defer 0
)
(use-package go-mode
  :commands go-mode
  ;; :defer 0
)
(use-package php-mode
  :commands php-mode
  ;; :defer 0
)
(use-package racket-mode
  :commands racket-mode
  ;; :defer 0
)
(use-package qml-mode
  :commands qml-mode
  ;; :defer 0
)
(use-package ag
  :commands ag
  ;; :defer 0
)
(use-package hindent
  :commands hindent
  ;; :defer 0
)
(use-package elpy
  :commands elpy
  ;; :defer 0
)
(use-package typescript-mode
  :commands typescript-mode
  ;; :defer 0
)
(use-package rfc-mode
  :commands rfc-mode
  ;; :defer 0
)
;; (use-package sml-mode
;;  :commands sml-mode
;;;; :defer 0
;;)

(use-package compile
  ;; :defer 0
)
 
;;gg tags
(use-package ggtags
  :commands ggtags
  ;; :defer 0
  :custom (add-hook 'c-mode-common-hook
		    (lambda ()
		      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
			(ggtags-mode 1))))
  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))

;; Enable helm-gtags-mode
(use-package helm-gtags
  :after helm
  :custom(add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)

  (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))


;;; tramp
;;; http://stackoverflow.com/questions/13794433/how-to-disable-autosave-for-tramp-buffers-in-emacs
  (setq tramp-auto-save-directory "/tmp")

  ;; confirm before exiting emacs

  (setq confirm-kill-emacs 'y-or-n-p)

  ;; Make gc pauses faster by decreasing the threshold.
  (setq gc-cons-threshold (* 40 1024 1024))
