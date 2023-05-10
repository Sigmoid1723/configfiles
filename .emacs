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

;; fonts
;; (set-face-attribute 'default nil
;;                     :font "FiraCode Nerd Font-10")
;; (add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font"))
;; (add-to-list 'load-path "~/.emacs.d/unicode-fonts/")
;; (require 'unicode-fonts)
;; (unicode-fonts-setup)

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
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; turn of line nomber
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-node-hool))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

;; ido
(ido-mode 1)
(ido-everywhere 1)

;; recent files
(require 'recentf)
(recentf-mode 1)
(global-set-key "\C-xf" 'recentf-open-files) ;;removed preset key 'set-fill-column)
(setq recentf-auto-cleanup 'never)

;; Themes
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("bddf21b7face8adffc42c32a8223c3cc83b5c1bbd4ce49a5743ce528ca4da2b6" default))
 '(package-selected-packages
   '(move-text multiple-cursors forge magit rainbow-delimiters rainbow-delimeters doom-modeline doom-modline ivy command-log-mode gruber-darker-theme smex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; backup emacs
(setq backup-directory-alist '(("." . "~/.emacs_show")))

;; To saves which command im typing(optional)
(use-package command-log-mode)

;; keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; window movement
(global-set-key (kbd "M-<left>") 'windmove-left)       ; move to left window
(global-set-key (kbd "M-<right>") 'windmove-right)     ; move to right window
(global-set-key (kbd "M-<up>") 'windmove-up)           ; move to upper window
(global-set-key (kbd "M-<down>") 'windmove-down)       ; move to lower window

;; Pretty line below
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Icon
(use-package all-the-icons)

;; Git
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
 :after magit)

(setq auth-sources '("~/.authinfo"))

;; multiple cursors
(use-package multiple-cursors)
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; move text up down
(use-package move-text)
(require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;; org mode
(use-package org)
