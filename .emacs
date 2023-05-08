 ;; Package install 
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
(add-to-list 'package-archives
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

;;smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;basic config
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)
(scroll-bar-mode 0)

;line no
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;turn of line nomber
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-node-hool))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

;;ido
(ido-mode 1)
(ido-everywhere 1)

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
   '(rainbow-delimiters rainbow-delimeters doom-modeline doom-modline ivy command-log-mode gruber-darker-theme smex)))
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
(global-set-key (kbd "C-x C-b") 'buffer-menu)=

;; window movement
(global-set-key [M-left] 'windmove-left)          ; move to left window
(global-set-key [M-right] 'windmove-right)        ; move to right window
(global-set-key [M-up] 'windmove-up)              ; move to upper window
(global-set-key [M-down] 'windmove-down)          ; move to lower window

;; Pretty line below
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

