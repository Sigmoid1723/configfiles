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
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ido
(ido-mode 1)
(ido-everywhere 1)

;; recent files
(require 'recentf)
(recentf-mode 1)
(global-set-key "\C-xf" 'recentf-open-files) ;;removed preset key 'set-fill-column)
(setq recentf-auto-cleanup 'never)

;; backup emacs
(setq backup-directory-alist '(("." . "~/.emacs_show")))

;; To saves which command im typing(optional)
(use-package command-log-mode
  :commands command-log-mod)

;; keybindings.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; window movement
(global-set-key (kbd "M-<left>") 'windmove-left)       ; move to left window
(global-set-key (kbd "M-<right>") 'windmove-right)     ; move to right window
(global-set-key (kbd "M-<up>") 'windmove-up)           ; move to upper window
(global-set-key (kbd "M-<down>") 'windmove-down)       ; move to lower window

;; Pretty line below
;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))

;; Icon
;;(use-package all-the-icons)

;; Git
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
 :after magit)

(setq auth-sources '("~/.authinfo"))

;; multiple cursors
;; (use-package multiple-cursors)
;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-\"") 'mc/skip-to-next-like-this)
;; (global-set-key (kbd "C-:")  'mc/skip-to-previous-like-this)
;; (global-set-key (kbd "C-M-j")  'mc/mark-all-dwim)

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

;; Theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("f74e8d46790f3e07fbb4a2c5dafe2ade0d8f5abc9c203cd1c29c7d5110a85230" "bddf21b7face8adffc42c32a8223c3cc83b5c1bbd4ce49a5743ce528ca4da2b6" default))
 '(package-selected-packages
   '(gruvbox-theme move-text unicode-fonts doom-themes command-log-mode all-the-icons ivy smex forge visual-fill-column org-bullets avy zenburn-theme use-package rainbow-delimiters multiple-cursors sqlite3 gruber-darker-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
