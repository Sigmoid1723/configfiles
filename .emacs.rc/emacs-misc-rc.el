(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; kill autoload buffers
(defun rc/kill-autoloads-buffers ()
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (string-match-p "-autoloads.el" name)
        (kill-buffer buffer)
        (message "Killed autoloads buffer %s" name)))))

;;eglot
;; (require 'eglot)
;; (add-hook 'c-mode-hook 'eglot-ensure)  
;; (add-hook 'c++-mode-hook 'eglot-ensure)
;; (add-to-list 'eglot-server-programs '((c++-mode) "ccls"))

;; assembly setup
(defun my-asm-mode-hook ()
  ;; you can use `comment-dwim' (M-;) for this kind of behaviour anyway
  (local-unset-key (vector asm-comment-char))
  ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
  (setq tab-always-indent (default-value 'tab-always-indent)))

(add-hook 'asm-mode-hook #'my-asm-mode-hook)
 
;;gg tags
(use-package ggtags
  :commands ggtags
  ;; :defer 2
  :custom (add-hook 'c-mode-common-hook
		    (lambda ()
		      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
			(ggtags-mode t))))
  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))


;; yasnippet
(use-package yasnippet
  :after interactive
  :ensure t
  ;; :defer 2
  :custom
  (setq yas/triggers-in-field nil))

(yas-global-mode 1)

;; Company mode(for autofilling)
(use-package company
  :ensure t
  ;; :defer 2
  :config (global-company-mode 1))

;; Haskell mode
(use-package haskell-mode) 

(setq haskell-process-type 'cabal-new-repl)
(setq haskell-process-log t)

(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'hindent-mode)
