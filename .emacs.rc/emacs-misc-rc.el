;; Set default tab width and indentation
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Kill autoload buffers
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

;; Assembly setup
(defun my-asm-mode-hook ()
  ;; You can use `comment-dwim' (M-;) for this kind of behaviour anyway
  (local-unset-key (vector asm-comment-char))
  ;; asm-mode sets it locally to nil, to "stay closer to the old TAB behaviour".
  (setq tab-always-indent (default-value 'tab-always-indent)))

(add-hook 'asm-mode-hook #'my-asm-mode-hook)

;; Ggtags
(use-package ggtags
  :commands (ggtags-mode ggtags-find-other-symbol ggtags-view-tag-history ggtags-find-reference
                         ggtags-find-file ggtags-create-tags ggtags-update-tags pop-tag-mark)
  :hook (c-mode-common . (lambda ()
                           (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                             (ggtags-mode 1))))
  :bind (:map ggtags-mode-map
              ("C-c g s" . ggtags-find-other-symbol)
              ("C-c g h" . ggtags-view-tag-history)
              ("C-c g r" . ggtags-find-reference)
              ("C-c g f" . ggtags-find-file)
              ("C-c g c" . ggtags-create-tags)
              ("C-c g u" . ggtags-update-tags)
              ("M-," . pop-tag-mark)))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :hook (after-init . yas-global-mode)
  :custom
  (yas/triggers-in-field nil))

;; Company mode (for autofilling)
(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

;; Haskell mode
(use-package haskell-mode
  :ensure t
  :hook ((haskell-mode . haskell-indent-mode)
         (haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-doc-mode)
         (haskell-mode . hindent-mode))
  :custom
  (haskell-process-type 'cabal-new-repl)
  (haskell-process-log t))
