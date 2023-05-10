;; Different theme for org mode
(add-hook 'org-mode-hook
   (lambda ()
     (load-theme "doom-gruvbox" t t)))

;; Set faces for heading levels
(with-eval-after-load 'org-faces
 ;; (font-lock-add-keywords 'org-mode
 ;;                          '(("^ *\\([-]\\) "
 ;;                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "."))))))
  
   (dolist (face '((org-level-1 . 1.2)
               (org-level-2 . 1.1)
               (org-level-3 . 1.05)
               (org-level-4 . 1.0)
               (org-level-5 . 1.1)
               (org-level-6 . 1.1)
               (org-level-7 . 1.1)
               (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "FiraCode Nerd Font" :weight 'regular :height(cdr face)))

   ;; Ensure that anything that should be fixed-pitch in Org files appears that way
   (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
;;  :pin org
;;  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"))

  ;; (setq org-agenda-start-with-log-mode t)
  ;; (setq org-log-done 'time)
  ;; (setq org-log-into-drawer t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; text wrapping to center
(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))





