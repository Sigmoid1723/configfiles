;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; move text up down
(use-package move-text
  :ensure t
  ;; :defer 2
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

;; Multiple-cursors
(use-package multiple-cursors
  :ensure t
  ;; :defer 2
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
	 ("C-\"" . mc/skip-to-next-like-this)
	 ("C-:" . mc/skip-to-previous-like-this)
	 ("C-M-j" . mc/mark-all-dwim)
         ))

