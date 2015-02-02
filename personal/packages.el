;;; code:


(prelude-require-packages '(use-package paredit helm-swoop multiple-cursors
                             delight real-auto-save company))

(require 'use-package)

(use-package helm-swoop)


(use-package real-auto-save
  :init
  (progn
    (add-hook 'prog-mode-hook 'real-auto-save-mode)))


(use-package paredit
  :init
  (progn
    (autoload 'enable-paredit-mode
      "paredit" "Turn on pseudo-structural editing of Lisp code." t)

    (add-hook 'prog-mode-hook 'enable-paredit-mode)

    ;; no space before parens
    (defun paredit-space-for-delimiter-p (endp delimiter)
      (and (not (if endp (eobp) (bobp)))
           (memq (char-syntax (if endp (char-after) (char-before)))
                 (list ?\"  ;; REMOVED ?w ?_
                       (let ((matching (matching-paren delimiter)))
                         (and matching (char-syntax matching)))))))

    (global-set-key (kbd "{") 'paredit-open-curly)
    (global-set-key (kbd "[") 'paredit-open-square)
    (global-set-key (kbd "<") 'paredit-open-angled)))


(use-package multiple-cursors
  :init
  (progn
    (global-set-key (kbd "C-c m e") 'mc/edit-lines)
    (global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)))


(use-package delight
  :init
  (delight '((abbrev-mode " Abv" abbrev)
             (smart-tab-mode " t" smart-tab)
             (eldoc-mode nil "eldoc")
             (paredit-mode " par" paredit)
             (projectile-mode " proj" projectile)
             (emacs-lisp-mode "Elisp" :major)
             (rainbow-mode)
             (flyspell-mode nil flyspell)
             (guru-mode nil guru))))


(use-package company
  :init
  (progn
    (setq company-idle-delay 0.5)
    (setq company-tooltip-limit 10)
    (setq company-minimum-prefix-length 1)
    (setq company-tooltip-flip-when-above t)
    (global-company-mode 1)))

(provide 'packages)
;;; packages.el ends here
