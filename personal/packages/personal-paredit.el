;; activate paredit
(require 'paredit)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'web-mode-hook              #'enable-paredit-mode)
(add-hook 'python-mode-hook           #'enable-paredit-mode)

;; turn off smart parens
(advice-add #'smartparens-mode :before-until (lambda (&rest args) t))

;; no space before parens
(defun paredit-space-for-delimiter-p (endp delimiter)
  (and (not (if endp (eobp) (bobp)))
       (memq (char-syntax (if endp (char-after) (char-before)))
             (list ?\"  ;; REMOVED ?w ?_
                   (let ((matching (matching-paren delimiter)))
                     (and matching (char-syntax matching)))))))

;; (add-hook 'python-mode-hook (paredit-space-for-delimiter-p))

;;kbd
(global-set-key (kbd "{") 'paredit-open-curly)
(global-set-key (kbd "[") 'paredit-open-square)
(global-set-key (kbd "<") 'paredit-open-angled)
(global-set-key (kbd "%") 'paredit-open-percentage)


(provide 'personal-paredit)
