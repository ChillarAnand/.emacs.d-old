;; global key bindings
(global-set-key (kbd "C-c C-v") 'eval-buffer)
(global-set-key (kbd "C-h C-m") 'discover-my-major)






(dolist (key '("\C-c \C-p"))
  (global-unset-key key))

;; python mode settings
(global-set-key (kbd "M-,") 'pop-tag-mark)
(defun python-mode-settings ()
  (global-set-key (kbd "C-c C-j") 'helm-imenu)
  ;;  (global-set-key (kbd "C-c C-f") 'next-buffer)
  )
(add-hook 'python-mode 'python-mode-settings)



;; (global-set-key (kbd "C-c C-b") 'previous-buffer)
;; (global-set-key (kbd "C-c C-f") 'next-buffer)
;; (global-set-key (kbd "C-c C-f") 'helm-recentf)
;; (global-set-key "\C-c\C-k" "\C-a\C- \C-n\M-w")



;;; kbd.el ends here
