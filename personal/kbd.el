;; evaluate buffer
(global-set-key (kbd "C-c C-v") 'eval-buffer)


;; previous buffer
(global-set-key (kbd "C-c C-b") 'previous-buffer)
(global-set-key (kbd "C-c C-f") 'next-buffer)

(dolist (key '("\C-c \C-p"))
  (global-unset-key key))

;; python mode settings
(global-set-key (kbd "M-,") 'pop-tag-mark)
(defun python-mode-settings ()
  (global-set-key (kbd "C-c C-f") 'next-buffer))
(add-hook 'python-mode 'python-mode-settings)


;;; key chord
(prelude-require-package 'key-chord)

(require 'key-chord)

(key-chord-define-global "ff" 'helm-recentf)
(key-chord-define-global "pp" 'helm-projectile-switch-project)
(key-chord-define-global "pf" 'helm-projectile-find-file)
(key-chord-define-global "bb" 'prelude-switch-to-previous-buffer)
(key-chord-define-global "mx" 'helm-M-x)
(key-chord-define-global "mg" 'magit-status)
(key-chord-define-global "jb" 'pop-global-mark)
(key-chord-define-global "mm" 'set-mark-command)
(key-chord-define-global "kk" 'kill-this-buffer)
(key-chord-define-global "ow" 'other-window)
(key-chord-define-global "hf" 'helm-recentf)


(key-chord-define-global "fj" 'helm-M-x)

;;; kbd.el ends here
