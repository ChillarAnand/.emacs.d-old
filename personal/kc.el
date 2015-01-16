;;; key chord
(prelude-require-package 'key-chord)

(require 'key-chord)

(key-chord-define-global "ff" 'helm-recentf)
(key-chord-define-global "hf" 'helm-recentf)

(key-chord-define-global "pp" 'helm-projectile-switch-project)
(key-chord-define-global "sp" 'helm-projectile-switch-project)
(key-chord-define-global "pf" 'helm-projectile-find-file)

(key-chord-define-global "mx" 'helm-M-x)
(key-chord-define-global "fj" 'helm-M-x)

(key-chord-define-global "mg" 'magit-status)

(key-chord-define-global "jb" 'pop-global-mark)
(key-chord-define-global "mm" 'set-mark-command)

(key-chord-define-global "kk" 'kill-this-buffer)
(key-chord-define-global "oo" 'delete-other-windows)
(key-chord-define-global "bb" 'prelude-switch-to-previous-buffer)

(key-chord-define-global "gg" 'beginning-of-buffer)
(key-chord-define-global "GG" 'end-of-buffer)

(key-chord-define-global "ow" 'other-window)
(key-chord-define-global "xo" 'other-window)
(key-chord-define-global "x0" 'delete-window)
(key-chord-define-global "x1" 'delete-other-windows)

(key-chord-define-global "dk" 'describe-key)
(key-chord-define-global "hk" 'describe-key)

(key-chord-define-global "he" 'helm-imenu)


(defvar personal-key-chord-tips
  '("Press <ff> to open recent files list."
    "Press <hf> to open helm recent files."
    "Press <pp> to switch projects with Projectile."
    "Press <sp> to switch projects with Projectile."
    "Press <pf> to find files in the current project."))

(setq prelude-tips (append prelude-tips personal-key-chord-tips))

(key-chord-mode +1)

(provide 'prelude-key-chord)

;;; kc.el ends here
