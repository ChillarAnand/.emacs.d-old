;;; key chord
(prelude-require-package 'key-chord)

(require 'key-chord)

(key-chord-define-global "gg" 'beginning-of-buffer)
(key-chord-define-global "GG" 'end-of-buffer)

(key-chord-define-global "sp" 'helm-projectile-switch-project)
(key-chord-define-global "pf" 'helm-projectile-find-file)
(key-chord-define-global "pg" 'helm-projectile-grep)

(key-chord-define-global "mx" 'helm-M-x)
(key-chord-define-global "jf" 'helm-mini)
(key-chord-define-global "jd" 'helm-imenu)

(key-chord-define-global "mg" 'magit-status)

(key-chord-define-global "jb" 'pop-global-mark)
(key-chord-define-global "mm" 'set-mark-command)

(key-chord-define-global "kk" 'kill-this-buffer)
(key-chord-define-global "x0" 'delete-window)
(key-chord-define-global "do" 'delete-other-windows)
(key-chord-define-global "ow" 'other-window)
(key-chord-define-global "xo" 'other-window)
(key-chord-define-global "bb" 'prelude-switch-to-previous-buffer)

(key-chord-define-global "dk" 'describe-key)
(key-chord-define-global "hk" 'describe-key)
(key-chord-define-global "hv" 'describe-variable)
(key-chord-define-global "dv" 'describe-variable)


(defvar personal-key-chord-tips
  '("gg 'beginning-of-buffer"
    "GG 'end-of-buffer"
    "sp 'helm-projectile-switch-project"
    "pf 'helm-projectile-find-file"
    "pg 'helm-projectile-grep"
    "mx 'helm-M-x"
    "jf 'helm-mini"
    "jd 'helm-imenu"
    "mg 'magit-status"
    "jb 'pop-global-mark"
    "mm 'set-mark-command"
    "kk 'kill-this-buffer"
    "oo 'delete-other-windows"
    "bb 'prelude-switch-to-previous-buffer"
    "ow 'other-window"
    "xo 'other-window"
    "x0 'delete-window"
    "x1 'delete-other-windows"
    "dk 'describe-key"
    "hk 'describe-key"
    "hv 'describe-variable"
    "dv 'describe-variable"))


(setq prelude-tips (append prelude-tips personal-key-chord-tips))

(key-chord-mode +1)

(provide 'prelude-key-chord)

;;; kc.el ends here
