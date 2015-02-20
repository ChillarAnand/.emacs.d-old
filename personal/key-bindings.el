;;; key-bindings.el --- key bindings
;;-*- lexical-binding: t; -*-

;; Copyright (C) 2015  K3

;; Author: K3;;; Code: <k3@k3-desktop>
;; Keywords:


(global-set-key (kbd "C-c C-v") 'eval-buffer)
(global-set-key (kbd "C-h C-m") 'discover-my-major)

(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)

;;(global-set-key (kbd "C-h") 'paredit-backward-delete)

(global-set-key (kbd "M-h") 'backward-kill-word)


;;; key chord

(prelude-require-package 'key-chord)

(require 'key-chord)

(key-chord-define-global "gg" 'beginning-of-buffer)
(key-chord-define-global "GG" 'end-of-buffer)
(key-chord-define-global "ff" 'helm-projectile-find-file)

(key-chord-define-global "ps" 'helm-projectile-switch-project)
(key-chord-define-global "pf" 'helm-projectile-find-file)
(key-chord-define-global "pg" 'helm-projectile-grep)

(key-chord-define-global "mx" 'helm-M-x)
(key-chord-define-global "jf" 'helm-mini)
(key-chord-define-global "js" 'helm-semantic-or-imenu)
(key-chord-define-global "hr" 'helm-resume)
(key-chord-define-global "mr" 'helm-all-mark-rings)

(key-chord-define-global "mg" 'magit-status)

(key-chord-define-global "jb" 'pop-global-mark)
(key-chord-define-global "mm" 'set-mark-command)

(key-chord-define-global "kf" 'kill-this-buffer)
(key-chord-define-global "x0" 'delete-window)
(key-chord-define-global "kw" 'delete-window)
(key-chord-define-global "kk" 'delete-other-windows)
(key-chord-define-global "xo" 'other-window)
(key-chord-define-global "bb" 'prelude-switch-to-previous-buffer)

(key-chord-define-global "dk" 'describe-key)
(key-chord-define-global "dv" 'describe-variable)
(key-chord-define-global "df" 'describe-function)
(key-chord-define-global "hk" 'describe-key)
(key-chord-define-global "hv" 'describe-variable)
(key-chord-define-global "hf" 'describe-function)

(key-chord-define-global "pt" 'prelude-tip-of-the-day)

(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "jl" 'ace-jump-line-mode)
(key-chord-define-global "jk" 'ace-jump-char-mode)
(key-chord-define-global "JJ" 'prelude-switch-to-previous-buffer)
(key-chord-define-global "uu" 'undo-tree-visualize)
(key-chord-define-global "xx" 'execute-extended-command)
(key-chord-define-global "dd" 'prelude-kill-whole-line)
(key-chord-define-global "md" 'helm-dired-history-view)


(defvar key-chord-tips
  '("pt prelude-tip-of-the-day"
    "kt key-chord-tips"

    "ff helm-projectile-find-file"
    "gg beginning-of-buffer"
    "GG end-of-buffer"

    "sp helm-projectile-switch-project"
    "pf helm-projectile-find-file"
    "pg helm-projectile-grep"

    "mx helm-M-x"
    "jf helm-mini"
    "js helm-semantic-or-imenu"
    "hr helm-resume"
    "mr helm-all-mark-rings"

    "mg magit-status"

    "jb pop-global-mark"
    "mm set-mark-command"

    "kf kill-this-buffer"
    "kk delete-other-windows"
    "bb prelude-switch-to-previous-buffer"
    "xo other-window"
    "x0 delete-window"

    "dk describe-key"
    "hk describe-key"
    "hv describe-variable"
    "dv describe-variable"

    "yy copy-line"

    "jj ace-jump-word-mode"
    "jl ace-jump-line-mode"
    "jk ace-jump-char-mode"
    "JJ prelude-switch-to-previous-buffer"
    "uu undo-tree-visualize"
    "xx execute-extended-command"))

(defun key-chord-tip-of-the-day ()
  "Display a random entry from `key-chord-tips'."
  (interactive)
  (unless (window-minibuffer-p)
    ;; pick a new random seed
    (random t)
    (message
     (concat "key chord: "
             (nth (random (length key-chord-tips)) key-chord-tips)))))

(key-chord-define-global "kt" 'key-chord-tip-of-the-day)

(setq prelude-tips (append prelude-tips key-chord-tips))

(key-chord-mode +1)


(provide 'key-bindings)
;;; kbd.el ends here
