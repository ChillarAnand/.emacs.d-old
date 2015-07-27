;;; key-bindings.el --- key bindings
;;-*- lexical-binding: t; -*-

;;; Code:

;; unbind-keys


;; global key binds
(global-set-key (kbd "C-c C-f") 'helm-projectile-find-file)
(global-set-key (kbd "C-c C-g") 'beginning-of-buffer)
(global-set-key (kbd "C-c C-k") 'delete-other-windows)
(global-set-key (kbd "C-c C-v") 'eval-buffer)

(global-set-key (kbd "C-x C-b") 'switch-to-previous-buffer)
(global-set-key (kbd "C-x C-d") 'current-dired)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-x C-o") 'other-window)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-,") 'avy-goto-char)
(global-set-key (kbd "C-^") 'prelude-top-join-line)

(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "M-h") 'backward-kill-word)


;; Start proced in a similar manner to dired
(unless (eq system-type 'darwin)
  (global-set-key (kbd "C-x p") 'proced))

(define-key 'help-command "A" 'apropos)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-i") 'info-display-manual)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-m") 'discover-my-major)
(define-key 'help-command (kbd "C-v") 'find-variable)


;; replace zap-to-char functionaity with the more powerful zop-to-char
(global-set-key (kbd "M-z") 'zop-up-to-char)
(global-set-key (kbd "M-Z") 'zop-to-char)

;; kill lines backward
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

(global-set-key [remap kill-whole-line] 'delete-whole-line)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)
;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'switch-to-previous-buffer)

(unless (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "<f11>") 'prelude-fullscreen))

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

(global-set-key (kbd "C-=") 'er/expand-region)

;;(global-set-key (kbd "C-h") 'paredit-backward-delete)
;;(global-set-key (kbd "C-h") 'delete-backward-char)


;;; key chord
(require 'key-chord)
(key-chord-mode +1)

(key-chord-define-global "dd" 'delete-whole-line)
(key-chord-define-global "df" 'describe-function)
(key-chord-define-global "dk" 'describe-key)
(key-chord-define-global "dv" 'describe-variable)
(key-chord-define-global "hr" 'helm-resume)
(key-chord-define-global "jc" 'avy-goto-char)
(key-chord-define-global "jb" 'switch-to-previous-buffer)
(key-chord-define-global "jd" 'helm-dired-recent-dirs-view)
(key-chord-define-global "jf" 'helm-mini)
(key-chord-define-global "jl" 'avy-goto-line)
(key-chord-define-global "js" 'helm-semantic-or-imenu)
(key-chord-define-global "kf" 'bury-buffer)
(key-chord-define-global "kw" 'delete-window)
(key-chord-define-global "md" 'current-dired)
(key-chord-define-global "mg" 'magit-status)
(key-chord-define-global "mx" 'helm-M-x)
(key-chord-define-global "ps" 'helm-projectile-switch-project)
(key-chord-define-global "pf" 'helm-projectile-find-file)
(key-chord-define-global "pg" 'helm-projectile-grep)
(key-chord-define-global "sm" 'set-mark-command)


(provide 'key-bindings)
;;; kbd.el ends here
