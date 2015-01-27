;;; code:

;; load downloaded packages
(add-to-list 'load-path "~/.emacs.d/personal/packages/")


;; personal packages
(require 'personal-paredit)
;; (require 'personal-web-mode)
(require 'sos)  ; search on stackoverflow

;; (load-file "~/.emacs.d/personal/packages/nxhtml/autostart.el")
;;(load-file "~/.emacs.d/personal/packages/python-django.el")
;;(require 'python-django)


;; visible mark
(prelude-require-package 'visible-mark)
(require 'visible-mark)
(global-visible-mark-mode 1)


;; swoop
(prelude-require-package 'helm-swoop)
(require 'helm-swoop)


;; multiple cursors
(prelude-require-package 'multiple-cursors)

(global-set-key (kbd "C-c m e") 'mc/edit-lines)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)


;; elpy
(when (not (require 'elpy nil 'noerror))
  (require 'package)
  (add-to-list 'package-archives
               '("elpy" . "http://jorgenschaefer.github.io/packages/"))
  (package-initialize)
  (package-refresh-contents)
  (package-install 'elpy))

(elpy-enable)
(elpy-use-ipython)
(defalias 'workon 'pyvenv-workon)


;; pony mode for django projects
;; (prelude-require-package 'pony-mode)
;; (require 'pony-mode)
;; (add-hook 'python-mode-hook 'pony-mode)
;; (add-to-list 'load-path "~/.emacs.d/personal/packages/pony-mode/src/")
;; (require 'pony-mode)


;; real-auto-save
(require 'real-auto-save)
(add-hook 'prog-mode-hook 'turn-on-real-auto-save)
(setq real-auto-save-interval 10) ;; in seconds



;; sx.el
;;(add-to-list 'load-path "~/.emacs.d/personal/packages/sx.el/")
;; (require 'sx-load)


;; edit server
  (when (require 'edit-server nil t)
    (setq edit-server-new-frame nil)
    (edit-server-start))


;; discover-my-major
(if (not (package-installed-p 'discover-my-major))
    (progn
      (package-refresh-contents)
      (package-install 'discover-my-major)))

(require 'discover-my-major)


;; org reveal
;; (load-file "~/.emacs.d/personal/prelude-personal-dir/ox-reveal.el")
;; (load-file "~/.emacs.d/personal/prelude-personal-dir/htmlize.el")
;; (setq org-reveal-root "file:///home/anand/Projects/js/reveal.js/js/reveal.js")
;; (require 'ox-reveal)
;; (require 'htmlize)


;; mysql for emacs
(require 'mysql)


;; company
(prelude-require-packages '(company))
(require 'company)

(setq company-idle-delay 0.5)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 1)
(setq company-tooltip-flip-when-above t)
(global-company-mode 1)


;; dired history
(require 'savehist)
(add-to-list 'savehist-additional-variables 'helm-dired-history-variable)
(savehist-mode 1)
(eval-after-load 'dired
  '(progn (require 'helm-dired-history)
          (define-key dired-mode-map "," 'helm-dired-history-view)))


;; smart tab
(require 'smart-tab)
(global-smart-tab-mode 1)

(require 'yasnippet)
(add-to-list 'hippie-expand-try-functions-list
             'yas/hippie-try-expand) ;put yasnippet in hippie-expansion list

(setq smart-tab-using-hippie-expand t)
(global-smart-tab-mode t)

;; (define-key company-active-map "\t" 'company-yasnippet-or-completion)

;; (defun company-yasnippet-or-completion ()
;;   (interactive)
;;   (if (yas/expansion-at-point)
;;       (progn (company-abort)
;;              (yas/expand))
;;     (company-complete-common)))

;; (defun yas/expansion-at-point ()
;;   "Tested with v0.6.1. Extracted from `yas/expand-1'"
;;   (first (yas/current-key)))
