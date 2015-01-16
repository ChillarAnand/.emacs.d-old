;;; code:


;; load downloaded packages
(add-to-list 'load-path "~/.emacs.d/personal/packages/")


;; multiple cursors
(prelude-require-package 'multiple-cursors)

(global-set-key (kbd "C-c m e") 'mc/edit-lines)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)


;; pony mode for django projects
(prelude-require-package 'pony-mode)


;; start smex when minibuffer is started
(prelude-require-package 'smex)

;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


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




;; real-auto-save
(require 'real-auto-save)
(add-hook 'text-mode-hook 'turn-on-real-auto-save)
(add-hook 'muse-mode-hook 'turn-on-real-auto-save)

;; (setq real-auto-save-interval 5) ;; in seconds


;; activate paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'python-mode-hook           #'enable-paredit-mode)

;; turn off smart parens
(advice-add #'smartparens-mode :before-until (lambda (&rest args) t))

;; smart tab
(require 'smart-tab)
(global-smart-tab-mode 1)

(add-to-list 'hippie-expand-try-functions-list
             'yas/hippie-try-expand) ;put yasnippet in hippie-expansion list

(setq smart-tab-using-hippie-expand t)
(global-smart-tab-mode t)


;; sos
(require 'sos)


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
