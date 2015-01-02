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
(require 'package)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)
(elpy-enable)
(elpy-use-ipython)


;; real-auto-save
(require 'real-auto-save)
(add-hook 'text-mode-hook 'turn-on-real-auto-save)
(add-hook 'muse-mode-hook 'turn-on-real-auto-save)

(setq real-auto-save-interval 5) ;; in seconds


;; org reveal
;; (load-file "~/.emacs.d/personal/prelude-personal-dir/ox-reveal.el")
;; (load-file "~/.emacs.d/personal/prelude-personal-dir/htmlize.el")
;; (setq org-reveal-root "file:///home/anand/Projects/js/reveal.js/js/reveal.js")
;; (require 'ox-reveal)
;; (require 'htmlize)


;; mysql for emacs
;; (defvar file-name "~/.emacs.d/personal/mysql.el")
;; (load-file file-name)
;; (require 'mysql)

