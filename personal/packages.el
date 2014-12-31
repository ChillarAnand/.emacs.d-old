;; multiple cursors
(prelude-require-package 'multiple-cursors)


;; pony mode for django projects
(prelude-require-package 'pony-mode)


;; start smex when minibuffer is started
(prelude-require-package 'smex)


;; add a dir to load path
(add-to-list 'load-path "~/.emacs.d/personal/")


;; elpy
(require 'package)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)
(elpy-enable)


;; real-auto-save
(require 'real-auto-save)
(add-hook 'text-mode-hook 'turn-on-real-auto-save)
(add-hook 'muse-mode-hook 'turn-on-real-auto-save)

(setq real-auto-save-interval 5) ;; in seconds


;; ctags
(setq path-to-ctags "/usr/big/ctags-exuberant")

;; (defun create-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   (shell-command
;;    (format "ctags -f %s -e -R %s" path-to-ctags (directory-file-name dir-name)))
;;   )

 
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

 
;; jedi - python auto complete
;; (prelude-require-package 'python-environment)
;; (prelude-require-package 'jedi)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

;; (require 'perspective)


;; learn lisp
;; (file-name-directory load-file-name)
;; (message "this is print")


