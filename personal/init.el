;;; Code:


;; add a dir to load path
(add-to-list 'load-path "~/.emacs.d/personal/")


;; load prelude modules
(load-file (expand-file-name "prelude-modules.el" prelude-personal-dir))


;; load custom packages
(load-file (expand-file-name "packages.el" prelude-personal-dir))


;; load custom config
(load-file (expand-file-name "config.el" prelude-personal-dir))


;; load custom key bindings
(load-file (expand-file-name "kbd.el" prelude-personal-dir))


(provide 'init)
;;; init.el ends here
