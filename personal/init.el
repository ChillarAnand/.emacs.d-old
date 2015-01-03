;;; init.el --- Prelude's personal configuration entry point.

;;; Commentary:

;; This file has personal config for prelude emacs

;;; Code:


;; add a dir to load path
(add-to-list 'load-path "~/.emacs.d/personal/")


;; check for files & load them
(defun load-file-if-exists (list)
  "Check for file & load it."
  (let (value)
    (dolist (element list value)
      (if (file-exists-p element)
          (load-file (expand-file-name element prelude-personal-dir))))))

(load-file-if-exists '("config.el" "kbd.el" "packages.el" "prelude-modules.el"))


(provide 'init)
;;; init.el ends here
