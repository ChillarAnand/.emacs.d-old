;;; config - personal config for prelude emacs

;;; Code:

;; unzip zipped file dired
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))


;; copy line
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))


(require 'key-chord)
(key-chord-define-global "yy" 'copy-line)


;; sql

;; (sql-set-product "mysql")

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; supress mumamo buffer file warnings
(when (and (equal emacs-major-version 24)
           (equal emacs-minor-version 2))
  (eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
           (delq 'buffer-file-name mumamo-per-buffer-local-vars))))

(semantic-mode 1)


;; python mode settings
(global-set-key (kbd "M-,") 'pop-tag-mark)


(add-to-list 'load-path "~/.emacs.d/personal/packages/elpy")
(require 'elpy)
(elpy-enable)
(elpy-use-ipython)
(defalias 'workon 'pyvenv-workon)


;; load downloaded packages
(add-to-list 'load-path "~/.emacs.d/personal/packages/")


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

;; dired history
(require 'savehist)
(add-to-list 'savehist-additional-variables 'helm-dired-history-variable)
(savehist-mode 1)
(eval-after-load 'dired
  '(progn (require 'helm-dired-history)
          (define-key dired-mode-map "," 'helm-dired-history-view)))


;; smart tab
;;(require 'smart-tab)
;; (global-smart-tab-mode 1)

;; (require 'yasnippet)
;; (add-to-list 'hippie-expand-try-functions-list
;;              'yas/hippie-try-expand) ;put yasnippet in hippie-expansion list

;; (setq smart-tab-using-hippie-expand t)
;; (global-smart-tab-mode t)


;; yow - easter egg
;; (unless (file-exists-p "~/.emacs.d/personal/yow.txt.gz")
;;   (shell-command "wget bit.ly/emacs-yow -O ~/.emacs.d/personal/yow.txt.gz"))
;; (setq yow-file "~/.emacs.d/yow.txt.gz")


(provide 'config)
;;; config.el ends here
