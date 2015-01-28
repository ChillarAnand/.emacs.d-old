;;; config - personal config for prelude emacs


;; unzip zipped file dired
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))


;; yow - easter egg
;; (unless (file-exists-p "~/.emacs.d/personal/yow.txt.gz")
;;   (shell-command "wget bit.ly/emacs-yow -O ~/.emacs.d/personal/yow.txt.gz"))
;; (setq yow-file "~/.emacs.d/yow.txt.gz")


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

;; company
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)

;; supress mumamo buffer file warnings
(when (and (equal emacs-major-version 24)
           (equal emacs-minor-version 2))
  (eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
           (delq 'buffer-file-name mumamo-per-buffer-local-vars))))



;; ctags
;; (setq path-to-ctags "/usr/big/ctags-exuberant")


;;; config.el ends here
