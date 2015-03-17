;;; config.el --- personal config
;;
;; Filename: config.el
;; Description:
;; Author: Anand

;;; Commentary:

;;; Code:


;; disable whitespace mode
(setq prelude-whitespace nil)

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
          (setq beg (save-excursion (goto-char (mark))
                                    (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))


(semantic-mode 1)


(global-set-key (kbd "M-,") 'pop-tag-mark)


;; org reveal
(load-file "~/.emacs.d/vendor/ox-reveal.el")
(load-file "~/.emacs.d/vendor/htmlize.el")
(setq org-reveal-root "file:///home/anand/.emacs.d/vendor/reveal.js/js/reveal.js")
(require 'ox-reveal)
(require 'htmlize)


(defun elpy-info ()
  "Show elpy env variables."
  (interactive)
  (message "WORKON_HOME: %S
VIRUTALENVWRAPPER_HOOK_DIR: %S
DJANGO_SETTINGS_MODULE: %S
VIRTUAL_ENV: %S
pyvenv-virtualenvwrapper-python: %s"

           (getenv "WORKON_HOME")
           (getenv "VIRTUALENVWRAPPER_HOOK_DIR")
           (getenv "DJANGO_SETTINGS_MODULE")
           (getenv "VIRTUAL_ENV")
           (getenv "pyvenv-virtualenvwrapper-python")
           ))

(defun real-auto-save-info ()
  "Show real-auto-save variables list.")


;; activate space to ctrl on start
(defun space-to-ctrl-start ()
"Activate space to ctrl programme."
    (interactive)
    (shell-command "~/projects/Space2Ctrl/s2cctl start"))

(defun space-to-ctrl-stop ()
  "Deactivate space to ctrl programme."
  (interactive)
  (shell-command "~/projects/Space2Ctrl/s2cctl stop"))
(space-to-ctrl-stop)
(space-to-ctrl-start)


(defun company-yasnippet-or-completion ()
  "Solve company yasnippet conflicts."
  (interactive)
  (let ((yas-fallback-behavior
         (apply 'company-complete-common nil)))
    (yas-expand)))

(add-hook 'company-mode-hook
          (lambda ()
            (substitute-key-definition
             'company-complete-common
             'company-yasnippet-or-completion
             company-active-map)))

(defun visit-multi-term-buffer ()
  "Create or visit a multi terminal buffer."
  (interactive)
  (multi-term))


(provide 'config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; config.el ends here
