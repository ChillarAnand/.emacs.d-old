;;; config.el --- personal config
;;
;; Filename: config.el
;; Description:
;; Author: Anand

;;; Commentary:

;;; Code:

(flymake-mode-on)
(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)

;; disable whitespace mode
(setq prelude-whitespace nil)

;; unzip zipped file dired
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(add-hook 'python-mode-hook (turn-on-smartparens-strict-mode))

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

(set-language-environment "UTF-8")

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

;; save session
;; (desktop-save-mode 1)


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
           (getenv "pyvenv-virtualenvwrapper-python")))


(defun ras-info ()
  "Show  env variables."
  (interactive)
  (message "timer: %S
buffers: %S
interval: %S"  real-auto-save-timer  real-auto-save-buffers-list real-auto-save-interval))


(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (sh . t)))


;; (defun message-beat ()
;;   (message "re")  )

;; (setq real-auto-save-timer
;;       (run-at-time
;;        (current-time) 2 'message-beat))

;; (cancel-timer message-beat)

;; (message "timer list: %S" timer-list)


;; (defun real-auto-save-info ()
;;   "Show real-auto-save variables list.")
;; (add-hook 'evil-mode-hook 'evil-mode-bindings)

;; (swap-numbers-symbols)


;;(define-key evil-normal-state-map "5" 'evil-beginning-of-line)





(bind-key "C-'" 'reselect-last-region)

(defun reselect-last-region ()
  (interactive)
  (let ((start (mark t))
        (end (point)))
    (goto-char start)
    (call-interactively' set-mark-command)
    (goto-char end)))
(global-set-key (kbd "C-'") 'reselect-last-region)

(defun print-region-point ()
  (interactive)
  (if (use-region-p)
      (progn
        (message "b: %s, e: %s, p: %s" (region-beginning) (region-end) (point))
        (let ((beg (region-beginning))
              (end (region-end)))
          (while (< (point) end)
            (message ":: %s - %s " (point-at-bol) (point-at-eol))
            (forward-line 1)))
        (setq deactivate-mark nil))))

(defun print-point ()
  (interactive)
  (message "%s" point))


(defun goto-first-tab ()
  (interactive)
  (Info-next-reference))


(defun foo ()
  (message "fooo"))

(add-hook 'help-mode 'forward-button)
(add-hook 'help-mode 'foo)

(add-hook 'Info-mode 'goto-first-tab)

(add-hook 'help-mode 'foo)

;; (defadvice isearch-search (after isearch-no-fail activate)
;;   "Automatically wrapping I-search"
;;   (unless isearch-success
;;     (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
;;     (ad-activate 'isearch-search)
;;     (isearch-repeat (if isearch-forward 'forward))
;;     (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
;;     (ad-activate 'isearch-search)))

;; (defun isearch-auto-wrap ()
;;   (unless isearch-success
;;     (isearch-repeat (if isearch-forward 'forward))

;;     )
;;   )
;; (advice-add 'isearch-forward :after #'isearch-auto-wrap)

;; (defun message-beat ()
;;   (message "re")  )

;; (setq real-auto-save-timer
;;       (run-at-time
;;        (current-time) 2 'message-beat))

;; (cancel-timer message-beat)

;; (message "timer list: %S" timer-list)


;; (defun real-auto-save-info ()
;;   "Show real-auto-save variables list.")


(provide 'config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; config.el ends here
