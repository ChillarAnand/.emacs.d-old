 ;;; packages.el --- 3rd party packages.
;;
;; Filename: packages.el
;; Description:
;; Author: Anand
;;; Commentary:
;;
;;; Code:


(prelude-require-packages '(use-package helm-swoop multiple-cursors
                             delight real-auto-save company header2
                             web-mode sqlup-mode company-quickhelp elpy
                             perspective nyan-mode magit sx smartparens
                             edit-server paredit guide-key helm-descbinds
                             multi-term free-keys pony-mode helm electric-case))


(require 'use-package)


(use-package smartparens
  :init
  (progn
    (require 'smartparens-config)
    (turn-on-smartparens-strict-mode)))


(use-package elpy
  :init
  (progn

    ;; to export venv
    ;; (let ((workon-home (expand-file-name "~/.virtualenvs/")))
    ;;   (setenv "WORKON_HOME" workon-home)
    ;;   (setenv "VIRTUALENVWRAPPER_HOOK_DIR" workon-home))

    (elpy-enable)
    (elpy-use-ipython)
    (defalias 'workon 'pyvenv-workon)
    (setq elpy-test-runner 'elpy-test-pytest-runner)

    (require 'smartparens-config)
    (turn-on-smartparens-strict-mode)

    (define-key smartparens-mode-map (kbd "M up") nil)
    (define-key smartparens-mode-map (kbd "M down") nil)))


;; (use-package real-auto-save
;;   :init
;;   (progn
;;     (add-hook 'prog-mode-hook 'real-auto-save-mode)
;;     (setq real-auto-save-interval 11)))


(use-package multiple-cursors
  :init
  (progn
    (global-set-key (kbd "C-c m e") 'mc/edit-lines)
    (global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)))


(use-package delight
  :init
  (delight '((abbrev-mode " Abv" abbrev)
             (smart-tab-mode " t" smart-tab)
             (eldoc-mode nil "eldoc")
             (paredit-mode " par" paredit)
             (projectile-mode " proj" projectile)
             (emacs-lisp-mode "Elisp" :major)
             (rainbow-mode)
             (flyspell-mode nil flyspell)
             (guru-mode nil guru))))


(use-package company
  :init
  (progn
    (global-company-mode 1)

    (setq company-idle-delay 0)
    (setq company-tooltip-limit 5)
    (setq company-minimum-prefix-length 1)
    (setq company-tooltip-flip-when-above t)

    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)))


(use-package header2
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'auto-make-header)))


(use-package web-mode
  :init
  (progn

    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

    (setq web-mode-engines-alist '(("django" . "\\.html\\'")))

    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)

    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-auto-expanding t)
    (setq web-mode-enable-css-colorization t)

    (set (make-local-variable 'company-backends) '(company-css))

    (define-key prelude-mode-map (kbd "C-c C-i") nil)
    (bind-key "C-c C-i" 'web-mode-buffer-indent)))


(use-package company-quickhelp
  :init
  (progn
    (company-quickhelp-mode 1)))


(use-package nyan-mode
  :init
  (nyan-mode))


(use-package magit
  :init
  (setq magit-status-buffer-switch-function 'switch-to-buffer))


(use-package sx
  :init
  (progn
    (require 'sx-load)))


(use-package edit-server
  :init
  (progn
    (when (require 'edit-server nil t)
      (setq edit-server-new-frame nil)
      (edit-server-start))))


(use-package mysql
  :init
  (progn
    (require 'sql)
    (add-hook 'sql-mode-hook 'sqlup-mode)
    (sql-set-product "mysql")

    (add-hook 'sql-interactive-mode-hook
              (lambda ()
                (toggle-truncate-lines t)))

    (load-file "~/.emacs.d/.private.el")

    (setq sql-connection-alist
          '((pool-server
             (sql-server sql-server-address)
             (sql-user sql-server-user)
             (sql-password sql-server-password)
             (sql-database sql-server-database)
             (sql-port sql-server-port))

            (pool-local
             (sql-server sql-local-server)
             (sql-user sql-local-user)
             (sql-password sql-local-password)
             (sql-database sql-local-database)
             (sql-port sql-local-port))))

    (defun sql-connect-preset (name)
      "Connect to a predefined SQL connection listed in `sql-connection-alist'"
      (eval `(let ,(cdr (assoc name sql-connection-alist))
               (flet ((sql-get-login (&rest what)))
                 (sql-product-interactive sql-product)))))

    (defun sql-pool-server ()
      (interactive)
      (sql-connect-preset 'pool-server))

    (defun sql-pool-local ()
      (interactive)
      (sql-connect-preset 'pool-local))))


(use-package guide-key
  :init
  (progn
    (setq guide-key/guide-key-sequence
          '("C" "ESC"
            "C-c" "C-h" "C-x"
            "C-c p" "C-x r"
            "C-c C-e" "C-c C-t"))
    (guide-key-mode 1)))


(use-package multi-term
  :init
  (progn
    (setq multi-term-program "/bin/zsh")
    (bind-key "C-c C-t" 'multi-term)
    (bind-key "C-c C-n" 'multi-term-next)
    (bind-key "C-c C-p" 'multi-term-prev)))


(use-package helm
  :init
  (progn
    (bind-key "C-x r l" 'helm-bookmarks)))


(use-package helm-swoop)
(use-package free-keys)
(use-package pony-mode)
(use-package helm-descbinds)


(use-package electric-case
  :init
  (progn

    (defun electric-case-python-init ()

      (electric-case-mode 1)
      (setq electric-case-max-iteration 2)

      (setq electric-case-criteria
            (lambda (b e)
              (let ((proper (electric-case--possible-properties b e))
                    (key (key-description (this-single-command-keys))))
                (cond
                 ((member 'font-lock-variable-name-face proper)
                  ;; #ifdef A_MACRO  /  int variable_name;
                  (if (member '(cpp-macro) (python-guess-basic-syntax)) 'usnake 'snake))
                 ((member 'font-lock-string-face proper) nil)
                 ((member 'font-lock-comment-face proper) nil)
                 ((member 'font-lock-keyword-face proper) nil)
                 ((member 'font-lock-function-name-face proper) 'snake)
                 ((member 'font-lock-type-face proper) 'snake)
                 (electric-case-convert-calls 'snake)
                 (t nil)))))

      (defadvice electric-case-trigger (around electric-case-c-try-semi activate)
        (when (and electric-case-mode
                   (eq major-mode 'python-mode)))))

    (add-hook 'python-mode-hook 'electric-case-python-init)
    (setq electric-case-convert-calls t)))


(provide 'packages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; packages.el ends here
