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
                             edit-server paredit guide-key helm-descbinds))


(require 'use-package)

(use-package helm-swoop)


(use-package real-auto-save
  :init
  (progn
    (add-hook 'prog-mode-hook 'real-auto-save-mode)
    (setq real-auto-save-interval 8)))


(use-package paredit
  :init
  (progn
    (autoload 'enable-paredit-mode
      "paredit" "Turn on pseudo-structural editing of Lisp code." t)

    (add-hook 'prog-mode-hook 'enable-paredit-mode)

    ;; no space before parens
    (defun paredit-space-for-delimiter-p (endp delimiter)
      (and (not (if endp (eobp) (bobp)))
           (memq (char-syntax (if endp (char-after) (char-before)))
                 (list ?\"  ;; REMOVED ?w ?_
                       (let ((matching (matching-paren delimiter)))
                         (and matching (char-syntax matching)))))))

    (global-set-key (kbd "{") 'paredit-open-curly)
    (global-set-key (kbd "[") 'paredit-open-square)
    (global-set-key (kbd "<") 'paredit-open-angled)))


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
    (setq company-idle-delay 0.5)
    (setq company-tooltip-limit 10)
    (setq company-minimum-prefix-length 1)
    (setq company-tooltip-flip-when-above t)
    (global-company-mode 1)))


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

    (define-key paredit-mode-map (kbd "M-;") nil)
    (bind-key "C-c C-c" 'web-mode-comment-or-uncomment)))


(use-package company-quickhelp
  :init
  (progn
    (company-quickhelp-mode 1)))


(use-package elpy
  :init
  (progn

    ;; to export venv
    (let ((workon-home (expand-file-name "~/.virtualenvs/")))
      (setenv "WORKON_HOME" workon-home)
      (setenv "VIRTUALENVWRAPPER_HOOK_DIR" workon-home))

    (elpy-enable)
    (elpy-use-ipython)
    (defalias 'workon 'pyvenv-workon)
    (setq elpy-test-runner 'elpy-test-pytest-runner)))


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


(use-package smartparens
  :init
  (progn
    (require 'smartparens-config)
    (smartparens-strict-mode t)))


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
      (sql-connect-preset 'pool-local))

    ))


(use-package guide-key
  :init
  (progn
    (setq guide-key/guide-key-sequence '("C" "C-x" "C-c" "C-c p"))
    (guide-key-mode 1)))


(use-package helm-descbinds)


(provide 'packages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; packages.el ends here
