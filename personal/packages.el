;;; packages.el --- 3rd party packages.

(prelude-require-package 'use-package)
(require 'use-package)
(setq use-package-always-ensure t)
(load-file "~/.emacs.d/.private.el")

;; (load-file "~/projects/lisp/impatient-markup/impatient-markup.el")
;; (impatient-markup-enable)

(use-package save-sexp)

(use-package smartparens
  :config
  (sp-pair "`" "`" :wrap "C-`")
  (sp-pair "%" "%" :wrap "C-%")
  (sp-pair "<" ">" :wrap "C->")
  (defun strict-smartparens ()
    (turn-on-smartparens-strict-mode))
  (add-hook 'prog-mode-hook 'strict-smartparens)
  (add-hook 'prelude-prog-mode-hook (lambda () (smartparens-mode -1)) t))


(add-to-list 'load-path "~/projects/lisp/elpy") 
(load "elpy" nil t)
(elpy-enable)
(use-package elpy
  :config
  (setq python-indent-offset 4)
  (setq elpy-test-runner 'elpy-test-pytest-runner)
  (setq elpy-rpc-timeout nil)
  ;; (setq elpy-rpc-python-command "python3")
  (append grep-find-ignored-files "flycheck_*")

  (defun my/send-region-or-buffer (&optional arg)
    (interactive "P")
    (elpy-shell-send-region-or-buffer arg)
    (with-current-buffer (process-buffer (elpy-shell-get-or-create-process))
      (set-window-point (get-buffer-window (current-buffer))
                        (point-max))))
  (define-key elpy-mode-map (kbd "C-<right>") 'elpy-nav-forward-indent)
  (define-key elpy-mode-map (kbd "C-<left>") 'elpy-nav-backward-indent)
  (define-key elpy-mode-map (kbd "C-c C-c") 'my/send-region-or-buffer))


(load-file "~/projects/lisp/real-auto-save/real-auto-save.el")
(add-hook 'prog-mode-hook 'real-auto-save-mode)
(setq real-auto-save-interval 4)

(use-package multiple-cursors
  :init
  (global-set-key (kbd "C-c m e") 'mc/edit-lines)
  (global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))


(use-package company
  :init
  (global-company-mode 1)

  (setq company-idle-delay 0)
  (setq company-tooltip-limit 5)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-flip-when-above t)

  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))


(use-package header2
  :init
  (add-hook 'emacs-lisp-mode-hook 'auto-make-header))


(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")))
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-js-indent-offset 0)
  (setq web-mode-script-padding 0)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-auto-pairing nil)
  (set (make-local-variable 'company-backends) '(company-css))

  (define-key prelude-mode-map (kbd "C-c C-i") nil)
  (bind-key "C-c C-i" 'web-mode-buffer-indent))


(use-package company-quickhelp
  :init
  (company-quickhelp-mode 1))


(use-package nyan-mode
  :init
  (nyan-mode))


(use-package magit
  :init
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  (setq magit-last-seen-setup-instructions "1.4.0"))


(use-package sx
  :init
  (require 'sx-load))


(use-package edit-server
  :init
  (when (require 'edit-server nil t)
    (setq edit-server-new-frame nil)
    (edit-server-start)))


(require 'sql)
(require 'mysql)
(use-package sqlup-mode)
(progn
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (toggle-truncate-lines t)))

  (sql-set-product "mysql")
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

  (defun mysql-send-paragraph ()
    (interactive)
    (sql-send-paragraph)
    (with-current-buffer (process-buffer (get-process "SQL"))
      (set-window-point (get-buffer-window (current-buffer))
                        (point-max))))

  (define-key sql-mode-map (kbd "C-c C-c") 'mysql-send-paragraph))


(use-package multi-term
  :config
  (setq multi-term-program "/bin/zsh")
  (bind-key "C-c C-t" 'multi-term)
  (bind-key "C-c C-n" 'multi-term-next)
  (bind-key "C-c C-p" 'multi-term-prev))


(use-package free-keys)


(use-package electric-case
  :config
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
  (setq electric-case-convert-calls t))


(use-package smart-mode-line
  :config
  (sml/setup)
  (sml/apply-theme 'light)
  (rich-minority-mode 1))


;; (use-package circe
;;   :init
;;   (setq circe-network-options
;;         `(("Freenode"
;;            :nick "chillaranand"
;;            :channels
;;            ("#emacs" "#emacs-circe" "#emacs-elpy"
;;             "#python-india" "#python-dev"
;;             "#dgplug")
;;            :nickserv-password ,freenode-password)))
;;   (setq circe-reduce-lurker-spam t))


;; (use-package wakatime-mode
;;   :config
;;   (setq wakatime-python-bin "/usr/local/bin/wakatime")
;;   (global-wakatime-mode))


(use-package impatient-mode)

(use-package pony-mode
  :ensure t
  :config
  (setq pony-server-host "127.0.0.1")
  (add-hook 'python-mode-hook 'pony-mode))


(use-package highlight-symbol
  :init
  (progn
    (global-set-key [f3] 'highlight-symbol-next)
    (global-set-key [(shift f3)] 'highlight-symbol-prev)
    (global-set-key [(control f3)] 'highlight-symbol)
    (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
    (highlight-symbol-mode 1)
    (highlight-symbol-nav-mode 1)))


(use-package comment-dwim-2
  :init
  (global-set-key (kbd "M-;") 'comment-dwim-2))


(use-package openwith
  :init
  (openwith-mode t)
  (setq large-file-warning-threshold 500000000)
  (setq openwith-associations
        (list (list (openwith-make-extension-regexp '("pdf"))
                    "evince" '(file))
              (list (openwith-make-extension-regexp '("flac" "mp3" "wav"))
                    "vlc" '(file))
              (list (openwith-make-extension-regexp
                     '("avi" "flv" "mov" "mp4" "mkv" "mpeg" "mpg" "ogg" "wmv"))
                    "vlc" '(file))
              (list (openwith-make-extension-regexp '("bmp" "jpeg" "jpg" "png"))
                    "ristretto" '(file))
              (list (openwith-make-extension-regexp '("doc" "docx" "odt"))
                    "libreoffice" '("--writer" file))
              (list (openwith-make-extension-regexp '("ods" "xls" "xlsx"))
                    "libreoffice" '("--calc" file))
              (list (openwith-make-extension-regexp '("odp" "pps" "ppt" "pptx"))
                    "libreoffice" '("--impress" file))
              )))



(use-package helm-dired-recent-dirs)
(use-package helm-chrome)
(use-package helm-swoop)
(use-package helm-descbinds)

(use-package helm-github-stars
  :config
  (setq helm-github-stars-username "chillaranand"))


(use-package helm
  :config
  
  (defvar helm-source-emacs-commands
    (helm-build-sync-source "Emacs commands"
      :candidates (lambda ()
                    (let ((cmds))
                      (mapatoms
                       (lambda (elt) (when (commandp elt) (push elt cmds))))
                      cmds))
      :coerce #'intern-soft
      :action #'command-execute)
    "A simple helm source for Emacs commands.")

  (defvar helm-source-emacs-commands-history
    (helm-build-sync-source "Emacs commands history"
      :candidates (lambda ()
                    (let ((cmds))
                      (dolist (elem extended-command-history)
                        (push (intern elem) cmds))
                      cmds))
      :coerce #'intern-soft
      :action #'command-execute)
    "Emacs commands history")

  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-dired-recent-dirs
                                    helm-source-emacs-commands-history
                                    helm-source-emacs-commands
                                    helm-chrome-source
                                    hgs/helm-c-source-stars
                                    hgs/helm-c-source-repos
                                    helm-source-buffer-not-found
                                    hgs/helm-c-source-search))
  (setq helm-M-x-always-save-history t)
  (bind-key "C-x r l" 'helm-bookmarks))

(use-package phi-search
  :init
  (global-set-key (kbd "C-s") 'phi-search))


(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1))


(use-package google-translate
  :config
  (setq  google-translate-default-source-language "en")
  (setq  google-translate-default-target-language "kn")
  (require 'google-translate-default-ui))


;; (use-package slime
;;   :init
;;   (progn
;;     (setq inferior-lisp-program "/usr/bin/sbcl")
;;     (setq slime-contribs '(slime-fancy))
;;     (add-to-list 'slime-contribs 'slime-repl)))


(use-package ace-link
  :config
  (ace-link-setup-default))


;; (require 'company)
;; (require 'company-web-html)
;; (add-to-list 'company-backends 'company-web-html)

;; (define-key web-mode-map (kbd "C-'") 'company-web-html)
;; (add-hook 'web-mode-hook (lambda ()
;;                            (set (make-local-variable 'company-backends) '(company-web-html company-files))
;;                            (company-mode t)))


(use-package writegood-mode)
(use-package writeroom-mode)
(use-package sotlisp)


;; (require 'emmet-mode)
;; (add-hook 'sgml-mode-hook 'emmet-mode)
;; (add-hook 'html-mode-hook 'emmet-mode)
;; (add-hook 'css-mode-hook  'emmet-mode)

;; (use-package zencoding-mode)
;; (add-hook 'html-mode-hook 'zencoding-mode)


(use-package benchmark-init
  :config
  (benchmark-init/activate))


;; (use-package elisp-slime-nav
;;   :init
;;   (progn
;;     (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
;;       (add-hook hook 'turn-on-elisp-slime-nav-mode))
;;     (global-set-key (kbd "C-c C-d") 'elisp-slime-nav-describe-elisp-thing-at-point)))


(use-package auto-capitalize
  :config
  (autoload 'auto-capitalize-mode "auto-capitalize"
    "Toggle `auto-capitalize' minor mode in this buffer." t)
  (autoload 'turn-on-auto-capitalize-mode "auto-capitalize"
    "Turn on `auto-capitalize' minor mode in this buffer." t)
  (autoload 'enable-auto-capitalize-mode "auto-capitalize"
    "Enable `auto-capitalize' minor mode in this buffer." t))

(require 'markdown-mode)


(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


(use-package key-chord
  :config
  (setq key-chord-one-keys-delay 0.5)
  (setq key-chord-two-keys-delay 0.5))


(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))


(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))


(use-package bm
  :config
  (global-set-key (kbd "<f5>") 'bm-toggle)
  (global-set-key (kbd "<f7>") 'bm-next)
  (global-set-key (kbd "<f6>") 'bm-previous))


;; (use-package ws-butler
;;   :config
;;   (ws-butler-global-mode))


(require 'dired)
(setq delete-by-moving-to-trash t)
(setq dired-no-confirm t)
(define-key dired-mode-map "u" 'dired-up-directory)
(defun delete-current-item ()
  (interactive)
  (dired-flag-file-deletion 1)
  (dired-do-flagged-delete))
(define-key dired-mode-map  [delete] 'delete-current-item)
(setq dired-deletion-confirmer '(lambda (x) t))

(defadvice dired-delete-entry (before force-clean-up-buffers (file) activate)
  (kill-buffer (get-file-buffer file)))

(define-key dired-mode-map "u" 'dired-up-directory)
;; unzip zipped file dired
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(use-package dired+
  :config
  (defun dir ()
    (interactive)
    (dired-sort-other "ll"))
  ;; (add-hook 'dired-mode-hook 'dir)
  )


(use-package flycheck-pos-tip
  :config
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))


;; (load-file "~/.emacs.d/vendor/sql-completion.el")
;; (require 'sql-completion)
;; (setq sql-interactive-mode-hook
;;       (lambda ()
;;         (define-key sql-interactive-mode-map "\t" 'comint-dynamic-complete)
;;         (sql-mysql-completion-init)))

(use-package lispy
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))


;; (use-package soundcloud
;;   :config
;;   (require 'emms-setup)
;;   (emms-standard)
;;   (emms-default-players))


(provide 'packages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; packages.el ends here
