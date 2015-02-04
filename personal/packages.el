;;; packages.el ---
;;
;; Filename: packages.el
;; Description:
;; Author: K3
;; Maintainer:
;; Created: Tue Feb  3 11:06:20 2015 (+0530)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;;; code:


(prelude-require-packages '(use-package paredit helm-swoop multiple-cursors
                             delight real-auto-save company header2
                             web-mode sqlup-mode))

(require 'use-package)

(use-package helm-swoop)


(use-package real-auto-save
  :init
  (progn
    (add-hook 'prog-mode-hook 'real-auto-save-mode)
    (setq real-auto-save-interval 2)))


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

    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

    (set-face-attribute 'web-mode-javascript-string-face nil :foreground "green")

    (setq web-mode-engines-alist
          '(("django" . "\\.html\\'")))

    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)

    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-auto-expanding t)
    (setq web-mode-enable-css-colorization t)

    ;; (setq web-mode-ac-sources-alist
    ;;       '(("css" . (ac-source-css-property))
    ;;         ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

    (local-set-key (kbd "RET") 'newline-and-indent)

    (sp-with-modes '(web-mode)
      (sp-local-pair "%" "%"
                     :unless '(sp-in-string-p)
                     :post-handlers '(((lambda (&rest _ignored)
                                         (just-one-space)
                                         (save-excursion (insert " ")))
                                       "SPC" "=" "#")))
      (sp-local-pair "<% "  " %>" :insert "C-c %")
      (sp-local-pair "<%= " " %>" :insert "C-c =")
      (sp-local-pair "<%# " " %>" :insert "C-c #")
      (sp-local-tag "%" "<% "  " %>")
      (sp-local-tag "=" "<%= " " %>")
      (sp-local-tag "#" "<%# " " %>"))

    ))

(provide 'packages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; packages.el ends here
