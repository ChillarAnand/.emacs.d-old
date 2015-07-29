;;; Code:

;; disable zenburn
(disable-theme 'zenburn)

;; maximize on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; remove scroll bar
(scroll-bar-mode -1)


;; colors
(set-background-color "#f1f1f1")
(add-to-list 'default-frame-alist '(background-color . "#f1f1f1"))
(set-default-font "Ubuntu Mono 13")


(provide 'ui)
;;; ui.el ends here
