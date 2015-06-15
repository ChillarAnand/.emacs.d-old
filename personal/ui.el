;;; Code:

;; disable zenburn
(disable-theme 'zenburn)


;; maximize on startup
(defun fullscreen (&optional f)
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

(fullscreen)


;; remove scroll bar
(scroll-bar-mode -1)


;; colors
;;(set-background-color "#f1f1f1")
;;(set-background-color "#eeeeee")
;;(add-to-list 'default-frame-alist '(foreground-color . "#E0DFDB"))
(add-to-list 'default-frame-alist '(background-color . "#f1f1f1"))
(add-to-list 'default-frame-alist '(background-color . "#eeeeee"))



(provide 'ui)
;;; ui.el ends here
