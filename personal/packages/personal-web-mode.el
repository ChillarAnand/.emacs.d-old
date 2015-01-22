;;; Code:

(prelude-require-packages '(web-mode))

(require 'web-mode)

(defun personal-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)

  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t))

(add-hook 'web-mode-hook  'personal-web-mode-hook)


(provide 'personal-web-mode)
;;; personal-web-mode.el ends here
