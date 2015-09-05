;;; config.el --- 
;;; Code:

(setq confirm-nonexistent-file-or-buffer nil)
(setq helm-ff-newfile-prompt-p nil)

(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(eval-after-load 'comint
  '(progn
     (define-key comint-mode-map (kbd "<up>") #'comint-previous-matching-input-from-input)
     (define-key comint-mode-map (kbd "<down>") #'comint-next-matching-input-from-input)))

(defun my-recenter-on-find-function (orig &rest args)
  (let ((result (apply orig args)))
    (when result
      (recenter 0))
    result))
(advice-add 'help-button-action :around #'my-recenter-on-find-function)


(provide 'config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; config.el ends here

