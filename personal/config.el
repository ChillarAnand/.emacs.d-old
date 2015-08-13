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


(provide 'config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; config.el ends here

