;;; config.el --- 
;;; Code:

(setq confirm-nonexistent-file-or-buffer nil)
(setq helm-ff-newfile-prompt-p nil)

(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(provide 'config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; config.el ends here

