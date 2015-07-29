;;; utils.el --- 
;;; Code:

(defun delete-whole-line (&optional arg)
  "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))


(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


(defun swap-numbers-symbols ()
  "Bind symbols to digits."
  (interactive)
  (define-key key-translation-map (kbd "!") (kbd "1"))
  (define-key key-translation-map (kbd "@") (kbd "2"))
  (define-key key-translation-map (kbd "#") (kbd "3"))
  (define-key key-translation-map (kbd "$") (kbd "4"))
  (define-key key-translation-map (kbd "%") (kbd "5"))
  (define-key key-translation-map (kbd "^") (kbd "6"))
  (define-key key-translation-map (kbd "&") (kbd "7"))
  (define-key key-translation-map (kbd "*") (kbd "8"))
  (define-key key-translation-map (kbd "(") (kbd "9"))
  (define-key key-translation-map (kbd ")") (kbd "0"))

  (define-key key-translation-map (kbd "1") (kbd "!"))
  (define-key key-translation-map (kbd "2") (kbd "@"))
  (define-key key-translation-map (kbd "3") (kbd "#"))
  (define-key key-translation-map (kbd "4") (kbd "$"))
  (define-key key-translation-map (kbd "5") (kbd "%"))
  (define-key key-translation-map (kbd "6") (kbd "^"))
  (define-key key-translation-map (kbd "7") (kbd "&"))
  (define-key key-translation-map (kbd "8") (kbd "*"))
  (define-key key-translation-map (kbd "9") (kbd "("))
  (define-key key-translation-map (kbd "0") (kbd ")")))


(defun current-dired ()
  (interactive)
  (dired "."))


(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))


(defun launch-separate-emacs-under-x ()
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun restart-emacs ()
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (add-hook 'kill-emacs-hook
            (if (display-graphic-p)
                #'launch-separate-emacs-under-x
              #'launch-separate-emacs-in-terminal)
            t)
  (kill-emacs))


(provide 'utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utils.el ends here
