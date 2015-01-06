;;; config - personal config for prelude emacs

;; unzip zipped file dired
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))


;; yow - easter egg
(unless (file-exists-p "~/.emacs.d/personal/yow.txt.gz")
  (shell-command "wget bit.ly/emacs-yow -O ~/.emacs.d/personal/yow.txt.gz"))
(setq yow-file "~/.emacs.d/yow.txt.gz")


;; ctags
(setq path-to-ctags "/usr/big/ctags-exuberant")
