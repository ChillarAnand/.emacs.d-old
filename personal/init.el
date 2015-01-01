;; load custom packages
(defvar prelude-personal-packages (expand-file-name "packages.el" prelude-personal-dir)
  "This file contains a list of personal packages.")
(load-file prelude-personal-packages)


;; load custom config
(defvar prelude-personal-config (expand-file-name "config.el" prelude-personal-dir)
  "This file contains a list of personal config.")
(load-file prelude-personal-config)
