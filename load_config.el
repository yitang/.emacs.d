(require 'cask "~/.cask/cask.el")
(cask-initialize "~/git/.emacs.d")

(require 'org)
(require 'golden-ratio)

(defun yt/load-direcotry (dir)
  "load .el files in a direcotry dir"
  (interactive "P")
  (let ((config-files (directory-files dir t ".el$")))
    (mapcar #'load config-files))
  (message "Doen loading %s" dir)
  )
(yt/load-direcotry "~/git/.emacs.d/config")
