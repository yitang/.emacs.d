(require 'cask "~/.cask/cask.el")
(cask-initialize "~/git/.emacs.d")

(defun yt/load-emacs-config (config-dir)
  "docstring"
  (interactive "P")
  (let ((config-files (directory-files config-dir t ".el$")))
    (mapcar #'load config-files))
  )
(yt/load-config "./config")

