(setq use-package-always-ensure t)  ;; to ensure also install package

(defun yt/load-direcotry (dir)
  "load .el files in a direcotry dir"
  (interactive "P")
  (let ((config-files (directory-files dir t ".el$")))
    (mapcar #'load config-files))
  (message "Done loading %s" dir)
  )
(yt/load-direcotry "~/matrix/tools/.emacs.d/lisp")
