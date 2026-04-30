(setq use-package-always-ensure t)  ;; to ensure also install package

(defun yt/load-directory (dir)
  "load .el files in a directory dir"
  (interactive "P")
  (let ((config-files  (directory-files dir t ".el$")))
    (mapcar #'load config-files))
  (message "Done loading %s" dir)
  )
(yt/load-directory "~/matrix/tools/.emacs.d/lisp")
