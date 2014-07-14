config =~/.emacs= 
;; change starigng directory 
(setq default-directory "~/git/")
;; add library and lisp scripts
(let ((default-directory "~/git/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))
(load "init.el")
