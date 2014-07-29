config =~/.emacs= 
 
    ;; Configure before loading org mode (package-initialize)  
    (package-initialize) 
    ;; init.el and its depences files
    (add-to-list 'load-path "~/git/.emacs.d") 
    ;; libraries 
    (let ((default-directory "~/git/.emacs.d/elpa"))
      (normal-top-level-add-subdirs-to-load-path))
    (load "init.el")