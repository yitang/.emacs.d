(global-set-key [(f9)] 'compile)
(setq compilation-window-height 2)
(setq compilation-finish-function
      (lambda (buf str)

        (if (string-match "exited abnormally" str)

            ;;there were errors
            (message "compilation errors, press C-x ` to visit")

          ;;no errors, make the compilation window go away in 0.5 seconds
          (run-at-time 0.5 nil 'delete-windows-on buf)
          (message "NO COMPILATION ERRORS!"))))



(defun indent-buffer-ask()
   (when (y-or-n-p "Indent buffer before saving? ")
     (indent-region (point-min) (point-max))))

(defun indent-buffer-no-ask()
  (indent-region (point-min) (point-max)))

(setq c++-mode-hook
      '(lambda ()
         (c-set-style "cc-mode")
         (define-key c++-mode-map "\C-c\C-c" 'compile)
         (define-key c++-mode-map "\C-c\C-e" 'next-error)
;        (add-hook 'before-save-hook 'indent-buffer-ask nil t)                  
	 (add-hook 'before-save-hook 'indent-buffer-no-ask nil t)  ;; indent c++ files after save.
))




(require 'flymake-google-cpplint)
(add-hook 'c++-mode-hook 'flymake-google-cpplint-load)
;; (custom-set-variables
;; '(flymake-google-cpplint-command "/Library/Python/2.7/site-packages/cpplint/cpplint.py"))
;"/usr/local/bin/cpplint"))

; start google-c-style with emacs
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c++-mode-common-hook 'google-make-newline-indent)
