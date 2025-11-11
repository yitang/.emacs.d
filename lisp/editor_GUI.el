(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 180)

(tool-bar-mode -1)
(menu-bar-mode 1)
(scroll-bar-mode -1)

(load-theme 'modus-operandi)

(setq sml/no-confirm-load-theme t)
(use-package smart-mode-line
  :ensure t)
(setq powerline-arrow-shape 'curve)
(setq powerline-default-separator-dir '(right . left))
(setq sml/theme 'respectful)
(sml/setup)

(rich-minority-mode 1)
(setf rm-blacklist "")

(setq sml/mode-width 'full)
(setq sml/name-width 40)

(setq display-time-format "W%W %H:%M")
(display-time-mode)
;; (display-time)

(add-hook 'suspend-hook
	     (lambda () (or (y-or-n-p "Really suspend? ")
			    (error "Suspend canceled"))))
(add-hook 'suspend-resume-hook (lambda () (message "Resumed!")
				    (sit-for 2)))
