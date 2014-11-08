;;; org-jekyll-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "org-jekyll" "org-jekyll.el" (21597 22789 0
;;;;;;  0))
;;; Generated autoloads from org-jekyll.el

(autoload 'org-jekyll-export-current-entry "org-jekyll" "\


\(fn)" t nil)

(autoload 'org-jekyll-export-blog "org-jekyll" "\
Export all entries in project files that have a :blog: keyword
and an :on: datestamp.  Property drawers are exported as
front-matters, outline entry title is the exported document
title. 

\(fn)" t nil)

(autoload 'org-jekyll-export-project "org-jekyll" "\
Export all entries in project files that have a :blog: keyword
and an :on: datestamp.  Property drawers are exported as
front-matters, outline entry title is the exported document
title. 

\(fn PROJECT-NAME)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-jekyll-autoloads.el ends here
