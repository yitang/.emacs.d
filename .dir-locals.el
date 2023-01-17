((nil . ((eval . (progn
		   (setq-local project-dir (locate-dominating-file default-directory ".dir-locals.el"))
		   (setq-local note-dir (concat project-dir "notes/"))
                   (setq-local org-roam-directory note-dir)
                   (setq-local org-roam-db-location (concat note-dir "org-roam.db"))
		   (setq-local projectile-project-root project-dir)
		   (setq-local org-download-image-dir (concat project-dir "assets/org-download"))
		   )
	       ))))
