;;change default browser for 'browse-url'  to w3m
(setq browse-url-browser-function 'w3m-goto-url-new-session)
;;change w3m user-agent to android
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")
;; (setq w3m-user-agent "Emacs-w3m/1.4.540 w3m/0.5.3+debian-15")


(setq sml/mode-width 10)
(setq sml/name-wdith 10)
(olivetti-mode)


;; sunshine: weather forcaste service
(setq sunshine-units 'metric)
(setq sunshine-location "Keighley, GB")




(defun yt/daily-back-keyfreq ()
  "back up .emacs.keyfreq file.
Move it to ~/git/.emacs.d/keyfreq with file name being the date and machine"
  (interactive)
  (let* ((place (if (string= system-name "mbp.local")
		    "mac"
		  "ubuntu"))
	 (file-name (concat (format-time-string "%F")
			    "-"
			    place)))
    (if (yes-or-no-p (concat "will move .emacs.keyfreq to " file-name))
	(shell-command (concat "cd ~/git/.emacs.d/keyfreq/; mv ~/.emacs.keyfreq " file-name)) 
      "do nothing")
    ))




(defun yt/go-home ()
  "function meat to be called before I leave

It reminds of me to 
1. sync git folder,
2. back up keyfreq file"
  (interactive)
  
  (yt/daily-back-keyfreq)
  )

  
(defun yt/git-repo-info ()
  (interactive)
  (let* ((sh-num-unpushed-commits "git status | grep \"\'origin/master\'\" | grep -Po \"[0-9]\"")
	 (sh-num-uncommited-files "git status --porcelain 2>/dev/null| egrep \"^(M| M)\" | wc -l")
	 (unpush (shell-command-to-string sh-num-unpushed-commits))
	 (uncommit (shell-command-to-string sh-num-uncommited-files)))
    (concat "unpushed commits: " unpush "\n" "uncommited files: " uncommit)))




(setq org-export-with-toc nil
      org-export-with-todo-keywords t
      org-export-with-sub-superscripts nil
      org-export-with-planning t
      org-export-with-timestamps nil
      org-export-babel-evaluate nil)
(setq org-archive-location "::* Archived Tasks")

