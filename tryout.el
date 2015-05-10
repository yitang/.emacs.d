;;change default browser for 'browse-url'  to w3m
(setq browse-url-browser-function 'w3m-goto-url-new-session)
;;change w3m user-agent to android
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")
;; (setq w3m-user-agent "Emacs-w3m/1.4.540 w3m/0.5.3+debian-15")


(defun yt/hello ()
  "functon meant to be called first thing in the morning. 

It will open four windows:
1. weather of today and the next few days, 
2. my weekly calendar, without habits shown,
3. habits, 
4. git repo rpeort. "
  
  )


(defun yt/bye ()
  "function meat to be called before I leave

It reminds of me to 
1. sync git folder,
2. back up keyfreq file"
  (interactive)
  (yt/git-generate-report)
  (goto-char (point-max))
  (insert "
  (yt/daily-back-keyfreq)")
  )

  
(defun yt/git-repo-info ()
  (interactive)
  (let* ((sh-num-unpushed-commits "git status | grep \"\'origin/master\'\" | grep -Po \"[0-9]\"")
	 (sh-num-uncommited-files "git status --porcelain 2>/dev/null| egrep \"^(M| M)\" | wc -l")
	 (unpush (shell-command-to-string sh-num-unpushed-commits))
	 (uncommit (shell-command-to-string sh-num-uncommited-files)))
    (concat "unpushed commits: " unpush "\n" "uncommited files: " uncommit)))
