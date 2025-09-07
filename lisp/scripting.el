(defalias 'open 'find-file)


;; from emacs-reddit
;; (defun eshell-here ()
;;   "Opens up a new shell in the directory associated with the current buffer's file."
;;   (interactive)
;;   (let* ((parent (file-name-directory (buffer-file-name)))
;;          (name   (car
;;                   (last
;;                    (split-string parent "/" t)))))
;;     (split-window-vertically)
;;     (other-window 1)
;;     (eshell "new")
;;     (rename-buffer (concat "*eshell: " name "*"))

;;     (insert (concat "ls"))
;;     (eshell-send-input)))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
			 (file-name-directory (buffer-file-name))
		       default-directory))
	     (name (car (last (split-string parent "/" t)))))
	(split-window-vertically)
	(other-window 1)
	(eshell "new")
	(rename-buffer (concat "*eshell: " name "*"))

	(insert (concat "ls"))
	(eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

(defun delete-single-window (&optional window)
  "Remove WINDOW from the display.  Default is `selected-window'.
If WINDOW is the only one in its frame, then `delete-frame' too."
  (interactive)
  (save-current-buffer
	(setq window (or window (selected-window)))
	(select-window window)
	(kill-buffer)
	(if (one-window-p t)
	    (delete-frame)
	  (delete-window (selected-window)))))

(defun eshell/x (&rest args)
  (delete-single-window))

(use-package magit
  :ensure t
  :config
  (magit-auto-revert-mode nil)  ;; why disbale it? 
  (global-auto-revert-mode t)   ;; i think it's a good idea to have auto revert.
  :bind (("<f9> g" . magit-status)))

(defun yt/save-all-buffers ()
  "save all files-visiting buffers without user confirmation"
  (interactive)
  (save-some-buffers t nil)
  (message "save all buffers... done"))
(run-at-time "05:59" 3600 'yt/save-all-buffers)

(defun yt/git-backup ()
  (let ((git-sh-scripts "
echo Start to Sync: $(date) 

REPOS=\"org\"
for REPO in $REPOS
do
    echo
    echo \"Repository: $REPO\"
    cd ~/git/$REPO
    # update
    git pull 
    # Remove deleted files
    git ls-files --deleted -z | xargs -0 git rm >/dev/null 2>&1
    # Add new files
    git add -A . >/dev/null 2>&1
    git commit -m \"$(date)\"
    git push origin main
done

echo Finished Sync: $(date)
"))
    (async-shell-command git-sh-scripts))
  (message "all git sync... done"))

(defun yt/git-up ()
  (interactive)
  (yt/save-all-buffers)
  (yt/git-backup))

;; (cond ((eq system-type 'darwin)
;;        (run-at-time "05:59" 10800 'yt/git-up)))
