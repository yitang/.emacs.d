(defun org-effectiveness-count-keyword(keyword)
  "Print a message with the number of keyword outline in the current buffer"
  (interactive "sKeyword: ")
  (save-excursion
    (goto-char (point-min))
    (count-matches (concat "* " keyword))))

(defun org-effectiveness-count-keyword-list (keywords-list)
   (mapcar 'org-effectiveness-count-keyword keywords-list))
    
(defun count-keyword-file (this-file)
  (with-temp-buffer
    (insert-file-contents  this-file)
    (org-effectiveness-count-keyword-list effectiveness-keywords)))


(defun org-effectiveness-table ()
  (interactive)
  (setq counts (mapcar 'count-keyword-file (org-agenda-files)))
  (pairlis (org-agenda-files) counts)
  )

(defun yt/bakcup-todo-keywords-no-ask ()
  (interactive)
  (let* ((eff-table (org-effectiveness-table))
         (eff-table-str (prin1-to-string eff-table))
         (ymd (format-time-string "%Y-%m-%d"))
         (file-name (concat effectiveness-data-dir "/" ymd "-auto")))
    (with-temp-file file-name
      (insert ymd)
      (insert "\n")
      (insert (prin1-to-string effectiveness-keywords))
      (insert "\n")
      (insert eff-table-str))))


(defun yt/daily-back-keyfreq-no-ask ()
  "back up .emacs.keyfreq file.
Move it to ~/git/.emacs.d/keyfreq with file name being the date and machine"
  (interactive)
  (let* ((place (if (string= system-name "mbp.local")
                    "mac"
                  "ubuntu"))
         (file-name (concat (format-time-string "%F")
                            "-"
                            place
			    "-auto"))
	 (temporary-file-directory "~/git/.emacs.d/keyfreq")
	 (file-name (make-temp-file (concat file-name "-")))) ;; - add ann random string to the end
    ;; (shell-command (concat "cd ~/git/.emacs.d/keyfreq/; mv ~/.emacs.keyfreq " file-name))
    (shell-command (concat "mv ~/.emacs.keyfreq " file-name))
    (print file-name)
    ))

(setq effectiveness-keywords (list "TODO" "NEXT" "SOMEDAY"  "DONE"  "HOLD"  "CANCELLED" "PHONE" "MEETING"))
(setq effectiveness-data-dir "/home/yitang/git/.emacs.d/effectiveness")


(message "gonna backup")
(yt/bakcup-todo-keywords-no-ask)
(yt/daily-back-keyfreq-no-ask)
(message "88")
