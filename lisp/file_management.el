;; rename current buffer-visiting file
(defun yt/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;; full path of current buffer
(defun yt/copy-full-path-to-kill-ring ()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (let* ((file-truename buffer-file-name))
      ;;(rel-name (file-relative-name file-truename "~/")))  ; BUG: if filename is not relative to home directory.
      ;; (kill-new (concat "~/" rel-name)))))
      (kill-new file-truename))))

(defun yt/sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(defun yt/last-updated-date ()
  "return modification time of current file-visitng buffer"
  (interactive)
  (let* ((mtime (visited-file-modtime))) 
    (unless (integerp mtime)
      (concat "/Last UPdated/: "
              (format-time-string "%d %b %Y" mtime)))))

(defun yt/delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun yt/open-file-manager ()
  "Show current file in desktop (OS's file manager)."
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil)) (start-process "" nil "xdg-open" "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ⁖ with nautilus
    )))

;; sort files in dired mode by datetime
(setq dired-listing-switches "-lsh")
(setq dired-recursive-copies 'always)
(setq dired-dwim-target t)

(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode)))

(use-package projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; as of [2023-01-17 Tue 19:35], there's issue with
;; projectile-switch-project function. it cannot detect the right
;; project directory. so i have to use consult-projectile for this.
(use-package consult-projectile)
(setq consult-projectile-source-projectile-project-action #'(lambda (dir) (dired dir)))
(define-key projectile-command-map (kbd "p") 'consult-projectile-switch-project)

(use-package tramp)
(use-package ssh)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

(defun yt/tramp-abort ()
  ;; kill all tramp connections.
  (interactive)
  (recentf-cleanup)
  (tramp-cleanup-all-buffers)
  (tramp-cleanup-all-connections))

(setq password-cache-expiry nil)

;; (defun yt/sync-local-remote ()
;;   (interactive)
;;   "copy all files in remote:~/LR_share to local:~/LR_share,
;; does not support the ther way"
;;   (find-file "/ssh:remote_host:/remote_directory")
;;   ;; (mark-whole-buffer)
;;   (dired-mark-subdir-files)
;;   ;; (find-file "~/LR_share")
;;   ;; (setq-local dirqed-dwim-target t)
;;   (dired-do-copy))

(defvar yt/temp-dir "~/.tmp"
  "temporay folders")

(defun yt/open-tmp-R ()
  (interactive)
  (find-file (expand-file-name "tmp.R" yt/temp-dir)))
(defun yt/open-tmp-el ()
  (interactive)
  (find-file (expand-file-name "tmp.el" yt/temp-dir)))
(defun yt/open-tmp-org ()
  (interactive)
  (find-file (expand-file-name "tmp.org" yt/temp-dir)))
(global-set-key (kbd "<f9> f r") 'yt/open-tmp-R)
(global-set-key (kbd "<f9> f e") 'yt/open-tmp-el)
(global-set-key (kbd "<f9> f o") 'yt/open-tmp-org)
