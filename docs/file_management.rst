File Management
===============



Alternative to shell
--------------------



For the file management tasks like rename and delete, I'd like to
wrapper it as a Lisp function and call it directly in Emacs. 

.. code-block:: scheme

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

    (defun yt/last-updated-date ()
      "return modification time of current file-visitng buffer"
      (interactive)
      (let* ((mtime (visited-file-modtime))) 
        (unless (integerp mtime)
          (concat "/Last UPdated/: "
                  (format-time-string "%d %b %Y" mtime)))))

    (defun yt/sudo-find-file (file-name)
      "Like find file, but opens the file as root."
      (interactive "FSudo Find File: ")
      (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
        (find-file tramp-file-name)))


    (defhydra hydra-file-management (:color red
                                            :hint nil)
      "
    _o_pen file
    _O_pen file as Sudo user 
    copy file _p_ath to kill ring
    _r_ename buffer-visiting file 
    _d_elete buffer-visiting file"
      ("o" find-file)
      ("O" yt/sudo-find-file)
      ("p" yt/copy-full-path-to-kill-ring)
      ("r" yt/rename-current-buffer-file)
      ("c" yt/copy-file-to)
      ("d" yt/delete-this-buffer-and-file)
      )
    ;; (global-set-key [f3] 'hydra-file-management/body)

Another useful Lisp function is to copy the file path to clipboard. 

.. code-block:: scheme

    ;; full path of current buffer
    (defun yt/copy-full-path-to-kill-ring ()
      "copy buffer's full path to kill ring"
      (interactive)
      (when buffer-file-name
        (kill-new (file-truename buffer-file-name))))

Projectile - Directory Access
-----------------------------



I only use *projectile* to jump between different git folders, so
there isn't much configuration except using ``helm`` for selection.

.. code-block:: scheme

    (require 'projectile)
    (helm-projectile-on)
    (require 'helm-projectile)
    (projectile-global-mode)
    (setq projectile-enable-caching t)
    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-remember-window-configs t )
    (setq projectile-completion-system 'helm)
    (setq projectile-switch-project-action 'helm-projectile)

Remote (SSH)
------------



Sometimes, I need to sync between local and remote machine, I can do
it in termnial or in Emacs.

.. code-block:: scheme

    (require 'tramp)
    (require 'ssh)
    (setq password-cache-expiry nil)

    ;; (defun yt/sync-local-remote ()
    ;;   (interactive)
    ;;   "copy all files in remote:~/LR_share to local:~/LR_share,
    ;; does not support the ther way"
    ;;   (find-file "/ssh:JBA28:/home/local/JBANORTHWEST/yitang/LR_share")
    ;;   ;; (mark-whole-buffer)
    ;;   (dired-mark-subdir-files)
    ;;   ;; (find-file "~/LR_share")
    ;;   ;; (setq-local dirqed-dwim-target t)
    ;;   (dired-do-copy))

Git Sync
--------



I use git and Github a lot, and usually in ``shell-mode``, but I just
can't remember all the commands. Magit provides an interface to Git,
and it is really pleasant to use. So I don't need to remmeber all the
commands, also it comes with excellent `manual <http://magit.github.io/master/magit.html>`_ and `cheatsheet <http://daemianmack.com/magit-cheatsheet.html>`_.

.. code-block:: scheme

    (require 'magit)
    (setq magit-last-seen-setup-instructions "1.4.0")
    (setq magit-auto-revert-mode nil)

I use Emacs/org-mode as a unified system to do everything, at home and
in the office. I used to use Dropbox/Copy to
automatically sync the files on multiple machines, but this work-flow
can be dangerous. Image if I was editing same files on two machines at
the same time, then I can't track which is which.

The good thing about Git is that you can see what exactlly has been
changed by each version, and auto log, with commit information and
timesatmp. Magit helps me to do it conviently but I need 3 more
features:

1. automatically save all the buffers

Occasionally my office machine goes down because I run R with big
data, and it consumes all the memory. If that happens, I
potentially lose the newsiest version of scripts, which is bit
annoy. The following snippets will save all buffers in every hours.

.. code-block:: scheme

    (defun yt/save-all-buffers ()
      "save all files-visiting buffers without user confirmation"
      (interactive)
      (save-some-buffers t nil)
      (message "save all buffers... done"))
    (run-at-time "05:59" 3600 'yt/save-all-buffers)))

1. quick rush

I shared lift with my colluge, and someimes I left at the last
minutes, then what I do is call a functions that commits and upload
to the repo so that I can continue work at home.

The ``yt/save-git-backup`` function will do

1. pull from the remote repo, and make sure the local repo is always
   up-to-date.

2. add everything and commit with a timesamp.

3. push local changes to the remote repo.


Here is the snippts.

.. code-block:: scheme

    (defun yt/git-backup ()
      (let ((git-sh-scripts "
    echo Start to Sync: $(date) 

    REPOS=\"org jbarm\"
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
        git push origin master 
    done

    echo Finished Sync: $(date)
    "))
        (async-shell-command git-sh-scripts))
      (message "all git sync... done"))

    (defun yt/save-git-backup ()
      (interactive)
      (yt/save-all-buffers)
      (yt/git-backup))

1. automatically commit

Few times I did some important work over the weenend, but once I
arrived office I realised I forgot uploading, These situations are
quick frustrating. The following snippets will start to uploads once
every three hours on my MacbookPro, but I don't use it anymore, since
I can get most of my work done in the office.

.. code-block:: scheme

    ;; (cond ((eq system-type 'darwin)
    ;;        (run-at-time "05:59" 10800 'yt/save-git-backup)))
