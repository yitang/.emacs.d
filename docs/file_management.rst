===============
File Management
===============




Alternative to shell
--------------------



For the file management tasks like rename and delete, I'd like to
wrapper it as a Lisp function and call it directly in Emacs. 

Rename the buffer-visiting file, and also rename the buffer. Similar
to the *save as* idea but will remove the older file. 

.. code:: common-lisp

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

Another useful Lisp function is to copy the file path to clipboard for
cross reference. 

.. code:: common-lisp

    ;; full path of current buffer
    (defun yt/copy-full-path-to-kill-ring ()
      "copy buffer's full path to kill ring"
      (interactive)
      (when buffer-file-name
        (let* ((file-truename buffer-file-name))
          ;;(rel-name (file-relative-name file-truename "~/")))  ; BUG: if filename is not relative to home directory.
          ;; (kill-new (concat "~/" rel-name)))))
          (kill-new file-truename))))

Open a file as a root user in Emacs, very handy. 

.. code:: common-lisp

    (defun yt/sudo-find-file (file-name)
      "Like find file, but opens the file as root."
      (interactive "FSudo Find File: ")
      (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
        (find-file tramp-file-name))) 

Find out the last modified date for current buffer, I need this often
when updating a blog post or documents. 

.. code:: common-lisp

    (defun yt/last-updated-date ()
      "return modification time of current file-visitng buffer"
      (interactive)
      (let* ((mtime (visited-file-modtime))) 
        (unless (integerp mtime)
          (concat "/Last UPdated/: "
    	      (format-time-string "%d %b %Y" mtime)))))

Remove current buffer-visiting file, and kill the buffer. I use this
function often in testing and trying out. 

.. code:: common-lisp

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

It is a good practise to group all the file management related
commands together using hydra. 

.. code:: common-lisp

    (defhydra hydra-file-management (:color red
    					:hint nil)
      "
    _o_pen file
    _O_pen file as Sudo user 
    copy file _P_ath to kill ring
    _r_ename buffer-visiting file 
    _d_elete buffer-visiting file
    open with _e_xternal application
    _g_it sync"
      ("o" find-file)
      ("O" yt/sudo-find-file)
      ("P" yt/copy-full-path-to-kill-ring)
      ("r" yt/rename-current-buffer-file)
      ("c" yt/copy-file-to)
      ("d" yt/delete-this-buffer-and-file)
      ("e" prelude-open-with)
      ("g" yt/git-up))
    (global-set-key [f3] 'hydra-file-management/body)

Open the file manager at the default directory.

.. code:: common-lisp

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

Projectile - Directory Access
-----------------------------



*Projectile* is an powerful Emacs package but I only use *projectile*
to jump between different git folders, so there isn't much
configuration except using ``helm`` for selection.

.. code:: common-lisp

    (require 'projectile)
    (projectile-mode +1)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (helm-projectile-on)
    (require 'helm-projectile)
    (projectile-global-mode)
    (setq projectile-enable-caching t)
    (setq projectile-switch-project-action 'projectile-dired)
    (setq projectile-remember-window-configs t )
    (setq projectile-completion-system 'helm)
    (setq projectile-switch-project-action 'helm-projectile)
    (setq projectile-project-root-files-bottom-up '(".git" ".projectile")) ;; .projectile comes first

There are many things work out of box. For example, use ``C-p p`` to
choose which project to jump to, but I can type ``M-g`` to invoke Magit
or ``M-e`` to invoke Eshell for that project. 

Remote (SSH)
------------



I can work on the remote files in Emacs via ssh or tramp, both are
build-in packages.

.. code:: common-lisp

    (require 'tramp)
    (require 'ssh)

I'd like catch the password so that I don't need to type it every time
to open a file. 

.. code:: common-lisp

    (setq password-cache-expiry nil)

I mainly run R on a remote machine. Sometimes I want to copy the
charts I created to local to include them in my report. This workfow
is suspended because it fails when the file size is large. 

.. code:: common-lisp

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

Git Sync
--------



I use git and Github a lot, and usually in ``shell-mode``, but I just
can't remember all the commands. Magit provides an interface to Git,
and it is really pleasant to use. So I don't need to remmeber all the
commands, also it comes with excellent `manual <http://magit.github.io/master/magit.html>`_ and `cheatsheet <http://daemianmack.com/magit-cheatsheet.html>`_.

.. code:: common-lisp

    (require 'magit)
    (setq magit-last-seen-setup-instructions "1.4.0")
    (setq magit-auto-revert-mode nil)
    (global-set-key (kbd "<f9> g") 'magit-status)
    (global-auto-revert-mode t)

Occasionally my office machine goes down because I run R with big
data, and it consumes all the memory. If that happens, I
potentially lose the newsiest version of scripts, which is bit
annoy. The following snippets will save all buffers in every hours.

.. code:: common-lisp

    (defun yt/save-all-buffers ()
      "save all files-visiting buffers without user confirmation"
      (interactive)
      (save-some-buffers t nil)
      (message "save all buffers... done"))
    (run-at-time "05:59" 3600 'yt/save-all-buffers)

Sometimes I have to leave at the last minutes, then what I do is call
a functions that commits and upload to the repo so that I can
continue work at home.

The ``yt/git-up`` function will do

1. pull from the remote repo, and make sure the local repo is always
   up-to-date.

2. add everything and commit with a timesamp.

3. push local changes to the remote repo.

Here is the snippets.

.. code:: common-lisp

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
        git push origin master 
    done

    echo Finished Sync: $(date)
    "))
        (async-shell-command git-sh-scripts))
      (message "all git sync... done"))

    (defun yt/git-up ()
      (interactive)
      (yt/save-all-buffers)
      (yt/git-backup))


Few times I did some important work over the weenend, but once I
arrived office I realised I forgot uploading, These situations are
quick frustrating. The following snippets will start to uploads once
every three hours on my MacbookPro, but I don't use it anymore, since
I can get most of my work done in the office. 

Note this workflow is suspended for it's unsafe. 

.. code:: common-lisp

    ;; (cond ((eq system-type 'darwin)
    ;;        (run-at-time "05:59" 10800 'yt/git-up)))

Testing Buffers
---------------



*scratch* buffer is usually used for testing Emacs lisp functions. I
also need temporary buffers for testing R code and org-mode. In the
following settings, I can use ``F9-f`` to select temporal buffers.


.. code:: common-lisp

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

Frequently visiting buffers
---------------------------



.. code:: common-lisp

    (defun yt/org-find-file (filepath)
      (interactive)
      (find-file (expand-file-name filepath "~/git/org") nil))

    (defhydra hydra/open-common-files (:color blue)
      "Open file:
          "
      ("R" (find-file "~/git/career/Profession/R.org") "R.org")
      ("p" (find-file "~/git/career/Profession/Python.org") "Python.org")
      ("E" (find-file "~/git/career/Profession/Emacs.org") "Emacs.org") 
      ("l" (find-file "~/git/org/life/life.org") "life.org")
      ("i" (find-file "~/git/.emacs.d/init.org" t) "init.org")
      ("e" (find-file "~/.emacs" t) ".emacs")
      ("d" (yt/org-find-file "dournal/diary.org") "diary.org")
      ("r" (yt/org-find-file "life/review.org") "review.org")
      ("f" (yt/org-find-file "finance/ledger_transaction_2019.org") "ledger.org")
      )
    (global-set-key (kbd "<f6>") 'hydra/open-common-files/body)
