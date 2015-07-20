Archive
=======



open stree map
--------------



.. code-block:: scheme


    (defun omap_bbox (minlon minlat maxlon maxlat)
      "generate to hyperlink and also view an area in openstreemap.  
    the area is defined by the bounding box. "
    ;;   (interactive)
      (let ((address (concat  "http://www.openstreetmap.org/?"
               "minlon=" (number-to-string minlon) "&"
               "minlat=" (number-to-string minlat) "&"
               "maxlon=" (number-to-string maxlon) "&"
               "maxlat=" (number-to-string maxlat) "&")))
        (browse-url address)
        (kill-new address)))


    (defun omap_point (lon lat)
    ;;  (interactive)
      (let ((address (concat  "http://www.openstreetmap.org/?"
                              "mlat=" (number-to-string lat) "&"
                              "mlon=" (number-to-string lon))))
        (browse-url address)
        (kill-new address)))

    (defun omap_site (site)
    ;;  (interactive)
      (let ((address (concat "http://nominatim.openstreetmap.org/search.php?q=" site)))
        (browse-url address)
        (kill-new address)))

    (defun omap (arg)
    ;;  (interactive)
      (cond ((= 1 (length arg))
             (omap_site (nth 0 arg)))
            ((= 2 (length arg))
             (omap_point (nth 0 arg) (nth 1 arg)))
            ((= 4 (length arg))
             (omap_bbox (nth 0 arg)
                         (nth 1 arg)
                         (nth 2 arg)
                         (nth 3 arg)))
            (t "nope")))

    ;; (omap '(-0.489 51.28 0.236 51.686)) ;; london area
    ;; http://www.openstreetmap.org/?minlon=-0.489&minlat=51.28&maxlon=0.236&maxlat=51.686&
    ;; (omap '(-76.3412 38.6710)) ;; New york 
    ;; http://www.openstreetmap.org/?mlat=38.671&mlon=-76.3412
    ;; (omap '("UK"))  ;; UK, obvs 
    ;; http://nominatim.openstreetmap.org/search.php?q=UK

Office Setting
--------------

[2015-03-18 Wed 18:32]
In the office, I have differnet settings for 

1. org-agenda files

2. org-capture-templates,

3. org-html-export

In my office Ubuntu machine, set a variable WhereAmI to Office, then
the setting will be done automatically. wrap all the settings in the
``my/all-office-settings`` function for I can call it explicity when I
work on my Mac.

.. code-block:: scheme

    ;; (setq WhereAmI "Office") ;; set this variable in ~/.emacs
    (defun my/office-org-agenda-file ()
      (setq org-agenda-files '("~/git/jbarm" )))

    (defun my/office-org-capture-temp ()
      (setq org-capture-templates
            (quote (("t" "todo" entry (file "~/git/jbarm/re_org_refile.org")
                     "* TODO %?\nSCHEDULED:%t \n %U\n" :clock-in t :clock-resume t) ;; TODO: %? %U %a, what does these means??? %: %c 
                    ("r" "respond" entry (file "~/git/jbarm/re_org_refile.org")
                     "* To %? about :RESPONSE:  \nSCHEDULED: %t\n%U\n" :clock-in t :clock-resume t)
                    ("n" "note" entry (file "~/git/jbarm/re_org_refile.org")
                     "* %? :NOTE:\n%U\n" :clock-in t :clock-resume t)
                    ("j" "Journal" entry (file+datetree "~/git/jbarm/re_org_diary.org")
                     "* %?\n%U\n" :clock-in t :clock-resume t)
                    ("l" "Ledger Journal" plain (file "~/git/org/Finance/ledger")
                     "%(org-read-date) * %^{Payee}\n\t%^{Account}\t£ %^{Amount}\n\tAssets:Checking" :immediate-finish :clock-in t :clock-resume t) 
                    ("h" "Habit" entry (file "~/git/jbarm/re_org_habits.org")
                     "* NEXT %?\n%U\nSCHEDULED: %(format-time-string \"<%Y-%m-%d .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
                    ("v" "Vocabulary" entry (file "~/git/jbarm/re_org_refile.org")
                     "* %? :VOCA:\n%U" :clock-in t :clock-resume t)
                    ))))

    (defun my/all-office-settings ()
      (interactive)
      (my/office-org-agenda-file)
      (my/office-org-capture-temp)
      (setq org-html-head-extra "<link rel=\"stylesheet\" href=\"/home/yitang/git/.emacs.d/personal/css/office.css\" type=\"text/css\" />"
            org-html-head "<link rel=\"stylesheet\" href=\"\\SKI-CQV5LZ1\static\yi_report.css\" type=\"text/css\" />"
            ;; voca-builder/voca-file "~/git/org/vocabulary-office.org"
            )

      )

    (when (equal "Office" WhereAmI)
      (my/all-office-settings))

Git-Report
----------

.. code-block:: scheme

    ;; (defun yt/git-repo-info ()
    ;;   (interactive)
    ;;   (let* ((sh-num-unpushed-commits "git status | grep \"\'origin/master\'\" | grep -Po \"[0-9]\"")
    ;;       (sh-num-uncommited-files "git status --porcelain 2>/dev/null| egrep \"^(M| M)\" | wc -l")
    ;;       (unpush (shell-command-to-string sh-num-unpushed-commits))
    ;;       (uncommit (shell-command-to-string sh-num-uncommited-files)))
    ;;     (concat "unpushed commits: " unpush "\n" "uncommited files: " uncommit)))

    (defun yt/git-check-repo-update (path)
      (interactive)
      (let* ((git-cmd "git status | grep clean")
             (sh-cmd (concat "cd " path "; " git-cmd))
             (sh-result (shell-command-to-string sh-cmd))
             (is-git-repo (not (string= "fatal: Not a git repository (or any of the parent directories): .git\n"
                                      sh-result)))
             (repo-clean-p (string= "nothing to commit, working directory clean\n"
                                    sh-result)))
        (cond ((not is-git-repo)
               (concat path " is not a git repo"))
              (repo-clean-p
               (concat path " is clean"))
              (t
               (concat path " is dirty")))))

    (defun yt/git-status-report (repo-list)
      (interactive)
      (let ((repo-status (mapconcat 'yt/git-check-repo-update repo-list "\n")))
        repo-status))


    (defun yt/git-status-report-org-mode (repo-list)
      (interactive)
      (let ((repo-status (mapcar 'yt/git-check-repo-update-return-list repo-list)))    
        repo-status))

    (defun yt/git-check-repo-update-return-list (path)
      (let* ((status (yt/git-check-repo-update path))
             (status (car (cdr (split-string status " is "))))
             (magit-link 
              (if (string= "not a git repo" status)
                  " "
                (concat "[[magit:" path "]]"))))
        (list path
              status
              magit-link)))

    (defun yt/list-to-org-table (a-list)
      (let ((a-list-string (mapconcat 'identity a-list " | "))) ;; use | to separate each item
        (concat "| " a-list-string " |" "\n")))

    (defun yt/git-check-repo-update-org-table (repo-list)
      (let* ((status-list (mapcar 'yt/git-check-repo-update-return-list repo-list))
            (status-table (mapcar 'yt/list-to-org-table status-list))
            (status-table-string (mapconcat 'identity status-table " "))
            (table-header "| repo | status | magit |\n"))
        (concat table-header " " status-table-string)))

    (require 'f)
    (defun yt/git-all-git-dir (git-base-dir)
      (f-directories "~/git"
                     '(lambda (arg)
                        (let* ((f (directory-files arg))
                               (f-string (format "%s" f)))
                          (if (string-match "\\.git" f-string)
                              t
                            nil)))
                     t))

    (defun yt/git-generate-report ()
      (interactive)
      (let  ((git-report (yt/git-check-repo-update-org-table repo-list)))
        (with-temp-buffer
          (insert (format "%s" git-report))
          (write-region (point-min) (point-max) git-report-temp-file)
          )
        (find-file git-report-temp-file)
        (goto-char (+ 1 (point-min)))
        (org-mode)
        (org-ctrl-c-ret)
        (previous-line 2)
        (search-forward "status")
        (next-line 2)
        ;; (setq current-prefix-arg '(4)) ; C-u
        (org-table-sort-lines t ?a)
        )
      )


    (setq git-report-temp-file "~/git-report-buffer")
    ;; (setq repo-list (yt/git-all-git-dir "~/git"))
    (setq re-post '("/Users/yitang/git/voca-builder" "/Users/yitang/git/qs" "/Users/yitang/git/portfolio-statistician" "/Users/yitang/git/portfilio-yitang" "/Users/yitang/git/org" "/Users/yitang/git/myblog" "/Users/yitang/git/jbarm" "/Users/yitang/git/Free-Classic-Books" "/Users/yitang/git/.emacs.d" "/Users/yitang/git/R/yiR" "/Users/yitang/git/R/activitydashboard2" "/Users/yitang/git/R/RExercise" "/Users/yitang/git/R/Archive/yiR" "/Users/yitang/git/R/Archive/orgR" "/Users/yitang/git/R/Archive/learn_shiny")("/Users/yitang/git/voca-builder" "/Users/yitang/git/qs" "/Users/yitang/git/portfolio-statistician" "/Users/yitang/git/portfilio-yitang" "/Users/yitang/git/org" "/Users/yitang/git/myblog" "/Users/yitang/git/jbarm" "/Users/yitang/git/Free-Classic-Books" "/Users/yitang/git/.emacs.d" "/Users/yitang/git/R/yiR" "/Users/yitang/git/R/activitydashboard2" "/Users/yitang/git/R/RExercise" "/Users/yitang/git/R/Archive/yiR" "/Users/yitang/git/R/Archive/orgR" "/Users/yitang/git/R/Archive/learn_shiny"))
    ;; (yt/git-generate-report)

    ;; | repo                                     | status | magit                                          |
    ;; |------------------------------------------+--------+------------------------------------------------|
    ;; | /Users/yitang/git/voca-builder           | dirty  | [[magit:/Users/yitang/git/voca-builder]]           |
    ;; | /Users/yitang/git/qs                     | dirty  | [[magit:/Users/yitang/git/qs]]                     |
    ;; | /Users/yitang/git/portfolio-statistician | clean  | [[magit:/Users/yitang/git/portfolio-statistician]] |
    ;; | /Users/yitang/git/portfilio-yitang       | clean  | [[magit:/Users/yitang/git/portfilio-yitang]]       |
    ;; | /Users/yitang/git/org                    | dirty  | [[magit:/Users/yitang/git/org]]                    |
    ;; | /Users/yitang/git/myblog                 | dirty  | [[magit:/Users/yitang/git/myblog]]                 |
    ;; | /Users/yitang/git/jbarm                  | clean  | [[magit:/Users/yitang/git/jbarm]]                  |
    ;; | /Users/yitang/git/Free-Classic-Books     | dirty  | [[magit:/Users/yitang/git/Free-Classic-Books]]     |
    ;; | /Users/yitang/git/.emacs.d               | dirty  | [[magit:/Users/yitang/git/.emacs.d]]               |
    ;; | /Users/yitang/git/R/yiR                  | dirty  | [[magit:/Users/yitang/git/R/yiR]]                  |
    ;; | /Users/yitang/git/R/activitydashboard2   | clean  | [[magit:/Users/yitang/git/R/activitydashboard2]]   |
    ;; | /Users/yitang/git/R/RExercise            | clean  | [[magit:/Users/yitang/git/R/RExercise]]            |
    ;; | /Users/yitang/git/R/Archive/yiR          | dirty  | [[magit:/Users/yitang/git/R/Archive/yiR]]          |
    ;; | /Users/yitang/git/R/Archive/orgR         | dirty  | [[magit:/Users/yitang/git/R/Archive/orgR]]         |
    ;; | /Users/yitang/git/R/Archive/learn_shiny  | clean  | [[magit:/Users/yitang/git/R/Archive/learn_shiny]]  |

C++
---

.. code-block:: scheme

    ;; (global-set-key [(f9)] 'compile)
    (setq compilation-window-height 2)
    (setq compilation-finish-function
          (lambda (buf str)

            (if (string-match "exited abnormally" str)

                ;;there were errors
                (message "compilation errors, press C-x ` to visit")

              ;;no errors, make the compilation window go away in 0.5 seconds
              (run-at-time 0.5 nil 'delete-windows-on buf)
              (message "NO COMPILATION ERRORS!"))))



    (defun indent-buffer-ask()
       (when (y-or-n-p "Indent buffer before saving? ")
         (indent-region (point-min) (point-max))))

    (defun indent-buffer-no-ask()
      (indent-region (point-min) (point-max)))

    (setq c++-mode-hook
          '(lambda ()
             (c-set-style "cc-mode")
             (define-key c++-mode-map "\C-c\C-c" 'compile)
             (define-key c++-mode-map "\C-c\C-e" 'next-error)
    ;        (add-hook 'before-save-hook 'indent-buffer-ask nil t)                  
             (add-hook 'before-save-hook 'indent-buffer-no-ask nil t)  ;; indent c++ files after save.
    ))




    (require 'flymake-google-cpplint)
    (add-hook 'c++-mode-hook 'flymake-google-cpplint-load)
    ;; (custom-set-variables
    ;; '(flymake-google-cpplint-command "/Library/Python/2.7/site-packages/cpplint/cpplint.py"))
    ;"/usr/local/bin/cpplint"))

    ; start google-c-style with emacs
    (require 'google-c-style)
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    (add-hook 'c++-mode-common-hook 'google-make-newline-indent)

tryout.el
---------



.. code-block:: scheme

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

      ;; switch to *Org Agenda(a)* buffer

      (sunshine-forecast) ;; switch to *Sunshine* buffer
      (yt/git-generate-report) ;; switch to git-report-
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

Update Emacs library
--------------------

.. code-block:: sh
    :name: UpdateEmacsPackages

    cask install --path ~/git/.emacs.d
    cask upgrade-cask --path ~/git/.emacs.d
    git add -A 
    git commit -m "cask update $(date)"
    git push 

::


    #+call: UpdateEmacsPackages()

    #+results:
    : [master 3f5c750] cask update Sat Nov  8 18:06:08 GMT 2014
    :  2 files changed, 7 insertions(+), 3 deletions(-)
    :  delete mode 120000 .#yi.babel.org

image process
-------------

.. code-block:: sh
    :name: Extract_PDF_Pages

    ## http://www.linuxjournal.com/content/tech-tip-extract-pages-pdf
    function pdfpextr() 
    {
        # this function uses 3 arguments:
        #     $1 is the first page of the range to extract
        #     $2 is the last page of the range to extract
        #     $3 is the input file
        #     output file will be named "inputfile_pXX-pYY.pdf"
        # TODO: add format=png option 
        gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER \
           -dFirstPage=${1} \
           -dLastPage=${2} \
           -sOutputFile=${3%.pdf}_p${1}-p${2}.pdf \
           ${3}
    }
    pdfpextr $pageStart $pageEnd $pdfFile 

::

    #+name: Extract_PDF_Pages[:dir ~/git/org/img](1, 10, "tmp.pdf")

Python
------



.. code-block:: scheme

    (require 'eval-in-repl)
    ;; Python support
    ;; (require 'python) ; if not done elsewhere
    (require 'eval-in-repl-python)
    (define-key python-mode-map (kbd "<C-return>") 'eir-eval-in-python)
