(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "SOMEDAY" "ORG(o@/!)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "RUNNING(r)" "|" "CANCELLED(c@/!)" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
	      ("NEXT" :foreground "red" :weight bold)
	      ("ORG" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
	      ("RUNNING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; (setq org-log-done (quote time))
;; (setq org-log-into-drawer t)
;; (setq org-log-state-notes-insert-after-drawers nil)

(defun my/modify-org-done-face ()
  (setq org-fontify-done-headline t)
  (set-face-attribute 'org-done nil :strike-through t)
  (set-face-attribute 'org-headline-done nil
                      :strike-through t
                      :foreground "light gray"))
(add-hook 'org-mode-hook 'my/modify-org-done-face)
;; (setq org-fontify-done-headline t)
;; (set-face-attribute 'org-done nil :strike-through t)
;; (set-face-attribute 'org-headline-done nil :strike-through t)

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/git/org/life//refile.org")
               "* TODO %?\n\n" :clock-in t :clock-resume t) ;; TODO: %? %U %a, what does these means??? %: %c
              ("o" "org" entry (file "~/git/org/life//refile.org")
               "* TODO %?General Org\n%U\n" :clock-in t :clock-resume t) ;; TODO: %? %U %a, what does these means??? %: %c 
              ;; ("r" "respond" entry (file "~/git/org/life//refile.org")
              ;;  "* To %? about :RESPONSE:  \nSCHEDULED: %t\n%U\n%a" :clock-in t :clock-resume t)
              ("r" "read" entry (file "~/git/org/life//refile.org")
               "* TODO %? :READ:\n%U\n" :clock-in t :clock-resume t)
              ("n" "note" entry (file "~/git/org/life//refile.org")
               "* %? :NOTE:\n%U\n" :clock-in t :clock-resume t)
              
              ("h" "Habit" entry (file "~/git/org/habits.org")
               "* NEXT %?\nSCHEDULED: %(format-time-string \"<%Y-%m-%d .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n%U\n")
              ("v" "Vocabulary" entry (file "~/git/org/vocabulary.org")
               "* %? :VOCA:\n%U" :clock-in t :clock-resume t)

              ("j" "Journalsing")
              ("jd" "diary" entry (file+datetree "~/git/org/diary/2020.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("jk" "Kaggle Competition" entry (file+datetree "~/git/org/diary/Kaggle.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("js" "Statistician" entry (file+datetree "~/git/org/diary/Statistics.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("jo" "Office" entry (file+datetree "~/git/org/diary/2020.org")
               "* %?:office:\n%U\n" :clock-in t :clock-resume t)
	      ("jf" "Finance" entry (file+datetree "~/git/org/diary/2020.org")
               "* %?:finance:\n%U\t\n" :clock-in t :clock-resume t)
	      ("jc" "Career" entry (file+datetree "~/git/org/diary/Career.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              )))

;; ledger entries

(push '("l" "Ledger Journal" plain (file "~/git/data_finance/ledger/refile.ledger")
	   "%(org-read-date) * %(yt/helm-ledger-payee)
    %(yt/helm-ledger-account)    £ %(yt/helm-ledger-amount)
    %(yt/helm-ledger-account)")
	 org-capture-templates
	 )

(setq org-refile-use-cache t)

(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))
(setq org-outline-path-complete-in-steps nil)

(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'bh/verify-refile-target)

(setq org-refile-use-outline-path t)

(setq org-completion-handler 'helm)

(setq org-clock-persist t)

(org-clock-persistence-insinuate)
(setq org-clock-in-resume t)

;; Do not prompt to resume an active clock
;; (setq org-clock-persist-query-resume nil)

;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(set-face-attribute 'org-mode-line-clock nil
		    :weight 'bold :box '(:line-width 1 :color "#FFBB00") :foreground "white" :background "#FF4040")

;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; http://stackoverflow.com/questions/6156286/emacs-lisp-call-function-with-prefix-argument-programmatically
(defun yt/org-clock-in-select ()
  (interactive)
  (setq current-prefix-arg '(4)) ;; C-u, 
  (call-interactively 'org-clock-in))
(global-set-key (kbd "S-<f11>") 'yt/org-clock-in-select)
(global-set-key (kbd "<f11>") 'org-clock-jump-to-current-clock)

;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
(defun bh/clock-in-to-next (kw) 
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (if (member (org-get-todo-state) (list "TODO"))
        "NEXT")))

(defun yt/punch-in ()
  (interactive)
    (org-with-point-at (org-id-find "1b586ec1-fa8a-4bd1-a44c-faf3aa2adf51" 'marker)
    (org-clock-in)
     ))
(global-set-key (kbd "<f9> I") 'yt/punch-in)

;; https://github.com/abo-abo/hydra/wiki/Org-clock
(defhydra hydra-org-clock (:color blue :hint nil)
  "
Clock       In/out^     ^Edit^   ^Summary     (_?_)
---------------------------------------------------
            _i_n         _e_dit   _g_oto entry
_h_istory   _c_ontinue   _q_uit   _d_isplay
_j_ump      _o_ut        ^ ^      _r_eport
      "
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("c" org-clock-in-last)
  ("e" org-clock-modify-effort-estimate)
  ("q" org-clock-cancel)
  ("g" org-clock-goto)
  ("d" org-clock-display)
  ("r" org-clock-report)
  ("j" org-clock-jump-to-current-clock)
  ("h" yt/org-clock-in-select)
  ("?" (org-info "Clocking commands")))

(global-set-key (kbd "<f11>") 'hydra-org-clock/body)

(add-hook 'org-clock-out-hook 'org-clock-remove-empty-clock-drawer 'append)

(setq org-tag-alist (quote ((:startgroup)
                            ("@office" . ?O)
                            ("@home" . ?H)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("NOTE" . ?n)
                            ("READ" .?r)
                            ("CANCELLED" . ?c)
                            )))
;; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))
(setq org-agenda-tags-todo-honor-ignore-options t)

;;;; * Custom Key Bindings

(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))
;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
;; global Effort estimate values
;; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))
(setq org-agenda-log-mode-items (quote (closed clock)))

(setq org-use-speed-commands t)
(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))
(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)

(defun yt/insert-ts-as-file ()
    (interactive)
  (insert (format-time-string "%Y-%m-%d--%H-%M-%S"))
  )

(global-set-key (kbd "<f9> T") 'yt/insert-ts-as-file)

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (org-return)
    (org-cycle)
    (bh/insert-inactive-timestamp)))
(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)
(setq org-file-apps (quote ((auto-mode . emacs)
                            ("\\.png\\'" . emacs)
                            ("\\.svg\\'" . system)
                            ("\\.mm\\'" . system)
                            ("\\.x?html?\\'" . system)
                            ("\\.pdf\\'" . system))))
                                        ; Overwrite the current window with the agenda
(setq org-agenda-window-setup 'current-window)

(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; (setq org-agenda-span 'day)
;; (require 'org-habit)

(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

;; recursively find .org files in provided directory
  ;; modified from an Emacs Lisp Intro example
  (defun sa-find-org-file-recursively (&optional directory filext)
    "Return .org and .org_archive files recursively from DIRECTORY.
  If FILEXT is provided, return files with extension FILEXT instead."
    (interactive "DDirectory: ")
    (let* (org-file-list
           (case-fold-search t)         ; filesystems are case sensitive
           (file-name-regex "^[^.#].*") ; exclude dot, autosave, and backup files
           (filext (or filext "org$\\\|org_archive"))
           (fileregex (format "%s\\.\\(%s$\\)" file-name-regex filext))
           (cur-dir-list (directory-files directory t file-name-regex)))
      ;; loop over directory listing
      (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
        (cond
         ((file-regular-p file-or-dir) ; regular files
          (if (string-match fileregex file-or-dir) ; org files
              (add-to-list 'org-file-list file-or-dir)))
         ((file-directory-p file-or-dir)
          (dolist (org-file (sa-find-org-file-recursively file-or-dir filext)
                            org-file-list) ; add files found to result
            (add-to-list 'org-file-list org-file)))))))

  (setq org-agenda-files (append (sa-find-org-file-recursively "~/git/org")
                                 (sa-find-org-file-recursively "~/git/career")))

  (setq org-list-allow-alphabetical t)

  (defun yt/org-agenda-files--choose (candidate)
    (mapc 'identity (helm-marked-candidates)))
    

(defun yt/org-agenda-files-set-helm ()  ;; FIXME: path broken.
    (helm :sources '(((name . "Add directories to org-agenda-files variable")
                      (candidates . ("~/git/org/" "~/git/career" "~/git/org/finance"))
                      (action . yt/org-agenda-files--choose)))))

(defun yt/org-agenda-files-set ()
  (interactive)
  (setq org-agenda-files (list))
  (dolist (dir (yt/org-agenda-files-set-helm))
    (mapcar (lambda (arg)
	      (add-to-list 'org-agenda-files arg))
	    (sa-find-org-file-recursively dir))    
	    ))   


	    ;; (defun yt/org-agenda-files-set ()
  ;; (interactive)
    ;; (setq org-agenda-files (yt/org-agenda-files-set-helm)))
  ;; (yt/org-agenda-files-set)

  (global-set-key (kbd "<C-f12>") 'org-agenda)

  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)

  ;; Compact the block agenda view
  (setq org-agenda-compact-blocks nil)



  ;; Custom agenda command definitions
  (defvar bh/hide-scheduled-and-waiting-next-tasks t)
  (setq org-agenda-custom-commands
        (quote (("N" "Notes" tags "NOTE"
                 ((org-agenda-overriding-header "Notes")
                  (org-tags-match-list-sublevels t)))
                ("h" "Habits" tags-todo "STYLE=\"habit\""
                 ((org-agenda-overriding-header "Habits")
                  (org-agenda-sorting-strategy
                   '(todo-state-down effort-up category-keep))))
                ("d" "deadline" agenda ""
                 (
                  (org-agenda-entry-types '(:deadline))
                  (org-agenda-start-day "2016-01-01")
                  (org-agenda-span 'year)
                  (org-agenda-include-diary nil)
                  (org-agenda-show-all-dates nil)))
                ("s" "scheduled" agenda ""
                 (
                  (org-agenda-entry-types '(:scheduled))
                  (org-agenda-start-day "2016-01-01")
                  (org-agenda-span 'year)
                  (org-agenda-include-diary nil)
                  (org-agenda-show-all-dates nil)))
                (" " "Agenda"
                 ((agenda "" nil)
                  (tags-todo "-CANCELLED+WAITING|HOLD/!"
                             ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks (Ask them)"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-non-tasks)
                              (org-tags-match-list-sublevels nil)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                  (tags "RESPONSE"
                        ((org-agenda-overriding-header "Response (Make other's life easier)")
                         (org-tags-match-list-sublevels nil)))
                  (tags-todo "-CANCELLED/!NEXT"
                             ((org-agenda-overriding-header (concat "Project Next Tasks (Running out of things to do? pick one)"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                              (org-tags-match-list-sublevels t)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(todo-state-down effort-up category-keep))))
                  (tags-todo "-CANCELLED/!"
                             ((org-agenda-overriding-header "Stuck Projects (Make the project flows, assign Next)")
                              (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-HOLD-CANCELLED/!"
                             ((org-agenda-overriding-header "Projects (on-going)")
                              (org-agenda-skip-function 'bh/skip-non-projects)
                              (org-tags-match-list-sublevels 'indented)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                             ((org-agenda-overriding-header (concat "Project Subtasks (Will do in the furture)"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-non-project-tasks)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                             ((org-agenda-overriding-header (concat "Standalone Tasks (One-off/Small Tasks to pick)"
                                                                    (if bh/hide-scheduled-and-waiting-next-tasks
                                                                        ""
                                                                      " (including WAITING and SCHEDULED tasks)")))
                              (org-agenda-skip-function 'bh/skip-project-tasks)
                              (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags "-REFILE/"
                        ((org-agenda-overriding-header "Tasks to Archive (Two month old)")
                         (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                         (org-tags-match-list-sublevels nil)))
                  (tags "REFILE"
                        ((org-agenda-overriding-header "Tasks to Refile")
                         (org-tags-match-list-sublevels nil)))
                  nil)))))

  ;; Limit restriction lock highlighting to the headline only
  (setq org-agenda-restriction-lock-highlight-subtree nil)

  ;; Always hilight the current agenda line
  (add-hook 'org-agenda-mode-hook
            '(lambda () (hl-line-mode 1))
            'append)

  ;;;; * agenda ignore items 
  ;; Keep tasks with dates on the global todo lists
  (setq org-agenda-todo-ignore-with-date nil)

  ;; Keep tasks with deadlines on the global todo lists
  (setq org-agenda-todo-ignore-deadlines nil)

  ;; Keep tasks with scheduled dates on the global todo lists
  (setq org-agenda-todo-ignore-scheduled nil)

  ;; Keep tasks with timestamps on the global todo lists
  (setq org-agenda-todo-ignore-timestamp nil)

  ;; Remove completed deadline tasks from the agenda view
  (setq org-agenda-skip-deadline-if-done t)

  ;; Remove completed scheduled tasks from the agenda view
  (setq org-agenda-skip-scheduled-if-done t)

  ;; Remove completed items from search results
  (setq org-agenda-skip-timestamp-if-done t)

  (setq org-agenda-include-diary nil)
  (setq org-agenda-diary-file "~/git/org/diary.org")


  (setq org-agenda-insert-diary-extract-time t)

  ;; Include agenda archive files when searching for things
  (setq org-agenda-text-search-extra-files (quote (agenda-archives)))

  ;; Show all future entries for repeating tasks
  (setq org-agenda-repeating-timestamp-show-all t)

  ;; Show all agenda dates - even if they are empty
  (setq org-agenda-show-all-dates t)

  ;; Sorting order for tasks on the agenda
  (setq org-agenda-sorting-strategy
        (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
                (todo category-up effort-up)
                (tags category-up effort-up)
                (search category-up))))



  ;; (setq org-agenda-tags-column -102)
  ;; Use sticky agenda's so they persist
  ;; (setq org-agenda-sticky t)

(setq org-agenda-time-grid (quote ((daily today require-timed)
 (800 1000 1200 1400 1600 1800 2000)
 "......" "----------------")))

;; (quote ((daily today remove-match)
;;                                    #("----------------" 0 16 (org-heading t))
;;                                    (0700 0800 0900 1000 1100 1200 1200 1300 1400 1500 1600 1700))))

(setq org-agenda-span 'week)
(setq org-agenda-start-on-weekday 1)

(setq org-deadline-warning-days 30)

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"                 ;; highligh clock entries longer than 5 hours.
			    :min-duration "00:05"  ;; highlight clock smaller than 5 mins 
			    :max-gap "00:05"       ;; highlight clock gap loger than 5 mins.
			    :gap-ok-around ("4:00")))) 
(setq org-read-date-prefer-future 'time)

;; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (setq appt-display-format 'window) ;; YT: show notification in separate window
  (org-agenda-to-appt))

                                        ; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

                                        ; This is at the end of my .emacs - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)

(setq org-reverse-note-order t) ;; refiled headline will be the first under the taget

(setq org-archive-location "::* Archived Tasks") ;;in-file archive 

(require 'org-habit)
(setq org-habit-show-all-today t)
(setq org-habit-show-habits nil)
(setq org-habit-graph-column 80)
;; add the following 
(setq org-time-stamp-custom-formats '("<%A %d %B %Y>" . "<%A %d %B %Y %H:%M>"))
(setq org-agenda-tags-column 120)

(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM %10Mindfullness")

(setq org-startup-folded t
      org-hide-block-startup t
      org-startup-indented nil)

;; remove C-TAB
(define-key org-mode-map (kbd "C-S-<right>") 'mc/mark-next-like-this)
(define-key org-mode-map (kbd "C-S-<left>") 'mc/mark-previous-like-this)
(org-defkey org-mode-map (kbd "C-c [") nil)
(org-defkey org-mode-map (kbd "C-c ]") nil)
(org-defkey org-mode-map (kbd "C-TAB") nil)
(org-defkey org-mode-map (kbd "<f8>") nil)
;; use helm iwth org
;; (setq org-completion-handler 'helm)

(require 'org-bullets)
(setq org-bullets-bullet-list '(;;; Large
                                "◉" "○" ;"◎" "◌"
                                "◈" "◇"
                                "◼" "◻"
                                "▲" "△"
                                "❀" "✿" ;"✸"
                                "✚" "✜" 
                                "●" "☯" ;"☢" 
                                "♠" "♣" "♦" "♥"))
(setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))
;; org ellipsis options, other than the default Go to Node...
;; not supported in common font, but supported in Symbola (my fall-back font) ⬎, ⤷, ⤵
(setq org-ellipsis "⚡⚡⚡");; ⤵ ≫
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-download)
(setq-default org-download-image-dir "~/Downloads/img")
(setq-default org-download-heading-lvl nil)
;; (if (eq system-type 'darwin)
;;     "org-download: default download method"
;;     (setq org-download-screenshot-method "gnome-screens
;; hot -w --delay=1 -f %s"))
(setq org-download-image-wdith 400)

;; (setq org-download-screenshot-method "gnome-screenshot -a -f %s")

(sp-local-pair 'org-mode "=" "=") ; select region, hit = then region -> =region= in org-mode
(sp-local-pair 'org-mode "*" "*") ; select region, hit * then region -> *region* in org-mode
(sp-local-pair 'org-mode "/" "/") ; select region, hit / then region -> /region/ in org-mode
(sp-local-pair 'org-mode "_" "_") ; select region, hit _ then region -> _region_ in org-mode
(sp-local-pair 'org-mode "+" "+") ; select region, hit + then region -> +region+ in org-mode
(sp-local-pair 'org-mode "$" "$") ; select region, hit $ then region -> $region$ in org-mode

(global-set-key (kbd "C-c l") 'org-store-link)

;;;; * org-babel 
(setq org-src-window-setup 'current-window)
(setq org-src-fontify-natively nil)
(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)
(setq org-catch-invisible-edits 'error)
(setq org-export-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))


(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

(setq org-babel-results-keyword "results")
(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t) ;; TODO: simplifiy this list 
         (R . t)
         (shell . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (dot . t)
         (python .t)
	 (ipython .t)
	 ;; (bibtex .t)
         (octave . t)
         (latex . t)
	 ;; (jupyter . t)
	 (shell . t)
	 (ledger . t)
         (sql . t))))

(setq org-babel-default-header-args (append org-babel-default-header-args '((:colnames . "yes"))))

;; (add-to-list 'org-babel-default-header-args:R
;;              ;; '(:session . "*R-main*")
;;              '((:width . 640) (:height . 640)))


(setq org-confirm-babel-evaluate nil)

(setq org-plantuml-jar-path "~/git/.emacs.d/java/plantuml.jar") ;; TODO: change the location.. 
;; Use fundamental mode when editing plantuml blocks with C-c '
(setq plantuml-jar-path "~/git/.emacs.d/java/plantuml.jar")
(add-to-list 'org-src-lang-modes (quote ("plantuml" . plantuml)))

(require 'ox-html)
(require 'ox-latex)
(require 'ox-ascii)
(require 'ox-md)
(require 'htmlize)
(require 'ox-gfm)

(setq org-export-with-toc nil
      org-export-with-todo-keywords t
      org-export-with-sub-superscripts nil
      org-export-with-planning nil
      org-export-with-author t
      org-export-with-timestamps nil
      org-export-babel-evaluate t
      org-export-with-drawers nil)

(setq org-image-actual-width '(400))

(setq org-table-export-default-format "orgtbl-to-csv")

(setq org-emphasis-alist (quote (("*" bold "<b>" "</b>")
                                 ("/" italic "<i>" "</i>")
                                 ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                                 ("=" org-code "<code>" "</code>" verbatim)
                                 ("~" org-verbatim "<code>" "</code>" verbatim))))

;; http://emacs-fu.blogspot.co.uk/2011/04/nice-looking-pdfs-with-org-mode-and.html
;; 'djcb-org-article' for export org documents to the LaTex 'article', using
;; XeTeX and some fancy fonts; requires XeTeX (see org-latex-to-pdf-process)
(add-to-list 'org-latex-classes
             '("yt/org-article"
               "
\\documentclass[11pt,a4paper]{article}
\\usepackage{graphicx}    %% demo mode is a must when .img does not exists.
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\usepackage{hyperref}
\\hypersetup{
     colorlinks   = true,
     citecolor    = gray
}
\\usepackage{amsmath}
\\usepackage{amstext}
\\usepackage{amssymb} %% checkbox
\\usepackage{commath}
\\DeclareMathOperator*{\\argmin}{\\arg\\!\\min} %% use $\argmin_{b}$
\\DeclareMathOperator*{\\argmax}{\\arg\\!\\max} 
%% \\DeclareMathOperator{\\E}{\\mathbb{E}}
\\newcommand{\\E}[1]{{\\mathbb E}\\left[ #1 \\right]}
\\newcommand{\\Var}{\\mathrm{Var}}
\\DeclareMathOperator{\\P}{\\mathbb{Pr}}

\\usepackage{minted}
\\defaultfontfeatures{Mapping=tex-text}
% \\setromanfont[BoldFont={Gentium Basic Bold},
%                 ItalicFont={Gentium Basic Italic}]{Gentium Plus}
\\setsansfont{Charis SIL}
\\setmonofont[Scale=0.8]{DejaVu Sans Mono}
\\usepackage{geometry}
%% \\geometry{a4paper, textwidth=6.5in, textheight=10in,
 %%  marginparsep=7pt,
 %%  marginparwidth=1.2in, %% make sure it less than right=1.5in,
  %% otherwise, will go out of the paper
 %% right=1.5in, left=0.6in}

\\geometry{a4paper, textwidth=6.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
\\pagestyle{empty}
 
%% package from org-latex-default-packages-alist
\\usepackage{setspace}
\\onehalfspacing
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{ulem}
\\usepackage{amsthm}

\\theoremstyle{definition}
\\newtheorem{definition}{Definition}[section]% Conjecture is numbered
                                % within \section
\\newtheorem{lemma}[definition]{Lemma}
\\newtheorem{theorem}[definition]{Theorem}

\\newcommand{\\twodots}{\\mathinner {\\ldotp \\ldotp}}

%% \\renewcommand\\texttt[1]{{\\mint{cl}|#1|}} 


\\usepackage{environ}
\\NewEnviron{note}{\\marginpar{\\footnotesize \\BODY}}

%% algorithm 
\\usepackage{xcolor}
\\usepackage[linesnumbered]{algorithm2e}
\\newcommand\\mycommfont[1]{\\footnotesize\\ttfamily\\textcolor{blue}{#1}}
\\makeatletter
\\renewcommand{\\@algocf@capt@plain}{above}% formerly {bottom}
\\makeatother


\\title{}
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(setq org-latex-default-class "yt/org-article")

(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\defaultfontfeatures{Mapping=tex-text}
% \\setromanfont[BoldFont={Gentium Basic Bold},
%                 ItalicFont={Gentium Basic Italic}]{Gentium Plus}
\\setsansfont{Charis SIL}
\\setmonofont[Scale=0.8]{DejaVu Sans Mono}
\\usepackage{minted}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

(require 'ox-beamer)

;; code highlights using minted package 
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
      '(("frame" "lines")
        ("fontsize" "\\scriptsize")))
;; ("linenos" "")))

;;;; comple pdf 
(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction=nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction=nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction=nonstopmode -output-directory %o %f"))
