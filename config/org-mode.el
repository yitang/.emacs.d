(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "WIP(p)" "SOMEDAY" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
	      ("NEXT" :foreground "red" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
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

(setq org-log-done (quote time))
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

(defun yt/modify-org-done-face ()
  (setq org-fontify-done-headline t)
  (set-face-attribute 'org-done nil :strike-through nil)
  (set-face-attribute 'org-headline-done nil
		      :strike-through t
		      :foreground "light gray"))
;; turn it off for now.
;; (add-hook 'org-mode-hook 'yt/modify-org-done-face)
;; (setq org-fontify-done-headline t)
;; (set-face-attribute 'org-done nil :strike-through t)
;; (set-face-attribute 'org-headline-done nil :strike-through t)

(global-set-key (kbd "C-c c") 'org-capture)

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

(setq org-refile-use-cache t)

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
;; ;; http://stackoverflow.com/questions/6156286/emacs-lisp-call-function-with-prefix-argument-programmatically

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
(setq org-global-properties (quote (("Effort_ALL" . "0:05 0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
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
;; (add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)
(setq org-file-apps (quote ((auto-mode . emacs)
                            ("\\.png\\'" . emacs)
                            ("\\.svg\\'" . system)
                            ("\\.mm\\'" . system)
                            ("\\.x?html?\\'" . system)
                            ("\\.pdf\\'" . "evince %s"))))
                                        ; Overwrite the current window with the agenda
(setq org-agenda-window-setup 'current-window)

(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

(global-set-key (kbd "<f12>") 'org-agenda)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-dim-blocked-tasks 'invisible)

;; Compact the block agenda view
(setq org-agenda-compact-blocks nil)




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
 (600 630 700 730 800 830 900 930 1000 1030 1200 1400 1600 1800 2000)
 "......" "----------------")))

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
;; (bh/org-agenda-to-appt)

(setq org-reverse-note-order t) ;; refiled headline will be the first under the taget

(setq org-archive-location "::* Archived Tasks") ;;in-file archive 

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
	 (org . t)
	 (dot . t)
	 (python .t)
	 ;; (ipython .t)
	 ;; (bibtex .t)
	 (octave . t)
	 (latex . t)
	 (jupyter . t)
	 ;; (shell . t)
	 ;; (ledger . t)
	 (sql . t))))

(setq org-babel-default-header-args (append org-babel-default-header-args '((:colnames . "yes"))))

;; (add-to-list 'org-babel-default-header-args:R
;;              ;; '(:session . "*R-main*")
;;              '((:width . 640) (:height . 640)))

(setq org-confirm-babel-evaluate nil)

;; copy of org-sh-bash-initiate-session in ob-shell.el but with different name
;; per https://emacs.stackexchange.com/questions/55957/error-no-org-babel-initiate-session-function-for-bash
(defun org-babel-bash-initiate-session (&optional session _params)
  "Initiate a session named SESSION according to PARAMS."
  (when (and session (not (string= session "none")))
    (save-window-excursion
      (or (org-babel-comint-buffer-livep session)
          (progn
	    (shell session)
	    ;; Needed for Emacs 23 since the marker is initially
	    ;; undefined and the filter functions try to use it without
	    ;; checking.
	    (set-marker comint-last-output-start (point))
	    (get-buffer (current-buffer)))))))

;; (use-package ox-html)
;; (use-package ox-latex)
;; (use-package ox-ascii)
;; (use-package ox-md)
(use-package htmlize)

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
(require 'ox-latex)
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
\\usepackage{physics}   %% \\pdv for derivative operators https://tex.stackexchange.com/questions/225523/how-to-write-partial-differential-equation-ex-dq-dt-ds-dt-with-real-partial-d
\\DeclareMathOperator*{\\argmin}{\\arg\\!\\min} %% use $\\argmin_{b}$
\\DeclareMathOperator*{\\argmax}{\\arg\\!\\max} 
%% \\DeclareMathOperator{\\E}{\\mathbb{E}}
\\newcommand{\\E}[1]{{\\mathbb E}\\left[ #1 \\right]}
\\newcommand{\\Var}{\\mathrm{Var}}
%% \\DeclareMathOperator{\\P}{\\mathbb{Pr}}

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
             '("yt/beamer"
               "\\documentclass[aspectratio=169]{beamer}
\\usepackage[T1]{fontenc}
\\usepackage{fontspec}
\\usetheme[faculty=fi]{fibeamer}
\\usepackage[utf8]{inputenc}

\\usepackage[
  main=english, %% By using `czech` or `slovak` as the main locale
                %% instead of `english`, you can typeset the
                %% presentation in either Czech or Slovak,
                %% respectively.
  czech, slovak %% The additional keys allow foreign texts to be
]{babel}        %% typeset as follows:


[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

;; (use-package ox-beamer)

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
