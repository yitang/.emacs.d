=============================
ESS - Emacs Speaks Statistics
=============================


As Statistician, coding in R and writing report is what I do most of
the day. I have been though a long way of searching the perfect editor
for me, tried Rstudio, SublimeText, TextMate and settled down happily
with ESS/Emacs, for both coding and writing.

There three features that have me made the decision:

1) Auto Formatting 

   Scientists has reputation of being bad programmers, who wrote the
   code that is unreadable and therefore incomprehensible to others. I
   have intention to become top level programmer and followed a style
   guide strictly. It means I have to spent sometime in adding and
   removing space in the code.

   To my surprise, Emacs will do it for me automatically, just by
   hitting the TAB and it also indent smartly, which make me
   conformable to write long function call and split it into multiple
   lines. Here's an example. Also if I miss placed a ')' or ']' the
   formatting will become strange and it reminders me to check.

   .. code:: R

       rainfall.subset <- data.table(rainfall.london,
       			     rainfall.pairs,
       			     rainfall.dublin)

2) Search Command History

   I frequently search the command history. Imaging I was produce a
   plot and I realised there was something miss in the data, so I go
   back and fix the data first, then run the ggplot command again, I
   press Up/Down bottom many times, or just search once/two times.
   ``M-x ggplot(`` will gives me the most recent command I typed
   containing the keyword *ggplot(*, then I press ``RET`` to select the
   command, which might be ``ggplot(gg.df, aes(lon, lat, col = city)) + geom_line() + .....``. If it is not I want, I press ``C-r`` again to
   choose the second most recent one and repeat until I find right
   one.

3) Literate Programming 

   I am an supporter of literate statistical analysis and believe we
   should put code, results and discoveries together in developing
   models. Rstudio provides an easy to use tool for this purpose, but
   it does not support different R sessions, so if I need to generate
   a report, I have to re-run all the code from beginning, which isn't
   particle for me with volumes data because it will take quit long.

   ESS and org-mode works really well via Babel, which is more
   friendly to use. I can choose to run only part of the code and have
   the output being inserted automatically, no need to copy/paste.
   Also, I can choose where to execute the code, on my local machine
   or the remote server, or both at the same time.

   These are only the surface of ESS and there are lot more useful
   features like spell checking for comments and documentation templates,
   that makes me productive and I would recommend anyone use R to learn
   ESS/Emacs. The following is my current setting.

.. code:: common-lisp

    ;; Adapted with one minor change from Felipe Salazar at
    ;; http://www.emacswiki.org/emacs/EmacsSpeaksStatistics
    (require 'ess-site)
    (setq ess-ask-for-ess-directory nil) ;; start R on default folder
    (setq ess-local-process-name "R")
    (setq ansi-color-for-comint-mode 'filter) ;;
    ;; (setq comint-scroll-to-bottom-on-input t)
    ;; (setq comint-scroll-to-bottom-on-output nil)
    ;; (setq comint-move-point-for-output nil)
    (setq ess-eval-visibly-p 'nowait) ;; no waiting while ess evalating
    (defun my-ess-start-R ()
      (interactive)
      (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
          (progn
    	(delete-other-windows)
    	(setq w1 (selected-window))
    	(setq w1name (buffer-name))
    	(setq w2 (split-window w1))
    	(R)
    	(set-window-buffer w2 "*R*")
    	(set-window-buffer w1 w1name))))
    (defun my-ess-eval ()
      (interactive)
      (my-ess-start-R)
      (if (and transient-mark-mode mark-active)
          (call-interactively 'ess-eval-region)
        (call-interactively 'ess-eval-line-and-step)))
    (add-hook 'ess-mode-hook
    	  '(lambda()
    	     (local-set-key [(shift return)] 'my-ess-eval)))
    (add-hook 'ess-mode-hook
    	  (lambda ()
    	    (flyspell-prog-mode)
    	    (run-hooks 'prog-mode-hook)
    	    ))
    (add-hook 'ess-R-post-run-hook (lambda () (smartparens-mode 1)))

    ;; REF: http://stackoverflow.com/questions/2901198/useful-keyboard-shortcuts-and-tips-for-ess-r
    ;; Control and up/down arrow keys to search history with matching what you've already typed:
    (define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
    (define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)
    (setq ess-history-file "~/.Rhisotry")
    (setq ess-indent-with-fancy-comments nil)


    (define-key ess-r-mode-map "_" #'ess-insert-assign)
    (define-key inferior-ess-r-mode-map "_" #'ess-insert-assign)

Syntax highlight
----------------

In Emacs, syntax highlighting is known as font-locking.  You can customize the amount of syntax highlighting that you want to see.  At the top of the Emacs window, click on the ESS menu and select "Font Lock".  This will display a menu of buttons corresponding to language elements that you can syntax highlight.  

.. code:: common-lisp

    (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:modifiers . t)
         (ess-R-fl-keyword:fun-defs . t)
         (ess-R-fl-keyword:keywords . t)
         (ess-R-fl-keyword:assign-ops)
         (ess-R-fl-keyword:constants . t)
         (ess-fl-keyword:fun-calls . t)
         (ess-fl-keyword:numbers)
         (ess-fl-keyword:operators)
         (ess-fl-keyword:delimiters)
         (ess-fl-keyword:=)
         (ess-R-fl-keyword:F&T)
         (ess-R-fl-keyword:%op%)))

use pretty mode 

.. code:: common-lisp

    ;; (add-hook 'ess-mode-hook 'turn-on-pretty-mode)

Programming Mode
----------------

After 2014, Emacs comes a prog-mode, for programming langauge.  it is generic mode, just like text-mode, that sits underneth all the programming language, either R, phython, C++ etc.  The good thinkg to have this concept is that we can define few things that will apply to all these mode, when we write scripts.  

One thing I find particulaar usefull and necessary is to highliht  characters in comments that has particullar meaning, like TODO, FIXME or other.  which can be particular handy in code reivew, I can navite and jump between the code quickly. 

.. code:: common-lisp

    ;; highlights FIXME: TODO: and BUG: in prog-mode 
    ;; (add-hook 'prog-mode-hook
    ;;           (lambda ()
    ;;             (font-lock-add-keywords nil
    ;;                                     '(("\\<\\(YT\\|FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

we usually have long scripts, and in Subimetext, one cold folder and
unfolder a function. in Emacs, this feature could be extended to
furture, by define folder-characters. at this statge, I tented to used
the deafault, I.e. folder functions only. in the folliwng setting, I
could press ``F3`` to folder/unfolder a function, ``C-F3`` or ``S-F3`` to
folder/unfolder all functions.

One potentially solution is to use ``outshine`` package, to show/hide
the whole section.

.. code:: common-lisp

    ;; (add-hook 'prog-mode-hook 'hs-minor-mode)
    ;; (defalias 'fold-toggle 'hs-toggle-hiding)
    ;; (global-set-key (kbd "<f4>") 'hs-toggle-hiding)
    ;; (global-set-key (kbd "S-<f4>") 'hs-show-all) ;; S->show 
    ;; (global-set-key (kbd "C-<f4>") 'hs-hide-all) 
    ;; ;;   hs-hide-block                      C-c @ C-h
    ;; ;;   hs-show-block                      C-c @ C-s
    ;; ;;   hs-hide-all                        C-c @ C-M-h
    ;; ;;   hs-show-all                        C-c @ C-M-s
    ;; ;;   hs-hide-level                      C-c @ C-l
    ;; ;;   hs-toggle-hiding 
    ;; ;;   hs-mouse-toggle-hiding             [(shift mouse-2)]
    ;; ;;   hs-hide-initial-comment-block
    (global-set-key (kbd "C-d") 'comment-region) ;; overwite delete-char 
    (global-set-key (kbd "C-S-d") 'uncomment-region)

    (defhydra hydra-fold (:pre (hs-minor-mode 1))
      "fold"
      ("t" hs-toggle-hiding "toggle")
      ("s" hs-show-all "hide-all")
      ("h" hs-hide-all "show-all")
      ("q" nil "quit"))
    (global-set-key (kbd "<f4>") 'hydra-fold/body)


use subword-mode then ThisPhase has two word, and I can use ``C-DEL`` it will remove the Phase and left This. Very useful in CamerCase.

.. code:: common-lisp

    (subword-mode 1)


highlights the text that are longer than 80 columns rule. 

.. code:: common-lisp

    (require 'whitespace)
    (setq whitespace-line-column 120) ;; limit line length
    (setq whitespace-style '(face lines-tail))
    (add-hook 'prog-mode-hook 'whitespace-mode)


Rainbow-delimiters. constantly have problem with package, and tired of fixing it, so I turned it off at this stage. 

.. code:: common-lisp

    (require 'rainbow-delimiters)
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (show-paren-mode t) ;for Emacs

use f8 to remove the R process buffer.

.. code:: common-lisp

    (defun yt/prog-previous-output-region ()
      "return start/end points of previous output region"
      (save-excursion
        (beginning-of-line)
        (setq sp (point))
        (comint-previous-prompt 1)
        (next-line)
        (beginning-of-line)
        (setq ep (point))
        (cons sp ep)))
    (defun yt/prog-kill-output-backwards ()
      (interactive)
      (save-excursion
        (let ((reg (yt/prog-previous-output-region)))
          (delete-region (car reg) (cdr reg))
          (goto-char (cdr reg))
          (insert "*** output flushed ***\n"))))
    ;; (global-set-key (kbd "<f8>") 'yt/prog-kill-output-backwards)

Documentation
-------------



.. code:: common-lisp

    ;; edit roxy template
    ;; ess-roxy-update-entry
    (setq ess-roxy-template-alist '(("description" . " content for description")
    				("details" . "content for details")
    				("title" . "")
    				("param" . "")
    				("return" . "")
    				("export" . "")
    				("author" . "Yi Tang")))

R Style Check - Flycheck
------------------------



`https://github.com/jimhester/lintr <https://github.com/jimhester/lintr>`_
the default R-style is not meet my with current R project style, has to turn it off.     

.. code:: common-lisp

    (require 'flycheck)
    ;; '(flycheck-lintr-caching nil) ;; need to customised it inside of Emacs
    ;; (add-hook 'ess-mode-hook
    ;;           (lambda () (flycheck-mode t)))

Scripts editing
---------------

R programming
-------------



clean up the messy R scripts buffer. it will 

1. remove comments lines start with '## '

2. remove blank lines,

3. add one blank lines between sections, which defined by '#### '.

.. code:: common-lisp

    (defun yt/clean-R () 
      (interactive)
      (when (string= major-mode "ess-mode")
        (progn
          (goto-char (point-min))
          (flush-lines "^\\(\\|[[:space:]]+\\)[#]\\{1,3\\} ") ;; remove lines with only commenst and start with #, ##, or ###, but not #### for it's the section heading. 
          (flush-lines "^\\(\\|[[:space:]]+\\)$") ;; blank lines
          (replace-regexp "#### " "\n#### ") ;; add blank lines between sections. 
          (while (search-forward-regexp "##[^']" nil t) ;; remove inline comments start with ## 
    	(kill-region (- (point) 3) (line-end-position)))
        (save-buffer))))

apply the clean scripts to the tangled file.  also, preappend the date and my name on the tangled file. 

.. code:: common-lisp

    ;; add author info
    (defun yt/ess-author-date ()
      (interactive)
      (when (string= major-mode "ess-mode")
        (goto-char (point-min))
        (insert "##' @author: Yi Tang\n")
        (insert "##' @date: ")
        (insert (format-time-string "%F %T"))
        (insert "\n\n")
        (save-buffer)))
    (add-hook 'org-babel-post-tangle-hook 'yt/ess-author-date)
    (add-hook 'org-babel-post-tangle-hook 'yt/clean-R)
