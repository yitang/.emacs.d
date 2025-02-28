;; ;; Adapted with one minor change from Felipe Salazar at
;; ;; http://www.emacswiki.org/emacs/EmacsSpeaksStatistics
;; (use-package ess
;;   :ensure t
;;   :init (require 'ess-site))
;; (setq ess-ask-for-ess-directory nil) ;; start R on default folder
;; (setq ess-local-process-name "R")
;; (setq ansi-color-for-comint-mode 'filter) ;;
;; ;; (setq comint-scroll-to-bottom-on-input t)
;; ;; (setq comint-scroll-to-bottom-on-output nil)
;; ;; (setq comint-move-point-for-output nil)
;; (setq ess-eval-visibly-p 'nowait) ;; no waiting while ess evalating
;; (defun my-ess-start-R ()
;;   (interactive)
;;   (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
;;       (progn
;;         (delete-other-windows)
;;         (setq w1 (selected-window))
;;         (setq w1name (buffer-name))
;;         (setq w2 (split-window w1))
;;         (R)
;;         (set-window-buffer w2 "*R*")
;;         (set-window-buffer w1 w1name))))
;; (defun my-ess-eval ()
;;   (interactive)
;;   (my-ess-start-R)
;;   (if (and transient-mark-mode mark-active)
;;       (call-interactively 'ess-eval-region)
;;     (call-interactively 'ess-eval-line-and-step)))
;; (add-hook 'ess-mode-hook
;;           '(lambda()
;;              (local-set-key [(shift return)] 'my-ess-eval)))
;; (add-hook 'ess-mode-hook
;;           (lambda ()
;;             (flyspell-prog-mode)
;;             (run-hooks 'prog-mode-hook)
;;             ))
;; (add-hook 'ess-R-post-run-hook (lambda () (smartparens-mode 1)))

;; ;; REF: http://stackoverflow.com/questions/2901198/useful-keyboard-shortcuts-and-tips-for-ess-r
;; ;; Control and up/down arrow keys to search history with matching what you've already typed:
;; (define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
;; (define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)
;; (setq ess-history-file "~/.Rhisotry")
;; (setq ess-indent-with-fancy-comments nil)


;; (define-key ess-r-mode-map "_" #'ess-insert-assign)
;; (define-key inferior-ess-r-mode-map "_" #'ess-insert-assign)

;; (setq ess-R-font-lock-keywords
;;     '((ess-R-fl-keyword:modifiers . t)
;;      (ess-R-fl-keyword:fun-defs . t)
;;      (ess-R-fl-keyword:keywords . t)
;;      (ess-R-fl-keyword:assign-ops)
;;      (ess-R-fl-keyword:constants . t)
;;      (ess-fl-keyword:fun-calls . t)
;;      (ess-fl-keyword:numbers)
;;      (ess-fl-keyword:operators)
;;      (ess-fl-keyword:delimiters)
;;      (ess-fl-keyword:=)
;;      (ess-R-fl-keyword:F&T)
;;      (ess-R-fl-keyword:%op%)))

;; ;; (add-hook 'ess-mode-hook 'turn-on-pretty-mode)

;; ;; edit roxy template
;; ;; ess-roxy-update-entry
;; (setq ess-roxy-template-alist '(("description" . " content for description")
;;                                 ("details" . "content for details")
;;                                 ("title" . "")
;;                                 ("param" . "")
;;                                 ("return" . "")
;;                                 ("export" . "")
;;                                 ("author" . "Yi Tang")))

;; (use-package flycheck)
;; ;; '(flycheck-lintr-caching nil) ;; need to customised it inside of Emacs
;; ;; (add-hook 'ess-mode-hook
;; ;;           (lambda () (flycheck-mode t)))

;; (defun yt/clean-R () 
;;   (interactive)
;;   (when (string= major-mode "ess-mode")
;;     (progn
;;       (goto-char (point-min))
;;       (flush-lines "^\\(\\|[[:space:]]+\\)[#]\\{1,3\\} ") ;; remove lines with only commenst and start with #, ##, or ###, but not #### for it's the section heading. 
;;       (flush-lines "^\\(\\|[[:space:]]+\\)$") ;; blank lines
;;       (replace-regexp "#### " "\n#### ") ;; add blank lines between sections. 
;;       (while (search-forward-regexp "##[^']" nil t) ;; remove inline comments start with ## 
;;         (kill-region (- (point) 3) (line-end-position)))
;;     (save-buffer))))

;; ;; add author info
;; (defun yt/ess-author-date ()
;;   (interactive)
;;   (when (string= major-mode "ess-mode")
;;     (goto-char (point-min))
;;     (insert "##' @author: Yi Tang\n")
;;     (insert "##' @date: ")
;;     (insert (format-time-string "%F %T"))
;;     (insert "\n\n")
;;     (save-buffer)))
;; (add-hook 'org-babel-post-tangle-hook 'yt/ess-author-date)
;; (add-hook 'org-babel-post-tangle-hook 'yt/clean-R)

;; (defun yt/ess-chunk-args--line ()
;;   "sim.gc.table <- data.table(duration = sort(sim.duration, decreasing = TRUE), rp = 1e4 / seq(1, length(sim.duration))) becomes 


;; sim.gc.table <- data.table(duration = sort(sim.duration,
;;                                           decreasing = TRUE),
;;                           rp = 1e4 / seq(1, length(sim.duration)))
;; "
;;   (interactive)
;;   (save-excursion
;;     (let ((start-point (point)))
;;       (while (re-search-forward ", \\([a-z]+ =\\)" (line-end-position) t)
;; 	(replace-match (concat ",\n    " (match-string 1))))
;;       (indent-region start-point (line-end-position))
;;       (goto-char start-point))))
      
;; (defun yt/ess-chunk-plus--line ()
;;   "ggplot(obs.gc.table, aes(rp, duration)) + geom_point() + scale_x_log10() + scale_y_log10() 

;; becomes 

;; ggplot(obs.gc.table, aes(rp, duration)) +
;;     geom_point() +
;;     scale_x_log10() +
;;     scale_y_log10()
;; "
;;   (interactive)
;;   (save-excursion
;;     (let ((start-point (point)))
;;       (replace-regexp " \\+ " " +\n    " nil (point) (line-end-position))
;;       (indent-region start-point (line-end-position))
;;       (goto-char start-point))))

;; (defun yt/ess-script-variables ()
;;   (interactive)
;;   (let ((var-list '())
;;         (data-list '()))
;;     (save-excursion
;;       (while (search-forward-regexp "^[[:space:]]*\\([[:alpha:]]+\\) <- function\(" nil t)
;;         (add-to-list 'func-list (match-string-no-properties 1))))
;;     (save-excursion
;;       (while (search-forward-regexp "^[[:space:]]*\\([a-z\\.]+\\) <- " nil t)
;;         (add-to-list 'var-list (match-string-no-properties 1))))
;;     (append (set-difference var-list func-list) data-list)))

;; (defun yt/ess-remove-variables-not-in-scripts ()
;;   (interactive)
;;   (let* ((all-vars (yt/ess-script-variables))
;;          (all-vars-R (concat "c(\"" (mapconcat 'identity all-vars "\",\"")
;;                              "\")")))
;;     (kill-new (concat "rm(list = setdiff\(setdiff\(ls\(\), lsf.str\(\)\), " all-vars-R "\)\)"))))

;; (defun yt/bash_run_R ()
;;      (interactive)
;;      (let* ((args (concat "R --no-save --no-restore < " (file-name-nondirectory (buffer-file-name))))
;;             (output-buf-name (concat "*R:" (file-name-nondirectory (buffer-file-name)) "*"))
;;             )
;;        (async-shell-command args output-buf-name)
;;        (with-current-buffer output-buf-name
;;          (inferior-ess-mode))
;;        ))

;; ;; (visit-tags-table "~/R_tags")
