(defun yt/filter-life-agenda (tag)
  (concat "-" "life"))
(defun yt/filter-office-agenda (tag)
  (concat "-" "@office"))
(if (eq system-type 'darwin)
    (setq org-agenda-auto-exclude-function 'yt/filter-office-agenda)
  (setq org-agenda-auto-exclude-function 'yt/filter-life-agenda))

(defun yt/open-terminal ()
  (interactive)
  (shell-command (concat "gnome-terminal "
                         "--working-directory="
                         (file-truename default-directory)
                         )))
;; (global-set-key (kbd "<f5>") 'yt/open-terminal)

(global-set-key "\C-s" 'consult-line)

(use-package snakemake-mode
  :ensure t
  :mode ("\\.sfile\\'" "\\.Snakemake\\'"))

(defun yt/sh-chunk-args ()
(interactive)
(replace-string " -" " \\\n -")
)

(defun yt/insert-git-hash-value ()
  (interactive)
  (insert (shell-command-to-string (concat "git rev-parse HEAD"))))
(global-set-key (kbd "<f9> s") 'yt/insert-git-hash-value)

(use-package which-key)
(which-key-mode)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" "~/matrix/tools/.emacs.d/")
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  ;; :bind
  ;; (:map global-map
  ;;       ("M-0"       . treemacs-select-window)
  ;;       ("C-x t 1"   . treemacs-delete-other-windows)
  ;;       ("C-x t t"   . treemacs)
  ;;       ("C-x t d"   . treemacs-select-directory)
  ;;       ("C-x t B"   . treemacs-bookmark)
  ;;       ("C-x t C-t" . treemacs-find-file)
  ;;       ("C-x t M-t" . treemacs-find-tag))
  )

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))


;; (use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
;;   :after (treemacs)
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Tabs))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook ((nov-mode . visual-line-mode)
	 (nov-mode . visual-fill-column-mode)
	 (nov-mode . (lambda () (blink-cursor-mode 0))))
  :config
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t))
  
(use-package visual-fill-column
  :ensure t)  

(defun yt/read-mode ()
  (visual-line-mode)
  (visual-fill-column-mode))

(use-package org-download)
(add-hook 'dired-mode-hook 'org-download-enable)
(setq-default org-download-image-dir "~/Pictures/org-download")
(setq-default org-download-heading-lvl nil)   ;; flat file systme, otherwise, would be confusing.

(if (eq system-type 'darwin)
    (setq org-download-screenshot-method "screencapture -i %s"))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
    '(("d" "default" entry "* %<%H:%M>: %?"
       :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-dailies-directory "daily")

  ;; ; change the timestmap aslightly..
  ;; (org-roam-capture-templates
  ;;  '(("d" "default" plain "%?"
  ;;    :target (file+head "%<%Y%m%d_%H%M%S>-${slug}.org"
  ;;                       "#+title: ${title}\n")
  ;;    :unnarrowed t)))

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(defun yt/dailies ()
  "global function for creating daries"
  (interactive)
  (let ((org-roam-directory "~/git/org/diary"))
    (org-roam-dailies-capture-today)
    )
  )

(setq enable-local-variables t)

(use-package dired-rsync
  :demand t
  :after dired
  :bind (:map dired-mode-map ("r" . dired-rsync))
  :config (add-to-list 'mode-line-misc-info '(:eval dired-rsync-modeline-status 'append)))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml\\'" "\\.yml\\'")
  :bind (("C-m" . newline-and-indent))
  )

(use-package yaml-pro
  :ensure t)

(setq org-export-with-broken-links t) ;; broken links are fine in exporting. 
;; (setq org-latex-prefer-user-labels t)  ;; fix labels, otherwise, randomly generated, not git friendly.

;; somehow relative path doesn't work in osx when export org to other format. so use abslute path.
(setq org-download-abbreviate-filename-function #'expand-file-name)
(setq org-link-file-path-type 'absolute)

(defun yt/execute-src-org-file (filename)
 "run the src babel block in a given file"
  (interactive)
  (with-current-buffer (get-buffer-create (concat "*buffer:" filename))
    (erase-buffer)
    (insert-file-contents filename)
    (let ((default-directory (file-name-directory filename)))
      (org-babel-execute-buffer))
    )
  )

(defun yt/bind-to-compliation ()
  "bind current file-visiting buffer to a dedicated compilation buffer"
  (interactive)
  (let* ((filename (buffer-file-name))
	 (default-directory (file-name-directory filename))
	 (output-buffer-name (concat "*compilation*:" filename)))
    (compile "ls -l")
    (when (get-buffer output-buffer-name)
      (kill-buffer output-buffer-name))
    (with-current-buffer "*compilation*"
      (rename-buffer output-buffer-name)))
  )
(setq compilation-scroll-output 'first-error)


(defun yt/kill-buffer (name)
  "if buffer exists, kill, otherwise, do nothing"
  (when (get-buffer name)
    (kill-buffer name)
    ))

(defun yt/compile (output-id cmd &optional working-directory)
  (let ((default-directory (or working-directory default-directory))
	(buffer-name (concat "*compilation*" output-id)))
    (progn
      (compile cmd)
      (when (get-buffer buffer-name)
	(kill-buffer buffer-name))
      (with-current-buffer "*compilation*" 
	(rename-buffer buffer-name)))))

;;;; colorize output in compile buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(global-set-key (kbd "M-i") 'ibuffer)
(setq ibuffer-saved-filter-groups
      '(("home"
	 ("shell" (mode . shell-mode))
	 ;; ("emacs-config" (or (filename . ".emacs.d")
	 ;; 		     (filename . "emacs-config")))
         ;; ("martinowen.net" (filename . "martinowen.net"))
	 ("compilation" (mode . compilation-mode))
	 ("dired" (mode . dired-mode))
	 ("planner" (or
		     (name . "^\\*Calendar\\*$")
		     (name . "^\\*Org Agenda\\*")))
	 ("python" (mode . python-mode))
	 ("Org" (or (mode . org-mode)
		    (filename . "OrgMode")))
         ;; ("code" (filename . "code"))
	 ;; ("Web Dev" (or (mode . html-mode)
	 ;; 		(mode . css-mode)))
	 ;; ("Subversion" (name . "\*svn"))
	 ("Magit" (name . "\\*magit"))
	 ;; ("Magit2" (name . "magit"))
	 ;; ("ERC" (mode . erc-mode))
	 ;; ("Help" (or (name . "\*Help\*")
	 ;; 	     (name . "\*Apropos\*")
	 ;; 	     (name . "\*info\*")))
	 )))

(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-auto-mode 1)
	     (ibuffer-switch-to-saved-filter-groups "home")))
(keymap-set ibuffer-mode-map "M-o" nil)


;; nearly all of this is the default layout
(setq ibuffer-formats 
      '((mark modified read-only " "
              (name 30 30 :left :elide) ; change: 30s were originally 18s
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

(setq bookmark-save-flag 1)  ; save bookmark file everytime.

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(YT\\|FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

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

(subword-mode 1)

(use-package whitespace
  :ensure t
  :config
  (setq whitespace-line-column 120)
  (setq whitespace-style '(face lines-tail))
  :hook (prog-mode-hook . whitespace-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))
(show-paren-mode t) ;; highlight matched parentheses

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

(use-package vertico
  :bind (("C-x M-r" . vertico-repeat)
         :map vertico-map
         ("C-l" . vertico-directory-delete-word)
         ("M-g" . vertico-multiform-grid)
         ("M-q" . vertico-multiform-flat))
  :init (vertico-mode 1)
  :config (progn
            (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
            (vertico-mouse-mode 1)
            (vertico-multiform-mode 1)
            (setq vertico-multiform-categories '((consult-grep buffer))
                  vertico-multiform-commands '((tmm-menubar flat)
                                               (tmm-shortcut flat)))))

(use-package orderless
  :after vertico
  :config (progn
            (setq orderless-matching-styles '(orderless-regexp
                                              orderless-initialism
                                              orderless-prefixes)
                  orderless-component-separator #'orderless-escapable-split-on-space)

            ;; Use the built-in "partial-completion" style to complete
            ;; file inputs such as "/e/ni/co.nix" into
            ;; "/etc/nixos/configuration.nix".  The "basic" style is
            ;; needed to support the hostname completion in the TRAMP
            ;; inputs such as "/sshx:HOSTNAME".
            (setq completion-category-defaults nil
                  completion-category-overrides '((file (styles basic partial-completion))))

            (setq completion-styles '(orderless))

            (defun vifon/orderless-without-if-bang (pattern index total)
              (when (string-prefix-p "!" pattern)
                `(orderless-without-literal . ,(substring pattern 1))))
            (defun vifon/orderless-literal-if-equal (pattern index total)
              (when (string-suffix-p "=" pattern)
                `(orderless-literal . ,(substring pattern 0 -1))))
            (setq orderless-style-dispatchers '(vifon/orderless-without-if-bang
                                                vifon/orderless-literal-if-equal))))

(use-package embark
  :bind (("C-c o" . embark-act)
         ("C-."   . embark-act)
         :map minibuffer-local-map
         ("M-o"   . embark-act)
         :map embark-command-map
         ;; Unbind the dangerous `global-set-key' and `local-set-key'
         ;; actions.  It's far too easy to accidentally bind over some
         ;; `self-insert-command' binding or even over
         ;; \\[keyboard-quit].
         ("g" . nil)
         ("l" . nil))
  :config (progn
            (setq embark-mixed-indicator-delay 2)

            ;; Make the eval action editable.  Evaluating code
            ;; in-place is simple enough without Embark, if I invoke
            ;; it with Embark, I almost definitely want to edit the
            ;; expression beforehand.  And even if not, I can
            ;; just confirm.
            (cl-pushnew 'embark--allow-edit
                        (alist-get 'pp-eval-expression embark-target-injection-hooks))

            ;; Reload the project list after using
            ;; C-u `embark-act' with `project-forget-project'.
            (cl-pushnew 'embark--restart
                        (alist-get 'project-forget-project embark-post-action-hooks))

            (defun embark-act-with-eval (expression)
              "Evaluate EXPRESSION and call `embark-act' on the result."
              (interactive "sExpression: ")
              (with-temp-buffer
                (let ((expr-value (eval (read expression))))
                  (insert (if (stringp expr-value)
                              expr-value
                            (format "%S" expr-value))))
                (embark-act)))

            (dolist (keymap (list embark-variable-map embark-expression-map))
              (define-key keymap (kbd "v") #'embark-act-with-eval))

            ;; Source: https://github.com/oantolin/embark/wiki/Additional-Actions#attaching-file-to-an-email-message
            (autoload 'gnus-dired-attach "gnus-dired" nil t)
            (defun embark-attach-file (file)
              "Attach FILE to an email message."
              (interactive "fAttach: ")
              (gnus-dired-attach (list file)))
            (bind-key "a" #'embark-attach-file embark-file-map)))

(use-package embark-consult
  :after (embark consult))

(use-package marginalia
  :after vertico
  :demand t                     ; :demand applies to :bind but not
                                ; :after.  We want to eagerly load
                                ; marginalia once vertico is loaded.
  :bind (:map minibuffer-local-map
         ("C-o" . marginalia-cycle))
  :config (marginalia-mode 1))


(use-package corfu
  :init (global-corfu-mode 1))


;;; https://with-emacs.com/posts/tutorials/customize-completion-at-point/
(autoload 'ffap-file-at-point "ffap")
(add-hook 'completion-at-point-functions
          (defun complete-path-at-point+ ()
            (let ((fn (ffap-file-at-point))
                  (fap (thing-at-point 'filename)))
              (when (and (or fn (equal "/" fap))
                         (save-excursion
                           (search-backward fap (line-beginning-position) t)))
                (list (match-beginning 0)
                      (match-end 0)
                      #'completion-file-name-table :exclusive 'no))))
          'append)

;;; Add prompt indicator to `completing-read-multiple'.
;;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
;;;
;;; Taken from the Vertico docs.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;;; Use the completing-read UI for the M-tab completion unless
;;; overridden (for example by `corfu').
(setq-default completion-in-region-function
              (lambda (&rest args)
                (apply (if vertico-mode
                           #'consult-completion-in-region
                         #'completion--in-region)
                       args)))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

;; https://www.reddit.com/r/emacs/comments/tv022a/smooth_scrolling_on_emacs_29_is_a_dream_come_true/
(pixel-scroll-precision-mode 1)

(defhydra hydra/smerge ()
  "open file: "
  ("n" (smerge-next) "Jump to next confclit")
  ("p" (smerge-prev) "Jump to previous conflict")
  ("c" (smerge-keep-current) "keep current")
  ("u" (smerge-keep-upper) "keep upper version")
  ("l" (smerge-keep-lower) "keep lower version")
  ("a" (smerge-keep-all) "keep both versions")
  )
(global-set-key (kbd "<f7>") 'hydra/smerge/body)

(setq ediff-keep-variants nil)  ;; ask before close the file.
(setq ediff-keep-variants t)    ;; show buffer after exit.


(defun mkm/ediff-marked-pair ()
  "Run ediff-files on a pair of files marked in dired buffer

copied from https://stackoverflow.com/questions/18121808/emacs-ediff-marked-files-in-different-dired-buffers"
  (interactive)
  (let* ((marked-files (dired-get-marked-files nil nil))
         (other-win (get-window-with-predicate
                     (lambda (window)
                       (with-current-buffer (window-buffer window)
                         (and (not (eq window (selected-window)))
                              (eq major-mode 'dired-mode))))))
         (other-marked-files (and other-win
                                  (with-current-buffer (window-buffer other-win)
                                    (dired-get-marked-files nil)))))
    (cond ((= (length marked-files) 2)
           (ediff-files (nth 0 marked-files)
                        (nth 1 marked-files)))
          ((and (= (length marked-files) 1)
                (= (length other-marked-files) 1))
           (ediff-files (nth 0 marked-files)
                        (nth 0 other-marked-files)))
          (t (error "mark exactly 2 files, at least 1 locally")))))


(require 'dired)
(define-key dired-mode-map (kbd "C-c e") 'mkm/ediff-marked-pair)

(use-package diminish :ensure t)

(use-package ledger-mode
  :ensure t
  :diminish auto-fill-mode   ; it suppose to remove the auto-fill
			     ; minor mode, but i cannot get it
			     ; working.

					; an workaround for disbale auto-fill mode.
  :hook ((ledger-mode . (lambda () (set-fill-column 160)))
	 (ledger-mode . (lambda ()
			  (setq-local tab-always-indent 'complete)
			  (setq-local completion-cycle-threshold t)
			  (setq-local ledger-complete-in-steps nil))))
  :custom
  (ledger-default-date-format "%Y-%m-%d"))

(use-package flycheck-ledger
  :ensure t)



;; (add-hook 'ledger-mode-hook
;;           (lambda ()
;;             (setq-local tab-always-indent 'complete)
;;             (setq-local completion-cycle-threshold t)
;;             (setq-local ledger-complete-in-steps t)))

(defvar yt/matrix-setup--matrix-root "~/matrix")

;; ~/matrix/ds
;; ~/matrix/ds/dlsys

(defun yt/matrix-setup--find-matrix (filepath)
  "given a filepath, return the matrix it belongs to."
  (nth 0 (string-split (f-relative filepath yt/matrix-setup--matrix-root) "/")))


(defun yt/matrix-setup--find-project (filepath)
  "given a filepath, return the project it belongs to."
  (nth 1 (string-split (f-relative filepath yt/matrix-setup--matrix-root) "/")))
