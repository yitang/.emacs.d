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

(add-to-list 'auto-mode-alist '("sfile" . snakemake-mode))

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
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
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
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

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

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(setq nov-text-width t)
(setq visual-fill-column-center-text t)

(add-hook 'nov-mode-hook 'visual-line-mode)  ;; word-wrap and also use visual-line, not logical (actual) line.
(add-hook 'nov-mode-hook 'visual-fill-column-mode)  ;; center the text, looks good. have to call it again when change window size.
(add-hook 'nov-mode-hook '(lambda () (blink-cursor-mode 0)))  ;; blinking cursor is distracting.

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

(use-package yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

(add-hook 'yaml-mode-hook
	  '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

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
  (let* ((filename (buffer-file-name (current-buffer)))
	 (default-directory (file-name-directory filename))
	 (output-buffer-name (concat "*compilation:" filename "*")))
    (compile "ls -l")
    (with-current-buffer "*compilation*"
      (rename-buffer output-buffer-name)))
  )
(setq compilation-scroll-output 'first-error)


(defun yt/kill-buffer (name)
  "if buffer exists, kill, otherwise, do nothing"
  (when (get-buffer name)
    (kill-buffer name)
    ))

(defun yt/compile (buffer-name cmd &optional working-directory)
  (let ((default-directory (or working-directory default-directory)))
    (compile cmd))
  (yt/kill-buffer buffer-name)
  (with-current-buffer "*compilation*"
    (rename-buffer buffer-name)
    )
  )

(setq bookmark-save-flag 1)  ; save bookmark file everytime.

;; highlights FIXME: TODO: and BUG: in prog-mode 
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

(use-package whitespace)
(setq whitespace-line-column 120) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(show-paren-mode t) ;for Emacs

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

;;; -*- lexical-binding: t; -*-
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)


;; (add-to-list 'load-path "~/git_others/elpy")   ;; fix elpy-go-to-definition
(package-initialize)


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

(use-package consult
  :bind (("M-s f" . consult-line)
         ("M-g g" . consult-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g r" . consult-ripgrep)
         ("C-x C-r" . consult-recent-file)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)
         :map minibuffer-local-map
         ([remap previous-matching-history-element] . consult-history)
         :map isearch-mode-map
         ("TAB" . vifon/isearch-to-consult-line))
  :config (progn
            (setq consult-project-root-function #'vc-root-dir)
            (consult-customize
             consult-ripgrep consult-grep
             consult-buffer consult-recent-file
             :preview-key (kbd "M-."))

            (defun vifon/orderless-fix-consult-tofu (pattern index total)
              "Ignore the last character which is hidden and used only internally."
              (when (string-suffix-p "$" pattern)
                `(orderless-regexp . ,(concat (substring pattern 0 -1)
                                              "[\x200000-\x300000]*$"))))

            (dolist (command '(consult-buffer consult-line))
              (advice-add command :around
                          (lambda (orig &rest args)
                            (let ((orderless-style-dispatchers (cons #'vifon/orderless-fix-consult-tofu
                                                                     orderless-style-dispatchers)))
                              (apply orig args)))))

            ;; Disable consult-buffer project-related capabilities as
            ;; they are very slow in TRAMP.
            (setq consult-buffer-sources
                  (delq 'consult--source-project-buffer
                        (delq 'consult--source-project-file consult-buffer-sources)))

            (setq consult--source-hidden-buffer
                  (plist-put consult--source-hidden-buffer :narrow ?h))

            (defun vifon/isearch-to-consult-line ()
              "Search using `consult-line' what was being searched with `isearch'."
              (interactive)
              (isearch-exit)
              (let ((query (if isearch-regexp
                               isearch-string
                             (regexp-quote isearch-string))))
                (consult-line query)))))

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
