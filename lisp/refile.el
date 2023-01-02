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

(use-package swiper)
(global-set-key "\C-s" 'swiper)

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

(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                           :height 2.0))
(add-hook 'nov-mode-hook 'my-nov-font-setup)
;; (setq nov-text-width 80)  ;; or used the following for auto text filling
(setq nov-text-width t)
(setq visual-fill-column-center-text t)
(add-hook 'nov-mode-hook 'visual-line-mode)
(add-hook 'nov-mode-hook 'visual-fill-column-mode)
(add-hook 'nov-mode-hook '(lambda () (blink-cursor-mode 0)))

(require 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)
(setq-default org-download-image-dir "~/Pictures/org-download")
(setq-default org-download-heading-lvl nil)   ;; flat file systme, otherwise, would be confusing.

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/git/org/diary")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
    '(("d" "default" entry "* %<%H:%M>: %?"
       :if-new (file+head "%<%Y-%m-%d>.org.gpg" "#+title: %<%Y-%m-%d>\n"))))
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
	 ("C-c n D" . yt/dailies)
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

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

(add-hook 'yaml-mode-hook
	  '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(defun yt/bind-to-compliation ()
  "docstring"
  (interactive)
  (let* ((filename (buffer-file-name (current-buffer)))
	 (default-directory (file-name-directory filename))
	 (output-buffer-name (concat "*Compilation:" filename "*")))
    (compile "ls")
    (with-current-buffer "*compilation*"
      (rename-buffer output-buffer-name)))
  )
(setq compilation-scroll-output 'first-error)

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
