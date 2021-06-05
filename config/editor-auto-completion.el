(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;;(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-l") 'helm-mini) 
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h a") 'helm-apropos)
(setq helm-apropos-fuzzy-match t)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(helm-autoresize-mode t)
(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))
(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)
(helm-mode 1)

(defun yt/helm-copy-unmarked-to-buffer ()
  (interactive)
  (with-helm-buffer
    (helm-mark-all)
    (cl-loop for cand in (helm-marked-candidates)
             do (with-helm-current-buffer
                  (insert cand "\n")))))
;; by default, Cc Ci copy marked to buffer.
(define-key helm-map (kbd "C-c C-i") 'helm-copy-unmmarked-to-buffer)

(setq helm-ff-guess-ffap-urls nil)

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; (global-set-key (kbd "C-S-<right>") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-S-<left>") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'helm-swoop)
;; Change the keybinds to whatever you like :)
;; (global-set-key (kbd "M-i") 'helm-swoop)
;; (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
;; (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
(global-set-key (kbd "<C-f1>") 'helm-swoop)
;; When doing isearch, hand the word over to helm-swoop
;; (define-key isearchp-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
;; (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)
;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)
;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)
;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)
;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)
;; ----------------------------------------------------------------------

(global-set-key (kbd "C-c w") 'ace-jump-word-mode)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
