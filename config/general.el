(defun yt/reload-dot-emacs ()
  "Save the .emacs buffer if needed, then reload .emacs."
  (interactive)
  (let ((dot-emacs "~/.emacs"))
    (and (get-file-buffer dot-emacs)
         (save-buffer (get-file-buffer dot-emacs)))
    (load-file dot-emacs))
  (message "Re-initialized!"))
(setq confirm-kill-emacs 'y-or-n-p)

(fset 'yes-or-no-p 'y-or-n-p)

(global-unset-key (kbd "C-x b"))
(global-unset-key (kbd "C-x C-b"))
(global-unset-key (kbd "C-x C-c"))  ;; save-buffers-kill-terminal
(global-unset-key (kbd "C-x o"))  ;; other window. replace by f2 - ace-window.

;; ref: http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;; save all backup files (foo~) to this directory.
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      auto-save-timeout 20   ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200 ; number of keystrokes between auto-saves (default: 300)
      )

;; guide-key package 
;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence t) ;; on for all key-bindings 
;; (guide-key-mode 1) 

;; use company for all except few modes
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;; Don't enable company-mode in below major modes, OPTIONAL
(setq company-global-modes '(not eshell-mode comint-mode erc-mode rcirc-mode))

;; config company mode
(setq company-selection-wrap-around t
      company-tooltip-align-annotations t
      company-idle-delay 0.36
      company-minimum-prefix-length 2
      company-tooltip-limit 10)

(setq company-ddabbrev-code-everywhere t)
(setq company-dabbrev-code-modes t)
(setq company-dabbrev-code-other-buffers 'all)
(setq company-dabbrev-ignore-buffers "\\`\\'")
(setq company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\|_\\|-\\)")

;; config company for ESS mode
(defun yt/ess_company_mode_setup ()
  ;; this is really important. to source vairbales defined in the scripts.
     (make-local-variable 'company-backends)
     (add-to-list 'company-backends 'company-dabbrev-code)
     )
(add-hook 'ess-mode-hook 'yt/ess_company_mode_setup)


(defun text-mode-hook-setup ()
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-ispell)
  ;; (setq company-ispell-dictionary (file-truename "~/git/.emacs.d/english_words.txt"))
  )

(add-hook 'text-mode-hook 'text-mode-hook-setup)
(company-quickhelp-mode 1)
(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)
(define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)

(setq company-dabbrev-downcase nil)
(setq company-show-numbers t)

(recentf-mode 1)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)

(setq visible-bell nil) 
(setq ring-bell-function 'ignore)

(setq inhibit-startup-message t)

(require 'yasnippet)
(yas/global-mode 1)
(add-to-list 'yas/snippet-dirs "~/git/.emacs.d/snippets" t)
(yas/reload-all)

(require 'ace-window)
(global-set-key (kbd "<f2>") 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-scope 'frame)

(require 'golden-ratio)
(golden-ratio-mode 1)
(add-to-list 'golden-ratio-extra-commands 'ace-window) ;; active golden ratio when using ace-window

(winner-mode 1)
;; winner-undo -> C-c <left>
;; winner-redo -> C-c <right>

(defun yt/ref-frame ()
  (interactive)
  ;;   (frame-parameter (car (frame-list)) 'name)
  (if (eq 1 (length (frame-list)))
      (new-frame '((name . "***********************REFERENCE*******************")))
    nil))
(global-set-key (kbd "M-`") 'other-frame)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell 
         (replace-regexp-in-string "[[:space:]\n]*$" "" 
                                   (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))
;; windows path convention
;; (setenv "CYGWIN" "nodosfilewarning")

;; modify mac keyboard 
(cond ((eq system-type 'darwin)
       (setq mac-command-modifier 'meta)
       (fset 'insertPound "#")
       (global-set-key (kbd "M-3") 'insertPound)       
       (global-unset-key (kbd "M-`"))
       (global-set-key (kbd "M-`") 'other-frame)
       (global-set-key (kbd "C-Z") nil)
       ))

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; (require 'openwith)
;; (openwith-mode t)
;; (if (string= system-type "darwin")
;;     (setq openwith-associations '(("\\.pdf\\'" "Skim" (file))))
;;   (setq openwith-associations '(("\\.pdf\\'" "evince" (file)))))

(smartparens-global-mode 1)
(sp-pair "(" ")" :wrap "C-(")
;; |foobar
;; hit C-(
;; becomes (|foobar)
(sp-pair "'" nil :actions :rem)

(add-hook 'text-mode-hook 'turn-on-auto-fill) ;;

(defun yt/unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(defun yt/unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(setq savehist-file "~/git/.emacs.d/local/emacs-history")
(savehist-mode 1)

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key [\M-\S-up] 'move-text-up)
(global-set-key [\M-\S-down] 'move-text-down)
