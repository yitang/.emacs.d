;; save all backup files (foo~) to this directory.
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )
;; linum mode
(global-linum-mode 1)


;; ;;; winner-mode, undo window configration
(when (fboundp 'winner-mode)
  (winner-mode 0))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; PACKAGE: helm              ;;
;; ;;                            ;;
;; ;; GROUP: Convenience -> Helm ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path "~/git/.emacs.d/elpa/helm")
(require 'helm-config)
 (helm-mode 1)

;; ; Author: Baris Yuksel (2014)
;; ;
;; ; start package.el with emacs
;; (require 'package)
;; ; add MELPA to repository list
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;; ; initialize package.el
;; (package-initialize)
; start auto-complete with emacs
(require 'auto-complete)
; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; ; start yasnippet with emacs
;(add-to-list 'load-path "~/git/.emacs.d/elpa/")


;; (add-to-list 'load-path "~/git/.emacs.d/elpa/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)



;; recentf files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)

(global-set-key "\C-r" 'helm-recentf)





;; prefer horizentally split window
(setq split-height-threshold nil)
(setq split-width-threshold 0)


;; Use 10-pt Consolas as default font
(set-face-attribute 'default nil
                    :family "Consolas" :height 100)

;; windows path convention 
(setenv "CYGWIN" "nodosfilewarning")

;; kill *scratch* buffer if possible
(kill-buffer "*scratch*")
;; (kill-buffer "*GNU Emacs*")
(setq inhibit-startup-message t)        ; Disable startup message 


(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<right>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-<left>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; C-tab to switch bteween buffers. 
(global-set-key [C-tab] 'other-window)
