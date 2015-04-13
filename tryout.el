(require 'hydra)
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

;;change default browser for 'browse-url'  to w3m
(setq browse-url-browser-function 'w3m-goto-url-new-session)
;;change w3m user-agent to android
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")
;; (setq w3m-user-agent "Emacs-w3m/1.4.540 w3m/0.5.3+debian-15")

(require 'purpose)
(purpose-mode)
(defun my/reset-window ()
  (interactive)
  (purpose-load-window-layout "~/git/.emacs.d/style/4_window_layout.purpose"))





(defun my/modify-org-done-face ()
  (setq org-fontify-done-headline t)
  (set-face-attribute 'org-done nil :strike-through t)
  (set-face-attribute 'org-headline-done nil
                      :strike-through t
                      :foreground "light gray"))

(add-hook 'org-add-hook 'my/modify-org-done-face)

;; (require 'smartparens-config)
(smartparens-global-mode 1)



(set-face-attribute 'org-done nil :strike-through t)
(set-face-attribute 'org-headline-done nil :strike-through t)

(setq org-download-image-dir "~/Downloads/org-downloads/")



(set-face-attribute 'mode-line nil
                    :foreground "Black"
                    :background "DarkOrange"
                    :box nil)
(setq powerline-arrow-shape 'arrow)
(setq sml/mode-width 30)
(rich-minority-mode 1)

(smartparens-global-mode 1)
;; (add-hook 'ess-R-post-run-hook (lambda () (smartparens-mode 1))) ;; already done in init.org 

(defcustom org-download-screenshot-method "gnome-screenshot -a -f %s"
  "The tool to capture screenshots."
  :type '(choice
          (const :tag "gnome-screenshot" "gnome-screenshot -w -f %s -d 1")
          (const :tag "scrot" "scrot -s %s")
          (const :tag "gm" "gm import %s"))
  :group 'org-download)






(olivetti-mode)



(require 'voca-builder)
(setq voca-builder/voca-file "~/.vocabulary.org")
(setq voca-builder/current-tag "WinFriend")
(global-set-key (kbd "<f4>") 'voca-builder/search-popup)
(setq sentence-end-double-space nil)
