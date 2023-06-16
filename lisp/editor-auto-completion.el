(use-package multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; (global-set-key (kbd "C-S-<right>") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-S-<left>") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-c w") 'ace-jump-word-mode)

(use-package expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
