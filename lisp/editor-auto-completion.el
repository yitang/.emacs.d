(use-package multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

(global-set-key (kbd "C-c w") 'ace-jump-word-mode)

(use-package expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
