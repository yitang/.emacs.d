(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(global-set-key (kbd "C-c w") 'ace-jump-word-mode)

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))
