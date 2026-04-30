(when (require 'multiple-cursors nil t)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(global-set-key (kbd "C-c w") 'ace-jump-word-mode)

(when (require 'expand-region nil t)
  (global-set-key (kbd "C-=") 'er/expand-region))
