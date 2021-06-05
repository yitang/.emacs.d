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
