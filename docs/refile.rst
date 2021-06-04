======
Refile
======


quickly filter out non-work tasks in org-agenda. 

.. code:: common-lisp

    (defun yt/filter-life-agenda (tag)
      (concat "-" "life"))
    (defun yt/filter-office-agenda (tag)
      (concat "-" "@office"))
    (if (eq system-type 'darwin)
        (setq org-agenda-auto-exclude-function 'yt/filter-office-agenda)
      (setq org-agenda-auto-exclude-function 'yt/filter-life-agenda))


open this gnome-terminal here

.. code:: common-lisp

    (defun yt/open-terminal ()
      (interactive)
      (shell-command (concat "gnome-terminal "
    			 "--working-directory="
    			 (file-truename default-directory)
    			 )))
    ;; (global-set-key (kbd "<f5>") 'yt/open-terminal)

use swiper to replace default ``isearch``

.. code:: common-lisp

    (global-set-key "\C-s" 'swiper)


use snakemake-mode for smake file.

.. code:: common-lisp

    (add-to-list 'auto-mode-alist '("sfile" . snakemake-mode))


.. code:: common-lisp

    (defun yt/sh-chunk-args ()
    (interactive)
    (replace-string " -" " \\\n -")
    )

insert git sha1 value into current point.

.. code:: common-lisp

    (defun yt/insert-git-hash-value ()
      (interactive)
      (insert (shell-command-to-string (concat "git rev-parse HEAD"))))
    (global-set-key (kbd "<f9> s") 'yt/insert-git-hash-value)
