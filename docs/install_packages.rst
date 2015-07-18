install packages
================



.. code-block:: scheme

    (setq my-package-list '(ess
                            ssh
                            auto-complete
                            nyan-mode
                            yasnippet
                            projectile
                            magit
                            helm-swoop
                            nyan-mode
                            org-jekyll
                            org-plus-contrib
                            helm-projectile
                            rainbow-delimiters
                            zenburn-theme
                            htmlize
                            nanowrimo
                            golden-ratio
                            artbollocks-mode
                            langtool
                            flycheck
                            expand-region
                            guide-key
                            exec-path-from-shell
                            smart-mode-line
                            smart-mode-line-powerline-theme
                            powerline
                            synosaurus
                            hydra
                            w3m
                            ace-window
                            calfw
                            multiple-cursors
                            org-download
                            paradox
                            smartparens
                            ace-jump-mode
                            voca-builder
                            org-download
                            gist
                            sunshine
                            keyfreq
                            pretty-mode
                            f
                            olivetti
                            helm-mu
                            ))

    (require 'package)
    (setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                             ("org" . "http://orgmode.org/elpa/")
                             ("gnu" . "http://elpa.gnu.org/packages/")
                             ("marmalade" . "http://marmalade-repo.org/packages/")))
    (package-initialize)

    ;; fetch the list of packages available 
    (unless package-archive-contents
      (package-refresh-contents))
    ;; install 
    (dolist (i-package my-package-list)
      (unless (package-installed-p i-package)
        (package-install i-package)))
