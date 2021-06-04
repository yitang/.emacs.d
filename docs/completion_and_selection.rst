========================
Completion and Selection
========================




Helm - Fuzzy Match
------------------



Helm and fuzzy match makes selection a lot easier.  in 

.. code:: common-lisp

    (require 'helm)
    (require 'helm-config)

    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    ;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
    ;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    ;; (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)

    (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    ;;(global-set-key (kbd "C-x b") 'helm-mini)
    (global-set-key (kbd "M-l") 'helm-mini) 
    (setq helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match    t)
    (global-set-key (kbd "C-c h o") 'helm-occur)
    (global-set-key (kbd "C-h a") 'helm-apropos)
    (setq helm-apropos-fuzzy-match t)
    (setq helm-semantic-fuzzy-match t
          helm-imenu-fuzzy-match    t)

    (helm-autoresize-mode t)
    (defun pl/helm-alive-p ()
      (if (boundp 'helm-alive-p)
          (symbol-value 'helm-alive-p)))
    (add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)
    (helm-mode 1)

    (defun yt/helm-copy-unmarked-to-buffer ()
      (interactive)
      (with-helm-buffer
        (helm-mark-all)
        (cl-loop for cand in (helm-marked-candidates)
    	     do (with-helm-current-buffer
    		  (insert cand "\n")))))
    ;; by default, Cc Ci copy marked to buffer.
    (define-key helm-map (kbd "C-c C-i") 'helm-copy-unmmarked-to-buffer)

    (setq helm-ff-guess-ffap-urls nil)

Multi-Cursor & Helm-swoop  - Multiple Selection
-----------------------------------------------



When refactoring code, I need to rename a variable or function names,
the normal way to do that is via searching and replacing.
``multiple-cursors`` provides function to select all the words/symbols
that is highlighted and then modify all of them at the same time. 


.. code:: common-lisp

    (require 'multiple-cursors)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

    ;; (global-set-key (kbd "C-S-<right>") 'mc/mark-next-like-this)
    ;; (global-set-key (kbd "C-S-<left>") 'mc/mark-previous-like-this)
    ;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    ;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    ;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    ;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
    ;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

I usually use ``multi-cursor`` with ``helm-swoop``, which allows me to search, and then narrow down all
the occurrences in a temporary buffer, and then start to edit. 

.. code:: common-lisp

    (require 'helm-swoop)
    ;; Change the keybinds to whatever you like :)
    ;; (global-set-key (kbd "M-i") 'helm-swoop)
    ;; (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
    ;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
    ;; (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
    (global-set-key (kbd "<C-f1>") 'helm-swoop)
    ;; When doing isearch, hand the word over to helm-swoop
    ;; (define-key isearchp-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    ;; From helm-swoop to helm-multi-swoop-all
    ;; (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
    ;; When doing evil-search, hand the word over to helm-swoop
    ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)
    ;; Save buffer when helm-multi-swoop-edit complete
    (setq helm-multi-swoop-edit-save t)
    ;; If this value is t, split window inside the current window
    (setq helm-swoop-split-with-multiple-windows nil)
    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
    (setq helm-swoop-split-direction 'split-window-vertically)
    ;; If nil, you can slightly boost invoke speed in exchange for text color
    (setq helm-swoop-speed-or-color nil)
    ;; ----------------------------------------------------------------------

ace-jump
--------

Instead of moving into the place I want, ace-jump provides a way to
jump directly to there places, just by pressing 4-5 keys. The places
can be a character, line, or word. Personally I found it is really
efficient to jump to a word when editing. 

.. code:: common-lisp

    (global-set-key (kbd "C-c w") 'ace-jump-word-mode)

Expand-Region - Incremental Selection
-------------------------------------



`expand-region <https://github.com/magnars/expand-region.el>`_ provides smart way of sectioning, by expanding the scope
one at a time. for example, 

::

    S = "A B C"

If the cursor in inside of the quote, I press ``C-=``, everything inside
of the quote is selected, press it again, the quotes are also
selected, press it again, the whole line/region is selected. It saves
a lot of keystrokes in highlighting the area. 

It works well with *smartparens* mode, if I want to apply markup
syntax around a word, I press ``C-=`` to select it, then insert quote or
forward slash, the whole word will be warped inside of quote or
forward flash. 

.. code:: common-lisp

    (require 'expand-region)
    (global-set-key (kbd "C-=") 'er/expand-region)
