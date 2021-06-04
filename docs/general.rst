=======
General
=======


Utilities
---------



Firstly, define a function for reloading Emacs configuration, need
this function in debugging this configuration file.

.. code:: common-lisp

    (defun yt/reload-dot-emacs ()
      "Save the .emacs buffer if needed, then reload .emacs."
      (interactive)
      (let ((dot-emacs "~/.emacs"))
        (and (get-file-buffer dot-emacs)
    	 (save-buffer (get-file-buffer dot-emacs)))
        (load-file dot-emacs))
      (message "Re-initialized!"))
    (setq confirm-kill-emacs 'y-or-n-p)

Life is too short to type "yes" or "no". 'y' or 'n' is enough.

.. code:: common-lisp

    (fset 'yes-or-no-p 'y-or-n-p)

Remove Keybind
--------------

.. code:: common-lisp

    (global-unset-key (kbd "C-x b"))
    (global-unset-key (kbd "C-x C-b"))
    (global-unset-key (kbd "C-x C-c"))  ;; save-buffers-kill-terminal
    (global-unset-key (kbd "C-x o"))  ;; other window. replace by f2 - ace-window.

Assorted Pieces
---------------



Automatically backup buffers/files into the working directory and the
*~*.emacs.d/backup// directory. 

.. code:: common-lisp

    ;; ref: http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
    ;; save all backup files (foo~) to this directory.
    (setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
          backup-by-copying t    ; Don't delink hardlinks
          version-control t      ; Use version numbers on backups
          delete-old-versions t  ; Automatically delete excess backups
          kept-new-versions 20   ; how many of the newest versions to keep
          kept-old-versions 5    ; and how many of the old
          auto-save-timeout 20   ; number of seconds idle time before auto-save (default: 30)
          auto-save-interval 200 ; number of keystrokes between auto-saves (default: 300)
          )

    ;; guide-key package 
    ;; (require 'guide-key)
    ;; (setq guide-key/guide-key-sequence t) ;; on for all key-bindings 
    ;; (guide-key-mode 1) 

    ;; use company for all except few modes
    (require 'company)
    (add-hook 'after-init-hook 'global-company-mode)
    ;; Don't enable company-mode in below major modes, OPTIONAL
    (setq company-global-modes '(not eshell-mode comint-mode erc-mode rcirc-mode))

    ;; config company mode
    (setq company-selection-wrap-around t
          company-tooltip-align-annotations t
          company-idle-delay 0.36
          company-minimum-prefix-length 2
          company-tooltip-limit 10)

    (setq company-ddabbrev-code-everywhere t)
    (setq company-dabbrev-code-modes t)
    (setq company-dabbrev-code-other-buffers 'all)
    (setq company-dabbrev-ignore-buffers "\\`\\'")
    (setq company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\|_\\|-\\)")

    ;; config company for ESS mode
    (defun yt/ess_company_mode_setup ()
      ;; this is really important. to source vairbales defined in the scripts.
         (make-local-variable 'company-backends)
         (add-to-list 'company-backends 'company-dabbrev-code)
         )
    (add-hook 'ess-mode-hook 'yt/ess_company_mode_setup)


    (defun text-mode-hook-setup ()
      (make-local-variable 'company-backends)
      (add-to-list 'company-backends 'company-ispell)
      ;; (setq company-ispell-dictionary (file-truename "~/git/.emacs.d/english_words.txt"))
      )

    (add-hook 'text-mode-hook 'text-mode-hook-setup)
    (company-quickhelp-mode 1)
    (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)
    (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)

    (setq company-dabbrev-downcase nil)
    (setq company-show-numbers t)

Configure recent opened files. I use ``helm-mini`` to navigate between
files, which is a lot convenient and faster than actually locate the
file path.

.. code:: common-lisp

    (recentf-mode 1)
    (setq recentf-max-saved-items 200
          recentf-max-menu-items 15)

Shows an notication for invalid operations. 

.. code:: common-lisp

    (setq visible-bell nil) 
    (setq ring-bell-function 'ignore)

Disable startup message

.. code:: common-lisp

    (setq inhibit-startup-message t)        

yasnippet is a powerful package that I'd like to explore in the
future, and this stage, I turned if off since it will slow down the
start-up.

.. code:: common-lisp

    (require 'yasnippet)
    (yas/global-mode 1)
    (add-to-list 'yas/snippet-dirs "~/git/.emacs.d/snippets" t)
    (yas/reload-all)

Window Layout/Navigation
------------------------



Quickly jump between windows using ``ace-window``, I used it frequently and
bind it ``F2``.

.. code:: common-lisp

    (require 'ace-window)
    (global-set-key (kbd "<f2>") 'ace-window)
    (global-set-key (kbd "M-o") 'ace-window)
    (setq aw-scope 'frame)

Instead of equally split the window size, it make a lot sense to have
the current window, the one I am working one, has bigger size. 

.. code:: common-lisp

    (require 'golden-ratio)
    (golden-ratio-mode 1)
    (add-to-list 'golden-ratio-extra-commands 'ace-window) ;; active golden ratio when using ace-window

Some actions will add/remove windows, and sometimes I'd like to cycle
tough the window layout/changes. In the following settings, ``C-c <left>`` to undo window layout changes, and ``C-c <right>`` to redo.

.. code:: common-lisp

    (winner-mode 1)
    ;; winner-undo -> C-c <left>
    ;; winner-redo -> C-c <right>

I'd like to use two frames, one for doing and logging, and other for
reference/searching. 

.. code:: common-lisp

    (defun yt/ref-frame ()
      (interactive)
      ;;   (frame-parameter (car (frame-list)) 'name)
      (if (eq 1 (length (frame-list)))
          (new-frame '((name . "***********************REFERENCE*******************")))
        nil))
    (global-set-key (kbd "M-`") 'other-frame)

System Path/Keyboard
--------------------



Solve the PATH issues for the software installed via Homebrew in OS

X. Uncomment the ``setenv`` for CYGWIN since I am not using Windows any

more. 

.. code:: common-lisp

    (defun set-exec-path-from-shell-PATH ()
      (let ((path-from-shell 
    	 (replace-regexp-in-string "[[:space:]\n]*$" "" 
    				   (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
        (setenv "PATH" path-from-shell)
        (setq exec-path (split-string path-from-shell path-separator))))
    (when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))
    ;; windows path convention
    ;; (setenv "CYGWIN" "nodosfilewarning")

Modify the Mac keyboard: unset the C-z just in case I run Emacs in
terminal and C-z won't stop the program without asking. 

.. code:: common-lisp

    ;; modify mac keyboard 
    (cond ((eq system-type 'darwin)
           (setq mac-command-modifier 'meta)
           (fset 'insertPound "#")
           (global-set-key (kbd "M-3") 'insertPound)       
           (global-unset-key (kbd "M-`"))
           (global-set-key (kbd "M-`") 'other-frame)
           (global-set-key (kbd "C-Z") nil)
           ))

    (prefer-coding-system 'utf-8)
    (when (display-graphic-p)
      (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

Open PDF files using external program.

[2016-06-20 Mon 21:43]  ``helm-find-files`` has open with default tool
functionality. This block is no longer needed.

.. code:: common-lisp

    ;; (require 'openwith)
    ;; (openwith-mode t)
    ;; (if (string= system-type "darwin")
    ;;     (setq openwith-associations '(("\\.pdf\\'" "Skim" (file))))
    ;;   (setq openwith-associations '(("\\.pdf\\'" "evince" (file)))))

General Editing
---------------

There are a set of characters that are more likely to occur as a pair,
for example, quote and brackets. *smartparens mode* allows me to
define such set of pairing characters. 

.. code:: common-lisp

    (smartparens-global-mode 1)
    (sp-pair "(" ")" :wrap "C-(")
    ;; |foobar
    ;; hit C-(
    ;; becomes (|foobar)
    (sp-pair "'" nil :actions :rem)

Modern display is widen. Like many of the Emacs users, I prefer to
have the text wrapper inside a small region rather than have a stretch
across the whole screen. It's easier to read in this way. 

A well accepted rule is to set the width of lines to 80 characters,
and force a logical line breaks. This funcitonality is called
``auto-fill`` in Emacs, and I can do the filling by call
``fill-paragraph``.

.. code:: common-lisp

    (add-hook 'text-mode-hook 'turn-on-auto-fill) ;; 

Just in case I need to reverse the auto-fill process.

[2016-06-20 Mon 21:47]  Can't remember when was the last time I use
unfill. This snippet is not long used.

.. code:: common-lisp

    (defun yt/unfill-paragraph ()
      (interactive)
      (let ((fill-column (point-max)))
        (fill-paragraph nil)))
    (defun yt/unfill-region ()
      (interactive)
      (let ((fill-column (point-max)))
        (fill-region (region-beginning) (region-end) nil)))

Minibuffer history
------------------

Let Emacs remember what I've typed, so I don't need to tediously type
the whole thing. Most of the time, I could just select using ``helm``.

.. code:: common-lisp

    (setq savehist-file "~/git/.emacs.d/local/emacs-history")
    (savehist-mode 1)
