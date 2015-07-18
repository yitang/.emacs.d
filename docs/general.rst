General
=======

Remove Keybind
--------------

.. code-block:: scheme

    (global-unset-key (kbd "C-x b"))
    (global-unset-key (kbd "C-x C-b"))

Assorted Pieces
---------------



Define an auto-save feature, the backup files are saved in
*~*.emacs.d/backup/ folder. I've used it several times to recover the
most recent, but un-saved version, after power off. 

.. code-block:: scheme

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
    (require 'guide-key)
    (setq guide-key/guide-key-sequence t) ;; on for all key-bindings 
    (guide-key-mode 1) 

    ;; start auto-complete with emacs
    (require 'auto-complete)
    ;; do default config for auto-complete
    (require 'auto-complete-config)
    (ac-config-default)

I use ``helm-mini`` to navigate between files, which is a lot
convenient and faster than actually locate the file path. 

.. code-block:: scheme

    (recentf-mode 1)
    (setq recentf-max-saved-items 200
          recentf-max-menu-items 15)
    (setq inhibit-startup-message t)        ; Disable startup message

Shows an notication for invalid operations. 

.. code-block:: scheme

    (setq visible-bell t) 

yasnippet is a powerful package that I'd like to explore in the
future, and this stage, I turned if off since it will slow down the
start-up.

.. code-block:: scheme

    ;; (require 'yasnippet)
    ;; (yas-global-mode 1)
    ;; (setq yas-snippet-dirs '("~/git/.emacs.d/my-snippets"
    ;;                          "~/git/.emacs.d/.cask/24.4.2/elpa/yasnippet-20141102.1554/snippets"
    ;;                          "~/git/.emacs.d/.cask/25.0.50.1/elpa/yasnippet-20141102.1554/snippets"))

Window Layout/Navigation
------------------------



Quickly jump between windows using ``ace-window``, I used it frequently and
bind it ``F1``.

.. code-block:: scheme

    (require 'ace-window)
    (global-set-key (kbd "<f1>") 'ace-window)
    (setq aw-scope 'frame)

Instead of equally split the window size, it make a lot sense to have
the current window, the one I am working one, has bigger size. 

.. code-block:: scheme

    (require 'golden-ratio)
    (golden-ratio-mode 1)
    (add-to-list 'golden-ratio-extra-commands 'ace-window) ;; active golden ratio when using ace-window

Some actions will add/remove windows, and sometimes I'd like to cycle
tough the window layout/changes. In the following settings, ``C-c
<left>`` to undo window layout changes, and ``C-c <right>`` to redo.

.. code-block:: scheme

    (winner-mode 1)
    ;; winner-undo -> C-c <left>
    ;; winner-redo -> C-c <right>

I'd like to use two frames, one for doing and logging, and other for
reference/searching. 

.. code-block:: scheme

    (defun yt/ref-frame ()
      (interactive)
      ;;   (frame-parameter (car (frame-list)) 'name)
      (if (eq 1 (length (frame-list)))
          (new-frame '((name . "***********************REFERENCE*******************")))
        nil))
    (global-set-key (kbd "M-`") 'other-frame)

Utilities
---------



.. code-block:: scheme

    ;; Change "yes or no" to "y or n"
    (fset 'yes-or-no-p 'y-or-n-p)

    (defun yt/reload-dot-emacs ()
      "Save the .emacs buffer if needed, then reload .emacs."
      (interactive)
      (let ((dot-emacs "~/.emacs"))
        (and (get-file-buffer dot-emacs)
             (save-buffer (get-file-buffer dot-emacs)))
        (load-file dot-emacs))
      (message "Re-initialized!"))

Use keyfreq package to record the commands I use in Emacs.

.. code-block:: scheme

    (require 'keyfreq)
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1)

System Path/Keyboard
--------------------



Solve the PATH issues for the software installed via Homebrew in OS

X. Uncomment the ``setenv`` for CYGWIN since I am not using Windows any

more. 

.. code-block:: scheme

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

.. code-block:: scheme

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

General Editing
---------------

There are a set of characters that are more likely to occur as a pair,
for example, quote and brackets. *smartparens mode* allows me to
define such set of pairing characters. 

.. code-block:: scheme

    (smartparens-global-mode 1)
    (sp-pair "(" ")" :wrap "C-(")
    ;; |foobar
    ;; hit C-(
    ;; becomes (|foobar)
    (sp-pair "'" nil :actions :rem)

It is a terrible idea to have lines of context that expand the whole
screen, especially nowadays we have wide-screens, which just make it is
hard to read. A well accepted rule is to set the width of lines to 80
characters, and force a logical line breaks. This funcitonality is
called ``auto-fill`` in Emacs, and I can do the filling by call ``fill-paragraph``.

.. code-block:: scheme

    (add-hook 'text-mode-hook 'turn-on-auto-fill) ;; 

Just in case I need to reverse the auto-fill process.

.. code-block:: scheme

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

``savehist`` is an very powerful mode.

.. code-block:: scheme

    (setq savehist-file "~/git/.emacs.d/personal/emacs-history")
    (savehist-mode 1)
