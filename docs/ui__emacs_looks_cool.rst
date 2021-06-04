======================
GUI - Emacs Looks Cool
======================




Fonts
-----



I use the Adobe's *Source Code Pro* font, it is Monospaced font and
claimed to be suitable for coding environments but I use it for all
modes.

.. code:: common-lisp

    ;; (set-frame-font "Source Code Pro-11" nil t)
    ;; (set-face-attribute 'default nil :height 100)

Minimalists GUI
---------------



I never click any buttons in the tool-bar, nor need the scroll-bar to
tell me the cursor position the in the buffer, so I removed all of
them to have minimalist GUI of Emacs. 

Recently I found menu-bar is really useful, it shows commonly used
functions for a particular mode. Occasionally I found something
useful.

.. code:: common-lisp

    (tool-bar-mode -1)
    ; (menu-bar-mode -1)
    (scroll-bar-mode -1)

Theme
-----



I have been using *zenburn* theme for a while. It is a popular low
contrast colour theme and easy on the eye. Occasionally I apply
*tsdh-dark* theme on the top when I really need to focus on.

*leuven* theme is highly customised for org-mode and I like to use it
when my eyes are tired of the *zenburn* theme.

.. code:: common-lisp

    ;; (load-theme 'zenburn t) 
    ;; (load-theme 'leuven t)

Mode Line
---------



The mode line is at the bottom of every Emacs Window aside from
MiniBuffer windows. It has most of the relevant information about the
buffer, including Git status, Major mode, clock info, etc.

The smart-mode-line packages can make mode-line "smart and sexy".
There are many options to tweak.

.. code:: common-lisp

    (setq sml/no-confirm-load-theme t)
    (require 'smart-mode-line)
    (setq powerline-arrow-shape 'curve)
    (setq powerline-default-separator-dir '(right . left))
    (setq sml/theme 'powerline)
    (sml/setup)

There are too much information cluttered at the bottom. I disable the
display of minor modes, there are just too many and almost all are
irrelevant.

.. code:: common-lisp

    (rich-minority-mode 1)
    (setf rm-blacklist "")

This will leave empty spaces which can be removed by 

.. code:: common-lisp

    (setq sml/mode-width 0)
    (setq sml/name-width 20)

Finally, show the current time in the mode-line. 

.. code:: common-lisp

    (display-time-mode)
