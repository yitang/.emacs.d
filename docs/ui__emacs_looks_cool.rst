UI - Emacs Looks Cool
=====================



I never click any buttons in the menu-bar/tool-bar, nor need the
scroll-bar to tell me the cursor position the in the buffer, so I removed all of
them to have minimalist GUI of Emacs. 

.. code-block:: scheme

    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)

Mode-line contains most of the relevant information about the buffer,
Git status, Major mode, clock info, and current time. I disable the
display of minor modes, there are just too many and almost all are irrelevant.

.. code-block:: scheme

    (display-time-mode)
    (require 'smart-mode-line)
    (setq powerline-arrow-shape 'curve)
    (setq powerline-default-separator-dir '(right . left))
    (setq sml/theme 'powerline)
    (setq sml/mode-width 0)
    (setq sml/name-width 20)
    (rich-minority-mode 1)
    (setf rm-blacklist "")
    (sml/setup)


I use the Adobe's *Source Code Pro* font, it is Monospaced font and
claimed to be suitable for coding environments. But I use it for all modes.

.. code-block:: scheme

    (set-default-font "Source Code Pro" nil t)
    (set-face-attribute 'default nil :height 100)

I have been using *zenburn* theme for a while. It is a popular low contrast
colour theme and easy on the eye. Occasionally I apply *tsdh-dark*
theme on the top when I really need to focus on.

.. code-block:: scheme

    ;; (when window-system
    ;;   (load-theme 'zenburn t))
