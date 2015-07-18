Writing in Emacs
================



English Language
----------------



I type quit fast with lots of misspell and in writing, I don't need to
correct every single one when writing, which will stop the flow. I
will do it afterwards in editing, I will press C-, to move the cursor
to next misspelled word, and press ``C-.`` to correcct it, press it
again, to correct it to another words.

.. code-block:: scheme

    ;; check spelling  
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook 'flyspell-mode)
    (setq ispell-dictionary "british"
          ispell-extra-args '() ;; TeX mode "-t"
          ispell-silently-savep t)
    (if (eq system-type 'darwin)
        (setq ispell-program-name "/usr/local/bin/aspell")
      (setq ispell-program-name "/usr/bin/aspell"))
    (setq ispell-personal-dictionary "~/git/.emacs.d/personal/ispell-dict") ;; add personal dictionary 

I need an grammar check to let me know that 

::

    Have you do ...

is wrong, and also tell me to change *do* to *done*, and also why.
``langtool`` can do be the job, but currently I don't understand how to
get it works, so I am not using it anymore.

.. code-block:: scheme

    ;; check grammar 
    (require 'langtool)
    (setq langtool-language-tool-jar "~/java/LanguageTool-2.8/languagetool-commandline.jar")
    (setq langtool-mother-tongue "en")

English is my second language, and I am trying to avoid certain
guarding term in writing. The following snipts I get it from Sachua
will highlight the word like *shuold* or *I think*, which reminds to
confirm with what I am not sure about, and have more confidence in
what I am saying.

.. code-block:: scheme

    (require 'artbollocks-mode)
    (add-hook 'text-mode-hook 'artbollocks-mode)
    (setq artbollocks-weasel-words-regex
          (concat "\\b" (regexp-opt
                         '("should"
                           "just"
                           "sort of"
                           "a lot"
                           "probably"
                           "maybe"
                           "perhaps"
                           "I think"
                           "really"
                           "nice") t) "\\b"))

For about one month, I tried to write at least 500 words per day. I
also set up a special ``write-mode`` that has different color scheldules
that helps me to set the moode.

.. code-block:: scheme

    ;; [2014-12-25 Thu 22:21]
    (defun yt/write-mode ()
      (interactive)
      (hl-sentence-mode)
      (variable-pitch-mode)
      (nanowrimo-mode))

    ;; word count
    ;; https://bitbucket.org/gvol/nanowrimo.el
    (require 'org-wc)
    (require 'nanowrimo)
    (setq nanowrimo-today-goal 500)


    ;; [2014-12-23 Tue 22:06]
    ;; Highlight sentence
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Attribute-Functions.html
    (require 'hl-sentence)
    (add-hook 'nanowrimo-mode 'hl-sentence-mode)
    (set-face-attribute 'hl-sentence-face nil
                        ;; :foreground "black")
                        :foreground "white")
    (add-hook 'nanowrimo-mode 'variable-pitch-mode)
    (set-face-attribute 'variable-pitch nil
                        :foreground "gray40")






add synosaurus

.. code-block:: scheme


    ;; [2015-02-12 Thu 21:14]
    ;; https://github.com/rootzlevel/synosaurus
    ;; synosaurus-lookup
    ;; synosaurus-choose-and-replace
    ;; brew install wordnet
    (require 'synosaurus)
    (setq synosaurus-choose-method "popup")

    ;; synosaurus-lookup C-c s l
    ;; synosaurus-choose-and-replace C-c s r        
    (setq synosaurus-backend 'synosaurus-backend-wordnet)
    (setq synosaurus-choose-method 'popup)

Random Quotes
-------------



If I run out of idea, and I didn't write anything for 1 minutes, Emacs
will pop a random quote that I collected in the echo area. The random
quotes can inspire me sometimes.

.. code-block:: scheme

    (defconst yt/quotes
      '("You can't see paradise, if you don't pedal.  - Chicken Run "
        "He who who says he can and he who says he can’t are both usually right ― Confucius"
        "Why waste time proving over and over how great you are when you could be getting better? - Dweck The Mindset"
        "You’re not a failure until you start to assign blame. - The legendary basketball coach John Wooden"
        "I could hear my heart beating. I could hear everyone's heart. I could hear the human noise we sat there making, not one of us moving, not even when the room went dark. - Raymond Carver"
        "A writer is a sum of their experiences. Go get some - Stuck in Love (2012)"
        "If there is any one secret of success, it lies in the ability to get the other person's point of view and see things from that person's angle as well as from your own. - Henry Ford"
        "People who can put themselves in the place of other people who can understand the workings of their minds, need never worry about what the future has in store for them. - Owen D. Young"
        )
      "Good quotes 
       they can be useful for creative writers as well.")
    (defun yt/show-random-quotes ()
      "Show random quotes to minibuffer"
      (interactive)
      (message "%s"
               (nth (random (length yt/quotes))
                    yt/quotes)))
    (run-with-idle-timer 60 t 'yt/show-random-quotes)

Abbreviation
------------

I have been writing in Emacs/org-mode a lot, have been really tired of capitalise i to I, so I use abbrevitation table. 

.. table::
    :name: my-text-abbrevs

    +-----------+--------------------------------+----------+
    | name      | expand                         | Category |
    +===========+================================+==========+
    | i         | I                              | write    |
    +-----------+--------------------------------+----------+
    | amax      | annual maximum                 | stat     |
    +-----------+--------------------------------+----------+
    | gmap      | google map                     | website  |
    +-----------+--------------------------------+----------+
    | mailme    | yi.tang.uk@me.com              | aboutme  |
    +-----------+--------------------------------+----------+
    | twitterme | @yi\ :sub:`tang`\ \ :sub:`uk`\ | aboutme  |
    +-----------+--------------------------------+----------+
    | eqt       | equivalent to                  | english  |
    +-----------+--------------------------------+----------+
    | iif       | if and only if                 | maths    |
    +-----------+--------------------------------+----------+
    | wrt       | with respect to                | english  |
    +-----------+--------------------------------+----------+

.. code-block:: scheme

    (defun my-text-abbrev-expand-p ()
      "Return t if the abbrev is in a text context, which is: in
     comments and strings only when in a prog-mode derived-mode or
     src block in org-mode, and anywhere else."
      (if (or (derived-mode-p 'prog-mode)
              (and (eq major-mode 'org-mode)
                   (org-in-src-block-p 'inside)))
          (nth 8 (syntax-ppss))
        t))

    (define-abbrev-table 'my-text-abbrev-table ()
      "Abbrev table for text-only abbrevs. Expands only in comments and strings."
      :enable-function #'my-text-abbrev-expand-p)

    (dolist (table (list text-mode-abbrev-table
                         prog-mode-abbrev-table))
      (abbrev-table-put table
                        :parents (list my-text-abbrev-table)))

    (defun my-text-abbrev-table-init (abbrevs-org-list)
      "Parse 'name: expansion' pairs from an org list and insert into abbrev table."
      (message "Creating text-abbrev table...")
      (dolist (abbrev abbrevs-org-list)
        (let ((name (nth 0 abbrev))
              (expansion (nth 1 abbrev)))
          ;; (print (cons name expansion))
          (define-abbrev my-text-abbrev-table name expansion nil :system t))))
    (my-text-abbrev-table-init my-text-abbrevs)

Style
-----
