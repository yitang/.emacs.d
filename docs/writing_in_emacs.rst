================
Writing in Emacs
================




Spell and Grammar
-----------------



Spell checking and correcting are essential in writing. Emacs need
third party program do this. There are a couple of programs and I use
``aspell``. It is part of GNU and can be easily installed in OS X and
Ubuntu. The following snippet tells Emacs where ``aspell`` is installed
and use British dictionary. 

.. code:: common-lisp

    (if (eq system-type 'darwin)
        (setq ispell-program-name "/usr/local/bin/aspell")
      (setq ispell-program-name "/usr/bin/aspell"))
    (setq ispell-dictionary "british"
          ispell-extra-args '() ;; TeX mode "-t"
          ispell-silently-savep t)

I have a personal spelling dictionary, most are abbreviations and
jargon. I can tell aspell that they are not misspellings. 

.. code:: common-lisp

    (setq ispell-personal-dictionary "~/git/.emacs.d/local/ispell-dict") ;; add personal dictionary 


.. code:: common-lisp

    (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
    (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))

``Flyspell`` depends on ispell mode and enables on-the-fly spell
checking/correcting. I enable the flyspell mode for text-mode and
org-mode. 

By default, I use C-, to move the cursor to the next misspelled word,
and ``flycheck`` will provide a list of candidates for
auto-correlection. I press ``C-.`` select the first one, and press it
again to select the next one.

.. code:: common-lisp

    (require 'flyspell)
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook 'flyspell-mode)
    (define-key flyspell-mode-map (kbd "C-.") 'helm-flyspell-correct)

I need an grammar check to let me know that 

::

    Have you do ...

is wrong, and also tell me to change *do* to *done*, and also why.
``langtool`` can do be the job, but currently I don't understand how to
get it works, so I am not using it anymore.

.. code:: common-lisp

    ;; check grammar 
    (require 'langtool)
    (setq langtool-language-tool-jar "~/java/LanguageTool-2.8/languagetool-commandline.jar")
    (setq langtool-mother-tongue "en")

Abbreviation
------------

I have been writing in Emacs/org-mode a lot, have been really tired of
capitalise i to I, so I use abbrevitation table.

.. table::
    :name: my-text-abbrevs

    +-----------+-------------------+----------+
    | name      | expand            | Category |
    +===========+===================+==========+
    | i         | I                 | write    |
    +-----------+-------------------+----------+
    | amax      | annual maximum    | stat     |
    +-----------+-------------------+----------+
    | gmap      | google map        | website  |
    +-----------+-------------------+----------+
    | mailme    | yi.tang.uk@me.com | aboutme  |
    +-----------+-------------------+----------+
    | twitterme | @yi\_tang\_uk     | aboutme  |
    +-----------+-------------------+----------+
    | eqt       | equivalent to     | english  |
    +-----------+-------------------+----------+
    | iif       | if and only if    | maths    |
    +-----------+-------------------+----------+
    | wrt       | with respect to   | English  |
    +-----------+-------------------+----------+
    | st        | such that         | English  |
    +-----------+-------------------+----------+
    | d/n       | distribution      | Stats    |
    +-----------+-------------------+----------+
    | obs       | observation       | stats    |
    +-----------+-------------------+----------+
    | obss      | observations      | stats    |
    +-----------+-------------------+----------+

.. code:: common-lisp

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



English is my second language, and I am trying to avoid certain
guarding term in writing. The following snipts I get it from Sachua
will highlight the word like *shuold* or *I think*, which reminds to
confirm with what I am not sure about, and have more confidence in
what I am saying.

.. code:: common-lisp

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

add synosaurus

.. code:: common-lisp


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
