Refile
======

voca-builde 

.. code-block:: scheme

    (require 'voca-builder)
    (setq voca-builder/voca-file "~/git/org/vocabulary.org")
    (setq voca-builder/current-tag "General")
    (global-set-key (kbd "<f4>") 'voca-builder/search-popup)
    (setq sentence-end-double-space nil)


weather forecasting 

.. code-block:: scheme

    ;; sunshine: weather forcaste service
    (setq sunshine-units 'metric)
    (setq sunshine-location "Keighley, GB")

Add ad wrapper function ``yt/qs-bakcup-log`` for auto\ :sub:`bakc.el`\. call this
function at the end of the day.

.. code-block:: scheme

    (defun yt/qs-backup-log ()
      (interactive)
      (load "~/git/qs/auto_back.el"))
