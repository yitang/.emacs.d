=====
Hydra
=====




.. code:: common-lisp

    (defhydra yt-hydra/help (:color blue :hint nil)
      "
    _f_unction: Documentation for a function
    _v_ariable: Documentation for a variable
    _i_nfo: info mode 
    _G_oogle: search google 
    _d_ictionary: search meaning of a word"
      ("f" describe-function)
      ("v" describe-variable)
      ("i" helm-info-org)
      ("G" helm-google-suggest)
      ("d" voca-builder/search-popup))
    (global-set-key (kbd "<f1>") 'yt-hydra/help/body)

hydra 

.. code:: common-lisp

    (require 'hydra)

    (defhydra hydra-search (:color blue
    			       :hint nil)
      "
    Current Buffer   : _i_search helm-_s_woop _a_ce-jump-word 
    Multiple Buffers : helm-multi-_S_woop 
    Project Directory: projectile-_g_rep  helm-projectile-_G_rep
    "
      ("i" isearch-forward)
      ("s" helm-swoop)
      ("a" ace-jump-word-mode)
      ("S" helm-multi-swoop)
      ("g" projectile-grep)
      ("G" helm-projectile-grep))
    (global-set-key [f5] 'hydra-search/body)
