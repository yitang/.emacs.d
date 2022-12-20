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
  ("d" osx-dictionary-search-word-at-point)
  ("s" get-synonyms)
  ;; ("s" synosaurus-lookup)
  ;; ("d" voca-builder/search-popup)
  )
(global-set-key (kbd "<f1>") 'yt-hydra/help/body)

(use-package hydra)

(defhydra hydra-search (:color blue
                               :hint nil)
  "
Current Buffer   : _i_search helm-_s_woop _a_ce-jump-word 
Multiple Buffers : helm-multi-_S_woop 
Project Directory: projectile-_g_rep  helm-projectile-_G_rep
Python: elpy rgrep _p_ython project
"
  ("i" isearch-forward)
  ("s" helm-swoop)
  ("a" ace-jump-word-mode)
  ("S" helm-multi-swoop)
  ("g" projectile-grep)
  ("G" helm-projectile-grep)
  ("p" elpy-rgrep-symbol)
  )
(global-set-key [f5] 'hydra-search/body)

;; https://github.com/abo-abo/hydra/wiki/Org-clock
(defhydra hydra-org-clock (:color blue :hint nil)
  "
Clock       In/out^     ^Edit^   ^Summary     Doc(_?_)
---------------------------------------------------
            _i_n         _e_dit   
_h_istory   _c_ontinue   _q_uit   _d_isplay
_g_oto      _o_ut        ^ ^      _r_eport
      "
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("c" org-clock-in-last)
  ("e" org-clock-modify-effort-estimate)
  ("q" org-clock-cancel)
  ("g" org-clock-goto)
  ("d" org-clock-display)
  ("r" org-clock-report)
  ;; ("j" org-clock-jump-to-current-clock)
  ("h" yt/org-clock-in-select)
  ("?" (org-info "Clocking commands")))

(global-set-key (kbd "<f11>") 'hydra-org-clock/body)

(defun yt/org-clock-in-select ()
  (interactive)
  (setq current-prefix-arg '(4)) ;; C-u, 
  (call-interactively 'org-clock-in))
