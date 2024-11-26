(use-package hydra)
(defhydra hydra-file-management (:color red
                                        :hint nil)
  "
_o_pen file
_O_pen file as Sudo user 
copy file _P_ath to kill ring
copy file _p_ath relative to project dir to kill ring
_r_ename buffer-visiting file 
_d_elete buffer-visiting file
open with _e_xternal application
_g_it sync"
  ("o" find-file)
  ("O" yt/sudo-find-file)
  ("P" yt/copy-full-path-to-kill-ring)
  ("p" yt/copy-rel-path-to-kill-ring)  
  ("r" yt/rename-current-buffer-file)
  ("c" yt/copy-file-to)
  ("d" yt/delete-this-buffer-and-file)
  ("e" prelude-open-with)
  ("g" yt/git-up))
(global-set-key [f3] 'hydra-file-management/body)

(defhydra yt-hydra/help (:color blue :hint nil)
  "
_f_unction: Documentation for a function
_v_ariable: Documentation for a variable
_i_nfo: info mode 
_d_ictionary: search meaning of a word
_V_ocabulary: add to or visit vocablary
"
  ("f" describe-function)
  ("v" describe-variable)
  ("d" osx-dictionary-search-input)
  ;; ("s" get-synonyms)
  ("i" helm-info)
  ;; ("G" helm-google-suggest)
  ("s" synosaurus-lookup)
  ;; ("d" voca-builder/search-popup)
  ("V" yt/add-vocabulary)
  )
(global-set-key (kbd "<f1>") 'yt-hydra/help/body)

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
