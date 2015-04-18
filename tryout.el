(require 'hydra)
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

;;change default browser for 'browse-url'  to w3m
(setq browse-url-browser-function 'w3m-goto-url-new-session)
;;change w3m user-agent to android
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")
;; (setq w3m-user-agent "Emacs-w3m/1.4.540 w3m/0.5.3+debian-15")


(setq sml/mode-width 10)
(setq sml/name-wdith 10)
(olivetti-mode)


;; sunshine: weather forcaste service
(setq sunshine-units 'metric)
(setq sunshine-location "Keighley, GB")
