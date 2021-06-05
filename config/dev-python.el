(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-hook 'python-mode-hook 'elpy-mode)
(setq python-fill-docstring-style 'django)
(require 'elpy)
(elpy-enable)
;; (elpy-use-ipython "ipython3")
(setq elpy-rpc-python-command "python3")
(global-set-key (kbd "M-*") 'pop-tag-mark)
(setq elpy-test-discover-runner-command '("python3" "-m" "unittest"))
(setq elpy-rpc-backend "jedi")

;; make elpy more like ESS
(define-key elpy-mode-map (kbd "<C-return>") 'elpy-shell-send-statement-and-step)
(define-key elpy-mode-map (kbd "<C-c C-f>") 'python-shell-send-defun)
(define-key elpy-mode-map (kbd "<C-c C-b>") 'elpy-shell-send-region-or-buffer)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt --pprint")

;; (setq python-shell-interpreter "ipython3"
;;       python-shell-interpreter-args "-i")

;; for new elpy version
(setq elpy-shell-starting-directory 'current-directory)
(setq elpy-rpc-virtualenv-path 'current)

;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")

(setq elpy-dedicated-shells nil)   ; Ensure no conflict with dedicated shells

(defvar elpy-shell-python-shell-names '("Python")
      "List of existing python shell names.")

;; (define-key elpy-mode-map (kbd "C-c C-s") 'elpy-shell-set-local-shell)

(cl-defstruct sphinx-doc-doc
  (summary "FIXME: briefly describe function") ; summary line that fits on the first line
  before-fields                                ; list of comments before fields
  after-fields                                 ; list of comments after fields
  fields)                                      ; list of field objects

(add-hook 'python-mode-hook (lambda ()
                              ;; (require 'sphinx-doc)
                              (sphinx-doc-mode t)))
