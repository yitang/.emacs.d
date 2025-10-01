(use-package pyvenv)
(add-hook 'python-mode-hook 'flyspell-prog-mode)
;; (add-hook 'python-mode-hook 'elpy-mode)
(setq python-fill-docstring-style 'django)

;; (use-package elpy
;;   :ensure t
;;   :init)

;; (elpy-enable)
;; ;; ;; (elpy-use-ipython "ipython3")
;; ;; (setq elpy-rpc-python-command "python3")
;; ;; (global-set-key (kbd "M-*") 'pop-tag-mark)
;; ;; ;; (setq elpy-test-discover-runner-command '("python3" "-m" "unittest"))
;; ;; (setq elpy-test-pytest-runner-command '("py.test" "--maxfail=100" "-s"))
;; ;; (setq elpy-rpc-backend "jedi")

;; ;; make elpy more like ESS
;; (define-key elpy-mode-map (kbd "<C-return>") 'elpy-shell-send-statement-and-step)
;; (define-key elpy-mode-map (kbd "<C-c C-f>") 'python-shell-send-defun)
;; (define-key elpy-mode-map (kbd "<C-c C-b>") 'elpy-shell-send-region-or-buffer)

;; ;; for new elpy version
;; (setq elpy-shell-starting-directory 'project-root)  ;; set to project directory.
;; (setq elpy-rpc-virtualenv-path 'current)   ;; rpc is in the python dev env

(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(require 'python)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

(setq elpy-dedicated-shells nil)   ; Ensure no conflict with dedicated shells

(defvar elpy-shell-python-shell-names '("Python")
	  "List of existing python shell names.")

;; (define-key elpy-mode-map (kbd "C-c C-s") 'elpy-shell-set-local-shell)

;; this requires pip install -U jedi-language-server
(use-package lsp-mode
  :config
  (add-hook 'python-mode-hook 'lsp))
;; (use-package company-lsp)
(use-package lsp-ui)

(add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.wandb\\'")
(add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\wandb\\'")

;; (use-package lsp-jedi
;;   :ensure t
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-disabled-clients 'pyls)
;;     (add-to-list 'lsp-enabled-clients 'jedi)))



(use-package lsp-pyright
		:ensure t
		:hook (python-mode . (lambda ()
				       (require 'lsp-pyright)
				       (lsp))))  ; or lsp-deferred

;; additional add-ons
;; use consult/ivy to search symbols.
(use-package consult-lsp
  :ensure t)

(use-package lsp-ivy
  :ensure t)

(use-package sphinx-doc
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'sphinx-doc)
                         (sphinx-doc-mode t))))

(cl-defstruct sphinx-doc-doc
  (summary "FIXME: briefly describe function") ; summary line that fits on the first line
  before-fields                                ; list of comments before fields
  after-fields                                 ; list of comments after fields
  fields)                                      ; list of field objects

(use-package jupyter)

;; (define-key elpy-mode-map (kbd "<C-return>") 'elpy-shell-send-statement-and-step)
;; (define-key elpy-mode-map (kbd "<C-c C-f>") 'python-shell-send-defun)
;; (define-key elpy-mode-map (kbd "<C-c C-b>") 'elpy-shell-send-region-or-buffer)

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim)
  :config
  ;; better to install the black and macchiato on the system level
  (setq python-black-command "~/.uv_venv/train_llm/bin/black")
  (setq python-black-macchiato-command "~/.uv_venv/train_llm/bin/black-macchiato")
  )
