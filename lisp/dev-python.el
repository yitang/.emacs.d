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
    (add-hook 'python-mode-hook 'lsp)
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\][~]workflow")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\][~]storage")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\][.]wandb")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\][.]snakemake")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\][~]wandb")
   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\][~]configs"))
  (use-package lsp-ui)

 ;; (use-package lsp-jedi
 ;;   :config
 ;;   (with-eval-after-load "lsp-mode"
 ;;     (add-to-list 'lsp-disabled-clients 'pyls)
 ;;     (add-to-list 'lsp-enabled-clients 'jedi)))



 (use-package lsp-pyright
     :hook (python-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp))))

 ;; additional add-ons
 ;; use consult/ivy to search symbols.
 (use-package consult-lsp)

(use-package sphinx-doc
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

;; see https://dev.to/fredericlepied/emacs-how-to-switch-from-modulepy-to-testmodulepy-67k
(defun py-module (fname)
  (list (concat
         (match-string 1 (file-name-nondirectory fname))
         ".py")) )

(defun py-test (fname)
  (list (concat "test_"
                (match-string 1 (file-name-nondirectory fname))
                ".py")) )

(add-hook 'python-mode-hook
          (function
           (lambda()
             (setq
	      ;; setup below in .dir-locals.el for each project.
	      ;; ff-search-directories '("~/.tmp/src/*" "~/.tmp/tests/*")
	      ff-other-file-alist
              '(("test_\\(.*\\)\\.py$" py-module)
                ("\\(.*\\)\\.py$" py-test))
              ))))

(use-package python-pytest
 :custom
 (python-pytest-confirm t))
