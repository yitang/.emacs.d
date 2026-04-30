#!/bin/bash

/home/tangyi/bin/emacs30.2/bin/emacs --batch \
				     --init-directory=/home/tangyi/.config/emacs/emacs.d_30.2 \
				     --eval "(require 'org)" --eval "(setq org-babel-lock-file-directory nil)" --eval "(org-babel-tangle-file \"config.org\")"
/home/tangyi/bin/emacs30.2/bin/emacs --batch \
				     --init-directory=/home/tangyi/.config/emacs/emacs.d_30.2 \
				     --eval "(load \"~/.emacs\")"
# emacs --batch \
#   --eval "(load (expand-file-name \"load_config.el\"))" \
#   --eval "(load (expand-file-name \"lisp/Dev-R.el\"))" \
#   --eval "(load (expand-file-name \"lisp/blogging.el\"))" \
#   --eval "(load (expand-file-name \"lisp/dev-python.el\"))" \
#   --eval "(load (expand-file-name \"lisp/editor-auto-completion.el\"))" \
#   --eval "(load (expand-file-name \"lisp/editor-writing.el\"))" \
#   --eval "(load (expand-file-name \"lisp/editor_GUI.el\"))" \
#   --eval "(load (expand-file-name \"lisp/file_management.el\"))" \
#   --eval "(load (expand-file-name \"lisp/general.el\"))" \
#   --eval "(load (expand-file-name \"lisp/hydra.el\"))" \
#   --eval "(load (expand-file-name \"lisp/treemacs.el\"))" \
#   --eval "(kill-emacs 0)" 2>&1
