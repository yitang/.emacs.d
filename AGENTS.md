# Emacs Config — Persistent Rules

## Source of Truth
- config.org is the authoritative source; lisp/*.el files are generated exports
- Edit config.org source blocks only, then tangle to regenerate .el files
- Cask is removed; use-package-always-ensure t is set in load_config.el
- config.org has `#+OPTIONS: skip:nil d:(HIDE)` and default header-args `:tangle yes :results silent :eval never_export`
- Org headings with `:EXPORT_FILE_NAME` property control which `lisp/<name>.el` they tangle to
- load_config.el provides `yt/load-directory` for manual loading of all 13 tangled .el files

## Workflow
- Group related use-package declarations and their tangled config code
  in the same #+begin_src block to guarantee load order
- Use with-eval-after-load for keymap modifications
- Use use-package :config section to defer package initialization
- transient-define-prefix requires transient to be loaded first; co-locating
  declarations in a single src block fixes void-function errors
- lsp-file-watch-ignored-directories is void before lsp-mode loads; move
  add-to-list calls inside the :config block

## Environment
- GNU Emacs 30.0.92
- must invoke Emacs with: ~/bin/emacs30.2/bin/emacs --init-directory=~/.config/emacs/emacs.d_30.2
- Package directory: ~/.config/emacs/emacs.d_30.2
- Load test: run test_load.sh (tangles config.org then loads ~/.emacs in batch mode)

## Project Structure
- .dir-locals.el: sets org-roam-directory (notes/), org-roam-db-location, projectile-project-root, org-download-image-dir (assets/org-download)
- .gitignore: excludes personal/, .cask/, snippets/org-mode/weekly_review_tree, and local/ (except local/readme.org)
- lisp/: 13 tangled .el exports generated from config.org
- notes/: org-roam notes directory
- assets/org-download/: org-download asset directory
