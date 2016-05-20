(require 'org)
(setq org-confirm-babel-evaluate nil)  ;; evaluate src block without confirmation 

;;;;;;; [2015-01-22 Thu 21:27]
(defvar endless/init.org-message-depth 5
  "What depth of init.org headers to message at startup.")

(with-temp-buffer
  (insert-file "~/git/.emacs.d/init.org")
  (goto-char (point-min))

  ;; ;; org babels 
  ;; (search-forward "\n* Babel Library")
  ;; (org-copy-subtree)
  ;; (let ((tmp-file (make-temp-file "tmp")))
  ;;   (with-temp-file tmp-file (yank))
  ;;   (org-babel-lob-ingest tmp-file))

  ;; emacs lisp functions 
  (goto-char (point-min))
  (search-forward "\n* Emacs Configuration")
  (while (not (eobp))
    (forward-line 1)
    (cond
     ;; Report Headers
     ((looking-at
       (format "\\*\\{2,%s\\} +.*$" 
               endless/init.org-message-depth))
      (message "%s" (match-string 0)))
     ;; (message (format (current-time-string))))
     ;; Evaluate Code Blocks
     ((looking-at "[\s]*\\#\\+begin_src\semacs-lisp")
      ;; ((looking-at "#\\+BEGIN_SRC +emacs-lisp.*$")
      ;; ((looking-at "^#\\+BEGIN_SRC +.*$")
      (org-babel-execute-src-block))
     ;; Finish on the next level-1 header
     ((looking-at "^\\* End")
      (goto-char (point-max))))))
