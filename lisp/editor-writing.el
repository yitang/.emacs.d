(if (eq system-type 'darwin)
    (setq ispell-program-name "/opt/homebrew/bin/aspell")   ;; this semes not necessary
  (setq ispell-program-name "/usr/bin/aspell"))
(setq ispell-dictionary "british"
      ispell-extra-args '() ;; TeX mode "-t"
      ispell-silently-savep t)

(setq ispell-personal-dictionary "~/matrix/tools/.emacs.d/local/ispell-dict") ;; add personal dictionary

(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))

(use-package flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(define-key flyspell-mode-map (kbd "C-.") 'helm-flyspell-correct)

;; TODO - cannot get consult-flyspell working
(use-package consult-flyspell
  ;; :straight (consult-flyspell :type git :host gitlab :repo "OlMon/consult-flyspell" :branch "master")
  :config
  ;; default settings
  (setq consult-flyspell-select-function (lambda () (flyspell-correct-at-point) (consult-flyspell))
        consult-flyspell-set-point-after-word t
        consult-flyspell-always-check-buffer nil))

;; check grammar 
(use-package langtool)
(setq langtool-language-tool-jar "~/java/LanguageTool-2.8/languagetool-commandline.jar")
(setq langtool-mother-tongue "en")

(defun my-text-abbrev-expand-p ()
  "Return t if the abbrev is in a text context, which is: in
   comments and strings only when in a prog-mode derived-mode or
   src block in org-mode, and anywhere else."
  (if (or (derived-mode-p 'prog-mode)
          (and (eq major-mode 'org-mode)
               (org-in-src-block-p 'inside)))
      (nth 8 (syntax-ppss))
    t))

(define-abbrev-table 'my-text-abbrev-table ()
  "Abbrev table for text-only abbrevs. Expands only in comments and strings."
  :enable-function #'my-text-abbrev-expand-p)

(dolist (table (list text-mode-abbrev-table
                     prog-mode-abbrev-table))
  (abbrev-table-put table
                    :parents (list my-text-abbrev-table)))

(defun my-text-abbrev-table-init (abbrevs-org-list)
  "Parse 'name: expansion' pairs from an org list and insert into abbrev table."
  (message "Creating text-abbrev table...")
  (dolist (abbrev abbrevs-org-list)
    (let ((name (nth 0 abbrev))
          (expansion (nth 1 abbrev)))
      ;; (print (cons name expansion))
      (define-abbrev my-text-abbrev-table name expansion nil :system t))))
;;(my-text-abbrev-table-init my-text-abbrevs)  ; BUG: only work in org-mode

(defun xah-title-case-region-or-line (φbegin φend)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, φbegin φend are region boundaries.
URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
Version 2015-05-07"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           ξp1
           ξp2
           (ξskipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward ξskipChars (line-beginning-position))
         (setq ξp1 (point))
         (skip-chars-forward ξskipChars (line-end-position))
         (setq ξp2 (point)))
       (list ξp1 ξp2))))
  (let* (
         (ξstrPairs [
                     [" A " " a "]
                     [" And " " and "]
                     [" At " " at "]
                     [" As " " as "]
                     [" By " " by "]
                     [" Be " " be "]
                     [" Into " " into "]
                     [" In " " in "]
                     [" Is " " is "]
                     [" It " " it "]
                     [" For " " for "]
                     [" Of " " of "]
                     [" Or " " or "]
                     [" On " " on "]
                     [" Via " " via "]
                     [" The " " the "]
                     [" That " " that "]
                     [" To " " to "]
                     [" Vs " " vs "]
                     [" With " " with "]
                     [" From " " from "]
                     ["'S " "'s "]
                     ]))
    (save-excursion 
      (save-restriction
        (narrow-to-region φbegin φend)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda (ξx)
             (goto-char (point-min))
             (while
                 (search-forward (aref ξx 0) nil t)
               (replace-match (aref ξx 1) 'FIXEDCASE 'LITERAL)))
           ξstrPairs))))))
