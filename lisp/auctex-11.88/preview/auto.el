
(defvar preview-lispdir (expand-file-name "auctex" (file-name-directory load-file-name)))
(add-to-list 'load-path preview-lispdir)
(defvar preview-datadir (expand-file-name "auctex" (file-name-directory load-file-name)))
(defvar preview-TeX-style-dir (expand-file-name "auctex/latex" (file-name-directory load-file-name)))
