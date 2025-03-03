(defvar jekyll-source-directory (expand-file-name "~/matrix/learning/mywebsite/org/")
  "root directory of org files.")

;; replace those two with jekyll-source-drafts-dir, jekyll-source-post-dir
(defvar jekyll-source-drafts-dir (file-name-concat jekyll-source-directory "_drafts/")
  "Relative path to drafts directory.")
(defvar jekyll-source-posts-dir (file-name-concat jekyll-source-directory  "_posts/")
  "Relative path to posts directory.")


(defvar jekyll-site-dir "~/matrix/learning/mywebsite/blog/"
  "root directory of the Jekyll site.")
;; TODO: remove this variable, use jekyll-sit-post-dir instead
;; (defvar jekyll-publish-dir (concat jekyll-site-dir "_posts/")
;;   "Relative path to posts directory.")
;; (defvar jekyll-assets-dir (file-name-concat jekyll-site-dir "assets/")
;;   "Relative path to assets directory.")

(defvar jekyll-site-posts-dir (file-name-concat jekyll-site-dir "_posts/")
  "Relative path to posts directory.")
(defvar jekyll-site-drafts-dir (file-name-concat jekyll-site-dir "_drafts/")
  "Relative path to posts directory.")
(defvar jekyll-site-assets-dir (file-name-concat jekyll-site-dir "assets/")
  "Relative path to assets directory.")

(defvar jekyll-post-ext ".org"
  "File extension of Jekyll posts.")

(defvar jekyll-post-template
  " 
#+begin_export html
---
layout: post
title: %s
# date: add publish date when ready
excerpt: 
categories:
  -  
tags:
  -
comments: true
---
#+END_export

#+begin_export html
<script type=\"text/javascript\"
    src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\">
</script>
#+end_export

# #+call: GetLastUpdatedDate[:exports none]()[:results org]
#+BIND: org-md-toplevel-hlevel 2
#+TOC: headlines 4



* COMMENT checklist 

- [ ] check title 
- [ ] check gramamr
- [ ] check tags,
- [ ] check dates, publish front matter
- [ ] promote in social media
"
  "Default template for Jekyll posts. %s will be replace by the post title.")

(defun jekyll-yaml-escape (s)
  "Escape a string for YAML."
  (if (or (string-match ":" s)
          (string-match "\"" s))
      (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"")
    s))

(defun blog-draft-post (title) 
  "Create a new Jekyll blog post."
  (interactive "sPost Title: ")
  (let ((draft-file (concat jekyll-source-drafts-dir
                            (jekyll-make-slug title)
                            jekyll-post-ext)))
    (if (file-exists-p draft-file)
        (find-file draft-file)
      (find-file draft-file)
      (insert (format jekyll-post-template (jekyll-yaml-escape title))))))

(defun jekyll-make-slug (s)
  "Turn a string into a slug."
  (replace-regexp-in-string
   " " "-" (downcase
            (replace-regexp-in-string
             "[^A-Za-z0-9 ]" "" s))))

(defun blog-publish-post ()
  "Move a draft post to the posts directory, and rename it so that it
 contains the date."
  (interactive)
  (cond
   ((not (yt/jekyll-is-draft-p))
    (message "This is not a draft post."))
   ((buffer-modified-p)
    (message "Can't publish post; buffer has modifications."))
   (t
    ;; TODO: use a utlity function to for `filename`, find publish file? 
    (let ((filename
           (concat jekyll-source-posts-dir
                   (format-time-string "%Y-%m-%d-")
                   (file-name-nondirectory
                    (buffer-file-name (current-buffer)))))

	  (draft-file-exported (yt/jekyll-find-export))

          (old-point (point)))
      (delete-file draft-file-exported)
      (rename-file (buffer-file-name (current-buffer))
                   filename)
      (kill-buffer nil)
      (find-file filename)
      (set-window-point (selected-window) old-point)))))

(defun yt/jekyll--export-to-md ()
  "export draft/post to markdown"
  (interactive)
  (let* ((export-file (yt/jekyll-find-export)))
    (message "Exporting to %s" export-file)
    (org-export-to-file 'jekyll-md export-file nil nil nil t)))


(defun yt/jekyll--export-to-html ()
  ;; export current posts in org-mode to html.
  (interactive)
  (message "not implemented. use yt/jekyll--export-to-md (markdown) instead."))


(defun yt/jekyll-is-draft-p ()
  "if the file is inside of the draft directory, it is a draft.

can also implmenet as  (file-name-directory (buffer-file-name (current-buffer))) equals to jekyll-source-drafts-dir
"
  (let ((draft-dir  (file-truename jekyll-source-drafts-dir))
	(filepath (file-truename (buffer-file-name))))
    (string-prefix-p draft-dir filepath)))
  

(defun yt/jekyll-find-export ()
  "find the full path to the exported file of the current post."
  (let* ((src-file (file-name-nondirectory (buffer-file-name)))
	 (dest-file (file-name-with-extension src-file ".md")))
    (if (yt/jekyll-is-draft-p)
	(file-name-concat jekyll-site-drafts-dir dest-file)
      (file-name-concat jekyll-site-posts-dir dest-file))))

(transient-define-prefix yt/jekyll ()
  ""
  ["Jekyll Blog"
   [("n" "new draft" blog-draft-post)
    ("p" "publish post" blog-publish-post)
    ("dd" "Dired - drafts (org)" (lambda () (interactive) (find-file jekyll-source-drafts-dir)))
    ("dp" "Dired - posts (org)" (lambda () (interactive) (find-file jekyll-source-posts-dir)))
    ("dP" "Dired - published posts" (lambda () (interactive) (find-file jekyll-site-posts-dir)))
    ("m" "Export as Markdown" yt/jekyll--export-to-md)
    ("i" "Insert Image (Liquid Template)" yt/jekyll-insert-image)
    ;; ("eh" "Export as HTML" yt/jekyll--export-to-html)
    ;; ("u" "Update post title/date" yt/jekyll-update-post-name)
    ("s" "Jekyll Server" (lambda () (interactive) (yt/compile "*jekyll-sever*" "jekyll s  --watch --drafts" jekyll-site-dir)))
    ("S" "Sync website" (lambda () (interactive) (yt/compile "*jekyll-sync*" "bash script.sh" "~/matrix/learning/mywebsite")))
    ]])

(defun my-jekyll-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to ASCII.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (concat
   (format "{%% highlight %s %%} \n%s\n{%% endhighlight %%}"
           (org-element-property :language src-block)
           (org-element-normalize-string
            (org-export-format-code-default src-block info)))))

(require 'ox) ;; because blogging.el loadded first, before org-mode. need to load after org-mode.
(require 'ox-md) ;; 
(org-export-define-derived-backend 'jekyll-html 'html
  :translate-alist '((src-block . my-jekyll-src-block)))

(org-export-define-derived-backend 'jekyll-md 'md
  :translate-alist '((src-block . my-jekyll-src-block)))

(defun org-custom-link-img-follow (path)
  (org-open-file-with-emacs
   (format "../../assets/%s" path)))

(defun org-custom-link-img-export (path desc format)
  (cond
   ((eq format 'html)
    (format "<img src=\"/assets/%s\" alt=\"%s\"/>" path desc))))

(org-add-link-type "img" 'org-custom-link-img-follow 'org-custom-link-img-export)

(defvar  jekyll-insert-image-liquid-template
  " 
{%% include image.html
src=\"/assets/%s\"
caption=\"%s\"
width=450
%%}
"
  "insert image using liquid template.")

(defun yt/jekyll-insert-image (src caption)
  (interactive (list (read-file-name "images to include: " jekyll-site-assets-dir)
		     (read-string "Caption: ")))
  (insert (format jekyll-insert-image-liquid-template (file-name-nondirectory src) caption))
  )

(defun yt/jekyll-copy-to-assets-folder (file)
  "TODO: copy a file to the assets folder. if file not provided,
interactively select file, start with org-download-image-dir.

jekyll-make-slug to normalise the file name. jekyll does not like
_ in the file name.
"
  (interactive (list (read-file-name "file to copy: " org-download-image-dir)))
  (let* ((ext (file-name-extension file ))
	(base (file-name-base file))
	(dest-base (jekyll-make-slug base))
	(dest-file (expand-file-name (file-name-with-extension dest-base ext) jekyll-site-assets-dir)))
    (copy-file file dest-file)
    dest-file))

(defun yt/jekyll-copy-insert-image (file caption)
  (interactive (list (read-file-name "file to copy: " org-download-image-dir)
		     (read-string "Caption: ")))
  (yt/jekyll-insert-image (yt/jekyll-copy-to-assets-folder file) caption)
  )

;; from https://lists.gnu.org/archive/html/emacs-orgmode/2009-08/msg00460.html
;; magit link in org-mode 
(defun org-magit-store-link ()
  "Store a link to a directory to open with magit."
  (when (eq major-mode 'magit-mode)
    (let* ((dir default-directory)
           (link (org-make-link "magit:" dir))
           (desc (abbreviate-file-name dir)))
      (org-store-link-props :type "magit" :link link :description desc)
      link)))
(defun org-magit-open (dir)
  "Follow a magit link to DIR."
  (magit-status dir))
(org-add-link-type "magit" 'org-magit-open nil)
(add-hook 'org-store-link-functions 'org-magit-store-link)

(defun org-jekyll-post-link-follow (path)
  (org-open-file-with-emacs path))

(defun org-jekyll-post-link-export (path desc format)
  (cond
   ((eq format 'html)
    (format "<a href=\"{%% post_url %s %%}\">%s</a>" path desc))))

(org-add-link-type "jekyll-post" 'org-jekyll-post-link-follow 'org-jekyll-post-link-export)

(setq org-export-allow-bind-keywords t)

(defun yt/jekyll-update-post-title-and-date ()
  "it update the post filename with a new title and today's date.

it also update the font matter."
  (interactive)
  (let* ((title (read-string "new title: "))
	 (ext (file-name-extension (buffer-file-name)))  ;; as of now, the ext is always .org.

	 ;; the new filename is in the format of {date}-{new-title}.org
	 (filename (concat
		    (format-time-string "%Y-%m-%d-")
		    (file-name-with-extension (jekyll-make-slug title) ext)))

	 ;; normalise the filename. 
	 (filename (expand-file-name filename))

	 ;; keep the current point which we will go back to after editing.
	 (old-point (point))
	 )
    (rename-file (buffer-file-name) filename) ;; update the filename
    (kill-buffer nil)  ;; kill the current buffer, i.e. the old file.
    (find-file filename)  ;; open the new file.
    (set-window-point (selected-window) old-point)  ;; set the cursor to where i was in the old file.

    ;; udpate title field. 
    ;; note jekyll-yaml-escape is called to ensure the title field is yaml friendly.
    (yt/jekyll-update-frontmatter--title (jekyll-yaml-escape title))    
    )
  
  )

(defun yt/jekyll-update-frontmatter--title (title)
  "Update the title field in the front matter.

title case is used. 
"
  (let* ((old-point (point)))

    ;; ensure expand all the code/headers/drawers before editing.
    (org-show-all)

    ;; go to the first occurence of 'title:'.
    (goto-char (point-min))
    (search-forward "title: ")

    ;; update the title field with the new title.
    (beginning-of-line)
    (kill-line)
    (insert (format "title: %s" title))

    ;; ensure the title is in title case
    (xah-title-case-region-or-line (+ (line-beginning-position) 7) (line-end-position))

    ;; save and reset cursor back to where it started.
    (save-buffer)    
    (goto-char old-point)
    ))

(defun yt/jekyll--create-or-update-custom_id-field ()
  "so that the CUSTOM_ID property is the same as the headline and 
the URL reflects the headline.

by default, the URL to a section will be a random number."
  (org-entry-put nil "CUSTOM_ID" (org-entry-get nil "ITEM"))
  )

(defun yt/jekyll--create-or-update-custom_id-field-buffer (backend)
  (when (eq backend 'jekyll-md)
    (org-map-entries 'yt/jekyll--create-or-update-custom_id-field)
    ))

(add-hook 'org-export-before-processing-functions 'yt/jekyll--create-or-update-custom_id-field-buffer)
