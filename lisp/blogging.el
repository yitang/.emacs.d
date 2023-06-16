(defvar jekyll-directory (expand-file-name "~/matrix/learning/mywebsite/org/")
  "Path to Jekyll blog.")
(defvar jekyll-drafts-dir "_drafts/"
  "Relative path to drafts directory.")
(defvar jekyll-posts-dir "_posts/"
  "Relative path to posts directory.")
(defvar jekyll-publish-dir "~/matrix/learning/mywebsite/blog/_posts"
  "Relative path to posts directory.")

;; (defvar jekyll-post-ext ".org"
;;   "File extension of Jekyll posts.")

(defvar jekyll-post-template
  " 
#+begin_export html
---
layout: post
title: %s
# date: add publish date when ready
published: false
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

#+TOC: headlines 4
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
  (let ((draft-file (concat jekyll-directory jekyll-drafts-dir
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
   ((not (equal
          (file-name-directory (buffer-file-name (current-buffer)))
          (concat jekyll-directory jekyll-drafts-dir)))
    (message "This is not a draft post."))
   ((buffer-modified-p)
    (message "Can't publish post; buffer has modifications."))
   (t
    (let ((filename
           (concat jekyll-directory jekyll-posts-dir
                   (format-time-string "%Y-%m-%d-")
                   (file-name-nondirectory
                    (buffer-file-name (current-buffer)))))
          (old-point (point)))
      (rename-file (buffer-file-name (current-buffer))
                   filename)
      (kill-buffer nil)
      (find-file filename)
      (set-window-point (selected-window) old-point)))))

(transient-define-prefix yt/jekyll ()
  ""
  ["Jekyll Blog"
   [("n" "new draft" blog-draft-post)
    ("p" "publish post" blog-publish-post)
    ("dd" "Dired - drafts" (lambda ()
                             (interactive)
                             (find-file (expand-file-name jekyll-drafts-dir jekyll-directory))))
    ("dp" "Dired - posts" (lambda ()
                             (interactive)
                             (find-file (expand-file-name jekyll-posts-dir jekyll-directory))))
    ("em" "Export as Markdown" jekyll/export-to-markdown )
    ("eh" "Export as HTML" jekyll/export-to-html)
    ;; ("u" "Update post title/date" yt/jekyll-update-post-name)
     ]])

(defun my-jekyll-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to ASCII.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (concat
   (format "{%% highlight %s %%} \n %s {%% endhighlight %%}"
           (org-element-property :language src-block)
           (org-element-normalize-string
            (org-export-format-code-default src-block info)))))

(require 'ox) ;; why i need this line here? TODO: because blogging.el loadded first, before org-mode. need to load after org-mode.
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
