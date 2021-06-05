(defvar jekyll-directory (expand-file-name "~/git/myblog/org/")
  "Path to Jekyll blog.")
(defvar jekyll-drafts-dir "_drafts/"
  "Relative path to drafts directory.")
(defvar jekyll-posts-dir "_posts/"
  "Relative path to posts directory.")
(defvar jekyll-post-ext ".org"
  "File extension of Jekyll posts.")

(global-set-key (kbd "C-c j p") (lambda ()
                                  (interactive)
                                  (find-file "~/git/myblog/org/_posts/")))
(global-set-key (kbd "C-c j d") (lambda ()
                                  (interactive)
                                  (find-file "~/git/myblog/org/_drafts/")))

(defvar jekyll-post-template
  " 
#+begin_export html
---
layout: post
title: %s
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
        2  (string-match "\"" s))
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

(global-set-key (kbd "C-c j n") 'blog-draft-post)
(global-set-key (kbd "C-c j P") 'blog-publish-post)

;; ref: http://cute-jumper.github.io/emacs/2013/10/06/orgmode-to-github-pages-with-jekyll/
(setq org-publish-project-alist
      '(("myblog"
         :base-directory "~/git/myblog/org"
         :base-extension "org"
         :publishing-directory "~/git/myblog"
         :recursive t
         :publishing-function org-html-publish-to-html
         :with-toc nil
         :headline-levels 4
         :section-numbers nil
         :auto-preamble nil
         :auto-sitemap nil
         :html-extension "html"
         :htmlized-source t
         :body-only t
         :with-toc nil
         )))

(defvar jekyll-directory (expand-file-name "~/git/myblog/org/")
  "Path to Jekyll blog.")
(defvar jekyll-drafts-dir "_drafts/"
  "Relative path to drafts directory.")
(defvar jekyll-posts-dir "_posts/"
  "Relative path to posts directory.")
(defvar jekyll-post-ext ".org"
  "File extension of Jekyll posts.")

(defvar jekyll-post-template
  " 
#+begin_export html
---
layout: post
title: %s
excerpt: 
categories:
  -  
tags:
  -
comments: true 
---
#+END_export

# #+call: GetLastUpdatedDate[:exports none]()[:results org]

#+TOC: headlines 4
"
    "Default template for Jekyll posts. %s will be replace by the post title.")

(defun jekyll-make-slug (s)
  "Turn a string into a slug."
  (replace-regexp-in-string
   " " "-" (downcase
            (replace-regexp-in-string
             "[^A-Za-z0-9 ]" "" s))))

(defun jekyll-yaml-escape (s)
  "Escape a string for YAML."
  (if (or (string-match ":" s)
        2  (string-match "\"" s))
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


;; Improve our blogging experience with Org-Jekyll. This code sets four
;; functions with corresponding key bindings:
;;
;; C-c j n - Create new draft
;; C-c j P - Post current draft
;; C-c j d - Show all drafts
;; C-c j p - Show all posts
;;
;; Once a draft has been posted (i.e., moved from the _drafts
;; directory to _post with the required date prefix in the filename), we
;; then need to html-export it to the jekyll rootdir (with org-publish).

(global-set-key (kbd "C-c j n") 'blog-draft-post)
(global-set-key (kbd "C-c j P") 'blog-publish-post)
(global-set-key (kbd "C-c j p") (lambda ()
                                  (interactive)
                                  (find-file "~/git/myblog/org/_posts/")))
(global-set-key (kbd "C-c j d") (lambda ()
                                  (interactive)
                                  (find-file "~/git/myblog/org/_drafts/")))

(defvar jekyll-highlight-template-open
  "#+begin_export html
{%% highlight %s %%}"
  "%s will be replaced by the language identifier")

(defvar jekyll-highlight-template-close
  "{% endhighlight %}
#+end_export")

(defun yt/org-to-jekyll-highlight ()
  "wrap babel src block with jekyll syntax highlight block"
  (interactive)
  (save-excursion
    (goto-char (point-min))
(org-show-block-all)
    (while (search-forward-regexp "#\\+begin_src \\([a-z]+\\).*$" nil t)
      (replace-match (format jekyll-highlight-template-open (match-string 1)))
      (search-forward-regexp "#\\+end_src") ;; will throew error if src block is not closed. 
      (replace-match jekyll-highlight-template-close t))))

;; (add-hook 'org-export-before-processing-hook 'yt/org-to-jekyll-highlight) ;; won't work. all src blocks are wrapped before execuating. not ideal if i do need them. 
;; (add-hook 'org-export-before-parsing-hook 'yt/org-to-jekyll-highlight)

;;;; TODO: 
;; it won't be good to add a hook yt/org-jekyl-highlight
;; so that it won't effect my other exporting

;; (defun yt/test (html)
;;   (message (concat "I am here: " default-directory)))
;; for /home/itang/git/org/tmp.org, get 
;; I am here: /home/yitang/git/org/

(defun yt/my-blog-pre-process-hook (html) ;; only for html back-end
  (when (equal default-directory
               (concat jekyll-directory jekyll-posts-dir))
    (message "PROCESS SRC BLOCK")
    (goto-char (point-min))
    (yt/org-to-jekyll-highlight)))

;; (setq org-export-before-parsing-hook nil)
(add-hook 'org-export-before-parsing-hook 'yt/my-blog-pre-process-hook)

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
  (require 'magit)
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
