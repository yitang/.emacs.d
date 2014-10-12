;; ref: http://cute-jumper.github.io/emacs/2013/10/06/orgmode-to-github-pages-with-jekyll/
(setq org-publish-project-alist
      '(("yi-tang-github-io" ;; settings for cute-jumper.github.io
         :base-directory "~/git/yi-tang.github.io/org"
         :base-extension "org"
         :publishing-directory "~/git/yi-tang.github.io/deploy"
         :recursive t
         :publishing-function org-html-publish-to-html
         :with-toc nil
         :headline-levels 4
         :auto-preamble nil
         :auto-sitemap nil
         :html-extension "html"
         :body-only t)))

(defvar jekyll-directory (expand-file-name "~/git/yi-tang.github.io/org/")
  "Path to Jekyll blog.")
(defvar jekyll-drafts-dir "_drafts/"
  "Relative path to drafts directory.")
(defvar jekyll-posts-dir "_posts/"
  "Relative path to posts directory.")
(defvar jekyll-post-ext ".org"
  "File extension of Jekyll posts.")
(defvar jekyll-post-template
  "#+BEGIN_HTML\n---\nlayout: post\ntitle: %s\nexcerpt: \ncategories:\n  -  \ntags:\n  -  \n---\n#+END_HTML\n\n* "
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
          (string-match "\"" s))
      (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"")
    s))

(defun jekyll-draft-post (title)
  "Create a new Jekyll blog post."
  (interactive "sPost Title: ")
  (let ((draft-file (concat jekyll-directory jekyll-drafts-dir
                            (jekyll-make-slug title)
                            jekyll-post-ext)))
    (if (file-exists-p draft-file)
        (find-file draft-file)
      (find-file draft-file)
      (insert (format jekyll-post-template (jekyll-yaml-escape title))))))

(defun jekyll-publish-post ()
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

(defun org-jekyll-post-link-follow (path)
  (org-open-file-with-emacs path))

(defun org-jekyll-post-link-export (path desc format)
  (cond
   ((eq format 'html)
    (format "<a href=\"{%% post_url %s %%}\">%s</a>" path desc))))

(org-add-link-type "jekyll-post" 'org-jekyll-post-link-follow 'org-jekyll-post-link-export)

;; ;; Improve our blogging experience with Org-Jekyll. This code sets four
;; ;; functions with corresponding key bindings:
;; ;;
;; ;; C-c j n - Create new draft
;; ;; C-c j P - Post current draft
;; ;; C-c j d - Show all drafts
;; ;; C-c j p - Show all posts
;; ;;
;; ;; Once a draft has been posted (i.e., moved from the _drafts
;; ;; directory to _post with the required date prefix in the filename), we
;; ;; then need to html-export it to the jekyll rootdir (with org-publish).

;; (global-set-key (kbd "C-c j n") 'jekyll-draft-post)
;; (global-set-key (kbd "C-c j P") 'jekyll-publish-post)
;; (global-set-key (kbd "C-c j p") (lambda ()
;; 				  (interactive)
;; 				  (find-file "~/org-jekyll/org/_posts/")))
;; (global-set-key (kbd "C-c j d") (lambda ()
;; 				  (interactive)
;; 				  (find-file "~/org-jekyll/org/_drafts/")))
