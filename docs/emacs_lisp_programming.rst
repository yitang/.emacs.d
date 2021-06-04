======================
Emacs Lisp Programming
======================




Org-Mode API
------------



Get the link to current headline as an external link. 

.. code:: common-lisp

    (defun yt/org-get-heading-link ()
      (interactive)
      (let* ((file-name (file-truename buffer-file-name))
    	 (headline (org-heading-components))
    	 (level (nth 0 headline))
    	 (title (nth 4 headline))
    	 (link (concat file-name
    		       "::"
    		       "*" ;; (yt/lisp-rep "*" level)
    		       title)))
        (kill-new (concat "[["
    		      link
    		      "]["
    		      (concat "headline: " title)
    		      "]]"))))
