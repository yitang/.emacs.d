;; usage:
;; $ offlineimap
;; $ mu index
;; M-x mu4e
;; from mu's official manual 
;;----------------------------------------------------------
(require 'mu4e)
(setq mu4e-mu-binary "/usr/local/bin/mu")
;; default

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'sent)

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")


;; something about ourselves
;; ;; (setq
;; ;; user-mail-address "yi.tang.uk@me.com"
;; ;; user-full-name  "唐毅 (Yi Tang)"
;;  ;; message-signature
;;  ;; (concat
;;  ;;  "唐毅 (Yi Tang)\n"
;;  ;;  "Email: yi.tang.uk@me.com\n"
;;  ;;  "\n"))


;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu
(setq smtpmail-default-smtp-server "smtpserver") ; needs to be specified before the (require)
(require 'smtpmail)

(defun yt/mail-setup () 
  (interactive)
  (cond
   ((eq system-type 'gnu/linux)
    (setq message-send-mail-function 'smtpmail-send-it
	  smtpmail-stream-type 'starttls
	  smtpmail-default-smtp-server "smtp.office365.com"
	  smtpmail-smtp-server "smtp.office365.com"
	  smtpmail-smtp-service 587
	  smtpmail-smtp-user "yi.tang@jbarisk.com"
	  user-mail-address "yi.tang@jbarisk.com"
	  user-full-name  "Yi Tang"
	  mu4e-drafts-folder "/Drafts"
	  mu4e-sent-folder   "/Sent Items"
	  mu4e-trash-folder  "/Trash"
	  mu4e-maildir-shortcuts  '(("/JBA/INBOX"    . ?i)
				    ("/Sent Items"   . ?s)
				    ("/Trash"        . ?t)
				    ("/All Mail"     . ?a))
	  message-signature
	  (concat
	   "Yi Tang\n"
	   "Statistician\n"
	   "T: +44 (0) 1756 799919\n")
	  )
    (message "sent email via office365 email account"))
   ((eq system-type 'darwin)
    (setq message-send-mail-function 'smtpmail-send-it
	  smtpmail-stream-type 'starttls
	  smtpmail-default-smtp-server "smtp.mail.me.com"
	  smtpmail-smtp-server "smtp.mail.me.com"
	  smtpmail-smtp-service 587
	  smtpmail-smtp-user "yi.tang.uk@me.com"
	  user-mail-address "yi.tang.uk@me.com"
	  user-full-name  "Yi Tang"
	  message-signature
	  (concat
	   "唐毅 (Yi Tang)\n"
	   "Email: yi.tang.uk@me.com\n"
	   "\n")
	  )
    (setq mu4e-maildir "~/Maildir")
    (setq mu4e-drafts-folder "/iCloud/Drafts")
    (setq mu4e-sent-folder   "/iCloud/Sent Messages")
    (setq mu4e-trash-folder  "/iCloud/Deleted Messages")
    
    (setq mu4e-maildir-shortcuts
	  '( ("/iCloud/INBOX"               . ?i)
         ("/iCloud/Sent Messages"   . ?s)
         ("/iCloud/Deleted Messages"       . ?t)
         ;; ("/iCloud.All Mail"    . ?a)
	 ))
    
    (message "sent email via icloud email account"))
   ))
(yt/mail-setup)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)
;; save attachment to my desktop (this can also be a function)
(setq mu4e-attachment-dir "~/Desktop")
;; attempt to show images when viewing messages
(setq mu4e-view-show-images t
      mu4e-show-images t
      mu4e-view-image-max-width 800)



;; yt
(setq mu4e-view-prefer-html t) ;; try to render 
(add-to-list 'mu4e-view-actions 
	     '("ViewInBrowser" . mu4e-action-view-in-browser) t) ;; read in browser 
;; mu4e as default email agent in emacs
(setq mail-user-agent 'mu4e-user-agent)
(require 'org-mu4e)
					;== M-x org-mu4e-compose-org-mode==
(setq org-mu4e-convert-to-html t) ;; org -> html
					; = M-m C-c.= 





(require 'org-contacts)
(setq mu4e-org-contacts-file  "~/git/org/contacts")
(add-to-list 'mu4e-headers-actions
	     '("org-contact-add" . mu4e-action-add-org-contact) t)
(add-to-list 'mu4e-view-actions
	     '("org-contact-add" . mu4e-action-add-org-contact) t)

(setq mu4e-html2text-command "html2text -utf8 -width 72") ;; nil "Shel command that converts HTML



;;;; [2015-01-14 Wed 22:32]
;; org-mime
(require 'org-mime)
(setq org-mime-library 'mml)
(add-hook 'message-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" 'org-mime-htmlize)))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize)))
(add-hook 'org-mime-html-hook
	  (lambda ()
	    (insert-file-contents "~/git/.emacs.d/style/office.css")
	    ;; (goto-char 5)
	    )
	  t)
;; (setq org-mime-html-hook nil)
