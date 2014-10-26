;; usage:
;; $ offlineimap
;; $ mu index
;; M-x mu4e
;; from mu's official manual 
;;----------------------------------------------------------
 (add-to-list 'load-path "~/mu-master/mu4e")
(require 'mu4e)
(setq mu4e-mu-binary "/usr/local/bin/mu")
;; default
(setq mu4e-maildir "~/Maildir")
(setq mu4e-drafts-folder "/iCloud/Drafts")
(setq mu4e-sent-folder   "/iCloud/Sent Messages")
(setq mu4e-trash-folder  "/iCloud/Deleted Messages")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'sent)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
      '( ("/iCloud/INBOX"               . ?i)
         ("/iCloud/Sent Messages"   . ?s)
         ("/iCloud/Deleted Messages"       . ?t)
         ;; ("/iCloud.All Mail"    . ?a)
	 ))


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
	  user-full-name  "唐毅 (Yi Tang)"
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
	  user-full-name  "唐毅 (Yi Tang)"
	  message-signature
	  (concat
	   "唐毅 (Yi Tang)\n"
	   "Email: yi.tang.uk@me.com\n"
	   "\n")
	  )   
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



(require 'org-mime)
(setq org-mime-library 'mml)
(add-hook 'message-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" 'org-mime-htmlize)))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize)))


;; (defvar my-mu4e-account-alist
;;   '(("Gmail"
;;      (mu4e-sent-folder "/Gmail/Saved Items")
;;      (mu4e-drafts-folder "/Gmail/Drafts")
;;      (user-mail-address "yi.tang.uni@gmail.com")
;;      (message-signature-file "Gmail Yi Tang")
;;      (smtpmail-default-smtp-server "smtp.gmail.com")
;;      (smtpmail-local-domain "gmail.com")
;;      (smtpmail-smtp-server "smtp.gmail.com")
;;      (smtpmail-stream-type starttls)
;;      (smtpmail-smtp-service 25))
;;     ("iCloud"
;;      (mu4e-sent-folder "/iCloud/Saved Items")
;;      (mu4e-drafts-folder "/iCloud/Drafts")
;;      (user-mail-address "yi.tang.uk@mail.me.com")
;;      (message-signature-file "iCloud yi tang")
;;      (smtpmail-default-smtp-server "smtp.mail.me.com")
;;      (smtpmail-local-domain "mail.me.com")
;;      (smtpmail-smtp-server "smtp.mail.me.com")
;;      (smtpmail-stream-type starttls)
;;      (smtpmail-smtp-service 587))))

;; (defun my-mu4e-set-account ()
;;   "Set the account for composing a message."
;;   (let* ((account
;;           (if mu4e-compose-parent-message
;;               (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
;;                 (string-match "/\\(.*?\\)/" maildir)
;;                 (match-string 1 maildir))
;;             (completing-read (format "Compose with account: (%s) "
;;                                      (mapconcat #'(lambda (var) (car var))
;;                                                 my-mu4e-account-alist "/"))
;;                              (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
;;                              nil t nil nil (caar my-mu4e-account-alist))))
;;          (account-vars (cdr (assoc account my-mu4e-account-alist))))
;;     (if account-vars
;;         (mapc #'(lambda (var)
;;                   (set (car var) (cadr var)))
;;               account-vars)
;;       (error "No email account found"))))
;; (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)






(cond
 ((eq system-type 'gnu/linux)
  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-folder   "/Sent Items")
  (setq mu4e-trash-folder  "/Trash")
  (setq mu4e-maildir-shortcuts
	'( ("/JBA/INBOX"               . ?i)
	   ("/Sent Items"   . ?s)
	   ("/Trash"       . ?t)
	   ("/All Mail"    . ?a)))))

(setq mu4e-compose-signature
     (concat
      "Yi Tang\n"
      "Statistician\n"
      "T: +44 (0) 1756 799919\n")
     )
