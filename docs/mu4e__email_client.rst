mu4e - Email Client
===================

The advantage of use Emacs as an email client:

1. communication happens at the point where the content is generated. 
   as a statisician/programmer, most likely I need to communicate with
   numbers, table, graphs, or snippet. I could just copy these results
   from to email, do a quick editing.

2. HTML email with CSS style.  
   I like to format my email use headingline, fonts, and highlight
   the code, I used to be write a report in Word/LatEx and write an
   email with only one line, please see the attachment. which I don't
   like.

3. search properly 
   I use search all the time, and this functionality is not working at
   all in outlook 2013, it also shows up random info (mail.app in
   osx did a great job).

Disadvantage and the things it can't do:

1. book Meeting/Appointment 
   I am not aware if you can do it in emacs, and we need an
   iterative way to do. Outlook Schedule Asistant does a good job, it
   lists agenda of all attendence, and I could spot one time slot that
   suit for all or most people.

2. don't expect me to reply immediately 
   this is how it works: 1. download the email from server, 2)
   index with head, body, attachment, user name etc. 3) Emacs talk to
   and show in GUI. I usually update the email about 30 minutes. but it
   helps me out of disrupts.

3. calendar 
   I don't know how to integrate Office 365 calendar with Org-mode
   calandar yet. even it can, I doubt I can download and see other
   people's agenda.

This setting need two programs to work: 1) mu, 2) offlineimap. 

to install *mu* on osx, 

.. code-block:: sh

    brew install mu 

.. code-block:: scheme

    ;; usage:
    ;; $ offlineimap
    ;; $ mu index
    ;; M-x mu4e
    ;; from mu's official manual 
    ;;----------------------------------------------------------
    (add-to-list 'load-path "~/git/.emacs.d/elpa/mu4e")
    (require 'mu4e)
    (setq mu4e-mu-binary "/usr/local/bin/mu")
    ;; default

    ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
    (setq mu4e-sent-messages-behavior 'sent)

    ;; allow for updating mail using 'U' in the main view:
    (setq mu4e-get-mail-command "offlineimap -a JBA")
    (setq mu4e-update-interval 300)

    ;; sending mail -- replace USERNAME with your gmail username
    ;; also, make sure the gnutls command line utils are installed
    ;; package 'gnutls-bin' in Debian/Ubuntu
    (setq smtpmail-default-smtp-server "smtpserver") ; needs to be specified before the (require)
    (require 'smtpmail)


    ;; don't keep message buffers around
    (setq message-kill-buffer-on-exit t)


    ;; attempt to show images when viewing messages
    (setq mu4e-view-show-images t
          mu4e-show-images t
          mu4e-view-image-max-width 800)


    ;; (setq mu4e-html2text-command "html2text -utf8 -width 72") ;; nil "Shel command that converts HTML
    ;; ref: http://emacs.stackexchange.com/questions/3051/how-can-i-use-eww-as-a-renderer-for-mu4e
    (defun my-render-html-message ()
      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
        (erase-buffer)
        (shr-insert-document dom)
        (goto-char (point-min))))
    (setq mu4e-html2text-command 'my-render-html-message)

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

    ;; give me ISO(ish) format date-time stamps in the header list
    (setq  mu4e-headers-date-format "%Y-%m-%d %H:%M")

    ;; customize the reply-quote-string
    ;; M-x find-function RET message-citation-line-format for docs
    (setq message-citation-line-format "%N @ %Y-%m-%d %H:%M %Z:\n")
    (setq message-citation-line-function 'message-insert-formatted-citation-line)

    ;; the headers to show in the headers list -- a pair of a field
    ;; and its width, with `nil' meaning 'unlimited'
    ;; (better only use that for the last field.
    ;; These are the defaults:
    (setq mu4e-headers-fields
        '( (:date          .  25)
           (:flags         .   6)
           (:from          .  22)
           (:subject       .  nil)))

    ;; don't keep message buffers around
    (setq message-kill-buffer-on-exit t)
    ;; attachments go here
    (setq mu4e-attachment-dir  "~/Downloads")

    ;; should mu4e use fancy utf characters? NO. they're ugly.
    ;;(setq mu4e-use-fancy-chars 't)

use helm-mu to search emails 

.. code-block:: scheme

    (global-set-key (kbd "<f9> e") 'helm-mu)

Account
-------



set up email account, use office 365 in the office, and iCloud at
macbook pro.

.. code-block:: scheme

    (setq  mu4e-maildir-shortcuts  '(("/JBA/INBOX"    . ?j)
                              ("/iCloud/INBOX" . ?i)
                              ("/Gmail/INBOX" . ?g)
                              ("/Sent Items"   . ?s)
                              ("/Trash"        . ?t)
                              ("/All Mail"     . ?a)))


    ;; (defun yt/email-jba ()
    ;;   (interactive)
    ;;   ;; setup for smtp 
    ;;   (setq message-send-mail-function 'smtpmail-send-it
    ;;         smtpmail-stream-type 'starttls
    ;;         smtpmail-default-smtp-server "smtp.office365.com"
    ;;         smtpmail-smtp-server "smtp.office365.com"
    ;;         smtpmail-smtp-service 587
    ;;         smtpmail-smtp-user "yi.tang@jbarisk.com"
    ;;         ;; account info 
    ;;         user-mail-address "yi.tang@jbarisk.com"
    ;;         user-full-name  "Yi Tang"
    ;;         ;; mu4e 
    ;;         mu4e-drafts-folder "/JBA/Drafts"
    ;;         mu4e-sent-folder   "/JBA/Sent Items"
    ;;         mu4e-trash-folder  "/JBA/Trash"
    ;;         mu4e-maildir-shortcuts  '(("/JBA/INBOX"    . ?i)
    ;;                                   ("/Sent Items"   . ?s)
    ;;                                   ("/Trash"        . ?t)
    ;;                                   ("/All Mail"     . ?a))
    ;;         mu4e-compose-signature (concat
    ;;                                 "Yi Tang\n"
    ;;                                 "Statistician\n"
    ;;                                 "T: +44 (0) 1756 799919\n")))

    (defun yt/email-icloud ()
      (setq  ;; account info
       user-mail-address "yi.tang.uk@me.com"
       user-full-name  "Yi Tang"
       message-send-mail-function 'smtpmail-send-it
       smtpmail-stream-type 'starttls
       smtpmail-default-smtp-server "smtp.mail.me.com"
       smtpmail-smtp-server "smtp.mail.me.com"
       smtpmail-smtp-service 587
       smtpmail-smtp-user "yi.tang.uk@me.com"
       mu4e-maildir "~/Maildir"
       mu4e-drafts-folder "/iCloud/Drafts"
       mu4e-sent-folder   "/iCloud/Sent Messages"
       mu4e-trash-folder  "/iCloud/Deleted Messages"

       mu4e-maildir-shortcuts  '(("/iCloud/INBOX"    . ?i)
                                 ("/Sent Items"   . ?s)
                                 ("/Trash"        . ?t)
                                 ("/All Mail"     . ?a))


       mu4e-compose-signature (concat
                               "唐毅 (Yi Tang)\n"
                               "Email: yi.tang.uk@me.com\n"
                               "\n")))

    (defun yt/email-gmail ()

      ;; sent emails 
      (setq message-send-mail-function 'smtpmail-send-it
            starttls-use-gnutls t
            smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
            ;; smtpmail-auth-credentials
            ;;   '(("smtp.gmail.com" 587 "yi.tang.uni@gmail.com" nil))
            smtpmail-default-smtp-server "smtp.gmail.com"
            smtpmail-smtp-server "smtp.gmail.com"
            smtpmail-smtp-service 587
            smtpmail-smtp-user "yi.tang.uni@gmail.com")



      (setq user-mail-address "yi.tang.uni@gmail.com" )
      (setq user-full-name  "Yi Tang" )
      (setq mu4e-drafts-folder "/Gmail/Drafts" )
      (setq mu4e-sent-folder   "/Gmail/Sent Items" )
      (setq mu4e-trash-folder  "/Gmail/Trash" )
      (setq mu4e-maildir-shortcuts  '(("/Gmail/INBOX"    . ?i) 
                                      ("/Sent Items"   . ?s)
                                      ("/Trash"        . ?t)
                                      ("/All Mail"     . ?a)))
      (setq mu4e-compose-signature (concat
                                    "Yi Tang\n"
                                    "Statistician\n"
                                    "T: 07445510033\n")))




    (defun yt/mu4e-jba ()
      (interactive)
      (yt/email-jba)
      (mu4e)
      )
    (defun yt/mu4e-gmail()
      (interactive)
      (yt/email-gmail)
      (mu4e))
    (defun yt/mu4e-icloud()
      (interactive)
      (yt/email-icloud)
      (mu4e))

    (defhydra hydra-email (:color blue :hint nil)
      "
    Mu4e: _g_mail _j_ba _i_Cloud"
      ("g" yt/mu4e-gmail)
      ("j" yt/mu4e-jba)
      ("i" yt/mu4e-icloud))
    (global-set-key [f2] 'hydra-email/body)

    ;; (if (string= WhereAmI "Office")
    ;;     (yt/email-jba)
    ;;   (yt/email-icloud))

    (yt/email-icloud)

when I send an email, it will prompt and ask for email address, I only need to
type once per Emacs session, also, I can save the password to an
*.authoty* file.

Contacts
--------



have problem with BBDB installtion, and use
org-contact.el to manage contact. adding contact is very easy. I can use
tab to complete contacts, which is really handy.

.. code-block:: scheme

    (require 'org-contacts)
    (setq org-contacts-files '("~/git/org/contacts.org"))
    ;; (setq mu4e-org-contacts-file  "~/git/org/contacts.org")
    (add-to-list 'mu4e-headers-actions
                 '("org-contact-add" . mu4e-action-add-org-contact) t)
    (add-to-list 'mu4e-view-actions
                 '("org-contact-add" . mu4e-action-add-org-contact) t)

Workflow
--------



I am trying to avoid use ``C-x m`` to write/sent email directy, unless
it is really short. otherwise, if it relates to a project, I will make
an org headline to keep track the project communciation, to do that, I
compose email/message in org mode, then sent the whole subtree by ``C-c
M-o``.



.. code-block:: scheme

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
                (insert-file-contents "~/git/.emacs.d/personal/css/office.css")
                ;; (goto-char 5)
                )
              t)


    (add-hook 'org-mode-hook
              (lambda ()
                (local-set-key (kbd "C-c M-o") 'org-mime-subtree))
              'append)

just in case I didn't get the format right at the first place and need
some quick fix in message mode, save me few seconds in going back to
org-mode.

.. code-block:: scheme

    (add-hook 'message-mode-hook 'orgstruct++-mode 'append)
    (add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
    ;; (add-hook 'message-mode-hook 'bbdb-define-all-aliases 'append)
    (add-hook 'message-mode-hook 'orgtbl-mode 'append)
    (add-hook 'message-mode-hook 'turn-on-flyspell 'append)
    ;; (add-hook 'message-mode-hook
    ;;           '(lambda () (setq fill-column 270))
    ;;           'append)

TODO Comprehensive Email
------------------------
