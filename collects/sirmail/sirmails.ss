
(module sirmails mzscheme
  (require (lib "unitsig.ss"))
  
  (provide sirmail:environment^)
  (define-signature sirmail:environment^
    (mailbox-name 
     mailbox-options 
     open-folders-window
     get-active-folder

     open-mailbox
     start-new-window
     exit-sirmail))

  (provide sirmail:utils^)
  (define-signature sirmail:utils^
    (crlf
     split
     splice
     split-crlf
     split-lf
     crlf->lf
     lf->crlf
     enumerate
     find
     string->regexp

     as-background

     make-fixed-width

     confirm-box))

  (provide sirmail:send^)
  (define-signature sirmail:send^
    (new-mailer
     (struct enclosure (name subheader data))))

  (provide sirmail:options^)
  (define-signature sirmail:options^
    (IMAP-SERVER
     USERNAME
     get-PASSWORD
     set-PASSWORD

     LOCAL-DIR

     MAIL-FROM
     ALIASES
     DEFAULT-DOMAIN
     SMTP-SERVER
     SAVE-SENT

     ROOT-MAILBOX-FOR-LIST

     MESSAGE-FIELDS-TO-SHOW
     WARN-DOWNLOAD-SIZE
     AUTO-FILE-TABLE
     BIFF-DELAY
     SELF-ADDRESSES
     SORT
     SHOW-URLS
     
     USE-EXTERNAL-COMPOSER?

     parse-server-name))

  (provide sirmail:read^)
  (define-signature sirmail:read^
    (queue-directory)))
