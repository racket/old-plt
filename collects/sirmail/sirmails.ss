
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

     make-fixed-width))

  (provide sirmail:send^)
  (define-signature sirmail:send^
    (new-mailer
     queue-directory
     (struct enclosure (name subheader data))))

  (provide sirmail:options^)
  (define-signature sirmail:options^
    (IMAP-SERVER
     USERNAME
     get-PASSWORD
     set-PASSWORD

     LOCAL*

     MAIL-FROM
     ALIASES
     DEFAULT-DOMAIN
     SMTP-SERVERS
     SAVE-SENT

     ROOT-MAILBOX-FOR-LIST

     MESSAGE-FIELDS-TO-SHOW
     WARN-DOWNLOAD-SIZE
     AUTO-FILE-TABLE
     BIFF-DELAY
     SELF-ADDRESSES
     SORT
     SHOW-URLS))

  (provide sirmail:read^)
  (define-signature sirmail:read^
    ()))
