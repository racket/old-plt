
(define-signature mzlib:imap^
  (imap-connect
   imap-disconnect
   imap-force-disconnect
   imap-reselect
   imap-status

   imap-get-messages
   imap-copy
   imap-store imap-flag->symbol symbol->imap-flag
   imap-expunge
   
   imap-mailbox-exists?
   imap-create-mailbox))
