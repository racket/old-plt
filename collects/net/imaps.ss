
(define-signature mzlib:imap^
  (imap-connect
   imap-disconnect
   imap-force-disconnect
   imap-reselect
   imap-get-messages

   imap-flag->symbol
   symbol->imap-flag

   imap-store
   imap-expunge))
