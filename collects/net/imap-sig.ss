

(module imap-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide net:imap^)
  (define-signature net:imap^
    (imap-port-number

     imap-connect imap-connect*
     imap-disconnect
     imap-force-disconnect
     imap-reselect
     imap-examine
     imap-noop
     imap-status

     imap-get-messages
     imap-copy imap-append
     imap-store imap-flag->symbol symbol->imap-flag
     imap-expunge
     
     imap-mailbox-exists?
     imap-create-mailbox

     imap-list-child-mailboxes
     imap-mailbox-flags
     imap-get-hierarchy-delimiter)))

