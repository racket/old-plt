(define-signature mzlib:pop3^
  ((struct communicator (sender receiver server port state))
    connect-to-server disconnect-from-server
    authenticate/plain-text
    get-mailbox-status
    get-message/headers
    get-message/complete
    make-desired-header extract-desired-headers))
