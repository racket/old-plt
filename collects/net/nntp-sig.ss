
(module nntp-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide net:nntp^)
  
  (define-signature net:nntp^
    ((struct communicator (sender receiver server port))
     connect-to-server disconnect-from-server
     open-news-group
     head-of-message body-of-message
     newnews-since generic-message-command
     make-desired-header extract-desired-headers

     (struct nntp ())
     (struct unexpected-response (code text))
     (struct bad-status-line (line))
     (struct premature-close (communicator))
     (struct bad-newsgroup-line (line))
     (struct non-existent-group (group))
     (struct article-not-in-group (article))
     (struct no-group-selected ())
     (struct article-not-found (article)))))


