
(module pop3 mzscheme
  (require (lib "unitsig.ss"))

  (require "pop3-sig.ss")
  (require "pop3-unit.ss")

  (define-values/invoke-unit/sig net:pop3^
    net:pop3@)

  (provide-signature-elements net:pop3^))

#|

> (require-library "pop3.ss" "net")
> (define c (pop3:connect-to-server "cs.rice.edu"))
> (pop3:authenticate/plain-text "scheme" "********" c)
> (pop3:get-mailbox-status c)
100
177824
> (pop3:get-message/headers c 100)
("Date: Thu, 6 Nov 1997 12:34:18 -0600 (CST)"
 "Message-Id: <199711061834.MAA11961@new-world.cs.rice.edu>"
 "From: Shriram Krishnamurthi <shriram@cs.rice.edu>"
 ...
 "Status: RO")
> (pop3:get-message/complete  c 100)
("Date: Thu, 6 Nov 1997 12:34:18 -0600 (CST)"
 "Message-Id: <199711061834.MAA11961@new-world.cs.rice.edu>"
 "From: Shriram Krishnamurthi <shriram@cs.rice.edu>"
 ...
 "Status: RO")
("some body" "text" "goes" "." "here" "." "")
> (pop3:disconnect-from-server c)

|#
