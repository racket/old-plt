
(module sirmailr mzscheme
  (require (lib "unitsig.ss")
	   (lib "mred-sig.ss" "mred"))

  (require "sirmails.ss")

  (require (lib "imap-sig.ss" "net")
	  (lib "smtp-sig.ss" "net")
	  (lib "head-sig.ss" "net")
	  (lib "base64-sig.ss" "net"))

  (require (lib "hierlist-sig.ss" "hierlist"))

  (require "utilr.ss"
	  "optionr.ss"
	  "readr.ss"
	  "sendr.ss")

  (provide sirmail@)
  (define sirmail@
    (compound-unit/sig
     (import (ENV : sirmail:environment^)
	     (MRED : mred^)
	     (IMAP : net:imap^)
	     (SMTP : net:smtp^)
	     (HEAD : net:head^)
	     (BASE64 : net:base64^)
	     (HIER : hierlist^)
	     (TXT : (install-text-functions))
	     (EMACS : (install-emacs-bindings)))
     (link [UTILS : sirmail:utils^
		  (util@
		   MRED)]
	   [OPTIONS : sirmail:options^
		    (option@
		     ENV
		     IMAP
                     MRED)]
	   [READ : sirmail:read^
		 (read@
		  OPTIONS ENV UTILS SEND
		  MRED IMAP SMTP HEAD BASE64 HIER
		  TXT EMACS)]
	   [SEND : sirmail:send^
		 (send@
		  (ENV : (exit-sirmail)) UTILS OPTIONS
		  MRED IMAP SMTP HEAD BASE64 HIER
		  TXT EMACS)])
     (export))))
