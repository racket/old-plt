(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
	 (lib "head.ss" "net")
	 (lib "smtp.ss" "net"))

(require "private/util.ss")
(require "private/hd-css.ss")
	 
(unit/sig ()
  (import servlet^)

  (define bug-report-recipient "bugs")
  (define bug-email-server "bugs.plt-scheme.org")
  (define bug-email-server-port 1025)
  (define bug-report-email-address 
    (string-append bug-report-recipient "@plt-scheme.org"))

#| 
  (define bug-email-server "mail.plt-scheme.org")
  (define bug-email-server-port 25)
  (define bug-report-email-address "steck@plt-scheme.org")
|#

  (define (make-top-table s)
    `(TABLE ((CELLPADDING "0")
	     (CELLSPACING "0")
	     (COLS "2")
	     (WIDTH "85%"))
	    (TR 
	     (TD (FONT ((SIZE "+2")) (B ,s)))
	     (TD ((ALIGN "right")) 
		 (A ((HREF "/doc/")) "Help Desk home")))))

  (let* ([bindings (request-bindings initial-request)]
	 [raw-bug-report-data (extract-binding/single 'bug-report-data bindings)]
	 [bug-report-data (read (open-input-string raw-bug-report-data))]
	 [get-entry 
	  (lambda (s)
	    (cadr (assoc s bug-report-data)))]
	 [originator (get-entry 'originator)]
	 [reply-to (get-entry 'reply-to)]
	 [subject (get-entry 'subject)]
	 [bug-class (get-entry 'class)]
	 [severity (get-entry 'severity)]
	 [priority (get-entry 'priority)]
	 [description (get-entry 'description)]
	 [how-to-repeat (get-entry 'how-to-repeat)]
	 [synth-info (get-entry 'synth-info)]
	 [get-synth-item
	  (lambda (s)
	    (cadr (assoc s synth-info)))]
	 [plt-version (get-synth-item 'version)]
	 [environment (get-synth-item 'environment)]
	 [language (get-synth-item 'language)]
	 [docs-installed (get-synth-item 'documentation)]
	 [collects-installed (get-synth-item 'collections)]
	 [smtp-send-bug-report 
	  (lambda ()
	    (smtp-send-message
	     bug-email-server 
	     reply-to
	     (list bug-report-recipient)
	     (insert-field
	      "X-Mailer"
	      (format "Help Desk ~a (bug report form)" plt-version)
	      (insert-field     
	       "Subject" 
	       subject
	       (insert-field
		"To"
		bug-report-email-address
		(insert-field
		 "From"
		 (format "~a <~a>" originator reply-to)
		 empty-header))))
	     (list
	      ">Category:       all"
	      (format ">Synopsis:       ~a" subject)
	      ">Confidential:   no"
	      (format ">Severity:       ~a" severity)
	      (format ">Priority:       ~a" priority)
	      (format ">Class:          ~a" bug-class)
	      ">Submitter-Id:   unknown"
	      (format ">Originator:     ~a" originator)
	      ">Organization:   plt"
	      (format ">Release:        ~a" plt-version)
	      ">Environment:"
	      (format "~a" environment)
	      (format "~n~a" "Docs Installed:")
	      (format "~a" docs-installed)
	      (format "~n~a" "Collections:")
	      (format "~a" collects-installed)
	      (format "~nHuman Language: ~a" language)
	      ">Fix: "
	      ">Description:"
	      (format "~a" description)
	      ">How-To-Repeat:"
	      (format "~a" how-to-repeat))
	     bug-email-server-port))]
	 [sending-page
	  (lambda (k-url)
	    `(HTML 
	      (HEAD
	       ,(redir-javascript k-url)
	       ,hd-css)
	      (BODY ((onLoad ,(onload-redir 2)))
	       (FONT ((SIZE "+2")) (B "Sending bug report to PLT")))
	       (P)
	       (TABLE
		(TR 
		 (TD 
		  (B "Please wait ...")))
		(TR (TD 'nbsp))
		(TR 
		 (TD "If this page does not refresh within 10 seconds, "
		     (A ((HREF ,k-url)
			 (NAME "go")) "click here") " to continue")))))]
	 [error-page
	  `(HTML
	    (HEAD ,hd-css)
	    (BODY
	     ,(make-top-table "Transmission error")
	     (P)
	     ,(color-with "red"
	       "An error occurred during the transmission of your "
	       "bug report.  Use the \"Back\" button on your browser "
	       "and try sending it again.")
	       (P)
	       "If the problem continues, please send email to "
	       (TT "scheme@plt-scheme.org") "describing the problem."))]
	 [done-page
	  `(HTML 
	    (HEAD ,hd-css)
	    (BODY
	     ,(make-top-table "Bug report sent")
	     (P)
	     "Your bug report has been sent to PLT. "
	     "A confirmation e-mail message will be sent to "
	     (PRE
	      " " ,reply-to)
	     "That message will contain a Web link "
	     "that you can use to monitor the status of your "
	     "report."
	     (P)
	     "If you do not receive the confirmation message within "
	     "30 minutes, please send an e-mail describing the problem to "
	     (TT "scheme@plt-scheme.org") "."))])

    (with-handlers ; no harm if not saved
     ([void void])
     (put-prefs (list 'user-name 'user-email)
		(list originator reply-to)))

    (send/suspend sending-page)
    (with-handlers
     ([void (lambda (exn) (send/finish error-page))])
     (smtp-send-bug-report)
     (send/finish done-page))))
