(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
	 (lib "head.ss" "net")
	 (lib "smtp.ss" "net"))

(require "private/headelts.ss")
(require "private/remote.ss")
(require "private/util.ss")

(unit/sig ()
  (import servlet^)

  (report-errors-to-browser send/finish)

  (adjust-timeout! +inf.0)

  (define bug-report-recipient "bugs")
  (define bug-email-server "bugs.plt-scheme.org")
  (define bug-email-server-port 1025)
  (define bug-report-email-address 
    (string-append bug-report-recipient "@plt-scheme.org"))

  (define error-color "red")

  (define how-to-repeat-label "How to repeat")
  (define description-label "Description")

  (define (make-field-entry label val)
    (let ([text-field?
	   (member label 
		   `(,how-to-repeat-label ,description-label))])
    `(TR ,(if text-field?
	      '((VALIGN "top"))
	      '())
	 (TD (B ,label ": "))
	 (TD
	  ,(if text-field?
	      `(PRE ,val)
	      val)))))

  (define (make-field-entry-from-pair pr)
    (make-field-entry (car pr) (cadr pr)))

  (define (empty-string? s)
    (string=? s ""))

  (define missing-fields '())

  (define (update-missing s field)
    (when (string=? s "")
	  (set! missing-fields
		(cons field missing-fields))))

  (define (make-top-table s)
    `(TABLE ((CELLPADDING "0")
	     (CELLSPACING "0")
	     (COLS "2")
	     (WIDTH "85%"))
	    (TR 
	     (TD (FONT ((SIZE "+2")) (B ,s)))
	     (TD ((ALIGN "right")) 
		 ,home-page))))
  
  (define (get-binding sym) 
    (with-handlers
     ([void 
       (lambda _
	 (send/finish
	  `(HTML
	    (HEAD
	     (TITLE "Missing bug report field")
	     ,hd-css
	     ,@hd-links)
	    (BODY
	     (H1 ,(color-with "red" "Missing bug report field"))
	     (P)
	     "One or more fields is missing from a "
	     "submitted PLT bug report. "
	     (P)
	     "This error may have occurred if you tried "
	     "to restart the bug report confirmation "
	     "servlet.  If you were unable to complete "
	     "your bug report, you'll need to start over."
	     (P)
	     (A ((HREF "/servlets/bug-report.ss")) "Click here")
	     " to start a new bug report."
	     (P)
	     ,home-page))))])
     (extract-binding/single 
      sym 
      (request-bindings initial-request))))

  (define raw-synth-info (get-binding 'synth-info))
  (define port (open-input-string raw-synth-info))
  (define synth-info (read port))
  (define (make-synth-entry pr)
    (make-field-entry 
     (car pr)
     (cadr (assoc (cadr pr) synth-info))))
  (define (get-synth-item s)
    (cadr (assq s synth-info)))
  (define dynamic-items (get-synth-item 'dynamic-items))
  (define plt-version (get-synth-item 'version))
  (define environment (get-synth-item 'environment))
  (define human-language (get-synth-item 'human-language))
  (define docs-installed (get-synth-item 'documentation))
  (define collects-installed (get-synth-item 'collections))

  (define originator (get-binding 'originator))
  (define reply-to (get-binding 'reply-to))
  (define subject (get-binding 'subject))
  (define severity (get-binding 'severity))
  (define bug-class (get-binding 'class))
  (define priority (get-binding 'priority))
  (define description (get-binding 'description))
  (define how-to-repeat (get-binding 'how-to-repeat))

  (define (make-field-table)
    `(TABLE 
      ,@(map make-field-entry-from-pair
	     `(("Name" ,originator)
	       ("E-mail" ,reply-to)
	       ("Synopsis" ,subject)
	       ("Severity" ,severity)
	       ("Bug class" ,bug-class)
	       ("Priority" ,priority)
	       (,description-label ,description)
	       (,how-to-repeat-label ,how-to-repeat)))))

  (define confirm-page
    (lambda (k-url)
      (for-each
       update-missing
       (list originator reply-to         subject)
       (list "Name"     "E-mail address" "Summary of the problem"))
	
      (when (andmap (lambda (s) (string=? s ""))
		    (list description how-to-repeat))
	    (set! missing-fields
		  (cons "Description or Steps to reproduce"
			missing-fields)))
      
      (cond 
       [(not (null? missing-fields))
	`(HTML 
	  (HEAD ,hd-css
		(TITLE "PLT bug report, missing information"))
	  (BODY
	   (H1 ,(color-with error-color "Missing information"))
	   (P)
	   (B ,(color-with error-color 
			   "Your bug report is missing information for "
			   "the following fields:"))
	   (P)
	   (PRE
	    ,@(map (lambda (fld)
		     `(DIV ,(string-append "  " fld) (BR)))
		   (reverse missing-fields)))
	   (P)
	   (B ,(color-with error-color 
			   "Hit the \"Back\" button on "
			   "your browser and supply the missing information."))))]
       [(not (member #\@ (string->list reply-to)))
	`(HTML 
	  (HEAD ,hd-css
	        (TITLE "Invalid e-mail address"))
	  (BODY 
	   (H1 ,(color-with error-color "Invalid e-mail address"))
	   (P)
	   (B ,(color-with 
		error-color
		"The e-mail address supplied:"))
	   (PRE
	    "  " ,reply-to)
	   (B ,(color-with error-color "appears to be invalid."))
	   (P)
	   (B ,(color-with error-color 
			   "Hit the \"Back\" button on your browser "
			   "and enter a valid e-mail address."))))]
       [else
	`(HTML 
	  (HEAD ,hd-css
		(TITLE "Confirm PLT bug report"))
	  (BODY
	   (TABLE ((CELLPADDING "0")
		   (CELLSPACING "0")
		   (COLS "2")
		   (WIDTH "85%"))
		  (TR 
		   (TD (FONT ((SIZE "+2")) (B "Confirm bug report")))
		   (TD ((ALIGN "right")) 
		       ,home-page)))
	   (P)	
	   "Your bug report contains the "
	   "information shown below.  Hit the \"Confirm\" button "
	   "to finish.  Use the \"Back\" button on your browser " 
	   "if you need to make corrections."
	   (P)
	   (FORM ((ACTION ,k-url)
		  (METHOD "POST"))
		 (INPUT ((TYPE "submit")
			 (VALUE "Confirm"))))
	   (P)
	   ,(make-field-table)
	   (P)
	   (B "The following information has been extracted from your "
	      "PLT Scheme installation, and will be sent with your bug report.")
	   (P)
	   (TABLE 
	    ((COLS "2")
	     (BORDER "2")
	     (WIDTH "85%"))
	    ,@(map make-synth-entry 
		   '(("PLT version" version)
		     ("Environment" environment)))
	    ,@(map (lambda (item)
		     (make-field-entry (car item)
				       (cadr item)))
		   dynamic-items)
	    ,@(map make-synth-entry
		   '(("Human language" human-language)
		     ("Installed documentation" documentation)
		     ("Installed collections" collections))))))])))

  ; allow user to confirm bug

  (send/suspend confirm-page)
  
  ; then send bug

  (let ([smtp-send-bug-report 
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
	    (append
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
	      (format "~n~a" "Docs installed:")
	      (format "~a" docs-installed)
	      (format "~n~a" "Collections:")
	      (format "~a" collects-installed)
	      (format "~nHuman language: ~a" human-language))
	     (map
	      (lambda (item)
		(format "~a: ~a" (car item) (cadr item)))
	      dynamic-items)
	     (list 
	      ""
	      ">Description:"
	      (format "~a" description)
	      ">How-To-Repeat:"
	      (format "~a" how-to-repeat)
	      ">Fix: "))
	     bug-email-server-port))]
	[sending-page
	 (lambda (k-url)
	   `(HTML 
	     (HEAD
	      ,(redir-javascript k-url)
	      ,hd-css
              (TITLE "Sending PLT bug report"))		
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
	   (HEAD ,hd-css
	         (TITLE "Transmission error for bug report"))
	   (BODY
	    ,(make-top-table "Transmission error")
	    (P)
	    ,(color-with "red"
			 "An error occurred during the transmission of your "
			 "bug report.  Use the \"Back\" button on your browser "
			 "and try sending it again.")
	    (P)
	    "If the problem continues, please send email to "
	    ,(let ([address "scheme@plt-scheme.org"])
	       `(A ((HREF ,(string-append 
			    "mailto:" address)))
		   ,address))
	    " describing the problem. "
	    (P)
	    "You can also submit your bug report on the Web at "
	    ,(let ([bug-url "http://bugs.plt-scheme.org/"])
	       `(A ((HREF ,bug-url)) ,bug-url)) ". "
	    (P)
	    (HR)
	    (P)
	    "The information you provided in your bug report was: "
	    (P)
	    ,(make-field-table)))]
	[done-page
	 `(HTML 
	   (HEAD ,hd-css
		 (TITLE "PLT bug report sent"))
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
    
    ; don't save user/email if server accepts remote connections
    (unless (unbox remote-box)
      (with-handlers ; no harm if not saved
       ([void void])
       (put-prefs (list 'plt:hd:user-name 'plt:hd:user-email)
		  (list originator reply-to))))
    
    (send/suspend sending-page)
    (with-handlers
     ([void (lambda (exn) (send/finish error-page))])
     (smtp-send-bug-report)
     (send/finish done-page))))
