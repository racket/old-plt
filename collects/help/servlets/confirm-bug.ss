(require (lib "unitsig.ss")
         (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server"))

(require "private/hd-css.ss")

(unit/sig ()
  (import servlet^)

  (define error-color "red")

  (define (make-field-entry label val)
    `(TR (TD (B ,label ": ")) (TD ,val)))

  (define (make-field-entry-from-pair pr)
    (make-field-entry (car pr) (cadr pr)))

  (define (empty-string? s)
    (string=? s ""))

  (define missing-fields '())

  (define (update-missing s field)
    (when (string=? s "")
	  (set! missing-fields
		(cons field missing-fields))))

  (let* ([bindings (request-bindings initial-request)]
	 [get-binding (lambda (sym) (extract-binding/single sym bindings))]
	 [raw-synth-info (get-binding 'synth-info)]
	 [port (open-input-string raw-synth-info)]
	 [synth-info (read port)]
	 [make-synth-entry 
	  (lambda (pr)
	    (make-field-entry 
	     (car pr)
	     (cadr (assoc (cadr pr) synth-info))))]
     	 [originator (get-binding 'originator)]
	 [reply-to (get-binding 'reply-to)]
	 [subject (get-binding 'subject)]
	 [severity (get-binding 'severity)]
	 [bug-class (get-binding 'class)]
	 [priority (get-binding 'priority)]
	 [description (get-binding 'description)]
	 [how-to-repeat (get-binding 'how-to-repeat)])

     (for-each
      update-missing
      (list originator reply-to         subject                   description)
      (list "Name"     "E-mail address" "Summary of the problem"  "Description of the problem"))

     (cond 
      [(not (null? missing-fields))
       `(HTML 
	 (HEAD ,hd-css)
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
	 (HEAD ,hd-css)
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
	 (HEAD ,hd-css)
	 (BODY
	  (TABLE ((CELLPADDING "0")
		  (CELLSPACING "0")
		  (COLS "2")
		  (WIDTH "85%"))
		 (TR 
		  (TD (FONT ((SIZE "+2")) (B "Confirm bug report")))
		  (TD ((ALIGN "right")) 
		      (A ((HREF "/doc/")) "Help Desk home"))))
	  (P)
	  "Your bug report contains the "
	  "information shown below.  Hit the \"Confirm\" button "
	  "to finish.  Use the \"Back\" button on your browser " 
	  "if you need to make corrections."
	  (P)
	  (FORM ((ACTION "/servlets/handle-bug.ss")
		 (METHOD "POST"))
		(INPUT ((TYPE "submit")
			(VALUE "Confirm")))
		(INPUT ((TYPE "hidden") 
			(NAME "bug-report-data")
			(VALUE  ; code everything into one big assoc list
			 ,(format "~s"
				  `((synth-info ,synth-info)
				    (originator ,originator)
				    (reply-to ,reply-to)
				    (subject ,subject)
				    (severity ,severity)
				    (class ,bug-class)
				    (priority ,priority)
				    (description ,description)
				    (how-to-repeat ,how-to-repeat)))))))
	  (P)
	  (TABLE 
	   ,@(map make-field-entry-from-pair
		  `(("Name" ,originator)
		    ("E-mail" ,reply-to)
		    ("Synopsis" ,subject)
		    ("Severity" ,severity)
		    ("Bug class" ,bug-class)
		    ("Priority" ,priority)
		    ("Description" ,description)
		    ("How to repeat" ,how-to-repeat))))
	  (P)
	  (B "The following information has been extracted from your "
	     "PLT Scheme installation, and will be sent with your bug report.")
	  (P)
	  (TABLE 
	   ((COLS "2")
	    (BORDER "2"))
	   ,@(map make-synth-entry 
		  '(("PLT version" version)
		    ("Environment" environment)
		    ("Human language" language)
		    ("Installed documentation" documentation)
		    ("Installed collections" collections))))))])))







	

