;; mxdoc.ss

;; A MysterX program which serves as documentation

(load "mysterx.ss")

(define *doc* (make-object mx-document% "MysterX documentation" 700 600 0 0))

(define para "<p>")

(define h1
  (lambda (s)
    (string-append
     "<H1 STYLE=\"color:blue\">"
     s
     "</H1>")))

(define h2
  (lambda (s)
    (string-append
     "<H2 STYLE=\"color:olivedrab\">"
     s
     "</H2>")))

(define tt
  (lambda (s)
    (string-append
     "<tt STYLE=\"font-size:14px\">"
     s
     "</tt>")))

(define it
  (lambda (s)
    (string-append
     "<i>"
     s
     "</i>")))

(define table-row ; takes list of data elts
  (lambda (elts)
    (apply string-append 
	   (map (lambda (row)
		  (string-append
		   "<td>" row "</td>"))
		elts))))

(define table     ; takes list of rows
  (lambda (rows)
    (string-append 	
     "<table STYLE=\"font-size:x-small\">"
     (apply string-append 
	    (map (lambda (row)
		   (string-append
		    "<tr>" row "</tr>"))
		 rows))
     "</table>")))

(define code
  (lambda (s)
    (string-append
     "<DIV STYLE=\"text-indent:6px\">"
     "<tt STYLE=\"font-size:14px\">"
     s
     "</tt>"
     "</DIV>")))

(define ul-list
  (lambda (lis)
    (string-append 
     "<ul>"
     (apply string-append 
	    (map (lambda (s) (string-append "<li>" s "</li>"))
		 lis))
     "</ul>")))

(define next-button
  (string-append 
   "<BUTTON id=\"next-page\">"
   "-->"
   "</BUTTON>"))

(define did-it-button
  (string-append
    "<BUTTON id=\"did-it\">"
    "I did it!"
   "</BUTTON>"))

(define give-up-button
  (string-append
    "<BUTTON id=\"give-up\">"
    "I give up!  Do it for me..."
   "</BUTTON>"))
    
(define basefont
   "<BASEFONT FACE=\"sans-serif\" SIZE=2>")

(define click-text
  "Click on the code to execute it!")

(define (quote-tags s)
  (regexp-replace*
   ">"
   (regexp-replace* "<" (format "~s" s) "\\&lt")
   "\\&gt"))

(define (make-clickable-code id s)
  (code (string-append "<A href= \"#\" id=\""
		       id
		       "\">"
		       (quote-tags s)
		       "</A>")))


(define *months*
  (vector "January" "February" "March" "April" "May" "June"
	  "July" "August" "September" "October" "November" "December"))

(define (month->string n)
  (vector-ref *months* (sub1 n)))

; code that shows up in the DHTML

(define make-example-doc-command
  '(make-object mx-document% "MysterX example" 400 300 400 300))

(define insert-text-command
  '(send *example-doc* append-html "<b>A bold assertion!</b>"))

(define append-button-command
  '(send *example-doc* append-html 
	 "<BUTTON id=\"example-button\">Push me!</BUTTON>"))

(define insert-calendar-command
  '(send *example-doc* insert-object "Calendar Control 8.0"))

(define calendar-month-command
  '(get-property *calendar* "Month"))
	 
(define calendar-buttons-html
  (string-append
   "<BUTTON id=\"PreviousYear\"><--</BUTTON>"
   "<BUTTON id=\"NextYear\">--></BUTTON>"))
	 
(define calendar-buttons-command
  '(send *example-doc* append-html calendar-buttons-html))
	 
(define button-handler-sexp
  '(lambda (ev) 
     (when (event-click? ev)
	   (send *example-doc* append-html "click! "))))

(define button-handler
  (eval button-handler-sexp))

(define register-handler-command
  '(send *example-doc* register-event-handler 
	 'BUTTON 
	 'example-button 
	 button-handler))

(define handle-events-command
  '(send *example-doc* handle-events))

(define make-html 
  (lambda txt
    (apply string-append 
	   basefont
	   txt)))

(define intro-html

  (make-html

   (h1 "MysterX")

   para

   "MysterX is a toolkit for scripting COM objects, including "
   "ActiveX controls, with PLT MzScheme.  In order to build "
   "useful applications, MysterX also supports Dynamic HTML "
   "operations and events.  This documentation is a MysterX application."

   para

   (h2 "Documents")

   para

   "In MysterX, a <it>document</it> is a window that may contain "
   "HTML elements, including COM objects.  Documents are "
   "instances of the " (tt "mx-document%") " class. " 
   "To create a document, use "

   para 

   (code 
    "(make-object mx-document% label width height x y style-list)")

   para

   "where all the arguments are optional. " 
   (tt "label") " is a string used as the document window caption, "
   "and defaults to \"MysterX\".  "
   (tt "width") ", " (tt "height") ", " (tt "x") ", and " (tt "y")
   " are integers indicating the window size and position. "
   "You may use a default value for any of these parameters by " 
   "using the symbol " (tt "'default") ". "
   (tt "style-list") " is a list that contains any of the symbols "
   (tt "'iconize") ", " (tt "'maximize") ", " (tt "'no-caption") ", "
   (tt "'no-system-menu") ", or " (tt "'no-thick-border") ". "

   para

   "Let's create a document by entering"

   para

   (make-clickable-code "make-document" make-example-doc-command)

   para

   click-text

   ))

(define html-html

  (make-html

   (h2 "Inserting HTML")

   para

   "You can insert arbitrary HTML into documents, using"

   para

   (table
    
    (list

     (table-row
      (list
       (code "(send an-mx-document% insert-html html)")
       "or"))

     (table-row
      (list
       (code "(send an-mx-document% append-html html)")
       "or"))

     (table-row
      (list
       (code "(send an-mx-document% replace-html html)")))))

   para

   (tt "insert-html") " inserts HTML at the beginning of "
   "the document, " (tt "append-html") " appends HTML "
   "to the end of the document, while " (tt "replace-html") " "
   "replaces the current HTML. We use " (tt "replace-html") " " 
   "to jump from page to page in this documentation."
   
   para

   "For example, we might execute "

   para

   (make-clickable-code "insert-text" insert-text-command)

   para

   "to insert some text into the window we created on the " 
   "previous page."

   para

   click-text

))

(define events-html

  (make-html

   (h2 "Handling HTML events")

   para

   "User interaction with a document produces <it>events</it>. "
   "Each event is associated with a particular HTML element. "
   "You can associate procedures with the events in a "
   "particular HTML element. "
   "Suppose we add a button to our example window:"

   para

  (make-clickable-code "append-button" append-button-command)

  para

  click-text

))

(define events-bottom-html

  (make-html

   para

   "We can associate a procedure with mouse clicks on the button "
   "by declaring a procedure " (tt "button-handler")

   para

   (code (format "~v" button-handler-sexp))
   
   para

   "and then executing"

   para

   (code (string-append
	  "<A href=\"#\" id=\"register-handler\">"
	  "(send *example-doc* register-event-handler "
	  "'BUTTON 'example-button button-handler))"
	  "</A>"))

   para

   click-text

   para
))

(define events-very-bottom-html

  (make-html

   "Finally, we need to handle the events, by calling "

   para

   (make-clickable-code "handle-events" handle-events-command)

   para

   click-text

   " Then click on the button and see what happens. " 

))

(define more-events-html

  (make-html

   (h2 "More about events")

   "Events themselves are detectable using the "
   " predicate " (tt "event?") "."

   para

   "Besides mouse clicks, there are other kinds of events, which can "
   "be tested with the following " (it "predicates") ": " 

   para

   (table 

    (list

     (table-row

      (list

       (ul-list
	(map code 
	     '("event-dblclick?"
	       "event-error"
	       "event-keypress?"
	       "event-keyup?")))
      
       (ul-list
	(map code
	     '("event-mousedown?"
	       "event-mouseout?"
	       "event-mouseover?"
	       "event-mouseup?")))))))
   	
   "Events have " (it "attributes") " obtainable using these "
   "procedures, each of which takes an event argument:" 

   para

   (table

    (list
     
     (table-row

      (list

       (ul-list
	(map code 
	     '("event-tag?"
	       "event-id"
	       "event-from-tag"
	       "event-from-id")))

       (ul-list
	(map code
	     '("event-to-tag"
	       "event-to-id"
	       "event-x"
	       "event-y")))))))

   "The " (tt "event-tag") " and " (tt "event-id") " attributes "
   "are the tag and identifier of the HTML element where the event "
   "occurred.  The \"from\" and \"to\" versions are meaningful "
   "only for event for which either " (tt "event-mouseout?") " or " 
   (tt "event-mouseover?") " holds."

   para

   next-button

))

(define com-html

  (make-html

   (h2 "COM objects")

   "MysterX supports COM, Microsoft's Component Object Model."
   "COM objects, including ActiveX controls, can be loaded into "
   "MysterX documents.  To get a list of COM classes are "
   "registered on your machine, use " 
   (tt "(all-com-classes)") ".  If you're only interested in "
   "ActiveX controls, instead use " (tt "(all-controls)") ". " 

   "You can insert a COM object using either "
  
   para

   (table
    
    (list

     (table-row
      (list
       (code "(send an-mx-document insert-object coclass)")
       "or"))

     (table-row
      (list
       (code "(send an-mx-document append-object coclass)")))))

   para

   "where coclass is a string naming the COM class. " 
   "Both of these procedures return a value of type <com-object>. "
   "Alternatively, you can create an HTML string suitable for "
   "combining with other HTML using "

   para

   (code "(coclass->html coclass)")

   para

   "A list of COM objects in a MysterX document may be obtained with " 

   para

   (code "(send an-mx-document objects)")

   para

   next-button

))

(define activex-html

  (make-html

   (h2 "An ActiveX example")

   "We've taken the liberty of clearing the example window, "
   "so that we can insert an ActiveX control."
   
   para

   (make-clickable-code
    "insert-calendar" 
    insert-calendar-command)
   
   para

   click-text

   para

   ))

(define activex-2-html

  (make-html

   "Now that we have a COM object, in this case an ActiveX control, "
   "what can we do with it?  Most COM objects support " (it "OLE Automation") ", "
   "meaning that we can find out the " (it "properties") " and " (it "methods") " "
   "of the object dynamically.  From Scheme, we can call "

   para

   (table

    (list

     (table-row
      (list (code "(methods a-com-object)")
	    "or"))

     (table-row
      (list (code "(get-properties a-com-object)")
	    "or"))

     (table-row
      (list (code "(set-properties a-com-object)")))))

   para

   "to get lists of methods, readable properties, and writable properties, "
   "as strings. Each method and property has a " (tt "type") ", "
   "which we can get with "

   para

   (table

    (list

     (table-row
      (list (code "(method-type a-com-object method)")
	    "or"))

     (table-row
      (list (code "(get-property-type a-com-object property)")
	    "or"))

     (table-row
      (list (code "(set-property-type! a-com-object property)")))))

   para

   next-button

   ))

(define activex-3-html

  (make-html

   (h2 "Using COM methods and properties")

   para

   "The types tell you what Scheme arguments need to be supplied, and "
   "what value will be returned, when invoking a method or "
   "a property.  With that information, use "

   para

   (table

    (list

     (table-row
      (list (code "(invoke a-com-object method ...)")
	    "to invoke a method"))

     (table-row
      (list (code "(get-property a-com-object property ...)")
	    "to get a property"))

     (table-row
      (list (code "(set-property! a-com-object property ...)")
            "to set a property")))) 
             
   para

   "Let's retrieve a property associated with the calendar control. "

   para

   (make-clickable-code
    "calendar-month" 
    calendar-month-command)

   para

   click-text

   para

))

(define activex-4-html

  (make-html

   para
   
   "How did we do that?  We took the month value returned by the "
   "control, and passed the formatted result to the "
   (tt "append-html") " method of " (tt "mx-document%") ". "
   "In this case, the MysterX document was the main window, rather "
   "than the example window."

   para

   next-button))


(define complete-html

  (make-html

   (h2 "Completing the application")
   
   para

   "Let's add some buttons to the example window which invoke "
   "methods when pressed.  The HTML for the buttons is "

   para

   (code (quote-tags calendar-buttons-html))
   
   para

   "The code to add the buttons is "

   para

   (make-clickable-code "calendar-buttons" calendar-buttons-command)

   para

   click-text

   para

))

(define complete-middle-html

  (make-html

   "Your job is to add event handlers so that the buttons "
   "invoke the methods \"PreviousYear\" and \"NextYear\".  You can "
   "enter the code at the Scheme prompt.  Hints: use "
   (tt "get-method-type") " to get the type of the methods; "
   "the top-level variable " (tt "*calendar*") " is bound to "
   "the control."

   para

   (table

    (list

     (table-row
      (list did-it-button
	    " "
	    give-up-button))))
   para
))

(define plt-thanks
  (string-append
   "PLT hopes you enjoy MysterX, and invites your suggestions "
   "for improvements. "))

(define complete-did-it-html

  (make-html

   "You are truly a master programmer. " 
   "Try clicking on the arrow buttons to see if your code really works!."

   para

   plt-thanks))


(define complete-give-up-html

  (make-html

   "OK, we did it.  But we're not going to show you the code! "
   "Try clicking on the buttons."

   para

   plt-thanks))

(define intro-buttons
  (string-append
   para 
   next-button))

(define event-buttons-html
  (string-append
   para
   next-button))

(define html-buttons
  (string-append
   para
   next-button))

(define *example-doc* #f)
(define *calendar* #f)

; each page has an action, either 'append, 'insert, or 'replace

(define *page-actions* (make-hash-table))

; each page has a list of handlers that need to be
; installed, stored in page-handlers hash-table

(define *page-handlers* (make-hash-table))

(define page-entry
  (lambda (name action handlers)
    (list name action handlers)))

(define make-handler
  (lambda (tag id f)
    (list tag id f)))

(define page-entry-list

  (list 

   (page-entry 

    intro-html 
    'replace

    (list 
     (make-handler
      'A 'make-document
      (let ([done #f])
	(lambda (ev)
	  (when (and (event-click? ev)
		     (not *example-doc*)
		     (not done))
		(set! done #t)
		(set! *example-doc* (eval make-example-doc-command))
		(goto-page intro-buttons)))))))

   (page-entry 

    intro-buttons
    'append

    (list
     (make-handler
      'BUTTON 'next-page
      (lambda (ev)
	(when (event-click? ev)
	      (goto-page html-html))))))

   (page-entry 

    html-html 
    'replace
    
    (list 
     (make-handler 
      'A 'insert-text
      (let ([done #f])
	(lambda (ev)
	  (when (and (event-click? ev)
		     (not done))
		(set! done #t)
		(eval insert-text-command)
		(goto-page html-buttons)))))))

   (page-entry 

    html-buttons
    'append

    (list
     (make-handler
      'BUTTON 'next-page
      (lambda (ev)
	(when (event-click? ev)
	      (goto-page events-html))))))

   (page-entry 

    events-html 
    'replace

    (list
     (make-handler
      'A 'append-button
      (let ([done #f])
	(lambda (ev)
	  (when (and (event-click? ev)
		     (not done))
		(set! done #f)
		(send *example-doc* append-html para)
		(eval append-button-command)
		(send *example-doc* append-html para)
		(goto-page events-bottom-html)))))))


   (page-entry 

    events-bottom-html 
    'append

    (list
     (make-handler
      'A 'register-handler
      (let ([done #f])
	(lambda (ev)
	  (when (and (event-click? ev)
		     (not done))
		(set! done #t)
		(eval register-handler-command)
		(goto-page events-very-bottom-html)))))))


   (page-entry 

    events-very-bottom-html 
    'append

    (list
     (make-handler
      'A 'handle-events
       (let ([done #f])
	 (lambda (ev)
	   (when (and (event-click? ev)
		      (not done))
		 (set! done #t)
		 (eval handle-events-command)
		 (goto-page event-buttons-html)))))))

   (page-entry 

    event-buttons-html 
    'append

    (list
     (make-handler 
      'BUTTON 'next-page
      (lambda (ev)
	(when (event-click? ev)
	      (goto-page more-events-html))))))
        
   (page-entry 
    more-events-html 
    'replace

    (list
     (make-handler 
      'BUTTON 'next-page
      (lambda (ev)
	(when (event-click? ev)
	      (goto-page com-html))))))

   (page-entry 
    com-html 
    'replace

    (list 
     (make-handler 
      'BUTTON 'next-page
      (lambda (ev)
	(when (event-click? ev)
	      (send *example-doc* replace-html "")
	      (goto-page activex-html))))))

   (page-entry 
    activex-html 
    'replace

    (list
     (make-handler 
      'A 'insert-calendar
      (let ([done #f])
	(lambda (ev)
	  (when (and (event-click? ev)
		     (not done))
		(set! done #t)
		(set! *calendar* (eval insert-calendar-command))
		(goto-page activex-2-html)))))))

   (page-entry 
    activex-2-html 
    'append

    (list
     (make-handler 
      'BUTTON 'next-page
      (lambda (ev)
	(when (event-click? ev)
	      (goto-page activex-3-html))))))

   (page-entry 
    activex-3-html 
    'replace

    (list

     (make-handler
      'A 'calendar-month
      (let ([done #f])
	(lambda (ev)
	  (when (and (event-click? ev)
		     (not done))
		(set! done #t)
		(send *doc* 
		      append-html
		      (format "Let's see, we must be in the month of <b>~a</b>."
			      (month->string
			       (eval calendar-month-command))))
		(goto-page activex-4-html)))))))

   (page-entry 
    activex-4-html 
    'append

    (list

     (make-handler
      'BUTTON 'next-page
      (lambda (ev)
	(when (event-click? ev)
	      (goto-page complete-html))))))


   (page-entry 
    complete-html 
    'replace

    (list

     (make-handler
      'A 'calendar-buttons
      (let ([done #f])
	(lambda (ev)
	  (when (and (event-click? ev)
		     (not done))
		(set! done #t)
		(send *example-doc* append-html para)
		(eval calendar-buttons-command)
		(goto-page complete-middle-html)))))))

   (page-entry 
    complete-middle-html 
    'append

    (list
     (make-handler
      'BUTTON 'did-it
      (let ([done #f])
	(lambda (ev)
	  (when (and (event-click? ev)
		     (not done))
		(set! done #t)
		(send *doc* unregister-event-handler 'BUTTON 'give-up)
		(goto-page complete-did-it-html)))))
     (make-handler
      'BUTTON 'give-up
      (let ([done #f])
	(lambda (ev)
	  (when (and (event-click? ev)
		     (not done))
		(set! done #t)
		(send *doc* unregister-event-handler 'BUTTON 'did-it)
		(send *example-doc* register-event-handler
		      'BUTTON 'PreviousYear
		      (lambda (ev)
			(when (event-click? ev)
			      (invoke *calendar* "PreviousYear"))))
		(send *example-doc* register-event-handler
		      'BUTTON 'NextYear
		      (lambda (ev)
			(when (event-click? ev)
			      (invoke *calendar* "NextYear"))))
		(goto-page complete-give-up-html)))))))

   (page-entry 
    complete-did-it-html 
    'append
    '())

   (page-entry 
    complete-give-up-html 
    'append
    '())))

(define goto-page
  (lambda (html)
    (let ([action (hash-table-get *page-actions* html)]
	  [handlers (hash-table-get *page-handlers* html)])

      ; action

      (case action
         [(append)
             (send *doc* append-html html)]
         [(insert)
             (send *doc* insert-html html)]
         [(replace)
             (send *doc* replace-html html)]
	 [else
	   (printf "Unknown action ~a in goto-page" action)])

      ; update handlers

      (for-each
       (lambda (h)
	 (let ([tag (car h)]
	       [id (cadr h)]
	       [f (caddr h)])
	 (send *doc* register-event-handler tag id f)))
       handlers))))

; initialization

(for-each
 (lambda (entry)
   (let ([html  (car entry)]
	 [action (cadr entry)]
	 [handlers (caddr entry)])
     (hash-table-put! *page-actions* html action)
     (hash-table-put! *page-handlers* html handlers)))
 page-entry-list)

(goto-page intro-html)

(send *doc* handle-events)





