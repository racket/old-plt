  (unit/sig 
   mzlib:cgi^
   (import (imported : mzlib:cgi-imports^))
   
   ; --------------------------------------------------------------------

   ; Exceptions:

   (define-struct no-mail-recipients ())

   (define-struct cgi-error ())

   ; chars : list (char)
   ; -- gives the suffix which is invalid, not including the `%'

   (define-struct (incomplete-%-suffix struct:cgi-error) (chars))

   ; char : char
   ; -- an invalid character in a hex string

   (define-struct (invalid-%-suffix struct:cgi-error) (char))

   ; --------------------------------------------------------------------

   ; External parameterizations:

   (define sendmail-program-file imported:sendmail-program-file)
   
   ; "/usr/lib/sendmail"

   ; --------------------------------------------------------------------

   ; query-chars->string :
   ; list (char) -> string

   ; -- The input is the characters post-processed as per Web specs:
   ; spaces are turned into +es and lots of things are turned into %XX,
   ; where XX are hex digits, eg, %E7 for ~.  The output is a regular
   ; Scheme string with all the characters converted back.

   (define query-chars->string
     (lambda (chars)
       (list->string
	(let loop ((chars chars))
	  (if (null? chars) null
	      (let ((first (car chars))
		    (rest (cdr chars)))
		(let-values (((this rest)
			      (cond
			       ((char=? first #\+)
				(values #\space rest))
			       ((char=? first #\%)
				(if (and (pair? rest)
					 (pair? (cdr rest)))
				    (values
				     (integer->char
				      (or (string->number
					   (string
					    (car rest) (cadr rest))
					   16)
					  (raise (make-invalid-%-suffix
						  (if (string->number
						       (string (car rest))
						       16)
						      (cadr rest)
						      (car rest))))))
				     (cddr rest))
				    (raise
				     (make-incomplete-%-suffix rest))))
			       (else
				(values first rest)))))
			    (cons this (loop rest)))))))))

   ; string->html :
   ; string -> string
   ; -- the input is raw text, the output is HTML appropriately quoted

   (define string->html
     (lambda (s)
       (apply string-append
	      (map (lambda (c)
		     (case c
		       ((#\<) "&lt;")
		       ((#\>) "&gt;")
		       ((#\&) "&amp;")
		       (else (string c))))
		   (string->list s)))))

   (define default-text-color "#000000")
   (define default-bg-color "#ffffff")
   (define default-link-color "#cc2200")
   (define default-vlink-color "#882200")
   (define default-alink-color "#444444")

   ; generate-html-output :
   ; html-string x list (html-string) x ... -> ()

   (define generate-html-output
     (opt-lambda (title body-lines
			(text-color default-text-color)
			(bg-color default-bg-color)
			(link-color default-link-color)
			(vlink-color default-vlink-color)
			(alink-color default-alink-color))
       (let ((sa string-append))
	 (for-each 
	  (lambda (l)
	    (display l) (newline))
	  `("Content-type: text/html"
	    ""
	    "<html>"
	    "<!-- The form was processed, and this document was generated,"
	    "     using the CGI utilities for MzScheme.  For more information"
	    "     on MzScheme, see"
	    "     http://www.cs.rice.edu/CS/PLT/packages/mzscheme/"
	    "     and for the CGI utilities, contact Shriram Krishnamurthi"
	    "     (shriram@cs.rice.edu). -->"
	    
	    "<head>"
	    ,(sa "<title>" title "</title>")
	    "</head>"
	    ""
	    ,(sa "<body bgcolor=\"" bg-color "\" text=\"" text-color "\"")
	    ,(sa "      link=\"" link-color "\"")
	    ,(sa "      vlink=\"" vlink-color "\" alink=\"" alink-color "\">")
	    ""
	    ,@body-lines
	    ""
	    "</body>"
	    "</html>")))))
   
   ; read-until-char :
   ; char -> list (char) x bool
   ; -- operates on the default input port; the second value indicates
   ; whether reading stopped because an EOF was hit (as opposed to the
   ; delimiter being seen); the delimiter is not part of the result
   
   (define read-until-char
     (lambda (delimiter)
       (let loop ((chars '()))
	 (let ((c (read-char)))
	   (cond
	    ((eof-object? c)
	     (values (reverse chars) #t))
	    ((char=? c delimiter)
	     (values (reverse chars) #f))
	    (else
	     (loop (cons c chars))))))))

   ; read-name+value :
   ; () -> (string + bool) x (string + bool) x bool

   ; -- If the first value is false, so is the second, and the third is
   ; true, indicating EOF was reached without any input seen.  Otherwise,
   ; the first and second values contain strings and the third is either
   ; true or false depending on whether the EOF has been reached.  The
   ; strings are processed to remove the CGI spec "escape"s.

   ; This code is _slightly_ lax: it allows an input to end in `&'.  It's
   ; not clear this is legal by the CGI spec, which suggests that the last
   ; value binding must end in an EOF.  It doesn't look like this matters.
   ; It would also introduce needless modality and reduce flexibility.

   (define read-name+value
     (lambda ()
       (let-values 
	(((name eof?)
	  (read-until-char #\=)))
	(cond
	 ((and eof? (null? name))
	  (values #f #f #t))
	 (eof?
	  (generate-error-output
	   (list "Server generated malformed input for POST method:"
		 (string-append
		  "No binding for `" (list->string name) "' field."))))
	 (else
	  (let-values (((value eof?)
			(read-until-char #\&)))
		      (values (query-chars->string name)
			      (query-chars->string value)
			      eof?)))))))
   
   ; get-bindings/post :
   ; () -> list ((string . string))
   
   (define get-bindings/post
     (lambda ()
       (let-values (((name value eof?)
		     (read-name+value)))
		   (cond
		    ((and eof? (not name))
		     null)
		    ((and eof? name)
		     (list (cons name value)))
		    (else
		     (cons (cons name value)
			   (get-bindings/post)))))))

   ; generate-error-output :
   ; list (html-string) -> <exit>

   (define generate-error-output
     (lambda (error-message-lines)
       (generate-html-output "Internal Error"
			     error-message-lines)
       (exit)))

   ; bindings-as-html :
   ; list ((string . string)) -> list (html-string)
   ; -- formats name-value bindings as HTML appropriate for passing to
   ; generate-error-output or 

   (define bindings-as-html
     (lambda (bindings)
       `("<code>"
	 ,@(map
	    (lambda (bind)
	      (string-append (car bind) "&nbsp;-->&nbsp;" (cdr bind) "<br>"))
	    bindings)
	 "</code>")))

   ; extract-bindings :
   ; string x list ((string . string)) -> list (string)

   ; -- Extracts the bindings associated with a given name.  The semantics
   ; of forms states that a CHECKBOX may use the same NAME field multiple
   ; times.  Hence, a list of strings is returned.  Note that the result
   ; may be the empty list.

   (define extract-bindings
     (lambda (field-name bindings)
       (let loop ((found null) (bindings bindings))
	 (if (null? bindings)
	     found
	     (if (equal? field-name (caar bindings))
		 (loop (cons (cdar bindings) found) (cdr bindings))
		 (loop found (cdr bindings)))))))

   ; extract-binding/single : 
   ; string x list ((string . string)) -> string
   ; -- used in cases where only one binding is supposed to occur

   (define extract-binding/single
     (lambda (field-name bindings)
       (let ((result (extract-bindings field-name bindings)))
	 (cond
	  ((null? result)
	   (generate-error-output
	    `(,(string-append "No binding for field `" field-name "' in <p>")
	      ,@(bindings-as-html bindings))))
	  ((null? (cdr result))
	   (car result))
	  (else
	   (generate-error-output
	    `(,(string-append "Multiple bindings for field `" field-name "'")
	      ,(string-append "where only one was expected in <p>")
	      ,@(bindings-as-html bindings))))))))

   ; get-cgi-method :
   ; () -> string
   ; -- string is either GET or POST (though future extension is possible)

   (define get-cgi-method
     (lambda ()
       (getenv "REQUEST_METHOD")))

   ; send-mail-message/port :
   ; string x string x list (string) x list (string) x list (string) 
   ;   [x list (string)] -> iport

   ; -- sender can be anything, though spoofing is not recommended.
   ; The recipients must all be pure email addresses.  Note that
   ; everything is expected to follow RFC conventions.  If any other
   ; headers are specified, they are expected to be completely
   ; formatted already.  Clients are urged to use close-output-port on
   ; the port returned by this procedure as soon as the necessary text
   ; has been written, so that the sendmail process can complete.

   (define send-mail-message/port 
     (lambda (sender subject to-recipients cc-recipients bcc-recipients
	       . other-headers)
       (when (and (null? to-recipients) (null? cc-recipients)
	       (null? bcc-recipients))
	 (raise (make-no-mail-recipients)))
       (let ((return (apply process* sendmail-program-file "-i"
		       (append to-recipients cc-recipients bcc-recipients))))
	 (let ((reader (car return))
		(writer (cadr return))
		(pid (caddr return))
		(error-reader (cadddr return)))
	   (fprintf writer "From: ~a~n" sender)
	   (letrec ((write-recipient-header
		      (lambda (header-string recipients)
			(let ((header-space
				(+ (string-length header-string) 2)))
			  (fprintf writer "~a: " header-string)
			  (let loop ((to recipients) (indent header-space))
			    (if (null? to)
			      (newline writer)
			      (let ((first (car to)))
				(let ((len (string-length first)))
				  (if (>= (+ len indent) 80)
				    (begin
				      (fprintf writer "~n    ~a, " first)
				      (loop (cdr to) (+ len header-space 2)))
				    (begin
				      (fprintf writer "~a, " first)
				      (loop (cdr to)
					(+ len indent 2))))))))))))
	     (write-recipient-header "To" to-recipients)
	     (write-recipient-header "CC" cc-recipients))
	   (fprintf writer "Subject: ~a~n" subject)
	   (for-each (lambda (s)
		       (display s writer)
		       (newline writer))
	     other-headers)
	   (newline writer)
	   writer))))

   ; send-mail-message :
   ; string x string x list (string) x list (string) x list (string) x
   ;   list (string) [x list (string)] -> ()

   ; -- sender can be anything, though spoofing is not recommended.  The
   ; recipients must all be pure email addresses.  The text is expected
   ; to be pre-formatted.  Note that everything is expected to follow
   ; RFC conventions.  If any other headers are specified, they are
   ; expected to be completely formatted already.

   (define send-mail-message
     (lambda (sender subject to-recipients cc-recipients bcc-recipients text
		     . other-headers)
       (let ((writer (send-mail-message/port sender subject
		       to-recipients cc-recipients bcc-recipients
		       other-headers)))
	 (for-each (lambda (s)
		     (display s writer)	; We use -i, so "." is not a problem
		     (newline writer))
	   text)
	 (close-output-port writer))))

   ; generate-link-text :
   ; string x html-string -> html-string

   (define generate-link-text
     (lambda (url anchor-text)
       (string-append "<a href=\"" url "\">" anchor-text "</a>")))

   ; ====================================================================


   )
