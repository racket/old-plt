
(unit/sig mzlib:smtp^
  (import)

  (define ID "localhost")
  (define TIMEOUT 9000)

  (define debug-via-stdio? #f)

  (define crlf (string #\return #\linefeed))

  (define (log . args)
    '(apply printf args)
    (void))

  (define (starts-with? l n)
    (and (>= (string-length l) (string-length n))
	 (string=? n (substring l 0 (string-length n)))))

  (define (check-reply r v)
    (let ([t (let ([t (current-thread)])
	       (thread
		(lambda ()
		  (with-handlers ([void void])
		    (sleep TIMEOUT)
		    (break-thread t)))))]
	  [l (read-line r (if debug-via-stdio?
			      'linefeed
			      'return-linefeed))])
      (break-thread t) ; cancel timeout
      (if (eof-object? l)
	  (error 'check-reply "got EOF")
	  (let ([n (number->string v)])
	    (unless (starts-with? l n)
	      (error 'check-reply "expected reply ~a; got: ~a" v l))
	    (let ([n- (string-append n "-")])
	      (when (starts-with? l n-)
		; Multi-line reply. Go again.
		(check-reply r v)))))))

  (define (protect-line l)
    ; If it's only dots, add one more
    (if (or (string=? "" l) (not (char=? #\. (string-ref l 0))))
	l ; certainly no protection needed
	; stronger check...
	(if (andmap (lambda (c) (char=? c #\.)) (string->list l))
	    (string-append "." l) ; it was all dots
	    l)))

  (define smtp-sending-end-of-message
    (make-parameter void
		    (lambda (f)
		      (unless (and (procedure? f)
				   (procedure-arity-includes? f 0))
			(raise-type-error 'smtp-sending-end-of-message "thunk" f))
		      f)))
  
  (define smtp-send-message
    (case-lambda
     [(server sender recipients header message-lines)
      (smtp-send-message server sender recipients header message-lines 25)]
     [(server sender recipients header message-lines pos)
      (when (null? recipients)
	(error 'send-smtp-message "no recievers"))
      (let-values ([(r w) (if debug-via-stdio?
			      (values (current-input-port) (current-output-port))
			      (tcp-connect server pos))])
	(with-handlers ([void (lambda (x)
				(close-input-port r)
				(close-output-port w)
				(if (exn:misc:user-break? x)
				    (error 'send-smtp-message "communication timeout")
				    (raise x)))])
	  (check-reply r 220)
	  (log "hello~n")
	  (fprintf w "HELO ~a~a" ID crlf)
	  (check-reply r 250)
	  
	  (log "from~n")
	  (fprintf w "MAIL FROM:<~a>~a" sender crlf)
	  (check-reply r 250)
	  
	  (log "to~n")
	  (for-each
	   (lambda (dest)
	     (fprintf w "RCPT TO:<~a>~a" dest crlf)
	     (check-reply r 250))
	   recipients)
	  
	  (log "header~n")
	  (fprintf w "DATA~a" crlf)
	  (check-reply r 354)
	  (fprintf w "~a" header)
	  (for-each
	   (lambda (l)
	     (log "body: ~a~n" l)
	     (fprintf w "~a~a" (protect-line l) crlf))
	   message-lines)

	  ;; After we send the ".", then only break in an emergency
	  ((smtp-sending-end-of-message))

	  (fprintf w ".~a" crlf)
	  (check-reply r 250)
	  
	  (log "quit~n")
	  (fprintf w "QUIT~a" crlf)
	  (check-reply r 221)
	  
	  (close-output-port w)
	  (close-input-port r)))])))
