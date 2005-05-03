
(module smtp-unit mzscheme
  (require (lib "unitsig.ss"))

  (require "smtp-sig.ss")

  (provide net:smtp@)
  (define net:smtp@
    (unit/sig net:smtp^
      (import)

      (define ID "localhost")

      (define debug-via-stdio? #f)

      (define crlf (string #\return #\linefeed))

      (define (log . args)
	;; (apply printf args)
	(void))

      (define (starts-with? l n)
	(and (>= (string-length l) (string-length n))
	     (string=? n (substring l 0 (string-length n)))))

      (define (check-reply r v w)
	(flush-output w)
	(let ([l (read-line r (if debug-via-stdio?
				  'linefeed
				  'return-linefeed))])
	  (log "server: ~a~n" l)
	  (if (eof-object? l)
	      (error 'check-reply "got EOF")
	      (let ([n (number->string v)])
		(unless (starts-with? l n)
		  (error 'check-reply "expected reply ~a; got: ~a" v l))
		(let ([n- (string-append n "-")])
		  (when (starts-with? l n-)
		    ;; Multi-line reply. Go again.
		    (check-reply r v w)))))))

      (define (protect-line l)
	;; If begins with a dot, add one more
	(if (or (equal? l #"")
		(equal? l "")
		(and (string? l)
		     (not (char=? #\. (string-ref l 0))))
		(and (bytes? l)
		     (not (= (char->integer #\.) (bytes-ref l 0)))))
	    l
	    (if (bytes? l)
		(bytes-append #"." l)
		(string-append "." l))))

      (define smtp-sending-end-of-message
	(make-parameter void
			(lambda (f)
			  (unless (and (procedure? f)
				       (procedure-arity-includes? f 0))
			    (raise-type-error 'smtp-sending-end-of-message "thunk" f))
			  f)))
      
      (define (smtp-send-message* r w sender recipients header message-lines)
	    (with-handlers ([void (lambda (x)
				    (close-input-port r)
				    (close-output-port w)
				    (raise x))])
	      (check-reply r 220 w)
	      (log "hello~n")
	      (fprintf w "EHLO ~a~a" ID crlf)
	      (check-reply r 250 w)
	      
	      (log "from~n")
	      (fprintf w "MAIL FROM:<~a>~a" sender crlf)
	      (check-reply r 250 w)
	      
	      (log "to~n")
	      (for-each
	       (lambda (dest)
		 (fprintf w "RCPT TO:<~a>~a" dest crlf)
		 (check-reply r 250 w))
	       recipients)
	      
	      (log "header~n")
	      (fprintf w "DATA~a" crlf)
	      (check-reply r 354 w)
	      (fprintf w "~a" header)
	      (for-each
	       (lambda (l)
		 (log "body: ~a~n" l)
		 (fprintf w "~a~a" (protect-line l) crlf))
	       message-lines)

	      ;; After we send the ".", then only break in an emergency
	      ((smtp-sending-end-of-message))

	      (log "dot~n")
	      (fprintf w ".~a" crlf)
	      (flush-output w)
	      (check-reply r 250 w)
	      
	      (log "quit~n")
	      (fprintf w "QUIT~a" crlf)
	      (check-reply r 221 w)
	      
	      (close-output-port w)
	      (close-input-port r)))
      
      (define smtp-send-message
	(case-lambda
	 [(server sender recipients header message-lines)
	  (smtp-send-message server sender recipients header message-lines 25)]
	 [(server sender recipients header message-lines pos)
	  (when (null? recipients)
	    (error 'send-smtp-message "no receivers"))
	  (let-values ([(r w) (if debug-via-stdio?
				  (values (current-input-port) (current-output-port))
				  (tcp-connect server pos))])
	    (smtp-send-message* r w sender recipients header message-lines))])))))