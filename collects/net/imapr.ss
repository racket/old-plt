
(unit/sig mzlib:imap^
  (import)

  (define debug-via-stdio? #f)

  (define eol (if debug-via-stdio?
		  'linefeed
		  'return-linefeed))

  (define crlf (string #\return #\linefeed))
  
  (define (tag-eq? a b)
    (or (eq? a b)
	(and (symbol? a)
	     (symbol? b)
	     (string-ci=? (symbol->string a) 
			  (symbol->string b)))))

  (define field-names
    (list
     (list 'uid (string->symbol "UID"))
     (list 'header (string->symbol "RFC822.HEADER"))
     (list 'body (string->symbol "RFC822.TEXT"))
     (list 'flags (string->symbol "FLAGS"))))

  (define flag-names
    (list
     (list 'seen (string->symbol "\\Seen"))
     (list 'answered (string->symbol "\\Answered"))
     (list 'flagged (string->symbol "\\Flagged"))
     (list 'deleted (string->symbol "\\Deleted"))
     (list 'draft (string->symbol "\\Draft"))
     (list 'recent (string->symbol "\\Recent"))))

  (define (imap-flag->symbol f)
    (or (ormap (lambda (a) (and (tag-eq? f (cadr a)) (car a)))
	       flag-names)
	f))

  (define (symbol->imap-flag s)
    (let ([a (assoc s flag-names)])
      (if a
	  (cadr a)
	  s)))

  (define (log-warning . args)
    '(apply printf args)
    (void))
  (define log log-warning)

  (define make-msg-id
    (let ([id 0])
      (lambda ()
	(begin0
	 (format "a~a " id)
	 (set! id (add1 id))))))

  (define (starts-with? l n)
    (and (>= (string-length l) (string-length n))
	 (string=? n (substring l 0 (string-length n)))))

  (define (skip s n)
    (substring s
	       (if (number? n) n (string-length n))
	       (string-length s)))

  (define (splice l sep)
    (if (null? l)
	""
	(format "~a~a"
		(car l)
		(apply
		 string-append
		 (map
		  (lambda (n) (format "~a~a" sep n))
		  (cdr l))))))

  (define (imap-read s r)
    (let loop ([s s]
	       [r r]
	       [accum null]
	       [eol-k (lambda (accum) (reverse! accum))]
	       [eop-k (lambda (s accum) (error 'imap-read "unxpected close parenthesis"))])
      (cond
       [(string=? "" s) (eol-k accum)]
       [(char-whitespace? (string-ref s 0))
	(loop (skip s 1) r accum eol-k eop-k)]
       [else
	(case (string-ref s 0)
	  [(#\") (let ([m (regexp-match "([^\"]*)\"(.*)" s)])
		   (if m
		       (loop (caddr m) r (cons (cadr m) accum) eol-k eop-k)
		       (error 'imap-read "didn't find end of quoted string in: ~a" s)))]
	  [(#\)) (eop-k (skip s 1) accum)]
	  [(#\() (letrec ([next-line
			   (lambda (accum) 
			     (loop (read-line r eol) r
				   accum
				   next-line
				   finish-parens))]
			  [finish-parens
			   (lambda (s laccum)
			     (loop s r
				   (cons (reverse! laccum) accum)
				   eol-k eop-k))])
		   (loop (skip s 1) r null next-line finish-parens))]
	  [(#\{) (let ([m (regexp-match "{([0-9]+)}(.*)" s)])
		   (cond
		    [(not m) (error 'imap-read "couldn't read {} number: ~a" s)]
		    [(not (string=? (caddr m) "")) (error 'imap-read "{} not at end-of-line: ~a" s)]
		    [else (loop "" r
				(cons (read-string (string->number (cadr m)) r)
				      accum)
				eol-k eop-k)]))]
	  [else (let ([m (regexp-match "([^ (){}]+)(.*)" s)])
		  (if m
		      (loop (caddr m) r
			    (cons (let ([v (cadr m)])
				    (if (regexp-match "^[0-9]*$" v)
					(string->number v)
					(string->symbol (cadr m))))
				  accum)
			    eol-k eop-k)
		      (error 'imap-read "failure reading atom: ~a" s)))])])))

  (define (imap-send r w cmd info-handler)
    (let ([id (make-msg-id)])
      (log "sending ~a~a~n" id cmd)
      (fprintf w "~a~a~a" id cmd crlf)
      (let loop ()
	(let ([l (read-line r eol)])
	  ; (log "raw-reply: ~s~n" l)
	  (cond
	   [(starts-with? l id)
	    (let ([reply (imap-read (skip l id) r)])
	      (log "response: ~a~n" reply)
	      reply)]
	   [(starts-with? l "* ")
	    (let ([info (imap-read (skip l 2) r)])
	      (log "info: ~s~n" info)
	      (info-handler info))
	    (loop)]
	   [(starts-with? l "+ ")
	    (error 'imap-send "unexpected continuation request: ~a" l)]
	   [else
	    (log-warning "warning: unexpected response for ~a: ~a" id l)
	    (loop)])))))

  (define (check-ok reply)
    (unless (and (pair? reply)
		 (tag-eq? (car reply) 'OK))
      (error 'check-ok "server error: ~s" reply)))

  (define-struct imap-connection (r w))

  (define (imap-connect server username password inbox)
    ; => imap count-k recent-k
    (let-values ([(r w) (if debug-via-stdio?
			    (begin
			      (printf "stdin == ~a~n" server)
			      (values  (current-input-port) (current-output-port)))
			    (tcp-connect server 143))])
      (with-handlers ([void
		       (lambda (x)
			 (close-input-port r)
			 (close-output-port w)
			 (raise x))])

	(check-ok (imap-send r w "NOOP" void))
	(let ([reply (imap-send r w (format "LOGIN ~a ~a" username password) void)])
	  (if (and (pair? reply) (tag-eq? 'NO (car reply)))
	      (error "username or password rejected by server")
	      (check-ok reply)))
	
	(let ([imap (make-imap-connection r w)])
	  (let-values ([(init-count init-recent) 
			(imap-reselect imap inbox)])
	    (values imap
		    init-count
		    init-recent))))))
  
  (define (imap-reselect imap inbox)
    (let ([r (imap-connection-r imap)]
	  [w (imap-connection-w imap)])
      (let ([init-count 0]
	    [init-recent 0])
	(check-ok (imap-send r w (format "SELECT ~a" inbox)
			     (lambda (i)
			       (when (and (list? i) (= 2 (length i)))
				 (cond
				  [(tag-eq? (cadr i) 'EXISTS)
				   (set! init-count (car i))]
				  [(tag-eq? (cadr i) 'RECENT)
				   (set! init-recent (car i))])))))
	(values init-count init-recent))))

  (define (imap-status imap inbox flags)
    (unless (and (list? flags)
		 (andmap (lambda (s)
			   (memq s '(messages recent uidnext uidvalidity unseen)))
			 flags))
      (raise-type-error 'imap-status "list of status flag symbols" flags))
    (let ([r (imap-connection-r imap)]
	  [w (imap-connection-w imap)])
      (let ([results null])
	(check-ok (imap-send r w (format "STATUS ~a ~a" inbox flags)
			     (lambda (i)
			       (when (and (list? i) (= 3 (length i))
					  (tag-eq? (car i) 'STATUS))
				 (set! results (caddr i))))))
	(map
	 (lambda (f)
	   (let loop ([l results])
	     (cond
	      [(or (null? l) (null? (cdr l))) #f]
	      [(tag-eq? f (car l)) (cadr l)]
	      [else (loop (cdr l))])))
	 flags))))

  (define (imap-disconnect imap)
    (let ([r (imap-connection-r imap)]
	  [w (imap-connection-w imap)])
      (check-ok (imap-send r w "LOGOUT" void))
      (close-input-port r) 
      (close-output-port w)))

  (define (imap-force-disconnect imap)
    (let ([r (imap-connection-r imap)]
	  [w (imap-connection-w imap)])
      (close-input-port r) 
      (close-output-port w)))

  (define (imap-get-messages imap msgs field-list)
    (let ([r (imap-connection-r imap)]
	  [w (imap-connection-w imap)])
      (when (or (not (list? msgs))
		(not (andmap integer? msgs)))
	(raise-type-error 'imap-get-messages "non-empty message list" msgs))
      (when (or (null? field-list)
		(not (list? field-list))
		(not (andmap (lambda (f) (assoc f field-names)) field-list)))
	(raise-type-error 'imap-get-messages "non-empty field list" field-list))
      
      (if (null? msgs)
	  null
	  (let ([results null])
	    (imap-send r w (format "FETCH ~a (~a)"
				   (splice msgs ",")
				   (splice (map (lambda (f) (cadr (assoc f field-names))) field-list) " "))
		       (lambda (i)
			 (when (and (list? i) (<= 2 (length i))
				    (tag-eq? (cadr i) 'FETCH))
			   (set! results (cons i results)))))
	    (map
	     (lambda (msg)
	       (let ([m (assoc msg results)])
		 (unless m
		   (error 'imap-get-messages "no result for message ~a" msg))
		 (let ([d (caddr m)])
		   (map
		    (lambda (f)
		      (let ([fld (cadr (assoc f field-names))])
			(let loop ([d d])
			  (cond
			   [(null? d) #f]
			   [(null? (cdr d)) #f]
			   [(tag-eq? (car d) fld) (cadr d)]
			   [else (loop (cddr d))]))))
		    field-list))))
	     msgs)))))

  (define (imap-store imap mode msgs flags)
    (let ([r (imap-connection-r imap)]
	  [w (imap-connection-w imap)])
      (check-ok 
       (imap-send r w 
		  (format "STORE ~a ~a ~a"
			  (splice msgs ",")
			  (case mode
			    [(+) "+FLAGS.SILENT"]
			    [(-) "-FLAGS.SILENT"]
			    [(!) "FLAGS.SILENT"]			    
			    [else (raise-type-error
				   'imap-store
				   "mode: '!, '+, or '-")])
			  flags)
		  void))))

  (define (imap-copy imap msgs dest-mailbox)
    (let ([r (imap-connection-r imap)]
	  [w (imap-connection-w imap)])
      (check-ok 
       (imap-send r w
		  (format "COPY ~a ~a"
			  (splice msgs ",")
			  dest-mailbox)
		  void))))
  
  (define (imap-expunge imap)
    (let ([r (imap-connection-r imap)]
	  [w (imap-connection-w imap)])
      (check-ok (imap-send r w "EXPUNGE" void))))


  (define (imap-mailbox-exists? imap mailbox)
    (let ([r (imap-connection-r imap)]
	  [w (imap-connection-w imap)]
	  [exists? #f])
      (check-ok (imap-send r w 
			   (format "LIST \"\" ~s" mailbox)
			   (lambda (i)
			     (when (and (pair? i)
					(tag-eq? (car i) 'LIST))
			       (set! exists? #t)))))
      exists?))

  (define (imap-create-mailbox imap mailbox)
    (let ([r (imap-connection-r imap)]
	  [w (imap-connection-w imap)])
      (check-ok 
       (imap-send r w 
		  (format "CREATE ~a" mailbox) void)))))
