
(module imap-unit mzscheme
  (require (lib "unitsig.ss"))

  (require "imap-sig.ss")
  
  (provide net:imap@)
  (define net:imap@
    (unit/sig net:imap^
      (import)

      (define debug-via-stdio? #f)

      (define eol (if debug-via-stdio?
		      'linefeed
		      'return-linefeed))

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
	 (list 'size (string->symbol "RFC822.SIZE"))
	 (list 'flags (string->symbol "FLAGS"))))

      (define flag-names
	(list
	 (list 'seen (string->symbol "\\Seen"))
	 (list 'answered (string->symbol "\\Answered"))
	 (list 'flagged (string->symbol "\\Flagged"))
	 (list 'deleted (string->symbol "\\Deleted"))
	 (list 'draft (string->symbol "\\Draft"))
	 (list 'recent (string->symbol "\\Recent"))

	 (list 'noinferiors (string->symbol "\\Noinferiors"))
	 (list 'noselect (string->symbol "\\Noselect"))
	 (list 'marked (string->symbol "\\Marked"))
	 (list 'unmarked (string->symbol "\\Unmarked"))

	 (list 'hasnochildren (string->symbol "\\HasNoChildren"))
	 (list 'haschildren (string->symbol "\\HasChildren"))))

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
	;; (apply printf args)
	(void))
      (define log log-warning)

      (define make-msg-id
	(let ([id 0])
	  (lambda ()
	    (begin0
	     (string->bytes/latin-1 (format "a~a " id))
	     (set! id (add1 id))))))

      (define (starts-with? l n)
	(and (>= (bytes-length l) (bytes-length n))
	     (bytes=? n (subbytes l 0 (bytes-length n)))))

      (define (skip s n)
	(subbytes s
		  (if (number? n) n (bytes-length n))
		  (bytes-length s)))
      
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
	   [(bytes=? #"" s) (eol-k accum)]
	   [(char-whitespace? (integer->char (bytes-ref s 0)))
	    (loop (skip s 1) r accum eol-k eop-k)]
	   [else
	    (case (integer->char (bytes-ref s 0))
	      [(#\") (let ([m (regexp-match #rx#"\"([^\"]*)\"(.*)" s)])
		       (if m
			   (loop (caddr m) r (cons (cadr m) accum) eol-k eop-k)
			   (error 'imap-read "didn't find end of quoted string in: ~a" s)))]
	      [(#\)) (eop-k (skip s 1) accum)]
	      [(#\() (letrec ([next-line
			       (lambda (accum) 
				 (loop (read-bytes-line r eol) r
				       accum
				       next-line
				       finish-parens))]
			      [finish-parens
			       (lambda (s laccum)
				 (loop s r
				       (cons (reverse! laccum) accum)
				       eol-k eop-k))])
		       (loop (skip s 1) r null next-line finish-parens))]
	      [(#\{) (let ([m (regexp-match #rx#"{([0-9]+)}(.*)" s)])
		       (cond
			[(not m) (error 'imap-read "couldn't read {} number: ~a" s)]
			[(not (bytes=? (caddr m) #"")) (error 'imap-read "{} not at end-of-line: ~a" s)]
			[else (loop #"" r
				    (cons (read-bytes (string->number 
						       (bytes->string/latin-1 (cadr m)))
						      r)
					  accum)
				    eol-k eop-k)]))]
	      [else (let ([m (regexp-match #rx#"([^ (){}]+)(.*)" s)])
		      (if m
			  (loop (caddr m) r
				(cons (let ([v (cadr m)])
					(if (regexp-match #rx#"^[0-9]*$" v)
					    (string->number (bytes->string/latin-1 v))
					    (string->symbol (bytes->string/latin-1 v))))
				      accum)
				eol-k eop-k)
			  (error 'imap-read "failure reading atom: ~a" s)))])])))

      (define (get-response r id info-handler continuation-handler)
	(let loop ()
	  (let ([l (read-bytes-line r eol)])
	    ;; (log "raw-reply: ~s~n" l)
	    (cond
	     [(and id (starts-with? l id))
	      (let ([reply (imap-read (skip l id) r)])
		(log "response: ~a~n" reply)
		reply)]
	     [(starts-with? l #"* ")
	      (let ([info (imap-read (skip l 2) r)])
		(log "info: ~s~n" info)
		(info-handler info))
	      (loop)]
	     [(starts-with? l #"+ ")
	      (if (null? continuation-handler)
		  (error 'imap-send "unexpected continuation request: ~a" l)
		  ((car continuation-handler) loop (imap-read (skip l 2) r)))]
	     [else
	      (log-warning "warning: unexpected response for ~a: ~a~n" id l)
	      (loop)]))))

      ;; A cmd is
      ;;  * (box v) - send v literally via ~a
      ;;  * string or bytes - protect as necessary
      ;;  * (cons cmd null) - same as cmd
      ;;  * (cons cmd cmd) - send cmd, space, cmd

      (define (imap-send r w cmd info-handler . continuation-handler)
	(let ([id (make-msg-id)])
	  (log "sending ~a~a~n" id cmd)
	  (fprintf w "~a" id)
	  (let loop ([cmd cmd])
	    (cond
	     [(box? cmd) (fprintf w "~a" (unbox cmd))]
	     [(string? cmd) (loop (string->bytes/utf-8 cmd))]
	     [(bytes? cmd) (if (or (regexp-match #rx#"[ *\"\r\n]" cmd)
				   (equal? cmd #""))
			       (if (regexp-match #rx#"[\"\r\n]" cmd)
				   (begin
				     ;; Have to send size, then continue if the
				     ;;  server consents
				     (fprintf w "{~a}\r\n" (bytes-length cmd))
				     (get-response r #f void (list (lambda (gloop data) (void))))
				     ;; Continue by writing the data
				     (write-bytes cmd w))
				   (fprintf w "\"~a\"" cmd))
			       (fprintf w "~a" cmd))]
	     [(and (pair? cmd) (null? (cdr cmd))) (loop (car cmd))]
	     [(pair? cmd) (begin (loop (car cmd))
				 (fprintf w " ")
				 (loop (cdr cmd)))]))
	  (fprintf w "\r\n")
	  (get-response r id info-handler continuation-handler)))

      (define (check-ok reply)
	(unless (and (pair? reply)
		     (tag-eq? (car reply) 'OK))
	  (error 'check-ok "server error: ~s" reply)))

      (define-struct imap-connection (r w))

      (define imap-port-number (make-parameter 143))

      (define (imap-connect* r w username password inbox)
	(with-handlers ([void
			  (lambda (x)
			    (close-input-port r)
			    (close-output-port w)
			    (raise x))])
	  
	  (check-ok (imap-send r w "NOOP" void))
	  (let ([reply (imap-send r w (list "LOGIN" username password) void)])
	    (if (and (pair? reply) (tag-eq? 'NO (car reply)))
		(error 'imap-connect "username or password rejected by server: ~s" reply)
		(check-ok reply)))
	  
	  (let ([imap (make-imap-connection r w)])
	    (let-values ([(init-count init-recent) 
			  (imap-reselect imap inbox)])
	      (values imap
		init-count
		init-recent)))))

      (define (imap-connect server username password inbox)
	;; => imap count-k recent-k
	(let-values ([(r w) (if debug-via-stdio?
				(begin
				  (printf "stdin == ~a~n" server)
				  (values  (current-input-port) (current-output-port)))
				(tcp-connect server (imap-port-number)))])
	  (imap-connect* r w username password inbox)))
      
      (define (imap-reselect imap inbox)
        (imap-selectish-command imap (list "SELECT" inbox)))
	
      (define (imap-examine imap inbox)
	(imap-selectish-command imap (list "EXAMINE" inbox)))

      ;; returns (values #f #f) if no change since last check
      (define (imap-noop imap)
        (imap-selectish-command imap "NOOP"))

      ;; icky name, someone think of something better!
      (define (imap-selectish-command imap cmd)
        (let ([r (imap-connection-r imap)]
              [w (imap-connection-w imap)])
          (let ([init-count #f]
                [init-recent #f])
            (check-ok (imap-send r w cmd
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
	    (check-ok (imap-send r w (list "STATUS" inbox 
					   (box (format "~a" flags)))
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
		(imap-send r w (list "FETCH"
				     (box (splice msgs ","))
				     (box
				      (format "(~a)"
					      (splice (map (lambda (f) (cadr (assoc f field-names))) field-list) " "))))
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
		      (list "STORE"
			    (box (splice msgs ","))
			    (case mode
			      [(+) "+FLAGS.SILENT"]
			      [(-) "-FLAGS.SILENT"]
			      [(!) "FLAGS.SILENT"]			    
			      [else (raise-type-error
				     'imap-store
				     "mode: '!, '+, or '-"
				     mode)])
			    (box (format "~a" flags)))
		      void))))

      (define (imap-copy imap msgs dest-mailbox)
	(let ([r (imap-connection-r imap)]
	      [w (imap-connection-w imap)])
	  (check-ok 
	   (imap-send r w
		      (list "COPY"
			    (box (splice msgs ","))
			    dest-mailbox)
		      void))))
      
      (define (imap-append imap dest-mailbox msg)
        (let ([r (imap-connection-r imap)]
              [w (imap-connection-w imap)]
	      [msg (if (bytes? msg)
		       msg
		       (string->bytes/utf-8 msg))])
          (check-ok
           (imap-send r w (list "APPEND"
				dest-mailbox
				(box "(\\Seen)")
				(box (format "{~a}" (bytes-length msg))))
                      void
                      (lambda (loop contin)
                        (fprintf w "~a\r\n" msg)
			(loop))))))
                      
      
      (define (imap-expunge imap)
	(let ([r (imap-connection-r imap)]
	      [w (imap-connection-w imap)])
	  (check-ok (imap-send r w "EXPUNGE" void))))


      (define (imap-mailbox-exists? imap mailbox)
	(let ([r (imap-connection-r imap)]
	      [w (imap-connection-w imap)]
	      [exists? #f])
	  (check-ok (imap-send r w 
			       (list "LIST"
				     ""
				     mailbox)
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
		      (list "CREATE" mailbox)
		      void))))
      
      (define (imap-get-hierarchy-delimiter imap)
	(let* ([r (imap-connection-r imap)]
	       [w (imap-connection-w imap)]
	       [result #f])
	  (check-ok
	   (imap-send r w (list "LIST" "" "")
		      (lambda (x)
			(set! result (caddr x)))))
	  result))

      (define imap-list-child-mailboxes 
	(case-lambda
	 [(imap mailbox)
	  (imap-list-child-mailboxes imap mailbox #f)]
	 [(imap mailbox raw-delimiter)
	  (let* ([delimiter (or raw-delimiter (imap-get-hierarchy-delimiter imap))]
                 [mailbox-name (and mailbox (bytes-append mailbox delimiter))]
		 [pattern (if mailbox
			      (bytes-append mailbox-name #"%")
			      #"%")])
	    (map (lambda (p)
		   (list (car p)
			 (cond
			  [(symbol? (cadr p)) (string->bytes/utf-8 (symbol->string (cadr p)))]
			  [(string? (cadr p)) (string->bytes/utf-8 (symbol->string (cadr p)))]
			  [(bytes? (cadr p)) (cadr p)])))
		 (imap-list-mailboxes imap pattern mailbox-name)))]))
      
      (define (imap-mailbox-flags imap mailbox)
        (let ([r (imap-list-mailboxes imap mailbox #f)])
          (if (= (length r) 1)
              (caar r)
              (error 'imap-mailbox-flags "could not get flags for ~s (~a)"
                     mailbox
                     (if (null? r) "no matches" "multiple matches")))))
      
      (define (imap-list-mailboxes imap pattern except)
        (let* ([r (imap-connection-r imap)]
               [w (imap-connection-w imap)]
               [sub-folders null])
          (check-ok
           (imap-send r w (list "LIST" "" pattern)
                      (lambda (x)
                        (let ([flags (cadr x)]
                              [name (cadddr x)])
                          (unless (and except
                                       (bytes=? name except))
                            (set! sub-folders 
                                  (cons 
                                   (list flags name)
                                   sub-folders)))))))
          (reverse sub-folders))))))

