(module tcp-redirect mzscheme
  (provide tcp-redirect)
  
  (require (lib "unitsig.ss")
           (lib "channel.ss" "web-server")
           (lib "etc.ss")
           "tcp-sig.ss")
  
  (define raw:tcp-abandon-port tcp-abandon-port)
  (define raw:tcp-accept tcp-accept) 
  (define raw:tcp-accept-ready? tcp-accept-ready?)
  (define raw:tcp-addresses tcp-addresses)
  (define raw:tcp-close tcp-close)
  (define raw:tcp-connect tcp-connect)
  (define raw:tcp-connect/enable-break tcp-connect/enable-break)
  (define raw:tcp-listen tcp-listen)
  (define raw:tcp-listener? tcp-listener?)
  
  ; I cannot follow the data definiton well because I don't have
  ; predicates tcp-port? or pipe-port?.  I could print them out and
  ; look at the string, but that's ugly, too.  Instead I use with-handlers.
  
  ; For tcp-listeners, I do have suitable predicates, but I still use
  ; an else branch in the conds since I want the same error message as
  ; the raw primitive for bad inputs.
  
  ; : (listof nat) -> (unit/sig () -> net:tcp^)
  (define (tcp-redirect redirected-ports)
    (unit/sig net:tcp^
      (import)
      
      ; : (make-pipe-listener nat (channel (cons iport oport)))
      (define-struct pipe-listener (port channel))
      
      ; : port -> void
      (define (tcp-abandon-port tcp-port)
	(with-handlers ([exn:application:type? void])
	  (raw:tcp-abandon-port tcp-port)))
      
      ; : listener -> iport oport
      (define (tcp-accept tcp-listener)
        (cond
          [(pipe-listener? tcp-listener)
           (let ([in-out (channel-get (pipe-listener-channel tcp-listener))])
             (values (car in-out) (cdr in-out)))]
          [else (raw:tcp-accept tcp-listener)]))
      
      ; : tcp-listener -> iport oport
      ; FIX - check channel queue size
      (define (tcp-accept-ready? tcp-listener)
        (cond
          [(pipe-listener? tcp-listener) #t]
          [else (raw:tcp-accept-ready? tcp-listener)]))
      
      ; : tcp-port -> str str
      (define (tcp-addresses tcp-port)
        (with-handlers ([exn:application:type?
			 (lambda (exn) (values local-address local-address))])
          (raw:tcp-addresses tcp-port)))
      
      ; : port -> void
      (define (tcp-close tcp-listener)
        (with-handlers ([exn:application:type?
			 (lambda (exn)
			   (hash-table-remove!
			    port-table
			    (pipe-listener-port tcp-listener)))])
	  (raw:tcp-close tcp-listener)))
      
      ; : (str nat -> iport oport) -> str nat -> iport oport
      (define (gen-tcp-connect raw)
	(lambda (hostname-string port)
	  (if (and (string=? local-address hostname-string)
		   (redirect? port))
	      (let-values ([(to-in from-out) (make-pipe)]
                           [(from-in to-out) (make-pipe)])
		(channel-put
		 (pipe-listener-channel
		  (hash-table-get
		   port-table
		   port
		   (lambda ()
		     (raise (make-exn:i/o:tcp
			     (format "tcp-connect: connection to ~a, port ~a failed (nobody is listening)"
				     hostname-string port)
			     (current-continuation-marks))))))
		 (cons to-in to-out))
		(values from-in from-out))
	      (raw hostname-string port))))
      
      ; : str nat -> iport oport
      (define tcp-connect (gen-tcp-connect raw:tcp-connect))
      
      ; : str nat -> iport oport
      (define tcp-connect/enable-break (gen-tcp-connect raw:tcp-connect/enable-break))
      
      ; FIX - support the reuse? flag.
      (define tcp-listen
        (opt-lambda (port [max-allow-wait 4] [reuse? #f] [hostname-string #f])
	  (hash-table-get
	   port-table
	   port
	   (lambda ()
	     (if (redirect? port)
		 (let ([listener (make-pipe-listener port (create-channel))])
		   (hash-table-put! port-table port listener)
		   listener)
		 (raw:tcp-listen port max-allow-wait reuse? hostname-string))))))
      
      ; : tst -> bool
      (define (tcp-listener? x)
        (or (pipe-listener? x) (raw:tcp-listener? x)))
      
      ; ---------- private ----------
      
      ; : (hash-table nat[port] -> tcp-listener)
      (define port-table (make-hash-table))
      
      (define redirect-table
        (let ([table (make-hash-table)])
          (for-each (lambda (x) (hash-table-put! table x #t))
                    redirected-ports)
          table))
      
      ; : nat -> bool
      (define (redirect? port)
        (hash-table-get redirect-table port (lambda () #f)))))
  
  (define local-address "127.0.0.1"))