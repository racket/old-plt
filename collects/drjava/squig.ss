(load-relative "split.ss")

;; probably not used
;; eat-semi! : Queue(Scanned) Queue(Scanned) -> Void
(define (eat-semi! from to)
  (let loop ()
    (cond
      [(mtq? from) (error 'eat-semi! "Eof reached before semi-colon found.")]
      [else
       (let ([scanned (deq! from)])
	 (enq! scanned to)
	 (unless (= (scanned-token scanned) jSEMI)
	   (loop)))])))

;; probably not used
;; file->q : String -> Queue(Scanned)
(define (file->q filename)
  (let* ([fin (jnew fin-class fin-init filename)]
	 [log (jnew log-class log-init)]
	 [scan (jnew scan-class scanner-init fin log)]
	 [q (mtq)])
    (let loop ()
      (let ([tok (enq-token! scan q)])
	(jcall scan next)
	(unless (= tok jEOF)
	  (loop))))
    q))


;; squiglies-match? : Queue(Scanned) -> Boolean
(define (squiglies-match? q)
  (let* ([q (quick-dupq q)]
	 [matching `((,jLPAREN ,jRPAREN) (,jLBRACKET ,jRBRACKET) (,jLBRACE ,jRBRACE))]
	 [closed-list (map cadr matching)]
	 [open? (lambda (x) (assq x matching))]
	 [closed? (lambda (x) (memq x closed-list))]
	 [matches?
	  (lambda (o c)
	    (let ([m (assq o matching)])
	      (and m (eq? (cadr m) c))))])
    (let loop ([open null])
      (cond
	[(mtq? q) (null? open)]
	[else
	 (let ([tok (scanned-token (deq! q))])
	   (cond
	     [(open? tok) (loop (cons tok open))]
	     [(closed? tok)
	      (if (and (pair? open) (matches? (car open) tok))
		  (loop (cdr open))
		  #f)]
	     [else (loop open)]))]))))