
(module port mzscheme
  (require (lib "etc.ss")
	   (lib "contract.ss"))

  (provide open-output-nowhere
	   make-input-port/read-to-peek
	   merge-input
	   copy-port
	   input-port-append
	   convert-stream
	   make-limited-input-port)

  (define (exact-non-negative-integer? i)
    (and (number? i) (exact? i) (integer? i) (i . >= . 0)))

  (define (input-port-with-progress-evts? ip)
    (and (input-port? ip)
	 (port-provides-progress-evts? ip)))

  (define (mutable-bytes? b)
    (and (bytes? b) (not (immutable? b))))
  (define (mutable-string? b)
    (and (string? b) (not (immutable? b))))
  
  (provide/contract (read-bytes-avail!-evt (mutable-bytes? input-port-with-progress-evts? 
							   . -> . evt?))
		    (read-bytes!-evt (mutable-bytes? input-port-with-progress-evts? 
						     . -> . evt?))
		    (read-bytes-evt (exact-non-negative-integer? input-port-with-progress-evts? 
								 . -> . evt?))
		    (read-string!-evt (mutable-string? input-port-with-progress-evts? 
						       . -> . evt?))
		    (read-string-evt (exact-non-negative-integer? input-port-with-progress-evts? 
								  . -> . evt?))
		    (regexp-match-evt ((union regexp? byte-regexp? string? bytes?) 
				       input-port-with-progress-evts? 
				       . -> . evt?)))

  ;; ----------------------------------------

  (define open-output-nowhere
    (opt-lambda ([name 'nowhere])
      (make-output-port 
       name
       always-evt
       (lambda (s start end non-block? breakable?) (- end start))
       void
       (lambda (special non-block? breakable?) #t)
       (lambda (s start end) (convert-evt
			      always-evt
			      (lambda (x)
				(- end start))))
       (lambda (special) (convert-evt always-evt (lambda (x) #t))))))

  ;; ----------------------------------------

  (define (copy-port src dest . dests)
    (unless (input-port? src)
      (raise-type-error 'copy-port "input-port" src))
    (for-each
     (lambda (dest)
       (unless (output-port? dest)
	 (raise-type-error 'copy-port "output-port" dest)))
     (cons dest dests))
    (let ([s (make-bytes 4096)])
      (let loop ()
	(let ([c (read-bytes-avail! s src)])
	  (unless (eof-object? c)
	    (for-each
	     (lambda (dest)
	       (let loop ([start 0])
		 (unless (= start c)
		   (let ([c2 (write-bytes-avail s dest start c)])
		     (loop (+ start c2))))))
	     (cons dest dests))
	    (loop))))))
  
  (define merge-input
    (case-lambda
     [(a b) (merge-input a b 4096)]
     [(a b limit)
      (or (input-port? a)
	  (raise-type-error 'merge-input "input-port" a))
      (or (input-port? b)
	  (raise-type-error 'merge-input "input-port" b))
      (or (not limit)
	  (and (number? limit) (positive? limit) (exact? limit) (integer? limit))
	  (raise-type-error 'merge-input "positive exact integer or #f" limit))
      (let-values ([(rd wt) (make-pipe limit)]
		   [(other-done?) #f]
		   [(sema) (make-semaphore 1)])
	(let ([copy
	       (lambda (from)
                 (thread 
                  (lambda ()
                    (copy-port from wt)
                    (semaphore-wait sema)
                    (if other-done?
                        (close-output-port wt)
                        (set! other-done? #t))
                    (semaphore-post sema))))])
	  (copy a)
	  (copy b)
	  rd))]))

  ;; Not kill-safe.
  ;; Works only when read proc never returns an event.
  (define (make-input-port/read-to-peek name read fast-peek close)
    (define lock-semaphore (make-semaphore 1))
    (define-values (peeked-r peeked-w) (make-pipe))
    (define peeked-end 0)
    (define special-peeked null)
    (define special-peeked-tail #f)
    (define progress-requested? #f)
    (define (try-again)
      (convert-evt
       (semaphore-peek-evt lock-semaphore)
       (lambda (x) 0)))
    (define (make-progress)
      (write-byte 0 peeked-w)
      (read-byte peeked-r))
    (define (read-it s)
      (call-with-semaphore
       lock-semaphore
       (lambda ()
	 (do-read-it s))
       try-again))
    (define (do-read-it s)
      (if (char-ready? peeked-r)
	  (read-bytes-avail!* s peeked-r)
	  ;; If nothing is saved from a peeking read,
	  ;; dispatch to `read', otherwise
	  (cond
	   [(null? special-peeked) 
	    (when progress-requested? (make-progress))
	    (read s)]
	   [else (if (bytes? (car special-peeked))
		     (let ([b (car special-peeked)])
		       (set! peeked-end (+ peeked-end (bytes-length b)))
		       (write-bytes b peeked-w)
		       (set! special-peeked (cdr special-peeked))
		       (when (null? special-peeked)
			 (set! special-peeked-tail #f))
		       (read-bytes-avail!* s peeked-r))
		     (begin0
		      (car special-peeked)
		      (make-progress)
		      (set! special-peeked (cdr special-peeked))
		      (when (null? special-peeked)
			(set! special-peeked-tail #f))))])))
    (define (peek-it s skip unless-evt)
      (call-with-semaphore
       lock-semaphore
       (lambda ()
	 (do-peek-it s skip unless-evt))
       try-again))
    (define (do-peek-it s skip unless-evt)
      (let ([v (peek-bytes-avail!* s skip unless-evt peeked-r)])
	(if (zero? v)
	    ;; The peek may have failed because peeked-r is empty,
	    ;; because unless-evt is ready, or because the skip is
	    ;; far. Handle nicely the common case where there are no
	    ;; specials.
	    (cond
	     [(and unless-evt (sync/timeout 0 unless-evt))
	      0]
	     [(null? special-peeked)
	      ;; Empty special queue, so read through the original proc
	      (let ([r (read s)])
		(cond
		 [(number? r)
		  ;; The nice case --- reading gave us more bytes
		  (set! peeked-end (+ r peeked-end))
		  (write-bytes s peeked-w 0 r)
		  ;; Now try again
		  (peek-bytes-avail!* s skip #f peeked-r)]
		 [else
		  (set! special-peeked (cons r null))
		  (set! special-peeked-tail special-peeked)
		  ;; Now try again
		  (do-peek-it s skip unless-evt)]))]
	     [else
	      ;; Non-empty special queue, so try to use it
	      (let* ([pos (file-position peeked-r)]
		     [avail (- peeked-end pos)]
		     [sk (- skip avail)])
		(let loop ([sk sk]
			   [l special-peeked])
		  (cond
		   [(null? l)
		    ;; Not enough even in the special queue.
		    ;; Read once and add it.
		    (let* ([t (make-bytes (min 4096 (+ sk (bytes-length s))))]
			   [r (read s)])
		      (cond
		       [(evt? r) 
			;; We can't deal with an event, so complain
			(error 'make-input-port/read-to-peek 
			       "original read produced an event: ~e"
			       r)]
		       [(eq? r 0) 
			;; Original read thinks a spin is ok,
			;;  so we return 0 to skin, too.
			0]
		       [else (let ([v (if (number? r)
					  (subbytes t 0 r)
					  r)])
			       (set-cdr! special-peeked-tail v)
			       ;; Got something; now try again
			       (do-peek-it s skip unless-evt))]))]
		   [(eof-object? (car l)) 
		    ;; No peeking past an EOF
		    eof]
		   [(procedure? (car l))
		    (if (zero? sk)
			(car l)
			(loop (sub1 sk) (cdr l)))]
		   [(bytes? (car l))
		    (let ([len (bytes-length (car l))])
		      (if (sk . < . len)
			  (let ([n (min (bytes-length s)
					(- len sk))])
			    (bytes-copy! s 0 (car l) sk (+ sk n))
			    n)
			  (loop (- sk len) (cdr l))))])))])
	    v)))
    (define (commit-it amt unless-evt dont-evt)
      (call-with-semaphore
       lock-semaphore
       (lambda ()
	 (do-commit-it amt unless-evt dont-evt))))
    (define (do-commit-it amt unless-evt dont-evt)
      (if (sync/timeout 0 unless-evt)
	  #f
	  (let* ([pos (file-position peeked-r)]
		 [avail (- peeked-end pos)]
		 [p-commit (min avail amt)])
	    (let loop ([amt (- amt p-commit)]
		       [l special-peeked])
	      (cond
	       [(amt . <= . 0)
		;; Enough has been peeked...
		(unless (zero? p-commit)
		  (peek-byte peeked-r (sub1 amt))
		  (port-commit-peeked amt #f peeked-r))
		(set! special-peeked l)
		(when (null? special-peeked)
		  (set! special-peeked-tail #f))
		#t]
	       [(null? l)
		;; Requested commit was larger than previous peeks
		#f]
	       [(bytes? (car l))
		(let ([bl (bytes-length (car l))])
		  (if (bl . > . amt)
		      ;; Split the string
		      (let ([next (cons
				   (subbytes (car l) amt)
				   (cdr l))])
			(set-car! l (subbytes (car l) 0 amt))
			(set-cdr! l next)
			(when (eq? l special-peeked-tail)
			  (set! special-peeked-tail next))
			(loop 0 (cdr l)))
		      ;; Consume this string...
		      (loop  (- amt bl) (cdr l))))]
	       [else
		(loop (sub1 amt) (cdr l))])))))
    (make-input-port
     name
     ;; Read
     read-it
     ;; Peek
     (if fast-peek
	 (let ([fast-peek-k (lambda (s skip)
			      (peek-it s skip #f))])
	   (lambda (s skip unless-evt)
	     (if unless-evt
		 (peek-it s skip unless-evt)
		 (fast-peek s skip fast-peek-k))))
	 peek-it)
     close
     (lambda ()
       (set! progress-requested? #t)
       (port-progress-evt peeked-r))
     commit-it))
       

  (define input-port-append
    (opt-lambda (close-orig? . ports)
      (make-input-port
       (map object-name ports)
       (lambda (str)
	 ;; Reading is easy -- read from the first port,
	 ;;  and get rid of it if the result is eof
	 (if (null? ports)
	     eof
	     (let ([n (read-bytes-avail!* str (car ports))])
	       (cond
		[(eq? n 0) (car ports)]
		[(eof-object? n)
		 (when close-orig?
		   (close-input-port (car ports)))
		 (set! ports (cdr ports))
		 0]
		[else n]))))
       (lambda (str skip unless-evt)
	 ;; Peeking is more difficult, due to skips.
	 (let loop ([ports ports][skip skip])
	   (if (null? ports)
	       eof
	       (let ([n (peek-bytes-avail!* str skip unless-evt (car ports))])
		 (cond
		  [(eq? n 0) 
		   ;; Not ready, yet.
		   (car ports)]
		  [(eof-object? n)
		   ;; Port is exhausted, or we skipped past its input.
		   ;; If skip is not zero, we need to figure out
		   ;;  how many chars were skipped.
		   (loop (cdr ports)
			 (- skip (compute-avail-to-skip skip (car ports))))]
		  [else n])))))
       (lambda ()
	 (when close-orig?
	   (map close-input-port ports))))))

  (define (convert-stream from from-port
			  to to-port)
    (let ([c (bytes-open-converter from to)]
	  [in (make-bytes 4096)]
	  [out (make-bytes 4096)])
      (unless c
	(error 'convert-stream "could not create converter from ~e to ~e"
	       from to))
      (dynamic-wind
	  void
	  (lambda ()
	    (let loop ([got 0])
	      (let ([n (read-bytes-avail! in from-port got)])
		(let ([got (+ got (if (eof-object? n)
				      0
				      n))])
		  (let-values ([(wrote used status) (bytes-convert c in 0 got out)])
		    (when (eq? status 'error)
		      (error 'convert-stream "conversion error"))
		    (unless (zero? wrote)
		      (write-bytes out to-port 0 wrote))
		    (bytes-copy! in 0 in used got)
		    (if (eof-object? n)
			(begin
			  (unless (= got used)
			    (error 'convert-stream "input stream ended with a partial conversion"))
			  (let-values ([(wrote status) (bytes-convert-end c out)])
			    (when (eq? status 'error)
			      (error 'convert-stream "conversion-end error"))
			    (unless (zero? wrote)
			      (write-bytes out to-port 0 wrote))
			    ;; Success
			    (void)))
			(loop (- got used))))))))
	  (lambda () (bytes-close-converter c)))))

  ;; Helper for input-port-append; given a skip count
  ;;  and an input port, determine how many characters
  ;;  (up to upto) are left in the port. We figure this
  ;;  out using binary search.
  (define (compute-avail-to-skip upto p)
    (let ([str (make-bytes 1)])
      (let loop ([upto upto][skip 0])
	(if (zero? upto)
	    skip
	    (let* ([half (quotient upto 2)]
		   [n (peek-bytes-avail!* str (+ skip half) #f p)])
	      (if (eq? n 1)
		  (loop (- upto half 1) (+ skip half 1))
		  (loop half skip)))))))

  (define make-limited-input-port
    (opt-lambda (port limit [close-orig? #t])
      (let ([got 0])
	(make-input-port
	 (object-name port)
	 (lambda (str)
	   (let ([count (min (- limit got) (bytes-length str))])
	     (if (zero? count)
		 eof
		 (let ([n (read-bytes-avail!* str port 0 count)])
		   (cond
		    [(eq? n 0) port]
		    [(number? n) (set! got (+ got n)) n]
		    [else n])))))
	 (lambda (str skip)
	   (let ([count (max 0 (min (- limit got skip) (bytes-length str)))])
	     (if (zero? count)
		 eof
		 (let ([n (peek-bytes-avail!* str skip #f port 0 count)])
		   (if (eq? n 0)
		       port
		       n)))))
	 (lambda ()
	   (when close-orig?
	     (close-input-port port)))))))

  ;; ----------------------------------------

  (define (poll-or-spawn go)
    (poll-guard-evt
     (lambda (poll?)
       (if poll?
	   ;; In poll mode, call `go' directly:
	   (let ([v (go never-evt #f #t)])
	     (if v
		 (convert-evt always-evt (lambda (x) v))
		 never-evt))
	   ;; In non-poll mode, start a thread to call go
	   (nack-guard-evt
	    (lambda (nack)
	      (define ch (make-channel))
	      (let ([t (thread (lambda () 
				 (parameterize-break #t
				   (with-handlers ([exn:break? void])
				     (go nack ch #f)))))])
		(thread (lambda ()
			  (sync nack) 
			  (break-thread t))))
	      ch))))))

  (define (read-at-least-bytes!-evt orig-bstr input-port need-more? shrink combo)
    ;; go is the main reading function, either called directly for
    ;; a poll, or called in a thread for a non-poll read
    (define (go nack ch poll?)
      (let try-again ([pos 0][bstr orig-bstr])
	(let* ([progress-evt (port-progress-evt input-port)]
	       [v ((if poll? 
		       peek-bytes-avail!* 
		       peek-bytes-avail!)
		   bstr pos progress-evt input-port pos)])
	  (cond
	   [(sync/timeout 0 nack) (void)]
	   [(sync/timeout 0 progress-evt) (if poll?
					      #f
					      (try-again pos bstr))]
	   [(and poll? (equal? v 0)) #f]
	   [(and (number? v) (need-more? bstr (+ pos v)))
	    => (lambda (bstr)
		 (try-again (+ v pos) bstr))]
	   [else
	    (let* ([v2 (cond
			[(number? v) (shrink bstr (+ v pos))]
			[(positive? pos) pos]
			[else v])]
		   [result (combo bstr v2)])
	      (cond
	       [(port-commit-peeked (if (number? v2) v2 1)
				    progress-evt
				    (if poll?
					always-evt
					(channel-put-evt ch result))
				    input-port)
		result]
	       [poll? #f]
	       [else (try-again 0 orig-bstr)]))]))))
    (poll-or-spawn go))
  
  (define (read-bytes-avail!-evt bstr input-port)
    (read-at-least-bytes!-evt bstr input-port 
			      (lambda (bstr v) (if (zero? v)
						   bstr
						   #f))
			      (lambda (bstr v) v)
			      (lambda (bstr v) v)))

  (define (read-bytes!-evt bstr input-port)
    (read-at-least-bytes!-evt bstr input-port 
			      (lambda (bstr v)
				(if (v . < . (bytes-length bstr))
				    bstr
				    #f))
			      (lambda (bstr v) v)
			      (lambda (bstr v) v)))
  
  (define (read-bytes-evt len input-port)
    (let ([bstr (make-bytes len)])
      (convert-evt
       (read-bytes!-evt bstr input-port)
       (lambda (v)
	 (if (number? v)
	     (if (= v len)
		 bstr
		 (subbytes bstr 0 v))
	     v)))))
	   
  (define (read-string-evt goal input-port)
    (let ([bstr (make-bytes goal)]
	  [c (bytes-open-converter "UTF-8-permissive" "UTF-8")])
      (convert-evt
       (read-at-least-bytes!-evt bstr input-port 
				 (lambda (bstr v)
				   (if (= v (bytes-length bstr))
				       ;; We can't easily use bytes-utf-8-length here,
				       ;; because we may need more bytes to figure out
				       ;; the true role of the last byte. The
				       ;; `bytes-convert' function lets us deal with
				       ;; the last byte properly.
				       (let-values ([(bstr2 used status) 
						     (bytes-convert c bstr 0 v)])
					 (let ([got (bytes-utf-8-length bstr2)])
					   (if (= got goal)
					       ;; Done:
					       #f 
					       ;; Need more bytes:
					       (let ([bstr2 (make-bytes (+ v (- goal got)))])
						 (bytes-copy! bstr2 0 bstr)
						 bstr2))))
				       ;; Need more bytes in bstr:
				       bstr))
				 (lambda (bstr v)
				   ;; We may need one less than v,
				   ;; because we may have had to peek
				   ;; an extra byte to discover an
				   ;; error in the stream.
				   (if ((bytes-utf-8-length bstr #\? 0 v) . > . goal)
				       (sub1 v)
				       v))
				 cons)
       (lambda (bstr+v)
	 (let ([bstr (car bstr+v)]
	       [v (cdr bstr+v)])
	   (if (number? v)
	       (bytes->string/utf-8 bstr #\? 0 v)
	       v))))))

  (define (read-string!-evt str input-port)
    (convert-evt
     (read-string-evt (string-length str) input-port)
     (lambda (s)
       (if (string? s)
	   (begin
	     (string-copy! str 0 s)
	     (string-length s))
	   s))))

  (define (regexp-match-evt pattern input-port)
    (define (go nack ch poll?)
      (let try-again ()
	(let* ([progress-evt (port-progress-evt input-port)]
	       [m ((if poll?
		       regexp-match-peek-positions* 
		       regexp-match-peek-positions)
		   pattern input-port 0 #f progress-evt)])
	  (cond
	   [(sync/timeout 0 nack) (void)]
	   [(sync/timeout 0 progress-evt) (try-again)]
	   [(not m)
	    (if poll?
		#f
		(sync nack
		      (finish-evt progress-evt
				  (lambda (x) (try-again)))))]
	   [else
	    (let ([m2 (map (lambda (p)
			     (and p
				  (let ([bstr (make-bytes (- (cdr p) (car p)))])
				    (unless (= (car p) (cdr p))
				      (let loop ([offset 0])
					(let ([v (peek-bytes-avail! bstr (car p) progress-evt input-port offset)])
					  (unless (zero? v)
					    (when ((+ offset v) . < . (bytes-length bstr))
					      (loop (+ offset v)))))))
				    bstr)))
			   m)])
	      (cond
	       [(port-commit-peeked (cdar m) 
				    progress-evt 
				    (if poll?
					always-evt
					(channel-put-evt ch m2))
				    input-port)
		m2]
	       [poll? #f]
	       [else (try-again)]))]))))
    (poll-or-spawn go))

  )
