
(module port mzscheme
  (require (lib "etc.ss")
	   (lib "contract.ss")
	   (lib "list.ss"))

  (provide open-output-nowhere
	   make-pipe-with-specials
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

  (define (line-mode-symbol? s)
    (memq s '(linefeed return return-linefeed any any-one)))
  
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
				       . -> . evt?))

		    (read-bytes-line-evt (case->
					  (input-port-with-progress-evts? . -> . evt?)
					  (input-port-with-progress-evts? line-mode-symbol? . -> . evt?)))
		    (read-line-evt (case->
				    (input-port-with-progress-evts? . -> . evt?)
				    (input-port-with-progress-evts? line-mode-symbol? . -> . evt?)))
		    (eof-evt (input-port-with-progress-evts? . -> . evt?)))

  ;; ----------------------------------------

  (define open-output-nowhere
    (opt-lambda ([name 'nowhere])
      (make-output-port 
       name
       always-evt
       (lambda (s start end non-block? breakable?) (- end start))
       void
       (lambda (special non-block? breakable?) #t)
       (lambda (s start end) (wrap-evt
			      always-evt
			      (lambda (x)
				(- end start))))
       (lambda (special) (wrap-evt always-evt (lambda (x) #t))))))

  ;; ----------------------------------------

  (define (copy-port src dest . dests)
    (unless (input-port? src)
      (raise-type-error 'copy-port "input-port" src))
    (for-each
     (lambda (dest)
       (unless (output-port? dest)
	 (raise-type-error 'copy-port "output-port" dest)))
     (cons dest dests))
    (let ([s (make-bytes 4096)]
	  [dests (cons dest dests)])
      (let loop ()
	(let ([c (read-bytes-avail! s src)])
	  (cond
	   [(number? c)
	    (for-each
	     (lambda (dest)
	       (let loop ([start 0])
		 (unless (= start c)
		   (let ([c2 (write-bytes-avail s dest start c)])
		     (loop (+ start c2))))))
	     dests)
	    (loop)]
	   [(procedure? c)
	    (let ([v (let-values ([(l col p) (port-next-location src)])
		       (c (object-name src) l col p))])
	      (for-each
	       (lambda (dest) (write-special v dest))
	       dests))
	    (loop)]
	   [else
	    ;; Must be EOF
	    (void)])))))
  
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
      (let-values ([(rd wt) (make-pipe-with-specials limit)]
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
  ;; If the `read' proc returns an event, the event must produce 
  ;;  0 always
  (define (make-input-port/read-to-peek name read fast-peek close)
    (define lock-semaphore (make-semaphore 1))
    (define commit-semaphore (make-semaphore 1))
    (define-values (peeked-r peeked-w) (make-pipe))
    (define special-peeked null)
    (define special-peeked-tail #f)
    (define progress-requested? #f)
    (define manager-th #f)
    (define manager-ch #f)
    (define (suspend-commit)
      (when manager-ch
	(channel-put manager-ch #f)))
    (define (resume-commit)
      (when manager-ch
	(channel-put manager-ch #f)))
    (define (try-again)
      (wrap-evt
       (semaphore-peek-evt lock-semaphore)
       (lambda (x) 0)))
    (define (make-progress)
      (write-byte 0 peeked-w)
      (read-byte peeked-r))
    (define (read-it-with-lock s)
      (suspend-commit)
      (begin0
       (do-read-it s)
       (resume-commit)))
    (define (read-it s)
      (call-with-semaphore
       lock-semaphore
       read-it-with-lock
       try-again
       s))
    (define (do-read-it s)
      (if (char-ready? peeked-r)
	  (read-bytes-avail!* s peeked-r)
	  ;; If nothing is saved from a peeking read,
	  ;; dispatch to `read', otherwise return
	  ;; previously peeked data
	  (cond
	   [(null? special-peeked) 
	    (when progress-requested? (make-progress))
	    (read s)]
	   [else (if (bytes? (car special-peeked))
		     (let ([b (car special-peeked)])
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
	 (suspend-commit)
	 (begin0
	  (do-peek-it s skip unless-evt)
	  (resume-commit)))
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
	      #f]
	     [(null? special-peeked)
	      ;; Empty special queue, so read through the original proc
	      (let* ([t (make-bytes (min 4096 (+ skip (bytes-length s))))]
		     [r (read t)])
		(cond
		 [(number? r)
		  ;; The nice case --- reading gave us more bytes
		  (write-bytes t peeked-w 0 r)
		  ;; Now try again
		  (peek-bytes-avail!* s skip #f peeked-r)]
		 [(evt? r)
		  (if unless-evt
		      ;; Technically, there's a race condition here.
		      ;; We might choose r (and return 0) even when
		      ;; unless-evt becomes available first. However,
		      ;; this race is detectable only by the inside
		      ;; of `read'.
		      (choice-evt r (wrap-evt unless-evt (lambda (x) #f)))
		      r)]
		 [else
		  (set! special-peeked (cons r null))
		  (set! special-peeked-tail special-peeked)
		  ;; Now try again
		  (do-peek-it s skip unless-evt)]))]
	     [else
	      ;; Non-empty special queue, so try to use it
	      (let* ([avail (pipe-content-length peeked-r)]
		     [sk (- skip avail)])
		(let loop ([sk sk]
			   [l special-peeked])
		  (cond
		   [(null? l)
		    ;; Not enough even in the special queue.
		    ;; Read once and add it.
		    (let* ([t (make-bytes (min 4096 (+ sk (bytes-length s))))]
			   [r (read t)])
		      (cond
		       [(evt? r) 
			(if unless-evt
			    ;; See note above
			    (choice-evt r (wrap-evt unless-evt (lambda (x) #f)))
			    r)]
		       [(eq? r 0) 
			;; Original read thinks a spin is ok,
			;;  so we return 0 to skin, too.
			0]
		       [else (let ([v (if (number? r)
					  (subbytes t 0 r)
					  r)])
			       (let ([pr (cons v null)])
				 (set-cdr! special-peeked-tail pr)
				 (set! special-peeked-tail pr))
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
    (define (commit-it amt unless-evt done-evt)
      (call-with-semaphore
       lock-semaphore
       (lambda ()
	 (suspend-commit)
	 (begin0
	  (do-commit-it amt unless-evt done-evt)
	  (resume-commit)))))
    (define (do-commit-it amt unless-evt done-evt)
      (if (sync/timeout 0 unless-evt)
	  #f
	  (let* ([avail (pipe-content-length peeked-r)]
		 [p-commit (min avail amt)])
	    (let loop ([amt (- amt p-commit)]
		       [l special-peeked])
	      (cond
	       [(amt . <= . 0)
		;; Enough has been peeked. Do commit...
		(actual-commit p-commit l unless-evt done-evt)]
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
    (define (actual-commit p-commit l unless-evt done-evt)
      ;; The `finish' proc finally, actually, will commit...
      (define (finish)
	(unless (zero? p-commit)
	  (peek-byte peeked-r (sub1 p-commit))
	  (port-commit-peeked p-commit unless-evt always-evt peeked-r))
	(set! special-peeked l)
	(when (null? special-peeked)
	  (set! special-peeked-tail #f))
	(when (and progress-requested? (zero? p-commit))
	  (make-progress))
	#t)
      ;; If we can sync done-evt immediately, then finish.
      (if (sync/timeout 0 (wrap-evt done-evt (lambda (x) #t)))
	  (finish)
	  ;; We need to wait, so we'll have to release the lock.
	  ;; Send the work to a manager thread.
	  (let ([result-ch (make-channel)])
	    (unless manager-th
	      (set! manager-ch (make-channel))
	      (set! manager-th (thread manage-commits)))
	    (thread-resume manager-th (current-thread))
	    (channel-put manager-ch (list finish unless-evt done-evt result-ch))
	    (semaphore-post lock-semaphore)
	    (resume-commit)
	    (begin0
	     (sync result-ch)
	     (semaphore-wait lock-semaphore)
	     (suspend-commit)))))
    (define (manage-commits)
      (let loop ([commits null] [suspended? #t])
	(apply
	 sync
	 (handle-evt manager-ch
		     (lambda (c)
		       (if c
			   ;; adding a commit
			   (loop (cons c commits) suspended?)
			   ;; suspend/resume request:
			   (loop commits (not suspended?)))))
	 (if suspended?
	     (list never-evt)
	     (map (lambda (c)
		    (define (send-result v)
		      ;; Create a new thread to send the result asynchronously:
		      (thread-resume
		       (thread (lambda ()
				 (channel-put (list-ref c 3) v)))
		       (current-thread))
		      (loop (remq c commits) #f))
		    ;; Choose between done and unless:
		    (choice-evt
		     (handle-evt (list-ref c 1)
				 (lambda (x)
				   ;; unless ready, which means that the commit must fail
				   (send-result #f)))
		     (handle-evt (list-ref c 2)
				 (lambda (x)
				   ;; done-evt ready, which means that the commit
				   ;;  must succeed.
				   ;; If we get here, then commits are not
				   ;; suspended, so we implicitly have the
				   ;; lock.
				   ((list-ref c 0))
				   (send-result #t)))))
		  commits)))))
    (make-input-port
     name
     ;; Read
     read-it
     ;; Peek
     (if fast-peek
	 (let ([fast-peek-k (lambda (s skip)
			      (peek-it s skip #f))])
	   (lambda (s skip unless-evt)
	     (if (or unless-evt
		     (char-ready? peeked-r)
		     (pair? special-peeked))
		 (peek-it s skip unless-evt)
		 (fast-peek s skip fast-peek-k))))
	 peek-it)
     close
     (lambda ()
       (set! progress-requested? #t)
       (port-progress-evt peeked-r))
     commit-it))

  ;; Not kill-safe.
  (define make-pipe-with-specials
    ;; This implementation of pipes is almost CML-style, with a manager thread
    ;; to guard access to the pipe content. But we only enable the manager
    ;; thread when write evts are active; otherwise, we use a lock semaphore.
    ;; (Actually, the lock semaphore has to be used all the time, to guard
    ;; the flag indicating whether the manager thread is running.)
    (opt-lambda ([limit (expt 2 64)] [in-name 'pipe] [out-name 'pipe])
      (let-values ([(r w) (make-pipe limit)]
		   [(more) null]
		   [(more-last) #f]
		   [(more-sema) #f]
		   [(close-w?) #f]
		   [(lock-semaphore) (make-semaphore 1)]
		   [(mgr-th) #f]
		   [(via-manager?) #f]
		   [(mgr-ch) (make-channel)])
	(define (flush-more)
	  (if (null? more)
	      (begin
		(set! more-last #f)
		(when close-w?
		  (close-output-port w)))
	      (when (bytes? (car more))
		(let ([amt (bytes-length (car more))])
		  (let ([wrote (write-bytes-avail* (car more) w)])
		    (if (= wrote amt)
			(begin
			  (set! more (cdr more))
			  (flush-more))
			(begin
			  ;; This means that we let too many bytes
			  ;;  get written while a special was pending.
			  ;;  Too bad...
			  (set-car! more (subbytes (car more) wrote))
			  ;; By peeking, make room for more:
			  (peek-byte r (sub1 (min (pipe-content-length w)
						  (- amt wrote))))
			  (flush-more))))))))
	(define (read-one s)
	  (let ([v (read-bytes-avail!* s r)])
	    (if (eq? v 0)
		(if more-last
		    ;; Return a special
		    (let ([a (car more)])
		      (set! more (cdr more))
		      (flush-more)
		      (lambda (file line col ppos)
			a))
		    ;; Nothing available, yet.
		    (begin
		      (unless more-sema
			(set! more-sema (make-semaphore)))
		      (wrap-evt (semaphore-peek-evt more-sema)
				(lambda (x) 0))))
		v)))
	(define (close-it)
	  (set! close-w? #t)
	  (unless more-last
	    (close-output-port w))
	  (when more-sema
	    (semaphore-post more-sema)))
	(define (write-these-bytes str start end)
	  (begin0
	   (if more-last
	       (let ([p (cons (subbytes str start end) null)])
		 (set-cdr! more-last p)
		 (set! more-last p)
		 (- end start))
	       (let ([v (write-bytes-avail* str w start end)])
		 (if (zero? v)
		     (wrap-evt w (lambda (x) #f))
		     v)))
	   (when more-sema
	     (semaphore-post more-sema)
	     (set! more-sema #f))))
	(define (write-spec v)
	  (let ([p (cons v null)])
	    (if more-last
		(set-cdr! more-last p)
		(set! more p))
	    (set! more-last p)
	    (when more-sema
	      (semaphore-post more-sema)
	      (set! more-sema #f))))
	(define (serve)
	  ;; A request is
	  ;;  (list sym result-ch nack-evt . v)
	  ;; where `v' varies for different `sym's
	  ;; The possible syms are: read, reply, close, 
	  ;;  write, write-spec, write-evt, write-spec-evt
	  (let loop ([reqs null])
	    (apply
	     sync
	     ;; Listen for a request:
	     (handle-evt mgr-ch
			 (lambda (req)
			   (let ([req
				  ;; Most requests we handle immediately and
				  ;; convert to a reply. The manager thread
				  ;; implicitly has the lock.
				  (let ([reply (lambda (v)
						 (list 'reply (cadr req) (caddr req) v))])
				    (case (car req)
				      [(read)
				       (printf "read~n")
				       (reply (read-one (cadddr req)))]
				      [(close)
				       (reply (close-it))]
				      [(write)
				       (reply (apply write-these-bytes (cdddr req)))]
				      [(write-spec)
				       (reply (write-spec (cadddr req)))]
				      [else req]))])
			     (loop (cons req reqs)))))
	     (if (and (null? reqs)
		      via-manager?)
		 ;; If we can get the lock before another request
		 ;;  turn off manager mode:
		 (handle-evt lock-semaphore
			     (lambda (x)
			       (set! via-manager? #f)
			       (semaphore-post lock-semaphore)
			       (loop null)))
		 never-evt)
	     (append
	      (map (lambda (req)
		     (case (car req)
		       [(reply) (handle-evt (channel-put-evt (cadr req)
							     (cadddr req))
					    (lambda (x)
					      (loop (remq req reqs))))]
		       [(write-spec-evt) (if close-w?
					     ;; Report close error:
					     (handle-evt (channel-put-evt (cadr req) 'closed)
						    (lambda (x)
						      (loop (remq req reqs))))
					     ;; Try to write special:
					     (handle-evt (channel-put-evt (cadr req) #t)
							 (lambda (x)
							   ;; We sync'd, so now we *must* write
							   (write-spec (cadddr req))
							   (loop (remq req reqs)))))]
		       [(write-evt) (if close-w?
					;; Report close error:
					(handle-evt (channel-put-evt (cadr req) 'closed)
						    (lambda (x)
						      (loop (remq req reqs))))
					;; Try to write bytes:
					(let* ([start (list-ref req 4)]
					       [end (list-ref req 5)]
					       [len (if more-last
							(- end start)
							(min (- end start)
							     (max 0
								  (- limit (pipe-content-length w)))))])
					  (if (zero? len)
					      (handle-evt w (lambda (x) (loop reqs)))
					      (handle-evt (channel-put-evt (cadr req) len)
							  (lambda (x)
							    ;; We sync'd, so now we *must* write
							    (write-these-bytes (cadddr req) start (+ start len))
							    (loop (remq req reqs)))))))]))
		   reqs)
	      ;; nack => remove request (could be anything)
	      (map (lambda (req)
		     (handle-evt (caddr req)
				 (lambda (x)
				   (loop (remq req reqs)))))
		   reqs)))))
	(define (via-manager what req-sfx)
	  (thread-resume mgr-th (current-thread))
	  (let ([ch (make-channel)])
	    (sync (nack-guard-evt
		   (lambda (nack)
		     (channel-put mgr-ch (list* what ch nack req-sfx))
		     ch)))))
	(define (start-mgr)
	  (unless mgr-th
	    (set! mgr-th (thread serve)))
	  (set! via-manager? #t))
	(define (evt what req-sfx)
	  (nack-guard-evt
	   (lambda (nack)
	     (resume-mgr)
	     (let ([ch (make-channel)])
	       (call-with-semaphore
		lock-semaphore
		(lambda ()
		  (unless via-manager?
		    (set! mgr-th (thread serve)))
		  (set! via-manager? #t)
		  (thread-resume mgr-th (current-thread))
		  (channel-put mgr-ch (list* what ch nack req-sfx))
		  (wrap-evt ch (lambda (x)
				 (if (eq? x 'close)
				     (raise-mismatch-error 'write-evt "port is closed: " out)
				     x)))))))))
	(define (resume-mgr)
	  (when mgr-th
	    (thread-resume mgr-th (current-thread))))
	(define in 
	  ;; ----- Input ------
	  (make-input-port/read-to-peek 
	   in-name
	   (lambda (s)
	     (let ([v (read-bytes-avail!* s r)])
	       (if (eq? v 0)
		   (begin
		     (resume-mgr)
		     (call-with-semaphore
		      lock-semaphore
		      (lambda ()
			(if via-manager?
			    (via-manager 'read (list s))
			    (read-one s)))))
		   v)))
	   #f
	   void))
	(define out
	  ;; ----- Output ------
	  (make-output-port
	   out-name
	   w
	   ;; write
	   (lambda (str start end buffer? w/break?)
	     (if (= start end)
		 #t
		 (begin
		   (resume-mgr)
		   (call-with-semaphore
		    lock-semaphore
		    (lambda ()
		      (if via-manager?
			  (via-manager 'write (list str start end))
			  (write-these-bytes str start end)))))))
	   ;; close
	   (lambda ()
	     (resume-mgr)
	     (call-with-semaphore
	      lock-semaphore
	      (lambda ()
		(if via-manager?
		    (via-manager 'close null)
		    (close-it)))))
	   ;; write-special
	   (lambda (v buffer? w/break?)
	     (resume-mgr)
	     (call-with-semaphore
	      lock-semaphore
	      (lambda ()
		(if via-manager?
		    (via-manager 'write-spec (list v))
		    (write-spec v)))))
	   ;; write-evt
	   (lambda (str start end)
	     (if (= start end)
		 (wrap-evt always-evt (lambda (x) 0))
		 (evt 'write-evt (list str start end))))
	   ;; write-special-evt
	   (lambda (v)
	     (evt 'write-spec-evt (list v)))))
	(values in out))))
		   

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
		[(eq? n 0) (wrap-evt (car ports) (lambda (x) 0))]
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
		(let ([got (+ got (if (number? n)
				      n
				      0))])
		  (let-values ([(wrote used status) (bytes-convert c in 0 got out)])
		    (when (eq? status 'error)
		      (error 'convert-stream "conversion error"))
		    (unless (zero? wrote)
		      (write-bytes out to-port 0 wrote))
		    (bytes-copy! in 0 in used got)
		    (if (not (number? n))
			(begin
			  (unless (= got used)
			    (error 'convert-stream "input stream ~a with a partial conversion"
				   (if (eof-object? n) "ended" "hit a special value")))
			  (let-values ([(wrote status) (bytes-convert-end c out)])
			    (when (eq? status 'error)
			      (error 'convert-stream "conversion-end error"))
			    (unless (zero? wrote)
			      (write-bytes out to-port 0 wrote))
			    (if (eof-object? n)
				;; Success
				(void)
				(begin
				  (write-special n to-port)
				  (loop 0)))))
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
		    [(eq? n 0) (wrap-evt port (lambda (x) 0))]
		    [(number? n) (set! got (+ got n)) n]
		    [(procedure? n) (set! got (add1 got)) n]
		    [else n])))))
	 (lambda (str skip progress-evt)
	   (let ([count (max 0 (min (- limit got skip) (bytes-length str)))])
	     (if (zero? count)
		 eof
		 (let ([n (peek-bytes-avail!* str skip progress-evt port 0 count)])
		   (if (eq? n 0)
		       (wrap-evt port (lambda (x) 0))
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
		 (wrap-evt always-evt (lambda (x) v))
		 never-evt))
	   ;; In non-poll mode, start a thread to call go
	   (nack-guard-evt
	    (lambda (nack)
	      (define ch (make-channel))
	      (define ready (make-semaphore))
	      (let ([t (thread (lambda () 
				 (parameterize-break #t
				   (with-handlers ([exn:break? void])
				     (semaphore-post ready)
				     (go nack ch #f)))))])
		(thread (lambda ()
			  (sync nack) 
			  (semaphore-wait ready)
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
	   ;; the first two cases below are shortcuts, and not
	   ;;  strictly necessary
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
	       [(and (eof-object? eof)
		     (zero? pos)
		     (not (sync/timeout 0 progress-evt)))
		;; Must be a true end-of-file
		(let ([result (combo bstr eof)])
		  (if poll?
		      result
		      (channel-put-evt ch result)))]
	       [poll? #f]
	       [else (try-again 0 orig-bstr)]))]))))
    (if (zero? (bytes-length orig-bstr))
	(wrap-evt always-evt (lambda (x) 0))
	(poll-or-spawn go)))
  
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
      (wrap-evt
       (read-bytes!-evt bstr input-port)
       (lambda (v)
	 (if (number? v)
	     (if (= v len)
		 bstr
		 (subbytes bstr 0 v))
	     v)))))
	   
  (define (read-string-evt goal input-port)
    (if (zero? goal)
	(wrap-evt always-evt (lambda (x) ""))
	(let ([bstr (make-bytes goal)]
	      [c (bytes-open-converter "UTF-8-permissive" "UTF-8")])
	  (wrap-evt
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
		   v)))))))

  (define (read-string!-evt str input-port)
    (wrap-evt
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
		       regexp-match-peek-positions-immediate 
		       regexp-match-peek-positions)
		   pattern input-port 0 #f progress-evt)])
	  (cond
	   [(sync/timeout 0 nack) (void)]
	   [(sync/timeout 0 progress-evt) (try-again)]
	   [(not m)
	    (if poll?
		#f
		(sync nack
		      (handle-evt progress-evt
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
	       [(and (zero? (cdar m))
		     (or poll?
			 (channel-put ch m2)))
		m2]
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

  (define-syntax (newline-rx stx)
    (syntax-case stx ()
      [(_ str) (datum->syntax-object #'here
				     (byte-regexp 
				      (string->bytes/latin-1
				       (format "^(?:(.*?)~a)|(.*?$)"
					       (syntax-e #'str)))))]))

  (define read-bytes-line-evt
    (opt-lambda (input-port [mode 'linefeed])
      (wrap-evt
       (regexp-match-evt (case mode
			   [(linefeed) (newline-rx "\n")]
			   [(return) (newline-rx "\r")]
			   [(return-linefeed) (newline-rx "\r\n")]
			   [(any) (newline-rx "(?:\r\n|\r|\n)")]
			   [(any-one) (newline-rx "[\r\n]")])
			 input-port)
       (lambda (m)
	 (or (cadr m)
	     (let ([l (caddr m)])
	       (if (and l (zero? (bytes-length l)))
		   eof
		   l)))))))
  
  (define read-line-evt
    (opt-lambda (input-port [mode 'linefeed])
      (wrap-evt
       (read-bytes-line-evt input-port mode)
       (lambda (s)
	 (if (eof-object? s)
	     s
	     (bytes->string/utf-8 s #\?))))))

  (define (eof-evt input-port)
    (wrap-evt
     (regexp-match-evt #rx#"^$" input-port)
     (lambda (x)
       eof))))
