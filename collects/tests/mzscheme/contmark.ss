

(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'continuation-marks)

(test null current-continuation-marks 'key)

(test '(10) 'wcm (with-continuation-mark 'key 10 
		   (current-continuation-marks 'key)))
(test '(11) 'wcm (with-continuation-mark 'key 10 
		   (with-continuation-mark 'key 11
		     (current-continuation-marks 'key))))
(test '(9) 'wcm (with-continuation-mark 'key 10 
	     (with-continuation-mark 'key2 9
	       (with-continuation-mark 'key 11
		 (current-continuation-marks 'key2)))))
(test '() 'wcm (with-continuation-mark 'key 10 
	    (with-continuation-mark 'key2 9
	      (with-continuation-mark 'key 11
		(current-continuation-marks 'key3)))))

(test '() 'wcm (let ([x (with-continuation-mark 'key 10 (list 100))])
		 (current-continuation-marks 'key)))

(test '(11) 'wcm (with-continuation-mark 'key 11
		   (let ([x (with-continuation-mark 'key 10 (current-continuation-marks 'key))])
		     (current-continuation-marks 'key))))

(define (get-marks)
  (current-continuation-marks 'key))

(define (tail-apply f)
  (with-continuation-mark 'key 'tail
    (f)))

(define (non-tail-apply f)
  (with-continuation-mark 'key 'non-tail
    (car (cons (f) null))))

(test '(tail) tail-apply get-marks)
(test '(non-tail) non-tail-apply get-marks)
(test '(tail non-tail) non-tail-apply (lambda () (tail-apply get-marks)))
(test '(non-tail) tail-apply (lambda () (non-tail-apply get-marks)))

(define (mark-x f)
  (lambda ()
    (with-continuation-mark 'key 'x (f))))

(test '(x) tail-apply (mark-x get-marks))
(test '(x non-tail) non-tail-apply (mark-x get-marks))

(test '(x) tail-apply (lambda () (tail-apply (mark-x get-marks))))
(test '(x non-tail non-tail) non-tail-apply (lambda () (non-tail-apply (mark-x get-marks))))
(test '(x non-tail) tail-apply (lambda () (non-tail-apply (mark-x get-marks))))
(test '(x non-tail) non-tail-apply (lambda () (tail-apply (mark-x get-marks))))

;; Make sure restoring continuations restores the marks:
(let ([l null])
 (let ([did-once? #f]
       [did-twice? #f]
       [try-again #f]
       [get-marks #f])
   
   (with-continuation-mark
    'key (let/cc k (set! try-again k) 1)
    (begin
      (unless did-once?
	(set! get-marks (let/cc k k)))
      (set! l (cons (current-continuation-marks 'key) l))))

   (if did-once?
       (unless did-twice?
	 (set! did-twice? #t)
	 (get-marks #f))
       (begin
	 (set! did-once? #t)
	 (try-again 2))))

 (test '((1) (2) (1)) 'call/cc-restore-marks l))

;; Create a deep mark stack 10 times
(let loop ([n 10])
  (unless (zero? n)
    (let* ([max 1000]
	   [r (add1 (random max))])
      (test (list 0 r)
	    `(loop ,n)
	    (with-continuation-mark 'base 0
	      (let loop ([n max])
		(if (zero? n)
		    (append
		     (current-continuation-marks 'base)
		     (current-continuation-marks r))
		    (with-continuation-mark n n
		      (loop (sub1 n))))))))
    (loop (sub1 n))))

;; Make sure marks are separate in separate threads
(let ([s1 (make-semaphore 0)]
      [s2 (make-semaphore 0)]
      [result null])
  (thread (lambda ()
	    (with-continuation-mark 'key 'b.1
	      (begin
		(semaphore-wait s1)
		(with-continuation-mark 'key 'b.2
		  (begin
		    (semaphore-post s2)
		    (semaphore-wait s1)
		    (with-continuation-mark 'key 'b.4
		      (begin
			(set! result (current-continuation-marks 'key))
			(semaphore-post s2)))
		    'ok))
		'ok))))
  (thread-wait
   (thread (lambda ()
	     (with-continuation-mark 'key 'a.1
	       (begin
		 (semaphore-post s1)
		 (with-continuation-mark 'key 'a.2
		   (begin
		     (semaphore-wait s2)
		     (with-continuation-mark 'key 'a.3
		       (begin
			 (semaphore-post s1)
			 (with-continuation-mark 'key 'a.4
			   (begin
			     (semaphore-wait s2)
			     (set! result (append (current-continuation-marks 'key) result))))
			 'ok))
		     'ok))
		 'ok)))))
  (test '(a.4 a.3 a.2 a.1 b.4 b.2 b.1) 'thread-marks result))

(arity-test current-continuation-marks 1 1)

(report-errs)
