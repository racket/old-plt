
(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'deep)

; Test deep stacks

(define proc-depth 100000)

(test (- proc-depth) 'deep-recursion (let loop ([n proc-depth])
				       (if (zero? n)
					   0
					   (sub1 (loop (sub1 n))))))

(test 0 'deep-recursion-escape/ec
      (let/ec k
	(let loop ([n proc-depth])
	  (if (zero? n)
	      (k 0)
	      (sub1 (loop (sub1 n)))))))

(test 0 'deep-recursion-escape/cc
      (let/cc k
	(let loop ([n proc-depth])
	  (if (zero? n)
	      (k 0)
	      (sub1 (loop (sub1 n)))))))

(define paren-port
  (let* ([depth 50000]
	 [closing? #f]
	 [count depth])
    (make-input-port
     (lambda ()
       (cond
	[closing?
	 (if (= count depth)
	     eof
	     (begin
	       (set! count (add1 count))
	       #\) ))]
	[else
	 (set! count (sub1 count))
	 (when (zero? count)
	       (set! closing? #t))
	 #\(]))
     (lambda () #t)
     void)))

(define deep-list (read paren-port))

(test #t 'read-deep (pair? deep-list))

(define s (open-output-string))
(display deep-list s)
(test 'ok 'display 'ok)

(test #t 'equal? (equal? deep-list (read (open-input-string (get-output-string s)))))

(define going? #t)
(define (equal?-forever l1 l2)
  (let ([t (thread (lambda () 
		     (equal? l1 l2) ; runs forever; could run out of memory
		     (set! going? #f)))])
    (sleep 1)
    (kill-thread t)
    going?))


(define l1 (cons 0 #f))
(set-cdr! l1 l1)
(define l2 (cons 0 #f))
(set-cdr! l2 l2)
(test #t 'equal?-forever (equal?-forever l1 l2))

(define l1 (cons 0 #f))
(set-car! l1 l1)
(define l2 (cons 0 #f))
(set-car! l2 l2)
(test #t 'equal?-forever/memory (equal?-forever l1 l2))

(define l1 (vector 0))
(vector-set! l1 0 l1)
(define l2 (vector 0))
(vector-set! l2 0 l2)
(test #t 'equal?-forever/vector (equal?-forever l1 l2))

(define-struct a (b c))
(define l1 (make-a 0 #f))
(set-a-b! l1 l1)
(define l2 (make-a 0 #f))
(set-a-b! l2 l2)
(test #t 'equal?-forever/struct (equal?-forever l1 l2))

(define l1 (box 0))
(set-box! l1 l1)
(define l2 (box 0))
(set-box! l2 l2)
(test #t 'equal?-forever/struct (equal?-forever l1 l2))

(report-errs)
