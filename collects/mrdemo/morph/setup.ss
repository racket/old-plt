(print-struct #t)
(print-graph #t)
(define last
  (lambda (x)
    (cond
     [(null? x) (error 'last "empty list")]
     [(null? (cdr x)) (car x)]
     [else (last (cdr x))])))

(define foldl2
  (lambda (f init l)
    (letrec ([helper
	      (lambda (l sofar)
		(cond
		 [(null? l) sofar]
		 [else (helper (cdr l) (f (car l) sofar))]))])
      (helper l init))))

(define tabulate
  (lambda (n f)
    (letrec ([build (lambda (i)
		      (cond
		       [(= i n) null]
		       [else (cons (f i) (build (1+ i)))]))])
      (list->vector (build 0)))))

(define-macro package
  (lambda (x . args)
    (car 'package)))
