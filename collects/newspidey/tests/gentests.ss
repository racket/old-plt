(require-library "functio.ss")

(define *max-args* 5)
(define *max-cases* 6)
(define *max-formals* 5)
(define *cookie-max* 10)
(define *max-depth* 6)

(define (go)
  (gen-app '() 0))

(define (gen-var vars depth) 
  (if (null? vars)
      42
      (list-ref vars (random (length vars)))))
  
(define gen-formal gensym)

(define (gen-app vars depth)
  (if (>= depth *max-depth*)
      (gen-var vars depth)
      (let* ([numargs (random *max-args*)]
	     [rator (gen-exp vars (add1 depth))]
	     [rands (build-list numargs (lambda (_) 
					  (gen-exp vars (add1 depth))))])
	(cons rator rands))))

(define (gen-lambda vars depth)
  (if (>= depth *max-depth*)
      (gen-var vars depth)
      (let* ([num-cases (add1 (random *max-cases*))]
	     [cases
	      (let loop ([n 0])
		(if (>= n num-cases)
		    '()
		    (cons (gen-case vars depth) (loop (add1 n)))))])
	(cons 'case-lambda cases))))

(define (gen-case vars depth)
  (let* ([num-formals (add1 (random *max-formals*))]
	 [formals (build-list num-formals 
			      (lambda (_) (gen-formal)))]) 
    (list formals (gen-exp (append formals vars) (add1 depth)))))

(define (gen-exp vars depth)
  (let ([cookie (random *cookie-max*)])
    (cond
     [(<= cookie 2) (gen-app vars depth)]
     [(<= cookie 9) (gen-lambda vars depth)]
     [else (gen-var vars depth)])))

