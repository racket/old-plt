(module xelda-lib mzscheme

  (require (lib "list.ss"))

  (provide 
   average
   gen-equal-units
   gen-mult-units
   gen-div-units
   gen-exp-units
   split-list
   check-empty-units
   check-equal-units
   verify-equal-units)

  (define (split-list lst p?)
    (let loop ([lst lst]
	       [yes null]
	       [no null])
      (if (null? lst)
	  (list (reverse yes) (reverse no))
	  (if (p? (car lst))
	      (loop (cdr lst) (cons (car lst) yes) no)
	      (loop (cdr lst) yes (cons (car lst) no))))))

  (define (canonicalize-units us)
    (filter 
     (lambda (u)
       (not (zero? (cadr u))))
     (quicksort us (lambda (u1 u2)
		     (string<=?  
		      (symbol->string (car u1))
		      (symbol->string (car u2)))))))

  (define (average . ns)
      (/ (apply + ns) (length ns)))
  
  (define (gen-equal-units . us)
    (let ([u1 (car us)])
      (if (andmap (lambda (u) (equal? u u1))
		  (cdr us))
	  u1
	  '((@error-equality 1)))))
  
  (define (check-equal-units us)
    (let ([base-unit (first us)])
      (if (andmap (lambda (u) (equal? u base-unit)) us)
          base-unit
          '((@error-equality 1)))))

  (define (verify-equal-units cell-name u1 u2)
    (if (equal? u1 u2)
	(printf "Verified units for cell ~a~n" cell-name)
	(printf (string-append 
		 "Units problem with cell ~a:~n"
		 "Cell annotated with ~a, formula yields ~a~n")
		cell-name u1 u2)))

  (define (gen-mult-units u1 u2)
    (let ([raw-us (append u1 u2)])
      (let loop ([new-us raw-us]
		 [result null])
	(if (null? new-us)
	    (canonicalize-units result)
	    (let* ([u (car new-us)]
		   [u-name (car u)]
		   [u-exp (cadr u)]
		   [u-and-non-u 
		    (split-list (cdr new-us) 
				(lambda (x) 
				  (eq? (car x) u-name)))]
		   [new-u-exp (+ u-exp 
				 (apply + (map cadr (car u-and-non-u))))]
		   [non-u (cadr u-and-non-u)])
	      (loop non-u (cons (list u-name new-u-exp) result)))))))

  (define (gen-div-units u1 u2)
    (gen-mult-units u1 (map (lambda (u)
			      (let ([u-name (car u)]
				    [u-exp (cadr u)])
				(list u-name (- 0 u-exp)))) u2)))

  (define (gen-exp-units u exp)
    (if (not (integer? exp))
	'((@error-exponentiation 1))
	(map (lambda (u)
		 (let ([u-name (car u)]
		       [u-exp (cadr u)])
		   (list u-name (inexact->exact 
				 (* exp u-exp)))))
	     u)))

  (define (check-empty-units . us)
    (if (andmap null? us)
	null
	'((@error-empty-unit 1)))))

