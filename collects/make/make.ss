
(require-relative-library "makes.ss")
(invoke-open-unit/sig (require-relative-library "maker.ss"))

(define-macro make
  (let ([make (lambda (spec argv)
		(let ([form-error (lambda (s . p) (apply syntax-error 'make s spec p))])
		  (and (or (list? spec) (form-error "illegal specification (not a sequence)"))
		       (or (pair? spec) (form-error "empty specification"))
		       (andmap
			(lambda (line)
			  (and (or (and (list? line) (>= (length line) 2))
				   (form-error "clause does not have at least 2 parts" line))
			       (let ([name (car line)])
				 (or (list? (cadr line))
				     (line-error "second part of clause is not a sequence" (cadr line))))))
			spec))
		  `(make* (list ,@(map (lambda (line)
					 `(list ,(car line)
						(list ,@(cadr line))
						,@(let ([l (cddr line)])
						    (if (null? l)
							null
							`((lambda ()
							    ,@l))))))
				       spec))
			  ,argv)))])
    (case-lambda
     [(spec) (make spec #())]
     [(spec argv) (make spec argv)])))
