
(load-relative "loadtest.ss")

;; Needs to be expanded!

(SECTION 'MACRO)

(define-syntax mx
  (lambda (stx)
    (syntax-case stx ()
      [(_ x)
       (syntax (x 1 8))])))
(test 9 'macro (mx +))
(test -7 'macro (mx -))
(test 18 'macro (let ([mx (lambda (x) (x 1 8 9))]) (mx +)))
(teval '(test 13 'let-macro (let-syntax ([mx (lambda (stx)
					       (syntax-case stx ()
						 [(_ x) (syntax (x 6 7))]))])
			      (mx +))))
(teval '(test -7 'let-macro (let-syntax ([mx2 (lambda (stx)
					       (syntax-case stx ()
						 [(_ x y) (syntax (mx y))]))])
				       (mx2 + -))))
(teval '(test '(10) 'let-macro ((lambda () (let-syntax ([x (lambda (stx)
							     (syntax-case stx ()
							       [(_ v) (syntax (list v))]))])
					     (x 10))))))

(teval '(test '(10) 'let-macro (let () 
				 (define-syntax x
				   (lambda (stx)
				     (syntax-case stx ()
				       [(_ v) (syntax (list v))])))
				 (x 10))))
(teval '(test '(10) 'let-macro ((lambda () 
				  (define-syntax x
				    (lambda (stx)
				      (syntax-case stx ()
					[(_ v) (syntax (list v))])))
				  (x 10)))))

(report-errs)
