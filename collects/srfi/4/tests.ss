(module tests mzscheme

  (provide homo-vec-test)
  
  (define (test desired proc . args)
    (let ([result (apply proc args)])
    (unless (equal? test result)
      (printf "test failed.  Expected: ~v\nGot: ~v\nAs a result of this test: ~v\n"
              desired result (cons proc args)))))
  
  ; cribbed from mzscheme's testing.ss:
  (define (nonneg-exact? x)
  (and (exact? x)
       (integer? x)
       (x . >= . 0)))

(define (pos-exact? x)
  (and (exact? x)
       (integer? x)
       (positive? x)))

  (define exn-table
  (list (cons exn? (cons exn-message string?))
	(cons exn? (cons exn-continuation-marks continuation-mark-set?))
	(cons exn:syntax? (cons exn:syntax-expr (lambda (x) (or (eq? x #f) (syntax? x)))))
	(cons exn:syntax? (cons exn:syntax-form (lambda (x) (or (not x) (symbol? x)))))
	(cons exn:syntax? (cons exn:syntax-module (lambda (x) (or (eq? x #f) (symbol? x) (module-path-index? x)))))
	(cons exn:variable? (cons exn:variable-id symbol?))
	(cons exn:application:arity? (cons exn:application-value integer?))
	(cons exn:application:arity? (cons exn:application:arity-expected
					   (lambda (a)
					     (or (integer? a)
						 (and (arity-at-least? a)
						      (integer? (arity-at-least-value a)))
						 (and (list? a)
						      (andmap
						       (lambda (a)
							 (or (integer? a)
							     (and (arity-at-least? a)
								  (integer? 
								   (arity-at-least-value a)))))
						       a))))))
	(cons exn:application:type? (cons exn:application:type-expected symbol?))
	
	(cons exn:read? (cons exn:read-line (lambda (x) (if x (pos-exact? x) #t))))
	(cons exn:read? (cons exn:read-column (lambda (x) (if x (pos-exact? x) #t))))
	(cons exn:read? (cons exn:read-position (lambda (x) (if x (pos-exact? x) #t))))
	(cons exn:read? (cons exn:read-span (lambda (x) (if x (nonneg-exact? x) #t))))

	(cons exn:i/o:port? (cons exn:i/o:port-port (lambda (x) (or (input-port? x) (output-port? x)))))
	(cons exn:i/o:port:read? (cons exn:i/o:port-port input-port?))
	(cons exn:i/o:port:write? (cons exn:i/o:port-port output-port?))
	(cons exn:i/o:filesystem? (cons exn:i/o:filesystem-pathname string?))
	(cons exn:i/o:filesystem? (cons exn:i/o:filesystem-detail (lambda (x)
								    (memq x '(#f
									      ill-formed-path
									      already-exists
									      wrong-version)))))))
  
  (define thunk-error-test
    (case-lambda 
      [(th expr) (thunk-error-test th expr exn:application:type?)]
      [(th expr exn?)
       (set! expr (syntax-object->datum expr))
       (write expr)
       (display "  =e=> ")
       (call/ec (lambda (escape)
                  (let* ([old-esc-handler (error-escape-handler)]
                         [old-handler (current-exception-handler)]
                         [orig-err-port (current-error-port)]
                         [test-handler
                          (lambda ()
                            (escape #t))]
                         [test-exn-handler
                          (lambda (e)
                            (when (and exn? (not (exn? e)))
                              (printf " WRONG EXN TYPE: ~s " e))
                            (when (exn:syntax? e)
                              (printf " LATE SYNTAX EXN: ~s " e))
                            
                            (for-each
                             (lambda (row)
                               (let ([pred? (car row)])
                                 (when (pred? e)
                                   (let ([sel (cadr row)]
                                         [pred? (cddr row)])
                                     (unless (pred? (sel e))
                                       (printf " WRONG EXN ELEM ~s: ~s " sel e))))))
                             exn-table)
                            
                            (old-handler e))])
                    (dynamic-wind
                     (lambda () 
                       (current-error-port (current-output-port))
                       (current-exception-handler test-exn-handler)
                       (error-escape-handler test-handler))
                     (lambda ()
                       (let ([v (th)])
                         (write v)
                         (display " BUT EXPECTED ERROR")
                         (newline)
                         #f))
                     (lambda () 
                       (current-error-port orig-err-port)
                       (current-exception-handler old-handler)
                       (error-escape-handler old-esc-handler))))))]))
  
  (define-syntax err/rt-test
    (lambda (stx)
      (syntax-case stx ()
        [(_ e exn?)
         (syntax
          (thunk-error-test (lambda () e) (quote-syntax e) exn?))]
        [(_ e)
         (syntax
          (err/rt-test e exn:application:type?))])))
  
  (define arity-test 
    (case-lambda
      [(f min max except)
       (letrec ([aok?
                 (lambda (a)
                   (cond
                     [(integer? a) (= a min max)]
                     [(arity-at-least? a) (and (negative? max)
                                               (= (arity-at-least-value a) min))]
                     [(and (list? a) (andmap integer? a))
                      (and (= min (car a)) (= max
                                              (let loop ([l a])
                                                (if (null? (cdr l))
                                                    (car l)
                                                    (loop (cdr l))))))]
                     [(list? a)
                      ;; Just check that all are consistent for now.
                      ;; This should be improved.
                      (andmap
                       (lambda (a)
                         (if (number? a)
                             (<= min a (if (negative? max) a max))
                             (>= (arity-at-least-value a) min)))
                       a)]
                     [else #f]))]
                [make-ok?
                 (lambda (v)
                   (lambda (e)
                     (and (exn:application:arity? e)
                          (= (exn:application-value e) v)
                          (aok? (exn:application:arity-expected e)))))]
                [do-test
                 (lambda (f args check?)
                   (printf "(apply ~s '~s)  =e=> " f args)
                   (let/ec done
                     (let ([v (with-handlers ([void
                                               (lambda (exn)
                                                 (if (check? exn)
                                                     (printf " ~a~n" (exn-message exn))
                                                     (let ([ok-type? (exn:application:arity? exn)])
                                                       (printf " WRONG EXN ~a: ~s~n" 
                                                               (if ok-type?
                                                                   "FIELD"
                                                                   "TYPE")
                                                               exn)))
                                                 (done (void)))])
                                (apply f args))])
                       (printf "~s~n BUT EXPECTED ERROR~n" v))))])
         (let loop ([n 0][l '()])
           (unless (>= n min)
             (unless (memq n except)
               (do-test f l (make-ok? n)))
             (loop (add1 n) (cons 1 l))))
         (let loop ([n min])
           (unless (memq n except)
             (test #t procedure-arity-includes? f n))
           (unless (>= n max)
             (loop (add1 n))))
         (if (>= max 0)
             (do-test f (let loop ([n 0][l '(1)])
                          (if (= n max)
                              l
                              (loop (add1 n) (cons 1 l))))
                      (make-ok? (add1 max)))
             (test #t procedure-arity-includes? f (arithmetic-shift 1 100))))]
      [(f min max) (arity-test f min max null)]))
  
  (define (homo-vec-test vec->homo   
                         maker
                         homo->vec
                         length
                         getter
                         setter
                         pred
                         sum
                         difference
                         scale
                         norm
                         type-idx
                         filling-maker
                         list->homo
                         homo->list)
    (test #t pred (maker 3 4 5))
    (test #t pred (maker))
    (arity-test pred 1 1)
    (test 3 getter (maker 3 4 5) 0)
    (test 4 getter (maker 3 4 5) 1)
    (test 5 getter (maker 3 4 5) 2)
    (test 3 length (maker 3 4 5))
    (test 0 length (maker))
    (arity-test length 1 1)
    (err/rt-test (length "apple") exn:application:mismatch?)
    (arity-test getter 2 2)
    (err/rt-test (getter "apple" 3) exn:application:mismatch?)
    (err/rt-test (getter (maker 4 5 6) 3) exn:application:mismatch?)
    (err/rt-test (getter (maker) 0) exn:application:mismatch?)
    (err/rt-test (getter (maker) (expt 2 100)) exn:application:mismatch?)
    (err/rt-test (getter (maker 4 5 6) -1))
    (err/rt-test (getter (maker 4 5 6) 2.0))
    (err/rt-test (getter (maker 4 5 6) "2"))
    (let ((vec (maker 4 5 6)))
      (setter vec 1 190)
      (test 190 getter vec 1))
    (test (maker 3 3) filling-maker 2 3)
    (test (maker) filling-maker 0)
    (test (maker) filling-maker 0 14)
    (test 2048 length (filling-maker 2048 14))
    (arity-test filling-maker 1 2)
    (err/rt-test (filling-maker "a" 2))
    (err/rt-test (filling-maker 1.0 2))
    (err/rt-test (filling-maker 10.2 2))
    (err/rt-test (filling-maker -1 2))
    (err/rt-test (filling-maker 1000000000000000000000 2) exn:misc:out-of-memory?)
    (arity-test setter 3 3)
    (err/rt-test (setter #() 0 'x) exn:application:mismatch?)
    (err/rt-test (setter (maker 1 2 3) -1 'x))
    (err/rt-test (setter (maker 1 2 3) 3 'x) exn:application:mismatch?)
    (err/rt-test (setter (maker 1 2 3) (expt 2 100) 'x) exn:application:mismatch?)
    (err/rt-test (setter '(1 2 3) 2 'x))
    (err/rt-test (setter (maker 1 2 3) "2" 'x))
    (arity-test sum 2 2)
    (arity-test difference 2 2)
    (arity-test norm 1 1)
    (arity-test scale 2 2)
    (test (maker 3 4 5) list->homo `(3 4 5))
    (test `(3 4 5) homo->list (maker 3 4 5))))