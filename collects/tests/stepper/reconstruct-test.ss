(require (prefix annotate: (lib "annotate.ss" "stepper" "private")))
(require (prefix kernel: (lib "kerncase.ss" "syntax")))
(require (prefix reconstruct: (lib "reconstruct.ss" "stepper" "private")))
(require (lib "shared.ss" "stepper" "private"))
(require (lib "highlight-placeholder.ss" "stepper" "private"))
(require (lib "etc.ss"))

(load "/Users/clements/plt/tests/mzscheme/testing.ss")

(SECTION 'stepper-reconstruct)


(define (make-break num-steps expr action)
  (let ([counter num-steps])
    (lambda (mark-set key break-kind returned-value-list)
      (if (> counter 0)
          (set! counter (- counter 1))
          (let ([mark-list (continuation-mark-set->list mark-set key)])
            (action (reconstruct:reconstruct-current expr mark-list break-kind returned-value-list)))))))

(define (string->stx-list stx)
  (let ([port (open-input-string stx)])
    (let loop ([first-stx (read-syntax 'test-program port)])
      (if (eof-object? first-stx)
          null
          (cons first-stx (loop (read-syntax 'test-program port)))))))
  
  
  
(define (annotate-exprs stx num-steps action)
    (let loop ([env annotate:initial-env-package] [stx-list (string->stx-list stx)])
      (if (null? stx-list)
          null
          (let*-values ([(break) (make-break num-steps (car stx-list) action)]
                        [(annotated new-env)
                         (annotate:annotate (expand (car stx-list))
                                            env break 'foot-wrap)])
          (cons annotated (loop new-env (cdr stx-list)))))))

(define (test-expr stx num-steps namespace)
  (let/ec k
    (parameterize ([current-namespace namespace])
      (map eval (annotate-exprs stx num-steps k)))))

(define (test-sequence source-list result-list namespace)
  (for-each (lambda (result step-num)
              (test result test-expr source-list step-num namespace))
            result-list
            (build-list (length result-list) (lambda (x) x))))

(define (namespace-rewrite-expr stx namespace)
  (parameterize ([current-namespace namespace])
    (map annotate:top-level-rewrite (map expand (string->stx-list stx)))))

(define mz-namespace (current-namespace))
(define (test-mz-sequence source-list result-list)
  (test-sequence source-list result-list mz-namespace))

(define beginner-namespace (make-namespace 'empty))
(parameterize ([current-namespace beginner-namespace])
  (namespace-attach-module mz-namespace 'mzscheme)
  (namespace-require '(lib "htdp-beginner.ss" "lang")))
(define (test-beginner-sequence source-list result-list)
  (test-sequence source-list result-list beginner-namespace))


(test `((,highlight-placeholder) (+)) test-expr "+" 0 mz-namespace)

(test-mz-sequence "(+ 3 4)"
                  `((((,highlight-placeholder 3 4)) (+))            
                    (((,highlight-placeholder 3 4)) (+))
                    ((,highlight-placeholder) ((+ 3 4)))
                    ((,highlight-placeholder) (7))))

(test-mz-sequence "((lambda (x) (+ x 3)) 4)"
                  `(((,highlight-placeholder) (((lambda (x) (+ x 3)) 4)))
                    ((,highlight-placeholder) ((+ 4 3)))
                    (((,highlight-placeholder 4 3)) (+))
                    (((,highlight-placeholder 4 3)) (+))
                    ((,highlight-placeholder) ((+ 4 3)))
                    ((,highlight-placeholder) (7))))

(test-mz-sequence "(if 3 4 5)"
                  `(((,highlight-placeholder) ((if 3 4 5)))
                    ((,highlight-placeholder) (4))))

(test-mz-sequence "((lambda (x) x) 3)"
                  `(((,highlight-placeholder) (((lambda (x) x) 3)))
                    ((,highlight-placeholder) (3))))

; 'begin' not yet supported by reconstruct
;(test-mz-sequence "((lambda (x) x) (begin (+ 3 4) (+ 4 5)"))
;                  `((((begin (,highlight-placeholder 3 4) (+ 4 5))) (+))
;                    (((begin (,highlight-placeholder 3 4) (+ 4 5))) (+))
;                    (((begin ,highlight-placeholder (+ 4 5))) ((+ 3 4)))
;                    (((begin ,highlight-placeholder (+ 4 5))) (7))
;                    ((,highlight-placeholder) ((begin 7 (+ 4 5))))
;                    ((,highlight-placeholder) ((+ 4 5)))
;                    (((,highlight-placeholder 4 5)) (+))
;                    (((,highlight-placeholder 4 5)) (+))
;                    ((,highlight-placeholder) ((+ 4 5)))
;                    ((,highlight-placeholder) (9))))

(test-mz-sequence "((lambda (a) (lambda (b) (+ a b))) 14)"
                  `(((,highlight-placeholder) (((lambda (a) (lambda (b) (+ a b))) 14)))
                    ((,highlight-placeholder) ((lambda (b) (+ 14 b))))))

(test-mz-sequence "((case-lambda ((a) 3) ((b c) (+ b c))) 5 6)"
                  `(((,highlight-placeholder) (((case-lambda ((a) 3) ((b c) (+ b c))) 5 6)))
                    ((,highlight-placeholder) ((+ 5 6)))
                    (((,highlight-placeholder 5 6)) (+))
                    (((,highlight-placeholder 5 6)) (+))
                    ((,highlight-placeholder) ((+ 5 6)))
                    ((,highlight-placeholder) (11))))

; reconstruct does not handle one-armed if's:
;(test-mz-sequence "(if 3 4)"
;                  `(((,highlight-placeholder) ((if 3 4)))
;                    ((,highlight-placeholder) (4))))

; reconstruct does not handle begin0

;(test-mz-sequence "(let ([a 3]) 4)"
;                  `(((,highlight-placeholder) ((let-values ([(a) 3]) 4)) (,highlight-placeholder ,highlight-placeholder) ((define-values (a_0) 3) (begin 4)))
;                    (((define a_0 3))))) 
;
;(test-mz-sequence "(let ([a (+ 4 5)] [b (+ 9 20)]) (+ a b))"
;                  `(((,highlight-placeholder) ((let-values ([(a) (+ 4 5)] [(b) (+ 9 20)]) (+ a b))) 
;                     (,highlight-placeholder ,highlight-placeholder ,highlight-placeholder) 
;                     ((define-values (a_0) (+ 4 5)) (define-values (b_1) (+ 9 20)) (begin (+ a_0 b_1))))
;                    (((define-values (a_0) (,highlight-placeholder 4 5)) (define-values (b_1) (+ 9 20)) (begin (+ a_0 b_1))) (+))
;                    (((define-values (a_0) (,highlight-placeholder 4 5)) (define-values (b_1) (+ 9 20)) (begin (+ a_0 b_1))) (+))
;                    (((define-values (a_0) ,highlight-placeholder) (define-values (b_1) (+ 9 20)) (begin (+ a_0 b_1))) ((+ 4 5)))
;                    (((define-values (a_0) ,highlight-placeholder) (define-values (b_1) (+ 9 20)) (begin (+ a_0 b_1))) (9))
;                    (((define a_0 9) (define-values (b_1) (,highlight-placeholder 9 20)) (begin (+ a_0 b_1))) (+))
;                    (((define a_0 9) (define-values (b_1) (,highlight-placeholder 9 20)) (begin (+ a_0 b_1))) (+))
;                    (((define a_0 9) (define-values (b_1) ,highlight-placeholder) (begin (+ a_0 b_1))) ((+ 9 20)))
;                    (((define a_0 9) (define-values (b_1) ,highlight-placeholder) (begin (+ a_0 b_1))) (29))
;                    (((define a_0 9) (define b_1 29)))
;                    (((,highlight-placeholder a_0 b_1)) (+))
;                    (((,highlight-placeholder a_0 b_1)) (+))
;                    (((+ ,highlight-placeholder b_1)) (a_0))
;                    (((+ ,highlight-placeholder b_1)) (9))
;                    (((+ 9 ,highlight-placeholder)) (b_1))
;                    (((+ 9 ,highlight-placeholder)) (29))
;                    ((,highlight-placeholder) ((+ 9 29)))
;                    ((,highlight-placeholder) (38))))

(test-mz-sequence "((call/cc call/cc) (call/cc call/cc))"
                  `(((((,highlight-placeholder call/cc) (call/cc call/cc))) (call/cc))
                    ((((,highlight-placeholder call/cc) (call/cc call/cc))) (call-with-current-continuation))
                    ((((call-with-current-continuation ,highlight-placeholder) (call/cc call/cc))) (call/cc))
                    ((((call-with-current-continuation ,highlight-placeholder) (call/cc call/cc))) (call-with-current-continuation))
                    (((,highlight-placeholder (call/cc call/cc))) ((call-with-current-continuation call-with-current-continuation)))
                    (((,highlight-placeholder (call/cc call/cc))) ((lambda args ...)))
                    ((((lambda args ...) (,highlight-placeholder call/cc))) (call/cc))
                    ((((lambda args ...) (,highlight-placeholder call/cc))) (call-with-current-continuation))
                    ((((lambda args ...) (call-with-current-continuation ,highlight-placeholder))) (call/cc))
                    ((((lambda args ...) (call-with-current-continuation ,highlight-placeholder))) (call-with-current-continuation))
                    ((((lambda args ...) ,highlight-placeholder)) ((call-with-current-continuation call-with-current-continuation)))
                    ((((lambda args ...) ,highlight-placeholder)) ((lambda args ...)))
                    ((,highlight-placeholder) (((lambda args ...) (lambda args ...))))))

;(test-mz-sequence '(begin (define g 3) g)
;                  `(((,highlight-placeholder) (g))
;                    ((,highlight-placeholder) 3)))

;(syntax-object->datum (cadr (annotate-expr test2 'mzscheme 0 (lambda (x) x))))

(test-beginner-sequence "(if true 3 4)"
                        `((((if ,highlight-placeholder 3 4)) (true))
                          (((if ,highlight-placeholder 3 4)) (true))
                          ((,highlight-placeholder) ((if true 3 4)))
                          ((,highlight-placeholder) (3))))

(test-beginner-sequence "(cond [false 4] [false 5] [true 3])"
               `((((cond (,highlight-placeholder 4) (false 5) (true 3))) (false))
                 (((cond (,highlight-placeholder 4) (false 5) (true 3))) (false))
                 ((,highlight-placeholder) ((cond (false 4) (false 5) (true 3))))
                 ((,highlight-placeholder) ((cond (false 5) (true 3))))
                 (((cond (,highlight-placeholder 5) (true 3))) (false))
                 (((cond (,highlight-placeholder 5) (true 3))) (false))
                 ((,highlight-placeholder) ((cond (false 5) (true 3))))
                 ((,highlight-placeholder) ((cond (true 3))))
                 (((cond (,highlight-placeholder 3))) (true))
                 (((cond (,highlight-placeholder 3))) (true))
                 ((,highlight-placeholder) ((cond (true 3))))
                 ((,highlight-placeholder) (3))))

(test-beginner-sequence "(cond [false 4] [else 9])"
               `((((cond [,highlight-placeholder 4] [else 9])) (false))
                 (((cond [,highlight-placeholder 4] [else 9])) (false))
                 ((,highlight-placeholder) ((cond [false 4] [else 9])))
                 ((,highlight-placeholder) (9))))

(syntax-case (car (namespace-rewrite-expr "(or true false true)" beginner-namespace)) (let-values if)
  [(let-values ([(or-part-0) true-0]) (if or-part-1 or-part-2 rest))
   (begin
     (test 'or-part syntax-e (syntax or-part-0))
     (test 'let-bound syntax-property (syntax or-part-0) 'stepper-binding-type)
     (test 'or-part syntax-e (syntax or-part-1))
     (test 'let-bound syntax-property (syntax or-part-1) 'stepper-binding-type))])

(test-beginner-sequence "(or true false true)"
                        `((((or ,highlight-placeholder false true)) (true))
                          (((or ,highlight-placeholder false true)) (true))
                          ((,highlight-placeholder) ((or true false true)))
                          ((,highlight-placeholder) ((or false true)))
                          (((or ,highlight-placeholder true)) (false))
                          (((or ,highlight-placeholder true)) (false))
                          ((,highlight-placeholder) ((or false true)))
                          ((,highlight-placeholder) (false))))

(report-errs)