(require (prefix annotate: (lib "annotate.ss" "stepper" "private")))
(require (prefix kernel: (lib "kerncase.ss" "syntax")))
(require (prefix reconstruct: (lib "reconstruct.ss" "stepper" "private")))
(require (lib "shared.ss" "stepper" "private"))
(require (lib "highlight-placeholder.ss" "stepper" "private"))
(require (lib "etc.ss"))

(load "/Users/clements/plt/tests/mzscheme/testing.ss")

(SECTION 'stepper-reconstruct)


(define (wrap-expand-unwrap stx language-level-spec)
  (let* ([wrapped (datum->syntax-object #f `(module test-module ,language-level-spec
                                              ,@stx))]
         [expanded (expand wrapped)])
    (with-syntax ([(module name lang (_ . exprs)) expanded])
      (syntax->list (syntax exprs)))))

(test '((if (#%app verify-boolean (#%datum . 3) 'if) (#%datum . 4) (#%datum . 5)))
      map syntax-object->datum (wrap-expand-unwrap (list #'(if 3 4 5)) '(lib "htdp-beginner.ss" "lang")))

(define (make-break num-steps expr action)
  (let ([counter num-steps])
    (lambda (mark-set key break-kind returned-value-list)
      (if (> counter 0)
          (set! counter (- counter 1))
          (let ([mark-list (continuation-mark-set->list mark-set key)])
            (action (reconstruct:reconstruct-current expr mark-list break-kind returned-value-list)))))))
        
(define (annotate-expr stx lang num-steps action)
  (let loop ([env annotate:initial-env-package] [exprs (if lang 
                                                           (wrap-expand-unwrap (list stx) lang)
                                                           (list (expand stx)))])
    (if (null? exprs)
        null
        (let*-values ([(break) (make-break num-steps stx action)]
                      [(annotated new-env)
                       (annotate:annotate (car exprs) env break 'foot-wrap)])
          (cons annotated (loop new-env (cdr exprs)))))))

(define (test-expr stx lang num-steps selector)
  (let/ec k
    (map eval (selector (annotate-expr stx lang num-steps k)))))

(define (test-sequence source lang result-list oper)
  (for-each (lambda (result step-num)
              (test result test-expr source lang step-num oper))
            result-list
            (build-list (length result-list) (lambda (x) x))))

(define (test-mz-sequence source result-list)
  (test-sequence source 'mzscheme result-list cdr))

(test `((,highlight-placeholder) (+)) test-expr #'+ 'mzscheme 0 cdr)
(test-mz-sequence #'(+ 3 4)
                  `((((,highlight-placeholder 3 4)) (+))            
                    (((,highlight-placeholder 3 4)) (+))
                    ((,highlight-placeholder) ((+ 3 4)))
                    ((,highlight-placeholder) (7))))

(test-mz-sequence #'((lambda (x) (+ x 3)) 4)
                  `(((,highlight-placeholder) (((lambda (x) (+ x 3)) 4)))
                    ((,highlight-placeholder) ((+ 4 3)))
                    (((,highlight-placeholder 4 3)) (+))
                    (((,highlight-placeholder 4 3)) (+))
                    ((,highlight-placeholder) ((+ 4 3)))
                    ((,highlight-placeholder) (7))))

(test-mz-sequence #'(if 3 4 5)
                  `(((,highlight-placeholder) ((if 3 4 5)))
                    ((,highlight-placeholder) (4))))

(test-mz-sequence #'((lambda (x) x) 3)
                  `(((,highlight-placeholder) (((lambda (x) x) 3)))
                    ((,highlight-placeholder) (3))))

; 'begin' not yet supported by reconstruct
;(test-mz-sequence #'((lambda (x) x) (begin (+ 3 4) (+ 4 5)))
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

(test-mz-sequence #'((lambda (a) (lambda (b) (+ a b))) 14)
                  `(((,highlight-placeholder) (((lambda (a) (lambda (b) (+ a b))) 14)))
                    ((,highlight-placeholder) ((lambda (b) (+ 14 b))))))

(test-mz-sequence #'((case-lambda ((a) 3) ((b c) (+ b c))) 5 6)
                  `(((,highlight-placeholder) (((case-lambda ((a) 3) ((b c) (+ b c))) 5 6)))
                    ((,highlight-placeholder) ((+ 5 6)))
                    (((,highlight-placeholder 5 6)) (+))
                    (((,highlight-placeholder 5 6)) (+))
                    ((,highlight-placeholder) ((+ 5 6)))
                    ((,highlight-placeholder) (11))))

; reconstruct does not handle one-armed if's:
;(test-mz-sequence #'(if 3 4)
;                  `(((,highlight-placeholder) ((if 3 4)))
;                    ((,highlight-placeholder) (4))))

; reconstruct does not handle begin0

(test-mz-sequence #'(let ([a 3]) 4)
                  `(((,highlight-placeholder) ((let-values ([(a) 3]) 4)) (,highlight-placeholder ,highlight-placeholder) ((define-values (a_0) 3) (begin 4)))
                    (((define a_0 3))))) 

(test-mz-sequence #'(let ([a (+ 4 5)] [b (+ 9 20)]) (+ a b))
                  `(((,highlight-placeholder) ((let-values ([(a) (+ 4 5)] [(b) (+ 9 20)]) (+ a b))) 
                     (,highlight-placeholder ,highlight-placeholder ,highlight-placeholder) 
                     ((define-values (a_0) (+ 4 5)) (define-values (b_1) (+ 9 20)) (begin (+ a_0 b_1))))
                    (((define-values (a_0) (,highlight-placeholder 4 5)) (define-values (b_1) (+ 9 20)) (begin (+ a_0 b_1))) (+))
                    (((define-values (a_0) (,highlight-placeholder 4 5)) (define-values (b_1) (+ 9 20)) (begin (+ a_0 b_1))) (+))
                    (((define-values (a_0) ,highlight-placeholder) (define-values (b_1) (+ 9 20)) (begin (+ a_0 b_1))) ((+ 4 5)))
                    (((define-values (a_0) ,highlight-placeholder) (define-values (b_1) (+ 9 20)) (begin (+ a_0 b_1))) (9))
                    (((define a_0 9) (define-values (b_1) (,highlight-placeholder 9 20)) (begin (+ a_0 b_1))) (+))
                    (((define a_0 9) (define-values (b_1) (,highlight-placeholder 9 20)) (begin (+ a_0 b_1))) (+))
                    (((define a_0 9) (define-values (b_1) ,highlight-placeholder) (begin (+ a_0 b_1))) ((+ 9 20)))
                    (((define a_0 9) (define-values (b_1) ,highlight-placeholder) (begin (+ a_0 b_1))) (29))
                    (((define a_0 9) (define b_1 29)))
                    (((,highlight-placeholder a_0 b_1)) (+))
                    (((,highlight-placeholder a_0 b_1)) (+))
                    (((+ ,highlight-placeholder b_1)) (a_0))
                    (((+ ,highlight-placeholder b_1)) (9))
                    (((+ 9 ,highlight-placeholder)) (b_1))
                    (((+ 9 ,highlight-placeholder)) (29))
                    ((,highlight-placeholder) ((+ 9 29)))
                    ((,highlight-placeholder) (38))))

;(test-mz-sequence #'(begin (define g 3) g)
;                  `(((,highlight-placeholder) (g))
;                    ((,highlight-placeholder) 3)))

;(syntax-object->datum (cadr (annotate-expr test2 'mzscheme 0 (lambda (x) x))))



(report-errs)