(current-library-collection-paths '("/Users/clements/hot/plt/collects"))

(require (prefix annotate: (lib "annotate.ss" "stepper" "private")))
(require (prefix kernel: (lib "kerncase.ss" "syntax")))

(define-syntax (tand stx)
  (syntax-case stx ()
    [(_) (syntax #t)]
    [(_ first . rest)
     (with-syntax ([error-exp (syntax/loc (syntax first) (error 'tand "test failed"))])
       (syntax/loc stx
                   (let ([temp first])
                     (when (not temp)
                       error-exp)
                     (tand . rest))))]))

; test cases:
;(tand 1 2 3)
;(tand)
;(tand 3 4 #f 5)
;(tand #t
;      #t
;      (tand 5 #f 9)
;      #f)

; syntax-symbol=? : compares a list of symbols to a given symbol (or to each other if sym = #f)
; (union symbol #f) (listof syntax-object) -> boolean

(define (syntax-symbol=? sym lostx)
  (if sym
      (andmap (lambda (stx) (eq? sym (syntax-e stx))) lostx)
      (if (pair? lostx)
          (andmap (lambda (stx) (eq? (syntax-e stx) (syntax-e (car lostx)))) lostx)
          #t)))

(define (wrap-expand-unwrap stx language-level-spec)
  (let* ([wrapped (datum->syntax-object #f `(module test-module ,language-level-spec
                                              ,@stx))]
         [expanded (expand wrapped)])
    (with-syntax ([(module name lang (_ . exprs)) expanded])
      (syntax->list (syntax exprs)))))

(equal? (map syntax-object->datum (wrap-expand-unwrap (list #'(if 3 4 5)) '(lib "htdp-beginner.ss" "lang")))
        '((if (#%app verify-boolean (#%datum . 3) 'if) (#%datum . 4) (#%datum . 5))))
         
(define (break) 3)

(define (annotate-expr stx lang)
  (let loop ([env annotate:initial-env-package] [exprs (wrap-expand-unwrap (list stx) lang)])
    (if (null? exprs)
        null
        (let*-values ([(annotated new-env)
                       (annotate:annotate #f (car exprs) env break 'foot-wrap)])
          (cons annotated (loop new-env (cdr exprs)))))))

; strip-outer-let takes off a let wrapped around a test expression.  For testing purposes,
; we often want to establish lexical bindings, then strip them off to check the test expr

(define (strip-outer-let stx)
  (syntax-case stx (let*-values begin with-continuation-mark)
    [(let*-values bindings
       (with-continuation-mark 
        key
        mark
        (begin
          break-proc-1
          (begin
            .
            clauses))))
     (syntax-case (car (reverse (syntax->list (syntax clauses)))) (begin)
       [(begin
          break-proc-2
          inner-expr)
        (syntax inner-expr)])]))

(syntax-case (syntax-object->datum (strip-outer-let (cadr (annotate-expr #'(let ([a 9] [b 19] [c 193]) 3) 'mzscheme))))
  (with-continuation-mark lambda quote-syntax #%datum)
  [(with-continuation-mark 
    key
    (lambda () 
      (mark-maker
       (quote-syntax (#%datum . 3))
       0))
    (#%datum . 3))
   #t])
        
     
     
     
     ; test notes to myself:
; the never-undefined property can only be tested in the non-foot-wrap modes
; hard to test 3D values like closure-capturing-proc

(define test-cases
  ; lambda : general test
  (list (list #'(let ([a 9] [b 12]) (lambda (b c) (+ b c) (+ a b 4))) 'mzscheme
              (lambda (stx)
                (syntax-case (strip-outer-let stx) (with-continuation-mark begin let*-values let-values lambda quote-syntax)
                  [(with-continuation-mark
                    debug-key
                    (lambda ()
                      (make-full-mark-proc
                       source
                       label-num
                       +-label
                       (quote-syntax +-label_2)
                       a-label
                       (quote-syntax a-label_2)
                       lifted-label
                       (quote-syntax lifted-label_2)))
                    (closure-capturing-proc
                     (lambda (b c) (let-values arg-temp-bindings
                                     (with-continuation-mark
                                      debug-key_2
                                      debug-mark_2
                                      (begin
                                        pre-break
                                        _h))) _i)
                     (lambda ()
                       (make-full-mark-proc_2
                        source_2
                        label-num_2
                        +-label_3
                        (quote-syntax +-label_4)
                        a-label_3
                        (quote-syntax a-label_4)
                        lifted-label_3
                        (quote-syntax lifted-label_4)))))
                    (printf "my-mark: ~a~n" (syntax-object->datum (syntax my-mark-2)))
                    (tand (= (syntax-object->datum (syntax label-num)) 0)
                          (syntax-symbol=? 'a (syntax->list (syntax (a-label a-label_2 a-label_3 a-label_4))))
                          (syntax-symbol=? #f (syntax->list (syntax (lifted-label lifted-label_2 lifted-label_3 lifted-label_4)))))])))
        ; test of lambda's one-label inferred-names :
        (list #'(define (a x) (+ x 1)) 'mzscheme
              (lambda (stx)
                (kernel:kernel-syntax-case stx #f
                   [(define-values (a)
                      (with-continuation-mark
                       debug-key
                       debug-mark
                       (closure-capturing-proc
                        lambda-exp
                        closure-info)))
                    (eq? (syntax-property (syntax lambda-exp) 'inferred-name) 'a)])))
        ; test of lambda's cons-pair inferred-names (that is, with lifting):
        (list #'(let ([a (lambda (x) (+ x 1))]) 3) 'mzscheme
              (lambda (stx)
                (syntax-case stx (let*-values with-continuation-mark begin set!-values)
                   [(let*-values _c
                       (with-continuation-mark debug-key_1 debug_mark_1
                                               (begin
                                                 (break-proc_1)
                                                 (begin
                                                   (set!-values a-label
                                                                  (with-continuation-mark
                                                                   debug-key
                                                                   debug-mark
                                                                   (closure-capturing-proc
                                                                    lambda-exp
                                                                    closure-info
                                                                    lifter-val)))
                                                   body))))
                    (printf "inferred name: ~a~n" (syntax-property (syntax lambda-exp) 'inferred-name))
                    (tand (eq? (syntax-property (syntax lambda-exp) 'inferred-name) 'a))])))
        ; case-lambda
        (list #'(let ([d 1][e 2]) (case-lambda ((b) b d) ((c) c e))) 'mzscheme
              (lambda (stx)
                (syntax-case (strip-outer-let stx) (with-continuation-mark lambda quote-syntax)
                  [(with-continuation-mark
                    debug-key
                    (lambda ()
                      (mark-maker
                       source-loc
                       label
                       d-label_1
                       (quote-syntax d-label_2)
                       e-label_1
                       (quote-syntax e-label_2)
                       d-lifter_1
                       (quote-syntax d-lifter_2)
                       e-lifter_1
                       (quote-syntax e-lifter_2)))
                    (closure-storing-proc
                     (case-lambda ((b) . bodies_1)
                                  ((c) . bodies_2))
                     closure-info))
                   (printf "d-label_1: ~a~n" (syntax-e (syntax d-label_1)))
                   (tand (syntax-symbol=? 'd (syntax->list (syntax (d-label_1 d-label_2))))
                         (syntax-symbol=? 'e (syntax->list (syntax (e-label_1 e-label_2))))
                         (syntax-symbol=? #f (syntax->list (syntax (d-lifter_1 d-lifter_2))))
                         (syntax-symbol=? #f (syntax->list (syntax (e-lifter_1 e-lifter_2)))))])))
        
        ; if
        (list #'(let ([a 1] [b 2] [c 3] [d 4]) (if (a b) (a c) (a d))) '(lib "htdp-intermediate.ss" "lang")
              (lambda (stx)
                (syntax-case (strip-outer-let stx) ()
                  [(with-continuation-mark
                    debug-key_1
                    (lambda ()
                      (mark-maker_1
                       source_1
                       label_1
                       . 
                       bindings))
                    annotated)
                   (printf " bindings : ~a~n" (syntax bindings))
                   (printf " annotated: ~a~n" (syntax annotated))])))
        ; let 
;        (list #'( (let ([c 1] [d 2]) 
        
                    ))

(andmap (lambda (test-case)
            ((caddr test-case) (cadr (annotate-expr (car test-case) (cadr test-case)))))
          test-cases)
