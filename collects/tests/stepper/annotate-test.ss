(current-library-collection-paths '("/Users/clements/hot/plt/collects"))
(require (prefix annotate: (lib "annotate.ss" "stepper" "private")))
(require (prefix kernel: (lib "kerncase.ss" "syntax")))

(load "/Users/clements/plt/tests/mzscheme/testing.ss")

(SECTION 'stepper-annotater)

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

; check-mark : test the validity of a mark.  the symbols in 'binding-ids' must be included. the symbols
; in excluded-ids must not appear in the list.  If binding-ids is 'all, then no symbols other than those
; in the binding-ids may appear.
; (syntax-object (listof symbol) (union (listof symbol) 'all) -> void)
(define (check-mark mark-stx binding-ids excluded-ids) 
  (let* ([bindings (syntax-case mark-stx (lambda )
                     [(lambda ()
                        (mark-maker
                         source
                         label
                         . bindings))
                      (let loop ([binding-list (syntax->list (syntax bindings))])
                        (cond [(null? binding-list) null]
                              [(null? (cdr binding-list)) 
                               (error 'check-mark "odd number of elements in binding list: ~a" (syntax-object->datum (syntax bindings)))]
                              [else
                               (when (not (eq? (car binding-list)
                                               (syntax-case (cadr binding-list) (quote-syntax)
                                                 [(quote-syntax stx)
                                                  (syntax stx)])))
                                 (error 'check-mark "binding pair does not match: ~a, ~a" (syntax-object->datum (car binding-list))
                                        (syntax-object->datum (cadr binding-list))))
                               (when (not (identifier? (car binding-list)))
                                 (error 'check-mark "syntax object is not an identifier: ~a" (syntax-object->datum (car bindings-list))))
                               (cons (syntax-e (car binding-list))
                                     (loop (cddr binding-list)))]))])])
    (let loop ([remaining bindings])
      (unless (null? remaining)
        (when (memq (car remaining) (cdr remaining))
          (error 'check-mark "binding ~a appears twice in binding-list: ~a" (car remaining) bindings))
        (loop (cdr remaining))))
    (for-each (lambda (desired)
                (unless (memq desired bindings)
                  (error 'check-mark "binding ~a not contained in binding-list: ~a" desired bindings)))
              binding-ids)
    (if (eq? excluded-ids 'all)
        (for-each (lambda (binding)
                    (unless (memq binding binding-ids)
                      (error 'check-mark "binding ~a does not appear in desired list: ~a" binding binding-ids)))
                  bindings)
        (for-each (lambda (not-desired)
                    (when (memq not-desired bindings)
                      (error 'check-mark "excluded binding ~a contained in binding-list: ~a" not-desired bindings)))
                  excluded-ids))))

; test cases

(syntax-case (expand #'(let ([a 1] [b 2]) (begin a b))) (let-values begin)
  [(let-values bindings a b)
   (begin
     (err/rt-test (check-mark (syntax (lambda ())) '() '()) exn:syntax?) ; badly formed mark
     (err/rt-test (check-mark (syntax (lambda () ('mark-maker 'source 'label a (quote-syntax a) b))) '() '()) exn:user?) ; mismatched bindings
     (err/rt-test (check-mark (syntax (lambda () ('mark-maker 'source 'label (quote-syntax a) a))) '() '()) exn:syntax?) ; mismatched bindings
     (err/rt-test (check-mark (syntax (lambda () ('mark-maker 'source 'label a (quote-syntax a) a (quote-syntax a)))) '() '()) exn:user?) ; binding appears twice
     (err/rt-test (check-mark (syntax (lambda () ('mark-maker 'source 'label a (quote-syntax b)))) '() '()) exn:user?) ; mismatched bindings
     (test (void) check-mark (syntax (lambda () ('mark-maker 'source 'label a (quote-syntax a) b (quote-syntax b)))) '(a b) 'all)
     (err/rt-test (check-mark (syntax (lambda () ('mark-maker 'source 'label a (quote-syntax a) b (quote-syntax b)))) '(a b c) 'all) exn:user?) ; c isn't there
     (err/rt-test (check-mark (syntax (lambda () ('mark-maker 'source 'label a (quote-syntax a) b (quote-syntax b)))) '(a) 'all) exn:user?) ; bad 'all
     (test (void) check-mark (syntax (lambda () ('mark-maker 'source 'label a (quote-syntax a) b (quote-syntax b)))) '(a) '(c))
     (err/rt-test (check-mark (syntax (lambda () ('mark-maker 'source 'label a (quote-syntax a) b (quote-syntax b)))) '(a) '(b c)) exn:user?) ; b is there
     (test (void) check-mark (syntax (lambda () ('mark-maker 'source 'label a (quote-syntax a) b (quote-syntax b)))) '(a) '()))])

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

(test '((if (#%app verify-boolean (#%datum . 3) 'if) (#%datum . 4) (#%datum . 5)))
      map syntax-object->datum (wrap-expand-unwrap (list #'(if 3 4 5)) '(lib "htdp-beginner.ss" "lang")))
         
(define (break) 3)

(define (annotate-expr stx lang)
  (let loop ([env annotate:initial-env-package] [exprs (wrap-expand-unwrap (list stx) lang)])
    (if (null? exprs)
        null
        (let*-values ([(annotated new-env)
                       (annotate:annotate #f (car exprs) env break 'foot-wrap)])
          (cons annotated (loop new-env (cdr exprs)))))))

; strip-outer-lambda takes off a lambda wrapped around a test expression. For testing purposes,
; we often want to establish lexical bindings, then strip them off to check the test expr

(define (strip-outer-lambda stx)
  (syntax-case stx (lambda begin with-continuation-mark)
    [(with-continuation-mark
      debug-key-1
      debug-mark-1
      (closure-storing-proc
       (lambda args
         content)
       debug-mark-2))
     (syntax content)]))


; test case:
(test #t 
      (lambda ()
        (syntax-case (syntax-object->datum (strip-outer-lambda (cadr (annotate-expr #'(lambda (a b c) 3) 'mzscheme))))
          (with-continuation-mark lambda quote-syntax #%datum)
          [(with-continuation-mark 
            key
            (lambda () 
              (mark-maker
               (quote-syntax (#%datum . 3))
               0))
            (begin
              break-proc-1
              (#%datum . 3)))
           #t])))
        
     
     
     
; test notes to myself:
; the never-undefined property can only be tested in the non-foot-wrap modes
; hard to test 3D values like closure-capturing-proc

(define test-cases
  ; begin
  (list (list #'(lambda (a b c) (begin (begin a b) (begin a c))) 'mzscheme
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (with-continuation-mark begin)
                  [(with-continuation-mark
                    debug-key-1
                    debug-mark-1
                    (begin
                      pre-break-1
                      (begin
                        (with-continuation-mark
                         debug-key-2
                         debug-mark-2
                         begin-body-2)
                        (with-continuation-mark
                         debug-key-3
                         debug-mark-3
                         begin-body-3))))
                   (begin
                     (test (void) check-mark (syntax debug-mark-1) '(a b c) 'all)
                     (test (void) check-mark (syntax debug-mark-2) '() 'all)
                     (test (void) check-mark (syntax debug-mark-3) '(a c) 'all))])))
         
  ; lambda : general test
        (list #'(lambda (a b) (lambda (b c) (+ b c) (+ a b 4))) 'mzscheme
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (with-continuation-mark begin lambda)
                  [(with-continuation-mark
                    debug-key
                    debug-mark-1
                    (begin
                      pre-break-1
                      (closure-capturing-proc
                       (lambda (b c) (let-values arg-temp-bindings
                                       (with-continuation-mark
                                        debug-key-2
                                        debug-mark_2
                                        (begin
                                          pre-break-2
                                          _h))) _i)
                       debug-mark-3)))
                    (begin (test (void) check-mark (syntax debug-mark-1) '(+ a) 'all)
                           (test (void) check-mark (syntax debug-mark-3) '(+ a) 'all))])))
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
                    (test 'a syntax-property (syntax lambda-exp) 'inferred-name)])))
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
                    (test 'a syntax-property (syntax lambda-exp) 'inferred-name)])))
        ; case-lambda
        (list #'(let ([d 1][e 2]) (case-lambda ((b) b d) ((c) c e))) 'mzscheme
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (with-continuation-mark lambda quote-syntax)
                  [(with-continuation-mark
                    debug-key
                    debug-mark-1
                    (closure-storing-proc
                     (case-lambda ((b) . bodies_1)
                                  ((c) . bodies_2))
                     closure-info))
                   (check-mark (syntax debug-mark-1) '(d e) '(b c))])))
        
        ; if
        (list #'(let ([a 1] [b 2] [c 3] [d 4]) (if (a b) (a c) (a d))) 'mzscheme
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (with-continuation-mark if let-values)
                  [(with-continuation-mark
                    debug-key_1
                    debug-mark-1
                    (if (let-values temp-bindings
                          (with-continuation-mark
                           debug-key_2
                           debug-mark-test
                           . test-clauses))
                        (let-values temp-bindings_2
                          (with-continuation-mark
                           debug-key_3
                           debug-mark-then
                           . then-clauses))
                        (let-values temp-bindings-3
                          (with-continuation-mark
                           debug-key-4
                           debug-mark-else
                           . else-clauses))))
                   (begin
                     (test (void) check-mark (syntax debug-mark-1) '(a b c d) '())
                     (test (void) check-mark (syntax debug-mark-test) '() '(a b c d))
                     (test (void) check-mark (syntax debug-mark-then) '(a c) '(b d))
                     (test (void) check-mark (syntax debug-mark-else) '(a d) '(b c)))])))
        
        ; one-armed if
        (list #'(let ([a 1] [b 2] [c 3]) (if (a b) (a c))) 'mzscheme
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (with-continuation-mark if let-values)
                  [(with-continuation-mark
                    debug-key-1
                    debug-mark-1
                    (if (let-values temp-bindings
                          (with-continuation-mark
                           debug-key_2
                           debug-mark-test
                           . test-clauses))
                        (let-values temp-bindings_2
                          (with-continuation-mark
                           debug-key_3
                           debug-mark-then
                           . then-clauses))))
                   (begin
                     (test (void) check-mark (syntax debug-mark-1) '(a b c) 'all)
                     (test (void) check-mark (syntax debug-mark-test) '() 'all)
                     (test (void) check-mark (syntax debug-mark-then) '(a c ) 'all))])))
                    
        ; let 
;        (list #'( (let ([c 1] [d 2]) 
        
                    ))

(andmap (lambda (test-case)
            ((caddr test-case) (car (reverse (annotate-expr (car test-case) (cadr test-case))))))
        test-cases)

(report-errs)
