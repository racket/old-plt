(current-library-collection-paths '("/Users/clements/hot/plt/collects"))
(require (prefix annotate: (lib "annotate.ss" "stepper" "private")))
(require (prefix kernel: (lib "kerncase.ss" "syntax")))

(load "/Users/clements/plt/tests/mzscheme/testing.ss")

(SECTION 'stepper-annotater)


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
  (let loop ([env annotate:initial-env-package] [exprs (if lang 
                                                           (wrap-expand-unwrap (list stx) lang)
                                                           (list (expand stx)))])
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
; hard to test whether the source pointer is right.

(define test-cases
  ; begin
  (list (list #'(lambda (a b c) (begin a b (begin a c))) 'mzscheme cadr
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
                         begin-body-3)
                        (with-continuation-mark
                         debug-key-4
                         debug-mark-4
                         begin-body-4))))
                   (begin
                     (test (void) check-mark (syntax debug-mark-1) '(a b c) 'all)
                     (test (void) check-mark (syntax debug-mark-2) '() 'all)
                     (test (void) check-mark (syntax debug-mark-3) '() 'all)
                     (test (void) check-mark (syntax debug-mark-4) '(a c) 'all))])))
         
        ; lambda : general test
        (list #'(lambda (a b) (lambda (b c) (+ b c) (+ a b 4))) 'mzscheme cadr
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (with-continuation-mark begin lambda)
                  [(with-continuation-mark
                    debug-key
                    debug-mark-1
                    (begin
                      pre-break-1
                      (closure-capturing-proc
                       (lambda (b c) 
                         (with-continuation-mark
                          debug-key-2
                          debug-mark-lambda-body
                          (begin
                            pre-break-2
                            (begin
                              . begin-bodies))))
                       debug-mark-3)))
                    (begin (test (void) check-mark (syntax debug-mark-1) '(+ a) 'all)
                           (test (void) check-mark (syntax debug-mark-3) '(+ a) 'all))])))
        ; test of lambda's one-label inferred-names :
        (list #'(define (a x) (+ x 1)) 'mzscheme cadr
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
        (list #'(let ([a (lambda (x) (+ x 1))]) 3) 'mzscheme cadr
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
        (list #'(lambda (d e) (case-lambda ((b) b d) ((c) c e))) 'mzscheme cadr
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (with-continuation-mark lambda quote-syntax)
                  [(with-continuation-mark
                    debug-key
                    debug-mark-1
                    (begin
                      pre-break-1
                      (closure-storing-proc
                       (case-lambda ((b) . bodies_1)
                                    ((c) . bodies_2))
                       closure-info)))
                   (test (void) check-mark (syntax debug-mark-1) '(d e) '(b c))])))
        
        ; if
        (list #'(lambda (a b c d) (if (a b) (a c) (a d))) 'mzscheme cadr
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (with-continuation-mark if let-values begin)
                  [(with-continuation-mark
                    debug-key_1
                    debug-mark-1
                    (begin
                      pre-break-1
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
                             . else-clauses)))))
                   (begin
                     (test (void) check-mark (syntax debug-mark-1) '(a b c d) '())
                     (test (void) check-mark (syntax debug-mark-test) '() '(a b c d))
                     (test (void) check-mark (syntax debug-mark-then) '(a c) '(b d))
                     (test (void) check-mark (syntax debug-mark-else) '(a d) '(b c)))])))
        
        ; one-armed if
        (list #'(lambda (a b c) (if (begin a b) (begin a c))) 'mzscheme cadr
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (with-continuation-mark if let-values begin)
                  [(with-continuation-mark
                    debug-key-1
                    debug-mark-1
                    (begin
                      pre-break-1
                      (if (with-continuation-mark
                             debug-key_2
                             debug-mark-test
                             . test-clauses)
                          (with-continuation-mark
                             debug-key_3
                             debug-mark-then
                             . then-clauses))))
                   (begin
                     (test (void) check-mark (syntax debug-mark-1) '(a b c) 'all)
                     (test (void) check-mark (syntax debug-mark-test) '() 'all)
                     (test (void) check-mark (syntax debug-mark-then) '(a c ) 'all))])))
        
        ; top-level begin
        (list #'(begin (define a 3) a (begin a a)) #f car
              (lambda (stx)
                (syntax-case stx (begin with-continuation-mark define-values)
                  [(begin
                     (define-values . rest)
                     (with-continuation-mark key-2 mark-2 a-exp-2)
                     (begin
                       (with-continuation-mark key-3 mark-3 a-exp-3)
                       (with-continuation-mark key-4 mark-4 a-exp-4)))
                   #t])))
        
        ; begin0
        (list #'(lambda (a b) (begin0 a b)) 'mzscheme cadr
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (begin begin0 with-continuation-mark)
                  [(with-continuation-mark
                    key-0
                    mark-0
                    (begin
                      pre-break-0
                      (begin0
                        (with-continuation-mark
                         key-result
                         mark-result
                         result-expr)
                        (with-continuation-mark
                         key-other
                         mark-other
                         other-expr))))
                   (begin
                     (test (void) check-mark (syntax mark-0) '(a b) 'all)
                     (test (void) check-mark (syntax mark-result) '() 'all)
                     (test (void) check-mark (syntax mark-other) '() 'all))])))
        
        ; begin0 : inferred-name transfer
        (list #'(define-values (a) (begin0 (lambda () 3) 4)) 'mzscheme cadr
              (lambda (stx)
                (syntax-case stx (begin0 define-values with-continuation-mark)
                  [(define-values names
                     (with-continuation-mark
                      key-0
                      mark-0
                      (begin0
                        (with-continuation-mark
                         key-1
                         mark-1
                         (procedure-capturing-proc
                          procedure
                          mark-2))
                        . rest)))
                   (test 'a syntax-property (syntax procedure) 'inferred-name)])))
        ; let 
        (list #'(lambda (a b c) (let* ([d b] [e (begin a d)]) (begin a b c d))) 'mzscheme cadr
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (begin with-continuation-mark let*-values set!-values)
                  [(let*-values bindings
                     (with-continuation-mark
                      key-0
                      mark-0
                      (begin
                        pre-break-0
                        (begin
                          break-1
                          (begin
                            (set!-values vars-0 (with-continuation-mark key-1 mark-1 body-1))
                            (set!-values vars-1 (with-continuation-mark key-2 mark-2 body-2))
                            (begin
                              break-2
                              body-3))))))
                   (begin
                     (test (void) check-mark (syntax mark-0) '(a b c d e lifter-d-1 lifter-e-2) 'all)
                     (test '(d) syntax-object->datum (syntax vars-0))
                     (test '(e) syntax-object->datum (syntax vars-1))
                     (test (void) check-mark (syntax mark-1) '() 'all)
                     (test (void) check-mark (syntax mark-2) '() 'all))])))
        
        ; letrec --- the only thing that needs to be tested with letrec is that the undefined value is properly used.
        
        ; set!
        (list #'(lambda (a b c) (set! a (begin b c))) 'mzscheme cadr
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (begin with-continuation-mark set!)
                  [(with-continuation-mark
                    key-0
                    mark-0
                    (begin
                      pre-break-0
                      (set! var (with-continuation-mark key-1 mark-1 body))))
                   (begin
                     (test (void) check-mark (syntax mark-0) '(a b c) 'all)
                     (test 'a syntax-object->datum (syntax var))
                     (test (void) check-mark (syntax mark-1) '() 'all))])))
        
        ; set! with top-level-var
        (list #'(set! a 9) #f car
              (lambda (stx)
                (syntax-case stx (set! with-continuation-mark)
                  [(with-continuation-mark
                    key-0
                    mark-0
                    (set! var val))
                   (begin
                     (test (void) check-mark (syntax mark-0) '(a) 'all)
                     (test 'a syntax-object->datum (syntax var)))])))
        
        ; quote
        (list #'(quote a) 'mzscheme cadr
              (lambda (stx)
                (syntax-case stx (quote with-continuation-mark)
                  [(with-continuation-mark
                    key-0
                    mark-0
                    (quote sym))
                   (begin
                     (test (void) check-mark (syntax mark-0) '() 'all)
                     (test 'a syntax-e (syntax sym)))])))
                     
        ; quote-syntax
        (list #'(quote-syntax a) 'mzscheme cadr
              (lambda (stx)
                (syntax-case stx (quote-syntax with-continuation-mark)
                  [(with-continuation-mark
                    key-0
                    mark-0
                    (quote-syntax sym))
                   (begin
                     (test (void) check-mark (syntax mark-0) '() 'all)
                     (test 'a syntax-e (syntax sym)))])))
        
        ; wcm is explicitly not working (as opposed to _lots_ of other forms, which simply won't work in 
        ; a stepper sense.  Begin0, for example.  I think.  And set!.  etc., etc.
        
        ; application
        
        (list #'(lambda (a b c) (a b)) 'mzscheme cadr
              (lambda (stx)
                (syntax-case (strip-outer-lambda stx) (let\-values with-continuation-mark begin)
                  [(let-values arg-temps
                     (with-continuation-mark
                      key-0
                      mark-0
                      (begin
                        pre-break-0
                        (begin
                          (set! var-0 val-0)
                          (set! var-1 val-1)
                          . rest))))
                   (begin
                     (test (void) check-mark (syntax mark-0) '(a b arg-temp-0 arg-temp-1) 'all)
                     (printf "mark-0: ~a~n" (syntax-object->datum (syntax mark-0))))])))
        
                   
        
        ))

(andmap (lambda (test-case)
            ((cadddr test-case) ((caddr test-case) (annotate-expr (car test-case) (cadr test-case)))))
        test-cases)
 
(report-errs)
