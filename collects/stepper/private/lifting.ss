(module lifting mzscheme
  (require "highlight-placeholder.ss"
           (lib "etc.ss")
           (lib "contract.ss")
           (prefix kernel: (lib "kerncase.ss" "syntax"))
           (lib "match.ss")
           "shared.ss"
           (lib "mz-testing.ss" "tests" "utils")) 
  
  (define-struct context-record (stx index kind))

  (provide/contract [lift (->* (syntax? syntax? boolean?)
                               ((listof syntax?) (listof syntax?)))] )
  
  (define (lift stx highlight lift-in-highlight?)
    (lift-local-defs (find-highlight stx) highlight lift-in-highlight?))
  
  ; the success of this approach is predicated on the fact that all of the primitive forms of mzscheme are
  ; proper syntax-lists.  That is, none of them are improper lists. Consider myself warned.
  ; [find-highlight (-> syntax? (listof context-record?))]

  (define (find-highlight stx)
    (let/ec success-escape
      (local
          ((define (make-try-all-subexprs stx kind context-so-far)
             (lambda (index-mangler list-of-subtries)
               (let loop ([index 0] [remaining list-of-subtries])
                 (unless (null? remaining)
                   ((caar remaining) (cadar remaining) (cons (make-context-record stx (index-mangler index) kind) context-so-far))
                   (loop (+ index 1) (cdr remaining))))))
           
           (define try->offset-try
             (lambda (try)
               (lambda (offset subtries)
                 (try (lambda (index) (list (+ offset index))) subtries))))
           
           (define (top-level-expr-iterator stx context-so-far)
             (let ([try (try->offset-try (make-try-all-subexprs stx 'top-level context-so-far))])
               (kernel:kernel-syntax-case stx #f
                 [(module identifier name (#%plain-module-begin . module-level-exprs))
                  (try 3 (map (lambda (expr) `(,module-level-expr-iterator ,expr))
                              (syntax->list #'module-level-exprs)))]
                 [else-stx
                  (general-top-level-expr-iterator stx context-so-far)])))
           
           (define (module-level-expr-iterator stx context-so-far)
             (kernel:kernel-syntax-case stx #f
               [(provide . provide-specs)
                (void)]
               [else-stx
                (general-top-level-expr-iterator stx context-so-far)]))
           
           (define (general-top-level-expr-iterator stx context-so-far)
             (let ([try (try->offset-try (make-try-all-subexprs stx 'general-top-level context-so-far))])
               (kernel:kernel-syntax-case stx #f
                 [(define-values (var ...) expr)
                  (try 2 `((,expr-iterator ,#'expr)))]
                 [(define-syntaxes (var ...) expr)
                  (try 2 `((,expr-iterator ,#'expr)))]
                 [(begin . top-level-exprs)
                  (try 1 (map (lambda (expr) `(,top-level-expr-iterator ,expr))
                              (syntax->list #'exprs)))]
                 [(require . require-specs)
                  (void)]
                 [(require-for-syntax . require-specs)
                  (void)]
                 [else
                  (expr-iterator stx context-so-far)])))
           
           (define (expr-iterator stx context-so-far)
             (when (eq? stx highlight-placeholder-stx)
               (success-escape context-so-far))
             (let* ([try (make-try-all-subexprs stx 'expr context-so-far)]
                    [try-exprs (lambda (index-mangler exprs) (try index-mangler (map (lambda (expr) `(,expr-iterator ,expr)) (syntax->list exprs))))]
                    [try-exprs-offset (try->offset-try try-exprs)] 
                    [let-values-abstraction
                     (lambda (stx)
                       (kernel:kernel-syntax-case stx #f
                         [(kwd (((variable ...) rhs) ...) . bodies)
                          (begin
                            (try-exprs (lambda (index) (list 1 index 1)) #'(rhs ...))
                            (try-exprs-offset 2 #'bodies))]
                         [else
                          (error 'expr-syntax-object-iterator 
                                 "unexpected let(rec) expression: ~a"
                                 (syntax-object->datum stx))]))]) 
               (kernel:kernel-syntax-case stx #f
                 [var-stx
                  (identifier? (syntax var-stx))
                  (void)]
                 [(lambda vars . bodies)
                  (try-exprs-offset 2 #'bodies)]
                 [(case-lambda (vars . bodies) ...)
                  (let loop ([count 1] [clauses (syntax->list #'(bodies ...))])
                    (try-exprs (lambda (index) (list count (+ index 1))) (car clauses)))]
                 [(if test then)
                  (try-exprs-offset 1 #'(test then))]
                 [(if test then else)
                  (try-exprs-offset 1 #'(test then else))]
                 [(begin . bodies)
                  (try-exprs-offset 1 #'bodies)]
                 [(begin0 . bodies)
                  (try-exprs-offset 1 #'bodies)]
                 [(let-values . _)
                  (let-values-abstraction stx)]
                 [(letrec-values . _)
                  (let-values-abstraction stx)]
                 [(set! var val)
                  (try-exprs-offset 2 #'(val))]
                 [(quote _)
                  (void)]
                 [(quote-syntax _)
                  (void)]
                 [(with-continuation-mark key mark body)
                  (try-exprs-offset 1 #'(key mark body))]
                 [(#%app . exprs)
                  (try-exprs-offset 1 #'exprs)]
                 [(#%datum . _)
                  (void)]
                 [(#%top . var)
                  (void)]
                 [else
                  (error 'expr-iterator "unknown expr: ~a" 
                         (syntax-object->datum stx))]))))
        
        (if (eq? stx highlight-placeholder-stx)
            null
            (begin (top-level-expr-iterator stx null)
                   (error 'find-highlight "couldn't find highlight-placeholder in expression: ~v" (syntax-object->datum stx)))))))

  ; TESTING:
  
  (define-syntax (test-begin stx)
    (syntax-case stx ()
      [(_ expr ...)
       #'(begin expr ...) ; testing version
       ;#'(void) ; non-testing version
       ]))
  
  (define (datum-ize-context-record cr)
     (list (syntax-object->datum (context-record-stx cr))
                          (context-record-index cr)
                          (context-record-kind cr)))
  
  (test-begin (section 'stepper-lifting))

  (test-begin
   ; TEST OF FIND-HIGHLIGHT
   
   (define (sexp-shared a b)
     (if (equal? a b)
         a
         (if (and (pair? a) (pair? b))
             (cons
              (sexp-shared (car a) (car b))
              (sexp-shared (cdr a) (cdr b)))
             'DIFFERENT)))
   
   (define test-datum #`(define-values
                                 (f)
                                 (lambda (x)
                                   (let-values ()
                                     (letrec-values (((a) (lambda (x) (#%app b (#%app (#%top . -) x (#%datum . 1))))) 
                                                     ((b) (lambda (x) (#%app #,highlight-placeholder-stx x)))) (let-values () (#%app a x)))))))

   (define expected (list (list `(#%app ,highlight-placeholder x) '(0) 'expr)
                          (list `(lambda (x) (#%app ,highlight-placeholder x)) '(2) 'expr)
                          (list `(letrec-values ([(a) (lambda (x) (#%app b (#%app (#%top . -) x (#%datum . 1))))] [(b) (lambda (x) (#%app ,highlight-placeholder x))]) (let-values () (#%app a x))) '(1 1 1) 'expr)
                          (list `(let-values () (letrec-values ([(a) (lambda (x) (#%app b (#%app (#%top . -) x (#%datum . 1))))] [(b) (lambda (x) (#%app ,highlight-placeholder x))]) (let-values () (#%app a x)))) '(2) 'expr)
                          (list `(lambda (x) (let-values () (letrec-values ([(a) (lambda (x) (#%app b (#%app (#%top . -) x (#%datum . 1))))] [(b) (lambda (x) (#%app ,highlight-placeholder x))]) (let-values () (#%app a x))))) '(2) 'expr)                 
                          (list `(define-values (f) (lambda (x) (let-values () (letrec-values ([(a) (lambda (x) (#%app b (#%app (#%top . -) x (#%datum . 1))))] [(b) (lambda (x) (#%app ,highlight-placeholder x))]) (let-values () (#%app a x)))))) '(2)
                                               'general-top-level)))
   
   (test expected map datum-ize-context-record (find-highlight test-datum))
   ;(printf "shared: ~v\n" (sexp-shared actual expected))
   
   (test #t null? (find-highlight highlight-placeholder-stx)))
  
  ; substitute-in-syntax takes a syntax expression (which must be a proper syntax list) and a path
  ; (represented by a list of numbers) and a syntax-expression to insert.  If the path is null, the
  ; 'to-insert' expression is returned.  Otherwise, the nth element of the syntax-list is replaced
  ; by the recursive call with the nth element, the rest of the path, and the to-insert, where n is
  ; the first number in the list.

  (define (substitute-in-syntax src path to-insert)
    (if (null? path)
        to-insert
        (let* ([opened (syntax->list src)]
               [n (car path)])
          (when (>= n (length opened))
            (error 'substitute-in-syntax "given an n (~v) which is larger than the length of the source sytax ~v" n (syntax-object->datum src)))
          (datum->syntax-object
           src
           (let loop ([n n] [left opened])
            (if (= n 0) 
                (cons (substitute-in-syntax (car left) (cdr path) to-insert)
                      (cdr left))
                (cons (car left) 
                      (loop (- n 1) (cdr left)))))
           src
           src))))
  
  (define (n-times n fn arg)
    (if (= n 0)
        arg
        (fn (n-times (- n 1) fn arg))))
  
  (test-begin
   
   (local
       ((define expected '(let-values ([(a) (lambda (x) 'bar)]) (a)))
        (define actual (syntax-object->datum (substitute-in-syntax #'(let-values ([(a) (lambda (x) 'foo)]) (a)) '(1 0 1 2 1) #'bar))))
     (printf "equal? ~v\n" (equal? expected actual))))
  
  
  ; lift-local-defs takes a list of contexts and an instruction and works its way out, reconstructing the expression.
  ; the main action of lift-local-defs is on let-values and letrec-values, where (after performing the substitution)
  ; the binding clauses are lifted into top-level defs.
  ; [lift-local-defs (->* ((listof context-record?) syntax?)
  ;                       ((listof syntax?) (listof syntax?)))]
  
  (define (lift-local-defs ctx-list highlighted lift-in-highlighted?)
    (let-values ([(highlighted-defs highlighted-body) (if lift-in-highlighted?
                                                          (lift-helper highlighted #f null)
                                                          (values null highlighted))])
      (let loop ([ctx-list ctx-list] [so-far-defs (map (lambda (x) highlight-placeholder-stx) highlighted-defs)] [body highlight-placeholder-stx])
        (if (null? ctx-list)
            (values (append so-far-defs (list body)) (append highlighted-defs (list highlighted-body)))
            (let*-values ([(ctx) (car ctx-list)]
                          [(index) (context-record-index ctx)]
                          [(next-defs next-body) 
                           (lift-helper (substitute-in-syntax (context-record-stx ctx) index body)
                                        index
                                        so-far-defs)])
              (loop (cdr ctx-list) next-defs next-body))))))
  
  ; lift-helper takes a syntax object and a split path and a list of syntax objects and breaks it up
  ; iff its a let/rec, wrapping those before the split and those after the split around the list of syntax
  ; objects
  ;  (->* (syntax? (or/f false? (listof number?)) (listof syntax?)) ((listof syntax?) syntax?))
  (define (lift-helper stx path so-far-defs)
    (let* ([lift
            (lambda ()
              (kernel:kernel-syntax-case stx #f
                [(tag ([(var ...) rhs] ...) body ...)
                 (let* ([defns (map (lambda (defn-stx) (transfer-info defn-stx stx))
                                    (syntax->list #'((define-values (var ...) rhs) ...)))]
                        [bodies-list (syntax->list #'(body ...))]
                        [body (if (= (length bodies-list) 1) ; as far as I can tell, this source info is comprehensively lost.
                                  (car bodies-list)
                                  #'(values body ...))])
                   (cond [(or (not path) (and (= (length path) 1)
                                              (> (car path) 1)))
                          (values (append defns so-far-defs) body)]
                         [(match path [`(1 ,n 1) n]) =>
                          (lambda (n)
                            (values (append (sublist 0 n defns) so-far-defs (sublist n (length defns) defns))
                                    body))]))]
                [else (error 'lift-helper "let or letrec does not have expected shape: ~v\n" (syntax-object->datum stx))]))])
    (kernel:kernel-syntax-case stx #f
      [(let-values . dc)
       (not (eq? (syntax-property stx 'user-stepper-hint) 'comes-from-or))
       (lift)]
      [(letrec-values . dc)
       (lift)]
      [else (values so-far-defs stx)])))
  
  (test-begin
   (local 
       ((define-values (actual-stxs actual-stx-highlights)
          (lift-local-defs
            (list (make-context-record #'(dc 14) '(0) 'expr)
                  (make-context-record #'(letrec-values ([(a) 3] [(b) dc] [(c) 5]) (+ 3 4)) '(1 1 1) 'expr)
                  (make-context-record #'(f dc) '(1) 'expr))
            #'(let-values ([(a) 4] [(b) 9] [(c) 12]) (p q))
            #t))
 
        (define (so->d stx) 
          (if (syntax? stx)
              (syntax-object->datum stx)
              stx))
        
        (define actual-sexps (map so->d actual-stxs))
        (define actual-highlights (map so->d actual-stx-highlights))
        
        (define expected-sexps
          (list '(define-values (a) 3)
                highlight-placeholder
                highlight-placeholder
                highlight-placeholder
                `(define-values (b) (,highlight-placeholder 14))
                '(define-values (c) 5)
                '(f (+ 3 4))))
        
        (define expected-highlights 
          '((define-values (a) 4) (define-values (b) 9) (define-values (c) 12) (p q))))
     
     (test expected-sexps map so->d actual-stxs)
     (test expected-highlights map so->d actual-stx-highlights)
     ;(printf "shared: ~v\n" (sexp-shared actual expected))
     )
   
   (report-errs)
   ))
 