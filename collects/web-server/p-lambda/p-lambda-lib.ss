(module p-lambda-lib mzscheme
  (require (lib "etc.ss")
           (lib "list.ss")
           (lib "kerncase.ss" "syntax")
           "proc-table.ss"
           "closure.ss")
  (provide p-lambda)
  
  ;; ****************************************
  ;; FREE VARS
  
  ;; this data definition comes from chapter 12
  ;; of PLT MzScheme Language Manual
  ;expr is one of
  ;  variable
  ;  (lambda formals expr ...1)
  ;  (case-lambda (formals expr ...1) ...)
  ;  (if expr expr)
  ;  (if expr expr expr)
  ;  (begin expr ...1)
  ;  (begin0 expr expr ...)
  ;  (let-values (((variable ...) expr) ...) expr ...1)
  ;  (letrec-values (((variable ...) expr) ...) expr ...1)
  ;  (set! variable expr)
  ;  (quote datum)
  ;  (quote-syntax datum)
  ;  (with-continuation-mark expr expr expr)
  ;  (#%app expr ...1)
  ;  (#%datum . datum)
  ;  (#%top . variable)
  
  ;; data def from section 2.9
  ;formals is one of 
  ;  variable
  ;  (variable ...)
  ;  (variable ... . variable)
  
  ;; free-vars: expr -> (listof identifier)
  ;; Find the free variables in an expression
  (define (free-vars expr)
    ;(printf "free-vars: expr = ~s~n" expr)
    (kernel-syntax-case expr #f
      [(lambda formals body-exprs ...)
       (free-vars/lambda #'formals (syntax->list #'(body-exprs ...)))]
      [(case-lambda (formalss exprss ...) ...)
       (foldl
        (lambda (formals exprs acc)
          (union (free-vars/lambda formals (syntax->list exprs))
                 acc))
        '()
        (syntax->list #'(formalss ...))
        (syntax->list #'((exprss ...) ...)))]
      [(if test-expr csq-expr)
       (union (free-vars #'test-expr)
              (free-vars #'csq-expr))]
      [(if test-expr csq-expr alt-expr)
       (union (free-vars #'test-expr)
              (union (free-vars #'csq-expr)
                     (free-vars #'alt-expr)))]
      [(begin exprs ...)
       (free-vars* (syntax->list #'(exprs ...)))]
      [(begin0 expr0 exprs ...)
       (free-vars* (cons #'expr0 (syntax->list #'(exprs ...))))]
      [(let-values ([(varss ...) rhs-exprs] ...) body-exprs ...)
       (union (free-vars* (syntax->list #'(rhs-exprs ...)))
              (set-diff (free-vars* (syntax->list #'(body-exprs ...)))
                        (apply append
                               (map
                                syntax->list
                                (syntax->list #'((varss ...) ...))))))]
      [(letrec-values ([(varss ...) rhs-exprs] ...) body-exprs ...)
       (set-diff
        (union (free-vars* (syntax->list #'(rhs-exprs ...)))
               (free-vars* (syntax->list #'(body-exprs ...))))
        (apply append (map syntax->list (syntax->list #'((varss ...) ...)))))]
      [(set! var rhs-expr) (free-vars #'rhs-expr)]
      [(quote datum) '()]
      [(quote-syntax datum) '()]
      [(with-continuation-mark expr0 expr1 expr2)
       (union (free-vars #'expr0)
              (union (free-vars #'expr1)
                     (free-vars #'expr2)))]
      [(#%app exprs ...)
       (free-vars* (syntax->list #'(exprs ...)))]
      [(#%datum . datum) '()]
      [(#%top . var) '()]
      [s (identifier? #'s)
         (let ([bdg (identifier-binding #'s)])
           (cond
             [(eqv? bdg 'lexical) (list #'s)]
             [(not bdg)
              (if (namespace-defined? (syntax-object->datum #'s))
                  '()
                  (list #'id))]
             [else '()]))])) ;; module defined
  
  ;; free-vars/lambda: formals (listof expr) -> (listof identifier)
  ;; Find the free variables of a lambda
  (define (free-vars/lambda formals body-exprs)
    (syntax-case formals ()
      [(args ... . restarg)
       (set-diff (free-vars* body-exprs)
                 (cons #'restarg (syntax->list #'(args ...))))]
      [(args ...)
       (set-diff (free-vars* body-exprs)
                 (syntax->list #'(args ...)))]
      [restarg
       (set-diff (free-vars* body-exprs)
                 (list #'restarg))]))
  
  ;; free-vars*: (listof expr) -> (listof identifier)
  ;; union the free variables that occur in several expressions
  (define (free-vars* exprs)
    (foldl
     (lambda (expr acc) (union (free-vars expr) acc))
     '() exprs))
  
  ;; union: (listof identifier) (listof identifier) -> (listof identifier)
  ;; produce the set-theoretic union of two lists
  (define (union l1 l2)
    (cond
      [(null? l1) l2]
      [else (insert (car l1) (union (cdr l1) l2))]))
  
  ;; insert: symbol (listof identifier) -> (listof symbol)
  ;; insert a symbol into a list without creating a duplicate
  (define (insert sym into)
    (cond
      [(null? into) (list sym)]
      [(bound-identifier=? sym (car into)) into]
      [else (cons (car into) (insert sym (cdr into)))]))
  
  ;; set-diff: (listof identifier) (listof identifier) -> (listof identifier)
  ;; produce the set-theoretic difference of two lists
  (define (set-diff s1 s2)
    (cond
      [(null? s2) s1]
      [else (set-diff (sans s1 (car s2)) (cdr s2))]))
  
  ;; sans: (listof identifier) symbol -> (listof identifier)
  ;; produce the list sans the symbol
  (define (sans s elt)
    (cond
      [(null? s) '()]
      [(bound-identifier=? (car s) elt)
       (cdr s)] ;; if we maintain the no-dupe invariant then we don't need to recur
      [else (cons (car s)
                  (sans (cdr s) elt))]))
  
  ;; ****************************************
  ;; LAMBDA LIFTING
  
  ;flat-expr is one of
  ;  variable
  ;  (p-lambda identifier flat-expr ...)
  ;  (if flat-expr flat-expr)
  ;  (if flat-expr flat-expr flat-expr)
  ;  (begin flat-expr ...1)
  ;  (begin0 flat-expr flat-expr ...)
  ;  (let-values (((variable ...) flat-expr) ...) flat-expr ...1)
  ;  (letrec-values (((variable ...) flat-expr) ...) flat-expr ...1)
  ;  (set! variable flat-expr)
  ;  (quote datum)
  ;  (quote-syntax datum)
  
  ;; Need to think about this one a bit:
  ;  (with-continuation-mark expr expr expr)
  
  ;  (#%app flat-expr ...1)
  ;  (#%datum . datum)
  ;  (#%top . variable)
  
  ;flat-definition is one of
  ;  (define formals flat-expr ...)
  ;  (define id
  ;    (case-lambda (formals flat-expr ...) ...))
  
  ;; lift: expr -> flat-expr (listof flat-definition)
  ;; replace every lambda and case-lambda with a p-lambda
  ;; and generate the corresponding flat-definitions
  (define (lift expr)
    ;(printf "lift: expr = ~s~n" expr)
    (kernel-syntax-case expr #f
      [(lambda formals body-exprs ...)
       (let* ([f (namespace-syntax-introduce
                  (datum->syntax-object #f (genproc 'lifted)))]
              [fv (free-vars expr)]
              [new-formals (formals/fv #'formals fv)])
         (let-values ([(lifted-body-exprs body-definitions)
                       (lift* (syntax->list #'(body-exprs ...)))])
           (values
            #`(#%app p-lambda #,f (lambda () (list #,@fv)))
            (cons #`(define #,f
                      (lambda #,new-formals
                        #,@lifted-body-exprs)) body-definitions))))]
      [(case-lambda cases ...)
       (let ([f (namespace-syntax-introduce
                 (datum->syntax-object #f (genproc 'lifted)))]
             [fv (free-vars expr)])
         (let-values ([(new-cases case-definitions)
                       (lift-cases #'(cases ...) fv)])
           (values
            #`(#%app p-lambda #,f (lambda () (list #,@fv)))
            (cons
             #`(define #,f
                 (case-lambda #,@new-cases))
             case-definitions))))]
      [(if test-expr csq-expr)
       (let-values ([(new-test-expr test-definitions)
                     (lift #'test-expr)]
                    [(new-csq-expr csq-definitions)
                     (lift #'csq-expr)])
         (values #`(if #,new-test-expr #,new-csq-expr)
                 (append test-definitions
                         csq-definitions)))]
      [(if test-expr csq-expr alt-expr)
       (let-values ([(new-test-expr test-definitions)
                     (lift #'test-expr)]
                    [(new-csq-expr csq-definitions)
                     (lift #'csq-expr)]
                    [(new-alt-expr alt-definitions)
                     (lift #'alt-expr)])
         (values
          #`(if #,new-test-expr #,new-csq-expr #,new-alt-expr)
          (append test-definitions
                  csq-definitions
                  alt-definitions)))]
      [(begin exprs ...)
       (let-values ([(new-exprs definitions)
                     (lift* (syntax->list #'(exprs ...)))])
         (values #`(begin #,@new-exprs)
                 definitions))]
      [(begin0 expr0 exprs ...)
       (let-values ([(new-expr0 definitions0)
                     (lift #'expr0)]
                    [(new-exprs definitions)
                     (lift* (syntax->list #'(exprs ...)))])
         (values #`(begin0 #,new-expr0 #,@new-exprs)
                 (append definitions0 definitions)))]
      [(let-values ([(varss ...) rhs-exprs] ...) body-exprs ...)
       (let-values ([(new-bindings binding-definitions)
                     (lift-bindings #'([(varss ...) rhs-exprs] ...))]
                    [(new-body-exprs body-definitions)
                     (lift* (syntax->list #'(body-exprs ...)))])
         (values
          #`(let-values #,new-bindings #,@new-body-exprs)
          (append binding-definitions body-definitions)))]
      [(letrec-values ([(varss ...) rhs-exprs] ...) body-exprs ...)
       (let-values ([(new-bindings binding-definitions)
                     (lift-bindings #'([(varss ...) rhs-exprs] ...))]
                    [(new-body-exprs body-definitions)
                     (lift* (syntax->list #'(body-exprs ...)))])
         (values
          #`(letrec-values #,new-bindings #,@new-body-exprs)
          (append binding-definitions body-definitions)))]
      [(set! var rhs-expr)
       (error "set! is not allowed")
;       (let-values ([(new-rhs-expr definitions) (lift #'rhs-expr)])
;         (values
;          #`(set! var #,new-rhs-expr)
;          definitions))
       ]
      [(quote datum) (values expr '())]
      [(quote-syntax datum) expr]
      [(with-continuation-mark expr0 expr1 expr2)
       (error "with-continuation-mark not implemented")]
      [(#%app exprs ...)
       (let-values ([(new-exprs definitions) (lift* (syntax->list #'(exprs ...)))])
         (values #`(#%app #,@new-exprs) definitions))]
      [(#%datum . datum) (values expr '())]
      [(#%top . var) (values expr '())]
      [s (identifier? #'s) (values expr '())]))
  
  ;; lift*: (listof expr) -> (listof expr) (listof definition)
  ;; lift all lambdas in a list of exprs
  (define (lift* exprs)
    (cond
      [(null? exprs) (values '() '())]
      [else
       (let-values ([(expr definitions) (lift (car exprs))]
                    [(rest-exprs rest-defines) (lift* (cdr exprs))])
         (values
          (cons expr rest-exprs)
          (append definitions rest-defines)))]))
  
  ;; lift-bindings: ([(varss ...) rhs-exprs] ...)
  ;;                -> (listof [(varss ...) rhs-exprs]) (listof defintion)
  ;; lift all lambdas in the bindings part of a let- or letrec-values
  (define (lift-bindings bindings)
    (syntax-case bindings ()
      [() (values '() '())]
      [([(vars ...) rhs-expr] [(rest-varss ...) rest-rhs-exprs] ...)
       (let-values ([(new-rhs-expr rhs-definitions)
                     (lift #'rhs-expr)]
                    [(rest-bindings rest-definitions)
                     (lift-bindings #'([(rest-varss ...) rest-rhs-exprs] ...))])
         (values
          (cons #`[(vars ...) #,new-rhs-expr] rest-bindings)
          (append rhs-definitions rest-definitions)))]))
  
  ;; lift-cases: ((formals expr ...) ...) (listof identifier)
  ;;             -> (listof (formals expr ...)) (listof definition)
  ;; construct the new cases for a lifted case-lambda
  (define (lift-cases cases fv)
    (syntax-case cases ()
      [() (values '() '())]
      [((formals body-exprs ...) rest-cases ...)
       (let ([new-formals (formals/fv #'formals fv)])
         (let-values ([(lifted-body-exprs body-definitions)
                       (lift* (syntax->list #'(body-exprs ...)))]
                      [(rest-new-cases rest-definitions)
                       (lift-cases #'(rest-cases ...) fv)])
           (values
            (cons #`(#,new-formals #,@lifted-body-exprs)
                  rest-new-cases)
            (append body-definitions rest-definitions))))]))
  
  ;; formals/fv: formals (listof identifier) -> formals
  ;; create the new formals for a lambda or case-lambda using the free vars
  (define (formals/fv formals fv)
    (syntax-case formals ()
      [(args ...) #`(#,@fv args ...)]
      [(args ... . restarg) #`(#,@fv args ... . restarg)]
      [restarg #`(#,@fv . restarg)]))
  
  ;; ****************************************
  ;; A-NORMALIZING
  
  ;A-expr is one of
  ;  value
  ;  (if value A-expr)
  ;  (if value A-expr A-expr)
  ;  (let-values ([(variable ...) (#%app value value ...)] ...) A-expr)
  ;  (let-values ([(variable ...) (#%app primop value ...)] ...) A-expr)
  ;  (letrec-values ([(variable ...) (#%app value value ...)] ...) A-expr)
  ;  (letrec-values ([(variable ...) (#%app primop value ...)] ...) A-expr)
  ;  (#%app primop value ...)
  ;  (#%app value value ...)
  
  ;value is one of
  ;  (quote datum)
  ;  (#%datum . datum)
  ;  (#%top . variable)
  ;  (#%app p-lambda identifier value ...)
  ;  variable
  
  ;; value?: expr -> boolean
  (define (value? expr)
    (syntax-case expr (#%app #%top #%datum p-lambda quote)
      [(#%top . d) #t]
      [(#%datum . d) #t]
      [(quote . whatever) #t]
      [id (identifier? #'id) #t]
      [(#%app p-lambda id exprs ...)
       (andmap value? (syntax->list #'(exprs ...)))]
      [something_else #f]))
  
  ;; this data def is handy for contracts:
  ;app-v-expr is one of
  ;   value
  ;   (#%app primop value ...)
  ;   (#%app value value ...)
  
  ;; Notes:
  ;; (1) Not worrying about with-continuation-mark yet:
  ;;     (with-continuation-mark expr expr expr)
  ;; (2) I think I will unroll begin forms into lets:
  ;;     (begin flat-expr ...1)
  ;;     (begin0 flat-expr flat-expr ...)
  ;; (3) No set! for now
  ;;     (set! variable flat-expr)
  
  ;; normalize-term: flat-expr -> A-expr
  (define (normalize-term f-expr)
    (normalize f-expr (lambda (x) x)))
  
  ;; normalize: flat-expr (app-v-expr -> A-expr) -> A-expr
  ;; convert an expression into A-normal form
  (define (normalize f-expr k)
    (syntax-case f-expr (p-lambda if begin begin0 let-values letrec-values
                                  quote #%app #%datum #%top)
      [(p-lambda proc-id f-exprs ...)
       (normalize-name* (syntax->list #'(f-exprs ...))
                        (lambda (vals)
                          (k #`(p-lambda proc-id #,@vals))))]
      [(if test-f-expr csq-f-expr)
       (normalize-name #'test-f-expr
                       (lambda (val)
                         (k #`(if #,val #,(normalize-term #'csq-f-expr)))))]
      [(if test-f-expr csq-f-expr alt-f-expr)
       (normalize-name #'test-f-expr
                       (lambda (val)
                         (k #`(if #,val
                                  #,(normalize-term #'csq-f-expr)
                                  #,(normalize-term #'alt-f-expr)))))]
      [(begin f-exprs ...)
       (syntax-case #'(f-exprs ...) ()
         [(e) (normalize #'e k)]
         [(e es ...) (normalize #'(let-values ([(x) e]) (begin es ...)) k)])]
      [(begin0 f-exprs ...)
       (syntax-case #'(f-exprs ...) ()
         [(e) (normalize #'e k)]
         ; use of expand in the next line to put magic on x:
         [(e es ...) (normalize (expand #'(let-values ([(x) e]) (begin es ... x))) k)])]
      [(let-values ([(varss ...) rhs-f-exprs] ...) body-f-exprs ...)
       (normalize-let f-expr k)]
      [(letrec-values ([(varss ...) rhs-f-exprs] ...) body-f-exprs ...)
       (normalize-letrec f-expr k)]
      [(quote datum) (k f-expr)]
      [(#%app fn f-exprs ...)
       (if (primop? #'fn)
           (normalize-name* (syntax->list #'(f-exprs ...))
                            (lambda (vals)
                              (k #`(fn #,@vals))))
           (normalize-name #'fn
                           (lambda (val)
                             (normalize-name* (syntax->list #'(f-exprs ...))
                                              (lambda (vals)
                                                (k #`(#,val #,@vals)))))))]
      [(#%datum . datum) (k f-expr)]
      [(#%top . var) (k f-expr)]
      [s (identifier? #'s) (k f-expr)]))
  
  ;; normalize-name: flat-expr (value -> A-expr) -> A-expr
  ;; name the result of evaluating flat-expr if it is not a value
  (define (normalize-name f-expr k)
    (normalize
     f-expr
     (lambda (n)
       (if (value? n)
           (k n)
           (let ([t (namespace-syntax-introduce
                     (datum->syntax-object #f (gensym)))])
             #`(let-values ([(#,t) #,n]) #,(k t)))))))
  
  ;; normalize-name*: (listof flat-expr) ((listof value) -> A-expr) -> A-expr
  ;; name the results of evaluating flat-exprs if they are not values
  (define (normalize-name* f-exprs k)
    (if (null? f-exprs)
        (k '())
        (normalize-name
         (car f-exprs)
         (lambda (val)
           (normalize-name* (cdr f-exprs)
                            (lambda (vals)
                              (k #`(#,val #,@vals))))))))
  
  ;; normalize-let: flat-expr (app-v-expr -> A-expr) -> A-expr
  ;; convert a let-values expresion into A-normal form
  (define (normalize-let l-expr k)
    (syntax-case l-expr (let-values)
      [(let-values ([(vars ...) rhs-f-expr]) body-f-expr)
       (normalize #'rhs-f-expr
                  (lambda (a-v-e)
                    #`(let-values ([(vars ...) #,a-v-e]) #,(normalize #'body-f-expr k))))]
      
      ;; just unroll the let into nested lets
      ;; hopfully the syntax magic will prevent variable capture
      ;; syntax magic will only work with expanded code.
      [(let-values ([(vars ...) rhs-f-expr]
                    [(rest-varss ...) rest-rhs-f-exprs] ...)
         body-f-expr)
       (normalize-let
        #'(let-values ([(vars ...) rhs-f-expr])
            (let-values ([(rest-varss ...) rest-rhs-f-exprs] ...)
              body-f-expr))
        k)]
      ;; make the implicit begin explicit
      [(let-values ([(varss ...) rhs-f-exprs] ...) body-f-exprs ...)
       (normalize-let
        #'(let-values ([(varss ...) rhs-f-exprs] ...) (begin body-f-exprs ...))
        k)]))
  
  ;; normalize-letrec: flat-expr (app-v-expr -> A-expr) -> A-expr
  ;; convert a letrec-values expression into A-normal form (sort of)
  ;; Note: right now I'm not going to convert the r.h.s.
  ;;       the justification is that there should be no interaction
  ;;       while evaluation the r.h.s. of a letrec. To make this safe,
  ;;       I will put a mark around the r.h.s. and raise an error
  ;;       if a stack serialization happens during a r.h.s.  
  (define (normalize-letrec l-expr k)
    (syntax-case l-expr (letrec-values)
            [(letrec-values ([(varss ...) rhs-f-exprs] ...) body-f-expr)
       (normalize #'body-f-expr
                  (lambda (new-body-expr)
                    #`(letrec-values ([(varss ...) rhs-f-exprs] ...)
                        #,(k new-body-expr))))]
      
      ;; make the implicit begin explicit
      [(letrec-values ([(varss ...) rhs-f-exprs] ...) body-f-exprs ...)
       (normalize-letrec
        #'(letrec-values ([(varss ...) rhs-f-exprs] ...) (begin body-f-exprs ...))
        k)]))
  
  ;; primop?: x -> boolean
  (define (primop? x)
    (syntax-case x (#%top)
      [(#%top . d) (primop? #'d)]
      [id (identifier? #'id)
          (and (namespace-defined? (syntax-object->datum #'id))
               (primitive? (namespace-variable-value (syntax-object->datum #'id))))]
      [_else #f]))
  )