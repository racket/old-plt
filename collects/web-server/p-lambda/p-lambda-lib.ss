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
  ;  (p-lambda identifier flat-expr ...1)
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
  
  )