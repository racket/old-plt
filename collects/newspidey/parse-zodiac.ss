(require-library "core.ss")
(require-library "cores.ss")                      ;; for mzlib:core^
(require-library "invoke.ss" "zodiac")              ;; for zodiac:interface^ and z

(define counter 0)
(define (gensym)
  (set! counter (add1 counter))
  (string->symbol (string-append "g" (number->string counter))))
  
(define (parse-zodiac filename)
  (let ([port (open-input-file filename)])
    (letrec ([defs-thunk
              (zodiac:read ;; definitions text stream thunk
               port
               (zodiac:make-location 1 1 0 filename))]
             [read-from-thunk
              (lambda (thunk)
                (let ([sexp (thunk)])
                  (if (zodiac:eof? sexp)
                      ()
                      (cons sexp (read-from-thunk thunk)))))])
      (parameterize ([current-namespace (make-namespace)])
        (zodiac:prepare-current-namespace-for-vocabulary
         zodiac:scheme-vocabulary)
        (let ([terms (zodiac:scheme-expand-program
                      (read-from-thunk defs-thunk)
                      (zodiac:make-attributes)
                      zodiac:scheme-vocabulary)])
          (printf "terms: ~a~n" terms)
          (map (lambda (term) (derive-top-term-constraints '() term))
               terms)
          (propagate-constraints)
          (init-set-var-to-type)
          (type-reduce-rec-bindings)
          )))))


(define-struct Const (val))
(define-struct Token (name))
(define-struct Set-var (name))
(define-struct Dom-arity (arity pos set-var))
(define-struct Dom-interval (interval pos set-var))
(define-struct Rng-arity (arity set-var))
(define-struct Rng-interval (interval set-var))
(define-struct Label (name))
(define-struct Car (set-var))
(define-struct Cdr (set-var))
(define-struct Interval (lo hi))
(define-struct Arity (req proh))
;;(define-struct Listexp (set-vars))

(define *Nullexp* (make-Const '()))
(define *pair-token* (make-Token 'pair))

(define (gen-set-var)
  (let ([set-var (make-Set-var (gensym))])
    (add-set-var set-var)
    set-var))

(define-struct constraint (lo hi))

(define *the-constraints* '())
(define *constraint-table* (make-hash-table))

(define *dom-interval->dom-arity-table* (make-hash-table))
(define *dom-arity->dom-interval-table* (make-hash-table))
(define *rng-interval->rng-arity-table* (make-hash-table))
(define *rng-arity->rng-interval-table* (make-hash-table))

(define (encode-dom n alpha)
  (string->symbol (string-append "dom_" (number->string n) "(" (symbol->string (Set-var-name alpha)) ")")))

(define (relate-dom-int-to-ar dom-int)
  (let* ([n (Dom-interval-pos dom-int)]
         [alpha (Dom-interval-set-var dom-int)]
         [dom-encoding (encode-dom n alpha)]
         [dom-int-list (lookup-dom-ar dom-encoding)])
    (hash-table-put! *dom-arity->dom-interval-table* dom-encoding (cons dom-int dom-int-list))))

(define (lookup-dom-ar sym)
  (hash-table-get *dom-arity->dom-interval-table* sym (lambda () '())))

(define (relate-dom-ar-to-int dom-ar)
  (let* ([n (Dom-arity-pos dom-ar)]
         [alpha (Dom-arity-set-var dom-ar)]
         [dom-encoding (encode-dom n alpha)]
         [dom-ar-list (lookup-dom-int dom-encoding)])
    (hash-table-put! *dom-interval->dom-arity-table* dom-encoding (cons dom-ar dom-ar-list))))

(define (lookup-dom-int sym)
  (hash-table-get *dom-interval->dom-arity-table* sym (lambda () '())))

(define (encode-rng alpha)
  (string->symbol (string-append "rng_(" (symbol->string (Set-var-name alpha)) ")")))

(define (relate-rng-int-to-ar rng-int)
  (let* ([alpha (Rng-interval-set-var rng-int)]
         [rng-encoding (encode-rng alpha)]
         [rng-int-list (lookup-rng-ar rng-encoding)])
    (hash-table-put! *rng-arity->rng-interval-table* rng-encoding (cons rng-int rng-int-list))))

(define (lookup-rng-ar sym)
  (hash-table-get *rng-arity->rng-interval-table* sym (lambda () '())))

(define (relate-rng-ar-to-int rng-ar)
  (let* ([alpha (Rng-arity-set-var rng-ar)]
         [rng-encoding (encode-rng alpha)]
         [rng-ar-list (lookup-rng-int rng-encoding)])
    (hash-table-put! *rng-interval->rng-arity-table* rng-encoding (cons rng-ar rng-ar-list))))

(define (lookup-rng-int sym)
  (hash-table-get *rng-interval->rng-arity-table* sym (lambda () '())))


(define *set-var->rng-arities-table* (make-hash-table))
(define *set-var->dom-arities-table* (make-hash-table))

(define (relate-set-var-to-rng-ar rng-ar)
  (let* ([beta (Rng-arity-set-var rng-ar)]
         [rng-ars (hash-table-get *set-var->rng-arities-table* beta (lambda () '()))])
    (hash-table-put! *set-var->rng-arities-table* beta (cons rng-ar rng-ars))))

(define (relate-set-var-to-dom-ar dom-ar)
  (let* ([alpha (Dom-arity-set-var dom-ar)]
         [dom-ars (hash-table-get *set-var->dom-arities-table* alpha (lambda () '()))])
    (hash-table-put! *set-var->dom-arities-table* alpha (cons dom-ar dom-ars))))

(define (lookup-rng-ars-from-set-var set-var)
  (hash-table-get *set-var->rng-arities-table* set-var (lambda () '())))

(define (lookup-dom-ars-from-set-var set-var)
  (hash-table-get *set-var->dom-arities-table* set-var (lambda () '())))

(define *set-var-to-term-table* (make-hash-table))
(define *term-to-set-var-table* (make-hash-table))
(define *label-to-lambda-table* (make-hash-table))
(define *lambda-to-label-table* (make-hash-table))
(define *label-to-ars-table* (make-hash-table))

(define (associate-set-var-and-term alpha term)
  (hash-table-put! *set-var-to-term-table* alpha term)
  (hash-table-put! *term-to-set-var-table* term alpha))

(define (lookup-term-from-set-var set-var)
  (hash-table-get *set-var-to-term-table* set-var (lambda () #f)))

(define (lookup-set-var-from-term term)
  (hash-table-get *term-to-set-var-table* set-var (lambda () #f)))

;(define (get-all-set-vars)
;  (hash-table-map *set-var-to-term-table* (lambda (set-var term) set-var)))

(define (associate-label-and-lambda label term)
  (hash-table-put! *label-to-lambda-table* label term)
  (hash-table-put! *lambda-to-label-table* term label))

(define (lookup-lambda-from-label label)
  (hash-table-get *label-to-lambda-table* label (lambda () #f)))

(define (lookup-label-from-lambda ar)
  (hahs-table-get *lambda-to-label-table* ar (lambda () '())))

(define (associate-label-with-ars label ar)
  (let ([ars (hash-table-get *lambda-to-label-table* ar (lambda () '()))])
    (hash-table-put! *label-to-ars-table* label (cons ar ars))))

(define (lookup-ars-from-label label)
  (hash-table-get *label-to-ars-table* label (lambda () #f)))

(define *all-set-vars* '())
(define (add-set-var set-var)
  (set! *all-set-vars* (cons set-var *all-set-vars*)))
(define (get-all-set-vars)
  *all-set-vars*)

; (union bool num string char '() sym) -> string
(define (scalar->string v)
  (cond 
   [(boolean? v)
    (string-append "bool:" (if v "#t" "#f"))]
   [(number? v)
    (string-append "num:" (number->string v))]
   [(string? v)
    (string-append "str:" v)]
   [(char? v)
    (string-append "char:" (string v))]
   [(symbol? v)
    (string-append "sym:" (symbol->string v))]
   [(null? v) "empty-list"]
   [else (error (format "Unknown value: ~a" v))]))

; (nonempty-listof string) -> string
(define (make-hash-string . strings)
  (foldr (lambda (s1 s2)
           (if (string? s2)
               (string-append s1 "$$" s2)
               s1)) 'dummy strings))

(define (encode-interval interval)
  (string-append "interval:["
                 (number->string (Interval-lo interval))
                 ","
                 (let ([hi (Interval-hi interval)])
                   (if (number? hi)
                       (number->string hi)
                       "omega"))
                 "]"))

(define (encode-arity arity)
  (apply string-append
         "arity:"
         (encode-interval (Arity-req arity))
         ":"
         (map encode-interval (Arity-proh arity))))

; Set-exp -> string
(define (set-exp->string val)
  (let ([build-hash-from-sym
         (lambda (s sym)
           (make-hash-string s (symbol->string sym)))])
    (cond
      [(Const? val)
       (make-hash-string 
        "const"
        (scalar->string (Const-val val)))]
      [(Set-var? val)
       (build-hash-from-sym "var" (Set-var-name val))]
      [(Token? val)
       (build-hash-from-sym "token" (Token-name val))]
      [(Label? val)
       (build-hash-from-sym "label" (Label-name val))]
      [(Dom-arity? val)
       (make-hash-string
        "dom-arity" (encode-arity (Dom-arity-arity val)) (number->string (Dom-arity-pos val))
        (symbol->string (Set-var-name (Dom-arity-set-var val))))]
      [(Dom-interval? val)
       (make-hash-string
        "dom-interval" (encode-interval (Dom-interval-interval val)) (number->string (Dom-interval-pos val))
        (symbol->string (Set-var-name (Dom-interval-set-var val))))]
      [(Rng-arity? val)
       (build-hash-from-sym (string-append "rng-arity" (encode-arity (Rng-arity-arity val)))
                            (Set-var-name (Rng-arity-set-var val)))]
      [(Rng-interval? val)
       (build-hash-from-sym (string-append "rng-interval" (encode-interval (Rng-interval-interval val)))
                            (Set-var-name (Rng-interval-set-var val)))]
;      [(Listexp? val)
;       (apply string-append
;              "Listexp:"
;              (map set-exp->string (Listexp-set-vars val)))]
      [(Car? val)
       (build-hash-from-sym "car" (Set-var-name (Car-set-var val)))]
      [(Cdr? val)
       (build-hash-from-sym "cdr" (Set-var-name (Cdr-set-var val)))]
      [else (error (format "Unknown set expression: ~a" val))])))


(define *empty-pairs-thunk* (lambda () '(() ()))) 

; Set-exp -> (list (listof Set-exp) (listof Set-exp))
(define (lookup-in-constraint-table set-exp)
    (lookup-coded-in-constraint-table (string->symbol (set-exp->string set-exp))))

; ((list (listof Set-exp) (listof Set-exp)) -> (listof Set-exp)) -> 
;   Set-exp -> (listof Set-exp)
(define (make-lookup-in-constraint-table accessor)
  (lambda (set-exp)
    (accessor (lookup-in-constraint-table set-exp))))

; Set-exp -> (listof Set-exp)
(define lookup-in-hi-constraint-table
  (make-lookup-in-constraint-table car))

; (hash-table-of Set-exp 
;   (list (listof Set-exp) (listof Set-exp)) -> 
;     sym -> (listof Set-exp)
(define (make-lookup-coded-in-constraint-table tbl)
  (lambda (coded-set-exp accessor)
    (accessor (lookup-coded-in-constraint-table coded-set-exp))))

; sym -> (list (listof Set-exp) (listof Set-exp))
(define (lookup-coded-in-constraint-table coded-set-exp)
    (hash-table-get *constraint-table* coded-set-exp
                    *empty-pairs-thunk*))

; sym (listof Set-exp) (listof Set-exp) -> void
(define (set-bounds! coded-set-exp los his)
  (hash-table-put! *constraint-table*
                   coded-set-exp
                   (list los his)))

(define (add-new-bound! set-exp lo-fun hi-fun)
  (printf "set exp: ~a~n" set-exp)
  (let* ([coded-set-exp (string->symbol (set-exp->string set-exp))]
         [all-bounds 
          (lookup-coded-in-constraint-table coded-set-exp)]
         [old-los (car all-bounds)]
         [old-his (cadr all-bounds)])
    (set-bounds! coded-set-exp 
                 (lo-fun old-los) (hi-fun old-his)))) 

; Set-exp Set-exp -> void
(define (add-new-upper-bound! set-exp hi)
  (add-new-bound! set-exp
                  identity
                  (lambda (old-his)
                    (cons hi old-his))))

; Set-exp Set-exp -> void
(define (add-new-lower-bound! set-exp lo)
  (add-new-bound! set-exp
                  (lambda (old-los)
                    (cons lo old-los))
                  identity))

; Set-exp Set-exp -> (union #f (listof Set-exp))
; could just as well have used lo-constraint-table
(define (exists-constraint lo hi)
 (member lo (lookup-in-hi-constraint-table hi)))

(define (add-constraint constraint)
  (let ([lo (constraint-lo constraint)]
        [hi (constraint-hi constraint)])
    (if (exists-constraint lo hi)
        #f
        (begin
          (add-new-lower-bound! hi lo)
          (add-new-upper-bound! lo hi)
          (set! *the-constraints*
                (cons constraint
                      *the-constraints*))
          #t))))
    
(define (add-constraint-and-update-tables lo hi)
  (printf "lo: ~a~nhi: ~a~n" lo hi)
  (let ([result (add-constraint (make-constraint lo hi))])
    (when result
      (cond
        [(Dom-interval? lo) (relate-dom-int-to-ar lo)]
        [(Dom-arity? lo)
         (relate-dom-ar-to-int lo)
         (relate-set-var-to-dom-ar lo)]
        [(Rng-interval? lo) (relate-rng-int-to-ar lo)]
        [(Rng-arity? lo)
         (relate-rng-ar-to-int lo)
         (relate-set-var-to-rng-ar lo)]
        [else (void)])
      (cond
        [(Dom-interval? hi) (relate-dom-int-to-ar hi)]
        [(Dom-arity? hi)
         (relate-dom-ar-to-int hi)
         (relate-set-var-to-dom-ar hi)]
        [(Rng-interval? hi) (relate-rng-int-to-ar hi)]
        [(Rng-arity? hi)
         (relate-rng-ar-to-int hi)
         (relate-set-var-to-rng-ar hi)]
        [else (void)]))
    result))
  

(define (add-constraint-with-bounds lo hi flag)
  (if flag
      (add-constraint-and-update-tables lo hi)
      (add-constraint-and-update-tables hi lo)))

; Var (listof (sym Set-var)) -> (union Set-var #f)
(define (lookup-in-env Gamma x)
  (let ([binding (assq x Gamma)])
    (if binding
        (cadr binding)
        #f)))

(define (create-label)
  (make-Label (gensym)))

(define (extend-env Gamma xs alphas)
  (printf "xs: ~a~nGamma: ~a~nalphs: ~a~n" xs Gamma alphas)
  (append (map list xs alphas)
          Gamma))

(define *whole-interval* (make-Interval 0 'omega))

(define-struct Type-Arrow (doms rng))
(define-struct Type-Cons (car cdr))
(define-struct Type-Scheme (vars type))
(define-struct Type-Var (name)) ;; used for type schemes
(define-struct Type-Binding (set-var type))
(define-struct Type-Rec (bindings type))
(define-struct Type-Union (types))
;;(define-struct Type-Const (name))
(define-struct Type-Empty ())

(define *empty-type* (make-Type-Empty))

(define-struct Type-Var-Set-Vars (neg pos))

(define *prim-to-type* (make-hash-table))

(define (add-prim-type sym type)
  (hash-table-put! *prim-to-type* sym type))

(define (lookup-prim-type sym)
  (hash-table-get *prim-to-type* sym (lambda () #f)))

(define *prim-to-label* (make-hash-table))
(define *label-to-prim* (make-hash-table))

(define (lookup-prim-label sym)
  (hash-table-get *prim-to-label* sym (lambda () #f)))

(define (add-prim-label sym label)
  (hash-table-put! *prim-to-label* sym label)
  (hash-table-put! *label-to-prim* label sym))

(define (init-prim prim-entries)
  (for-each
   (lambda (prim-entry)
     (let ([prim-name (car prim-entry)]
           [prim-type (cadr prim-entry)]
           [label (make-Label (gensym))])
       (add-prim-type prim-name prim-type)
       (add-prim-label prim-name label)
       (let ([arg-list (cond
                         [(and (Type-Scheme? prim-type) (Type-Arrow? (Type-Scheme-type prim-type)))
                          (Type-Arrow-doms (Type-Scheme-type prim-type))]
                         [(Type-Arrow? prim-type)
                          (Type-Arrow--doms prim-type)]
                         [else #f])])
         (when arg-list
           (let ([len (length arg-list)])
             (associate-label-with-ars label (make-Arity (make-Interval len len) '())))))))
   prim-entries))

(define prim-init-list
  (list
   (list 'cons (make-Type-Scheme (list 'x 'y)
                                 (make-Type-Arrow (list (make-Type-Var 'x) (make-Type-Var 'y)) 
                                                  (make-Type-Cons (make-Type-Var 'x) (make-Type-Var 'y)))))
   (list 'car (make-Type-Scheme (list 'x 'y)
                                (make-Type-Arrow (list (make-Type-Cons (make-Type-Var 'x) (make-Type-Var 'y)))
                                                 (make-Type-Var 'x))))
   (list 'cdr (make-Type-Scheme (list 'x 'y)
                                (make-Type-Arrow (list (make-Type-Cons (make-Type-Var 'x) (make-Type-Var 'y)))
                                                 (make-Type-Var 'y))))))

(init-prim prim-init-list)

(define (create-set-vars-lists type-vars->set-vars type vars flag type-var->set-var)
  (cond
    [(Type-Var? type)
     (let ([name (Type-Var-name type)])
       (if (memq name vars)
           (let ([set-var (gen-set-var)]
                 [set-vars (hash-table-get type-vars->set-vars name (lambda () #f))])
             (hash-table-put! type-var->set-var type set-var)
             (if flag
                 (hash-table-put! type-vars->set-vars
                                  name
                                  (make-Type-Var-Set-Vars 
                                   (Type-Var-Set-Vars-neg set-vars)
                                   (cons set-var (Type-Var-Set-Vars-pos set-vars))))
                 (hash-table-put! type-vars->set-vars
                                  name
                                  (make-Type-Var-Set-Vars 
                                   (cons set-var (Type-Var-Set-Vars-neg set-vars))
                                   (Type-Var-Set-Vars-pos set-vars)))))
           (error 'type-var "not specified in a scheme")))]
    [(Type-Cons? type)
     (create-set-vars-lists type-vars->set-vars (Type-Cons-car type) vars flag type-var->set-var)
     (create-set-vars-lists type-vars->set-vars (Type-Cons-cdr type) vars flag type-var->set-var)]
    [(Type-Arrow? type)
     (for-each (lambda (param)
                 (create-set-vars-lists type-vars->set-vars param vars (not flag) type-var->set-var))
               (Type-Arrow-doms type))
     (create-set-vars-lists type-vars->set-vars (Type-Arrow-rng type) vars flag type-var->set-var)]
    [else (error 'create-set-vars-lists "unkown type: ~a~n" type)]))

(define (add-constraints-from-type ty alpha flag type-var->set-var)
  ;;(associate-set-var-and-term alpha 'dummy)
  (printf "type set var: ~a~ntype: ~a~n" alpha ty)
  (cond
    [(Type-Var? ty)
     (add-constraint-with-bounds (hash-table-get type-var->set-var ty) alpha flag)]
    [(Type-Cons? ty)
     (let ([car-set-var (gen-set-var)]
           [cdr-set-var (gen-set-var)])
       (add-constraint-with-bounds *pair-token* alpha flag)
       (add-constraints-from-type (Type-Cons-car ty) car-set-var flag type-var->set-var)
       (add-constraints-from-type (Type-Cons-cdr ty) cdr-set-var flag type-var->set-var)
       (add-constraint-with-bounds car-set-var (make-Car alpha) flag)
       (add-constraint-with-bounds cdr-set-var (make-Cdr alpha) flag))]
    [(Type-Arrow? ty)
     (let* ([dom-set-vars (map (lambda (_) (gen-set-var)) (Type-Arrow-doms ty))]
            [rng-set-var (gen-set-var)]
            [len (length dom-set-vars)]
            [arity (make-Arity (make-Interval len len) '())])
       ;; constraints for the doms and rng
       (for-each (lambda (dom set-var)
                   (add-constraints-from-type dom set-var (not flag) type-var->set-var))
                 (Type-Arrow-doms ty) dom-set-vars)
       (add-constraints-from-type (Type-Arrow-rng ty) rng-set-var flag type-var->set-var)
       ;; constraints for the arrow itself (same as lambda definition)
       (let loop ([j 1]
                  [cur-dom-set-vars dom-set-vars])
         (when (<= j len)
           (add-constraint-with-bounds (make-Dom-arity arity j alpha)
                                       (car cur-dom-set-vars)
                                       flag)
           (loop (add1 j) (cdr cur-dom-set-vars))))
       (add-constraint-with-bounds rng-set-var (make-Rng-arity arity alpha) flag))]
    [(Type-Scheme? ty)
     (let ([type-vars->set-vars (make-hash-table)]
           [scheme-type (Type-Scheme-type ty)]
           [type-vars (Type-Scheme-vars ty)])
       (for-each (lambda (type-var)
                   (hash-table-put! type-vars->set-vars type-var (make-Type-Var-Set-Vars '() '())))
                 type-vars)
       (create-set-vars-lists type-vars->set-vars scheme-type type-vars flag type-var->set-var)
       (hash-table-for-each
        type-vars->set-vars
        (lambda (type-var-name type-var-set-vars)
          (for-each
           (lambda (neg-set-var)
             (for-each
              (lambda (pos-set-var)
                (add-constraint-with-bounds neg-set-var pos-set-var flag))
              (Type-Var-Set-Vars-pos type-var-set-vars)))
           (Type-Var-Set-Vars-neg type-var-set-vars))))
       (add-constraints-from-type scheme-type alpha flag type-var->set-var))]
    [else
     (error "add-constraints-for-abstract-value: Expected type, got"
            ty)]))

(define (derive-top-term-constraints Gamma term)
  (let ([alpha (gen-set-var)])
    (associate-set-var-and-term alpha term)
    (cond
      [(zodiac:quote-form? term)
       (add-constraint-with-bounds (make-Const (zodiac:read-object (zodiac:quote-form-expr term))) alpha #t)]
      [(zodiac:lambda-varref? term)
       (let* ([name (zodiac:binding-orig-name (zodiac:bound-varref-binding term))]
              [set-var (lookup-in-env Gamma name)])
         (printf "set-var: ~a~nGamma: ~a~nterm: ~a~n" set-var Gamma (zodiac:binding-orig-name (zodiac:bound-varref-binding term)))
         (add-constraint-with-bounds set-var alpha #t))]
      [(zodiac:top-level-varref/bind/unit? term)
       (let* ([name (zodiac:varref-var term)]
              [type (lookup-prim-type name)]
              [label (lookup-prim-label name)])
         (if type
             (begin
               (add-constraint-with-bounds label alpha #t)
               (add-constraints-from-type type alpha #t (make-hash-table)))
             (error name "unbound variable")))]
      [(zodiac:case-lambda-form? term)
       (let* (;;[xs-l (map (lambda (l) (map zodiac:binding-orig-name (zodiac:arglist-vars l))) (zodiac:case-lambda-form-args term))]
              [xs-l (zodiac:case-lambda-form-args term)]
              [xs-l-sym (map (lambda (l) (map zodiac:binding-orig-name (zodiac:arglist-vars l))) xs-l)]
              [body-l (zodiac:case-lambda-form-bodies term)]
              [label (create-label)]
              [indices (map length xs-l-sym)]
              [bar (printf "indices: ~a~n" indices)]
              [intervals (map (lambda (arg-list len)
                                (cond
                                 [(zodiac:sym-arglist? arg-list) *whole-interval*]
                                 [(zodiac:list-arglist? arg-list) (make-Interval len len)]
                                 [(zodiac:ilist-arglist? arg-list) (make-Interval (sub1 len) 'omega)]))
                              xs-l indices)]
              [arities (let loop ([arities '()]
                                  [loc-int intervals]
                                  [previous '()])
                         (if (null? loc-int)
                             arities
                             (let ([cur-int (car loc-int)])
                               (loop (cons (make-Arity cur-int previous)
                                           arities)
                                     (cdr loc-int)
                                     (cons cur-int previous)))))]
              [alphas-l (map (lambda (index) (build-list index (lambda (_) (gen-set-var)))) indices)]
              [foo (printf "indices: ~a~n" indices)]
              [betas (map (lambda (xs body alphas)
                            (map associate-set-var-and-term alphas xs)
                            (derive-top-term-constraints (extend-env Gamma xs alphas) body))
                          xs-l-sym body-l alphas-l)])
         (printf "betas: ~a~n" betas)
         (associate-label-and-lambda label term)
         (add-constraint-with-bounds label alpha #t)
         (for-each (lambda (alphas arity index beta)
                     (let loop ([j 1]
                                [loc-alphas alphas])
                       (when (<= j index)
                         (add-constraint-with-bounds (make-Dom-arity arity j alpha) (car loc-alphas) #t)
                         (loop (add1 j) (cdr loc-alphas))))
                     (add-constraint-with-bounds beta (make-Rng-arity arity alpha) #t)
                     (associate-label-with-ars label arity))
                   alphas-l arities indices betas))]
      [(zodiac:app? term)
       (let* ([fun (zodiac:app-fun term)]
              [args (zodiac:app-args term)]
              [len (length args)]
              [beta0 (derive-top-term-constraints Gamma fun)]
              [betas (map (lambda (arg) (derive-top-term-constraints Gamma arg)) args)]
              [alphas (build-list (add1 len) (lambda (n) (gen-set-var)))])
         (add-constraint-with-bounds (make-Rng-interval (make-Interval len len) beta0) alpha #t)
         (let loop ([i 0])
           (when (<= i len)
             (add-constraint-with-bounds (make-Rng-interval (make-Interval i 'omega) beta0) alpha #t)
             (loop (add1 i))))
         (let ([alpha_n+1
                (let loop ([i 1]
                           [loc-betas betas]
                           [loc-alphas alphas])
                  (if (<= i len)
                      (begin
                        (let ([beta_i (car loc-betas)]
                              [alpha_i (car loc-alphas)])
                          (add-constraint-with-bounds beta_i
                                                      (make-Dom-interval (make-Interval len len) i beta0)
                                                      #t)
                          (add-constraint-with-bounds beta_i
                                                      (make-Car alpha_i)
                                                      #t)
                          (add-constraint-with-bounds (cadr loc-alphas)
                                                      (make-Cdr alpha_i)
                                                      #t)
                          (add-constraint-with-bounds alpha_i
                                                      (make-Dom-interval (make-Interval (sub1 i) 'omega) i beta0)
                                                      #t)
                          (add-constraint-with-bounds *pair-token*
                                                      alpha_i
                                                      #t))
                        (loop (add1 i) (cdr loc-betas) (cdr loc-alphas)))
                      (car loc-alphas)))])
           (add-constraint-with-bounds *Nullexp*
                                       alpha_n+1
                                       #t)
           (add-constraint-with-bounds alpha_n+1
                                       (make-Dom-interval (make-Interval len 'omega) (add1 len) beta0)
                                       #t))
         (let loop-i ([i 0])
           (when (<= i len)
             (let loop-j ([j 1]
                          [loc-betas betas])
               (when (<= j i)
                 (add-constraint-with-bounds (car loc-betas)
                                             (make-Dom-interval (make-Interval i 'omega) j beta0)
                                             #t)
                 (loop-j (add1 j) (cdr loc-betas))))
             (loop-i (add1 i)))))]
      [else (error 'unknown "unknown term ~a~n" term)]
           )
    alpha))


(define (constant-set-exp? set-exp)
  (or (Const? set-exp) (Label? set-exp) (Token? set-exp))) ;;(Listexp? set-exp)))

(define (selector? set-exp)
  (or (Car? set-exp) (Cdr? set-exp)))

(define (satisfies-int int1 int2)
  (let ([n (Interval-lo int1)]
        [m (Interval-hi int1)]
        [p (Interval-lo int2)]
        [q (Interval-hi int2)])
    (printf "n: ~a m: ~a p: ~a q: ~a~n" n m p q)
    (or (and (andmap number? (list n m p q))
             (= n m p q))
        (and (= n p)
             (andmap symbol? (list m q))
             (andmap (lambda (sym) (symbol=? sym 'omega)) (list m q))))))

(define (satisfies int ar)
  (let ([ar-req (Arity-req ar)]
        [ar-proh (Arity-proh ar)])
    (and (satisfies-int int ar-req)
         (not (ormap (lambda (ar-proh-int) (satisfies-int int ar-proh-int)) ar-proh)))))


(define (lookup-lo-and-filter pred set-exp)
  (filter pred (cadr (lookup-in-constraint-table set-exp))))

(define (lookup-hi-and-filter pred set-exp)
  (filter pred (car (lookup-in-constraint-table set-exp))))

(define (fire-covariant-selector-rule lo his make-selector)
  (printf "covariant: ~a ~a~n" lo his)
  (for-each
   (lambda (hi)
     (add-one-constraint-and-propagate lo (make-selector hi)))
   his))

(define (fire-contravariant-selector-rule los hi make-selector)
  (for-each
   (lambda (lo)
     (add-one-constraint-and-propagate (make-selector lo) hi))
   los))

(define (add-one-constraint-and-propagate lo hi)
  (if (add-constraint-with-bounds lo hi #t)
      (begin
        (printf "add-constraint~n")
        (propagate-one-constraint (make-constraint lo hi)))
      (printf "not add-constraint~n")))

(define (propagate-constraints)
  (for-each propagate-one-constraint *the-constraints*))

(define (propagate-one-constraint constraint)
  (let ([lo (constraint-lo constraint)]
        [hi (constraint-hi constraint)])
    (printf "constraint: ~a~n" constraint)
    ;; trans-cons
    (when (and (constant-set-exp? lo)
               (Set-var? hi))
      (let ([his-other (lookup-lo-and-filter Set-var? hi)])
        (for-each
         (lambda (hi-other)
           (add-one-constraint-and-propagate lo hi-other))
         his-other)))
    ;; trans-sel
    (when (and (selector? hi)
               (Set-var? lo))
      (let ([his-other (lookup-lo-and-filter Set-var? hi)])
        (for-each
         (lambda (hi-other)
           (add-one-constraint-and-propagate lo hi-other))
         his-other)))
    ;;trans-dom
    (when (and (Dom-interval? hi)
               (Set-var? lo))
      (let* ([interval (Dom-interval-interval hi)]
             [n (Dom-interval-pos hi)]
             [dom-arities (lookup-dom-int (encode-dom n (Dom-interval-set-var hi)))]
             [good-dom-ars (filter (lambda (dom-ar) (satisfies interval (Dom-arity-arity dom-ar))) dom-arities)]
             [his-other
              (apply append (map (lambda (dom-ar) (lookup-lo-and-filter Set-var? dom-ar)) good-dom-ars))])
        (for-each
         (lambda (hi-other)
           (add-one-constraint-and-propagate lo hi-other))
         his-other)))
    ;;trans-rng
    (when (and (Rng-arity? hi)
               (Set-var? lo))
      (printf "trans-rng 1~n")
      (let* ([arity (Rng-arity-arity hi)]
             [rng-intervals (lookup-rng-ar (encode-rng (Rng-arity-set-var hi)))]
             [good-rng-ints
              (filter (lambda (rng-int) (satisfies (Rng-interval-interval rng-int) arity)) rng-intervals)]
             [his-other
              (apply append (map (lambda (rng-int) (lookup-lo-and-filter Set-var? rng-int)) good-rng-ints))])
        (printf "arity: ~a~nrng-intervals: ~a~ngood-rng-ints: ~a~nhis-other: ~a~n" arity rng-intervals good-rng-ints his-other)
        (for-each
         (lambda (hi-other)
           (add-one-constraint-and-propagate lo hi-other))
         his-other)))
    ;; rng-prop
    (when (and (Rng-arity? hi)
               (Set-var? lo))
      (let* ([arity (Rng-arity-arity hi)]
             [beta (Rng-arity-set-var hi)]
             [his-other (lookup-lo-and-filter Set-var? beta)])
        (fire-covariant-selector-rule lo his-other (lambda (gamma) (make-Rng-arity arity gamma)))))
    ;; dom-prop
    (when (and (Dom-arity? lo)
               (Set-var? hi))
      (let* ([arity (Dom-arity-arity lo)]
             [n (Dom-arity-pos lo)]
             [alpha (Dom-arity-set-var lo)]
             [his-other (lookup-lo-and-filter Set-var? alpha)])
        (fire-contravariant-selector-rule his-other hi (lambda (gamma) (make-Dom-arity arity n gamma)))))
    ;; car-prop
    (when (and (Car? hi)
               (Set-var? lo))
      (let* ([beta (Car-set-var hi)]
             [his-other (lookup-lo-and-filter Set-var? beta)])
        (fire-covariant-selector-rule lo his-other (lambda (gamma) (make-Car gamma)))))
    ;; cdr-prop
    (when (and (Cdr? hi)
               (Set-var? lo))
      (let* ([beta (Cdr-set-var hi)]
             [his-other (lookup-lo-and-filter Set-var? beta)])
        (fire-covariant-selector-rule lo his-other (lambda (gamma) (make-Cdr gamma)))))
;    ;; list-car
;    (when (and (Listexp? lo)
;               (Set-var? hi))
;      (let* ([his-other (lookup-lo-and-filter Set-var? (make-Car hi))]
;             [alpha1 (car (Listexp-set-vars lo))])
;        (for-each
;         (lambda (hi-other)
;           (add-one-constraint-and-propagate alpha1 hi-other))
;         his-other)))
;    ;; list-cdr
;    (when (and (Listexp? lo)
;               (Set-var? hi))
;      
;      (let* ([his-other (lookup-lo-and-filter Set-var? (make-Cdr hi))]
;             [new-low (if (> (length (Listexp-set-vars lo)) 1)
;                          (make-Listexp (cdr (Listexp-set-vars lo)))
;                          *Nullexp*)])
;        (for-each
;         (lambda (hi-other)
;           (add-one-constraint-and-propagate new-low hi-other))
;         his-other)))

    
    ;; Reverse rules
    (when (and (Set-var? lo)
               (Set-var? hi))
      ;; trans-const
      (let* ([los-other (lookup-hi-and-filter constant-set-exp? lo)])
        (for-each
         (lambda (lo-other)
           (add-one-constraint-and-propagate lo-other hi))
         los-other))
      ;; rng-prop
      (let* ([rng-arities (lookup-rng-ars-from-set-var (Set-var-name lo))])
        (for-each
         (lambda (rng-ar)
           (let* ([arity (Rng-arity-arity rng-ar)]
                  [rng-arity-gamma (make-Rng-arity arity hi)]
                  [los-other (lookup-hi-and-filter Set-var? rng-ar)])
             (for-each
              (lambda (lo-other)
                (add-one-constraint-and-propagate lo-other rng-arity-gamma))
              los-other)))
         rng-arities))
      ;; dom-prop
      (let* ([dom-arities (lookup-dom-ars-from-set-var (Set-var-name lo))])
        (for-each
         (lambda (dom-ar)
           (let* ([arity (Rng-arity-arity dom-ar)]
                  [n (Rng-arity-pos dom-ar)]
                  [dom-arity-gamma (make-Dom-arity arity n hi)]
                  [his-other (lookup-lo-and-filter Set-var? dom-ar)])
             (for-each
              (lambda (hi-other)
                (add-one-constraint-and-propagate dom-arity-gamma hi-other))
              his-other)))
         dom-arities))
      ;; car-prop
      (let* ([car-gamma (make-Car hi)]
             [los-other (lookup-hi-and-filter Set-var? (make-Car lo))])
        (for-each
         (lambda (lo-other)
           (add-one-constraint-and-propagate lo-other car-gamma))
         los-other))
      ;; cdr-prop
      (let* ([cdr-gamma (make-Cdr hi)]
             [los-other (lookup-hi-and-filter Set-var? (make-Cdr lo))])
        (for-each
         (lambda (lo-other)
           (add-one-constraint-and-propagate lo-other cdr-gamma))
         los-other)))
    ;; trans-sel
    (when (and (selector? lo)
               (Set-var? hi))
      (let* ([los-other (lookup-hi-and-filter Set-var? lo)])
        (for-each
         (lambda (lo-other)
           (add-one-constraint-and-propagate lo-other hi))
         los-other)))
    ;; trans-dom
    (when (and (Dom-arity? lo)
               (Set-var? hi))
      (let* ([arity (Dom-arity-arity lo)]
             [n (Dom-arity-pos lo)]
             [dom-intervals (lookup-dom-ar (encode-dom n (Dom-arity-set-var lo)))]
             [good-dom-ints (filter (lambda (dom-int) (satisfies (Dom-interval-interval dom-int) arity)) dom-intervals)]
             [los-other
              (apply append (map (lambda (dom-int) (lookup-hi-and-filter Set-var? dom-int)) good-dom-ints))])
        (for-each
         (lambda (lo-other)
           (add-one-constraint-and-propagate lo-other hi))
         los-other)))
    ;; trans-rng
    (when (and (Rng-interval? lo)
               (Set-var? hi))
      (printf "trans-rng 2~n")
      (let* ([interval (Rng-interval-interval lo)]
             [rng-arities (lookup-rng-int (encode-rng (Rng-interval-set-var lo)))]
             [good-rng-ars
              (filter (lambda (rng-ar) (satisfies interval (Rng-arity-arity rng-ar))) rng-arities)]
             [los-other
              (apply append (map (lambda (rng-ar) (lookup-lo-and-filter Set-var? rng-ar)) good-rng-ars))])
        (for-each
         (lambda (lo-other)
           (add-one-constraint-and-propagate lo-other hi))
         los-other)))
;    ;; list-car
;    (when (and (Car? lo)
;               (Set-var? hi))
;      (let* ([los-other (lookup-hi-and-filter Listexp? (Car-set-var lo))])
;        (for-each
;         (lambda (lo-other)
;           (let ([alpha1 (car (Listexp-set-vars lo-other))])
;             (add-one-constraint-and-propagate alpha1 hi)))
;           los-other)))
;    ;; list-cdr
;    (when (and (Cdr? lo)
;               (Set-var? hi))
;      (let* ([los-other (lookup-hi-and-filter Listexp? (Cdr-set-var lo))])
;        (for-each
;         (lambda (lo-other)
;           (let ([new-lo (if (> (length (Listexp-set-vars lo-other)) 1)
;                             (make-Listexp (cdr (Listexp-set-vars lo-other)))
;                             *Nullexp*)])
;             (add-one-constraint-and-propagate new-lo hi)))
;         los-other)))
    ))

(load-relative "type-reconstruct.ss")
