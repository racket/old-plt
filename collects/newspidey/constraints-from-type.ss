;; constraint generation for primitives, based on their types
;; We have here both type variables and set variables...

(unit/sig newspidey:constraints-from-type^
  (import [setexp : newspidey:datadef-setexp^]
          [type : newspidey:datadef-types^]
          [cgp : newspidey:constraints-gen-and-prop^])

;; for a given type-var, lists the set-vars associated
;; with it that appear in contravariant and covariant
;; positions in the type being processed
(define-struct Type-Var-Set-Vars (neg pos))

;; (hash-table-of symbol Type)
;; associates a primitive name with its type
(define *prim-to-type* (make-hash-table))

;; symbol Type -> void
(define (add-prim-type sym type)
  (hash-table-put! *prim-to-type* sym type))

;; symbol -> Type
(define (lookup-prim-type sym)
  (hash-table-get *prim-to-type* sym (lambda () #f)))

;; (hash-table-of symbol Label)
;; associates a primitive with a label (unique for a given
;; primitive throughout the analysis)
(define *prim-to-label* (make-hash-table))

;; (hash-table-of Label symbol)
;; associates a label with a primitive
(define *label-to-prim* (make-hash-table))

;; symbol -> Label
(define (lookup-prim-label sym)
  (hash-table-get *prim-to-label* sym (lambda () #f)))

;; symbol Label -> void
(define (add-prim-label sym label)
  (hash-table-put! *prim-to-label* sym label)
  (hash-table-put! *label-to-prim* label sym))

;(let ([label (create-label)]) APPLY
;  (add-prim-label 'apply label)
;  (cgp:associate-label-with-ars label (setexp:make-Arity (setexp:make-Interval 2 2) '())))

;; (listof (list symbol Type)) -> void
;; initialize hash tables above + label-to-ars table
(define (init-prim prim-entries)
  (for-each
   (lambda (prim-entry)
     (let* ([prim-name (car prim-entry)]
            [prim-type (cadr prim-entry)]
            [label (cgp:create-label)]
            ;;[setvar (cgp:get-top-level-var prim-name)] ;; register primitive with top-level at the same time TOPLEVEL
            )
       (add-prim-type prim-name prim-type)
       (add-prim-label prim-name label)
;       (cgp:associate-label-and-set-var label (setexp:Set-var-name setvar)) TOPLEVEL
;       (cgp:add-constraint-with-bounds label setvar #t)
;       (add-constraints-from-type (lookup-prim-type prim-name) setvar #t (make-hash-table))
       (let ([arg-list (cond
                         [(and (type:Type-Scheme? prim-type) (type:Type-Arrow? (type:Type-Scheme-type prim-type)))
                          (type:Type-Arrow-doms (type:Type-Scheme-type prim-type))]
                         [(type:Type-Arrow? prim-type)
                          (type:Type-Arrow-doms prim-type)]
                         [else #f])])
         ;; XXX useless stuff, new labels are now created on the fly each time we see a primitive applied
         ;; get rid of associate-label-with-ars fron cgp sig, then.
         (when arg-list
           (let ([len (length arg-list)])
             (cgp:associate-label-with-ars
              (setexp:Label-name label)
              (setexp:make-Arity (setexp:make-Interval len len) '())))))))
   prim-entries))

;; (listof (list symbol Type))
;; type table for primitives XXX should use a type parser here...
(define prim-init-list
  (list
   (list 'cons
         (type:make-Type-Scheme
          (list 'x 'y)
          (type:make-Type-Arrow
           (list (type:make-Type-Var 'x) (type:make-Type-Var 'y)) 
           (type:make-Type-Cons
            (type:make-Type-Var 'x)
            (type:make-Type-Var 'y)))))
   (list 'car
         (type:make-Type-Scheme
          (list 'x 'y)
          (type:make-Type-Arrow
           (list (type:make-Type-Cons (type:make-Type-Var 'x) (type:make-Type-Var 'y)))
           (type:make-Type-Var 'x))))
   (list 'cdr
         (type:make-Type-Scheme
          (list 'x 'y)
          (type:make-Type-Arrow
           (list (type:make-Type-Cons (type:make-Type-Var 'x) (type:make-Type-Var 'y)))
           (type:make-Type-Var 'y))))))
  

;; (hash-table-of symbol Type-Var-Set-Vars) Type (listof symbol) boolean (hash-table-of Type-var Set-var) -> void
;; type-vars->set-vars: for each type variable that appears in a type, the table gives the list of
;;   set variables associated with each occurence of the type variable in the type, keeping separate
;;   contravariant and covariant positions.
;; type: the type being analized
;; vars: the list of type variables that appear in the "forall" of the type scheme (if the type is a scheme...)
;;   Note that nested "forall" are not allowed.
;; flag: tells whether we are currently in a covariant or contravariant position
;; type-var->set-var: associates with each occurence of a type variable one set variable which is going
;;   to represent the flow for this type variable. Note: even though a given type variable can appear several
;;   times in a type scheme, each occurence of the type variable will create a new entry in the hash-table
;;   because hash tables are using eq?, not equal?. That's what we want, because in add-constraints-from-type
;;   we will create a new constraint for each occurence (Type-Var? case) and link the different occurences
;;   together at the end (Type-Scheme? case) to simulate the flow from parameters to result inside the primitive.
;; So: create-set-vars-lists takes a type, creates set variables for all the occurences of all the type variables
;; in the type and returns (in type-vars->set-vars) all these set variables.
(define (create-set-vars-lists type-vars->set-vars type vars flag type-var->set-var)
  (cond
    [(type:Type-Var? type)
     (let ([name (type:Type-Var-name type)])
       (if (memq name vars)
           (let ([set-var (cgp:gen-set-var)]
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
           (error 'create-set-vars-lists "type var not specified in a scheme: ~a" name)))]
    [(type:Type-Cons? type)
     (create-set-vars-lists type-vars->set-vars (type:Type-Cons-car type) vars flag type-var->set-var)
     (create-set-vars-lists type-vars->set-vars (type:Type-Cons-cdr type) vars flag type-var->set-var)]
    [(type:Type-Arrow? type)
     (for-each (lambda (param)
                 (create-set-vars-lists type-vars->set-vars param vars (not flag) type-var->set-var))
               (type:Type-Arrow-doms type))
     (create-set-vars-lists type-vars->set-vars (type:Type-Arrow-rng type) vars flag type-var->set-var)]
    [else (error 'create-set-vars-lists "unkown type: ~a~n" type)]))


;; Type -> Arity
;; Gets the arity from a primitive type, or #f otherwise.
;; Was originaly directly computed inside add-constraints-from-type, but we need now an interface
;; to this function so it can be used in constraints-gen-and-prop.ss to associate the arity with
;; the label created on the fly when we see a primitive being used, so debug-arity won't flag the
;; use as an arity error.
(define (get-arity type)
  (cond
    [(type:Type-Arrow? type)
     (let* ([len (length (type:Type-Arrow-doms type))])
       (setexp:make-Arity (setexp:make-Interval len len) '()))]
    [(type:Type-Scheme? type)
     (get-arity (type:Type-Scheme-type type))]
    [else
     (error 'get-arity "type with no arity: ~a" type)]))


;; Type Set-var boolean (hash-table-of Type-var Set-var) -> boolean
;; generates all the constraints associated with a given type.
;; alpha is a set variable that represents the type as if it were a term
;; flag tells whether we are in a covariant or contravariant position
;; type-var->set-var is a lookup table associating each type variable occurence with a set variable
(define (add-constraints-from-type type alpha flag type-var->set-var)
  ;;(associate-set-var-and-term alpha 'dummy)
  ;;(printf "type set var: ~a~ntype: ~a~n" alpha type)
  (cond
    [(type:Type-Var? type)
     (cgp:add-constraint-with-bounds (hash-table-get type-var->set-var type) alpha flag)]
    [(type:Type-Cons? type)
     (let ([car-set-var (cgp:gen-set-var)]
           [cdr-set-var (cgp:gen-set-var)])
       (cgp:add-constraint-with-bounds cgp:*pair-token* alpha flag)
       (add-constraints-from-type (type:Type-Cons-car type) car-set-var flag type-var->set-var)
       (add-constraints-from-type (type:Type-Cons-cdr type) cdr-set-var flag type-var->set-var)
       (cgp:add-constraint-with-bounds car-set-var (setexp:make-Car alpha) flag)
       (cgp:add-constraint-with-bounds cdr-set-var (setexp:make-Cdr alpha) flag))]
    [(type:Type-Arrow? type)
     (let* ([dom-set-vars (map (lambda (_) (cgp:gen-set-var)) (type:Type-Arrow-doms type))]
            [rng-set-var (cgp:gen-set-var)]
            [len (length dom-set-vars)]
            [arity (setexp:make-Arity (setexp:make-Interval len len) '())])
       ;; constraints for the doms and rng
       (for-each (lambda (dom set-var)
                   (add-constraints-from-type dom set-var (not flag) type-var->set-var))
                 (type:Type-Arrow-doms type) dom-set-vars)
       (add-constraints-from-type (type:Type-Arrow-rng type) rng-set-var flag type-var->set-var)
       ;; constraints for the arrow itself (same as lambda definition)
       (let loop ([j 1]
                  [cur-dom-set-vars dom-set-vars])
         (when (<= j len)
           (cgp:add-constraint-with-bounds (setexp:make-Dom-arity arity j alpha)
                                           (car cur-dom-set-vars)
                                           flag)
           (loop (add1 j) (cdr cur-dom-set-vars))))
       (cgp:add-constraint-with-bounds rng-set-var (setexp:make-Rng-arity arity alpha) flag))]
    [(type:Type-Scheme? type)
     (let ([type-vars->set-vars (make-hash-table)]
           [scheme-type (type:Type-Scheme-type type)]
           [type-vars (type:Type-Scheme-vars type)])
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
                (cgp:add-constraint-with-bounds neg-set-var pos-set-var flag))
              (Type-Var-Set-Vars-pos type-var-set-vars)))
           (Type-Var-Set-Vars-neg type-var-set-vars))))
       (add-constraints-from-type scheme-type alpha flag type-var->set-var))]
    [else
     (error 'add-constraints-from-type "add-constraints-for-abstract-value: Expected type, got ~a"
            type)]))

  ) ;; unit/sig