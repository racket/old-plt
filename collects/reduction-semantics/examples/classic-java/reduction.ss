;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; reduction.ss
;; Richard Cobbe
;; $Id: reduction.ss,v 1.1 2004/07/27 22:41:35 cobbe Exp $
;;
;; Contains the definition of ClassicJava for PLT Redex
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module reduction mzscheme

  (require (lib "reduction-semantics.ss" "reduction-semantics")
           (lib "subst.ss" "reduction-semantics")
           (lib "list.ss")
           (lib "etc.ss")
           (lib "plt-match.ss")

           "utils.ss"
           "ast.ss"
           "program.ss"
           "store.ss")

  (with-public-inspector
   (define-struct ivar (class name value))
   ;; Ivar ::= (make-ivar Type[Class] Field-Name Value)

   (define-struct instance (class fields)))
  ;; Instance ::= (make-instance Type[Class] (Listof Ivar))

  ;; Value is as defined by the BNF below.
  ;; Reduction-Expr or RExpr is as defined by the BNF productions for the expr
  ;; nonterminal below.

  (set! make-instance
        (let ([orig-ctor make-instance])
          (lambda (c f)
            (unless (class-type? c)
              (error 'make-instance "expected class-type, got ~a" c))
            (unless (match f
                      [(list (? ivar? _) ...) #t]
                      [else #f])
              (error 'make-instance "expected field list, got ~a" f))
            (orig-ctor c f))))

  (define cj-lang
    (language [id (variable-except class new ref set send super this cast let
                                   if + - * == and or not null? zero? int bool
                                   null Object addr)]

              [class-name Object id]
              [binop + - * == and or]
              [unop not null? zero?]

              [value null
                     #t #f
                     number]            ; either a numeric literal or an addr

              [expr value
                    id
                    this
                    (new class-name)
                    (ref expr id id)    ; obj type field
                    (set expr id id expr) ;obj type field rhs
                    (send expr id expr ...)
                    (super expr class-name id expr ...)
                    (cast class-name expr)
                    (let id expr expr)
                    (binop expr expr)
                    (unop expr)
                    (if expr expr expr)]

              [store any]
              [program any]

              [context hole
                       (ref context id id)
                       (set context id id expr)
                       (set value id id context)
                       (send context id expr ...)
                       (send value id value ... context expr ...)
                       (super value class-name id value ... context expr ...)
                       (cast id context)
                       (let id context expr)
                       (binop context expr)
                       (binop value context)
                       (unop context)
                       (if context expr expr)]))

  (define cj-id? (language->predicate cj-lang 'id))

  ;; delta-1 :: Unary-Prim Value -> Value
  ;; implements unary primitives
  (define delta-1
    (lambda (rator rand)
      (case rator
        [(null?) (eq? rand 'null)]
        [(zero?) (= rand 0)]
        [(not) (not rand)])))

  ;; delta-2 :: Unary-Prim Value Value -> Value
  ;; implements binary primitives
  (define delta-2
    (lambda (op r1 r2)
      (case op
        [(+) (+ r1 r2)]
        [(-) (- r1 r2)]
        [(*) (* r1 r2)]
        [(==) (= r1 r2)]
        [(and) (and r1 r2)]
        [(or) (or r1 r2)])))

  ;; subst-args :: Tagged-Expr (Listof Value) (Listof ID) -> RExpr
  (define subst-args
    (lambda (expr vals ids)
      (foldl cj-subst (texpr->rexpr expr) ids vals)))

  ;; cj-subst :: ID RExp RExp -> RExp
  ;; substitutes 2nd arg for all free occurrences of 1st arg within 3rd arg
  (define cj-subst
    (subst
     ['null (constant)]
     [(? boolean?) (constant)]
     [(? number?) (constant)]
     [(? cj-id?) (variable)]
     ['this (variable)]
     [('new _) (constant)]
     [('ref obj type field)
      (all-vars '())
      (build (lambda (vars obj) `(ref ,obj ,type ,field)))
      (subterm '() obj)]
     [('set obj type field rhs)
      (all-vars '())
      (build (lambda (vars obj rhs) `(set ,obj ,type ,field ,rhs)))
      (subterm '() obj)
      (subterm '() rhs)]
     [('send obj md args ...)
      (all-vars '())
      (build (lambda (vars obj . args) `(send ,obj ,md ,@(args))))
      (subterm '() obj)
      (subterms '() args)]
     [('super obj type md args ...)
      (all-vars '())
      (build (lambda (vars obj . args) `(super ,obj ,type ,md ,@(args))))
      (subterm '() obj)
      (subterms '() args)]
     [('cast type obj)
      (all-vars '())
      (build (lambda (vars obj) `(cast ,type ,obj)))
      (subterm '() obj)]
     [('let id rhs body)
      (all-vars (list id))
      (build (lambda (vars rhs body) `(let ,@(vars) ,rhs ,body)))
      (subterm '() rhs)
      (subterm (list id) body)]
     [((? binary-prim-name? prim) rand1 rand2)
      (all-vars '())
      (build (lambda (vars rand1 rand2) `(,prim ,rand1 ,rand2)))
      (subterm '() rand1)
      (subterm '() rand2)]
     [((? unary-prim-name? prim) rand)
      (all-vars '())
      (build (lambda (vars rand) `(,prim ,rand)))
      (subterm '() rand)]
     [('if e1 e2 e3)
      (all-vars '())
      (build (lambda (vars e1 e2 e3) `(if ,e1 ,e2 ,e3)))
      (subterm '() e1)
      (subterm '() e2)
      (subterm '() e3)]))

  ;; texpr->rexpr :: Tagged-Expr -> Reduction-Expr
  ;; Converts a tagged expression AST into a reduction-expr that the reduction
  ;; rules can parse.
  (define texpr->rexpr
    (match-lambda
      [(struct new (type)) `(new ,(class-type-name type))]
      [(struct var-ref (var)) var]
      [(struct nil ()) 'null]
      [(struct tagged-ref (obj class field))
       `(ref ,(texpr->rexpr obj) ,(class-type-name class) ,field)]
      [(struct tagged-set (obj class field rhs))
       `(set ,(texpr->rexpr obj) ,(class-type-name class) ,field
             ,(texpr->rexpr rhs))]
      [(struct send (obj md args))
       `(send ,(texpr->rexpr obj) ,md ,@(map texpr->rexpr args))]
      [(struct tagged-super (c md args))
       `(super this ,(class-type-name c) ,md ,@(map texpr->rexpr args))]
      [(struct cast (c obj)) `(cast ,(class-type-name c) ,(texpr->rexpr obj))]
      [(struct cj-let (lhs rhs body))
       `(let ,lhs ,(texpr->rexpr rhs) ,(texpr->rexpr body))]
      [(struct num-lit (val)) val]
      [(struct bool-lit (val)) val]
      [(struct unary-prim (rator rand)) `(,rator ,(texpr->rexpr rand))]
      [(struct binary-prim (rator rand1 rand2))
       `(,rator ,(texpr->rexpr rand1) ,(texpr->rexpr rand2))]
      [(struct if-expr (test then else))
       `(if ,(texpr->rexpr test)
            ,(texpr->rexpr then)
            ,(texpr->rexpr else))]
      [bogus (error 'texpr->rexpr
                    "unexpected expression: ~a" bogus)]))

  (define cj-reductions
    (list

     ;; [new]
     [reduction cj-lang
                (program_ store_ (inhole context_ (new class-name_)))
                (let*-values ([(new-instance)
                               (create-instance (term program_)
                                                (term class-name_))]
                              [(new-store addr)
                               (store-alloc (new-instance))])
                  (term (program_ ,new-store
                                  ,(replace (term context_) (term hole)
                                            addr))))]

     ;; [get]
     [reduction
      cj-lang
      (side-condition (program_
                       store_
                       (inhole context_
                               (ref value_obj
                                    id_class
                                    id_field)))
                      (not (eq? (term value_obj) 'null)))
      (let ([instance (store-ref (term store_) (term value_obj))])
        (term (program_
               store_
               ,(replace (term context_) (term hole)
                         (get-field-val instance
                                        (term id_class)
                                        (term id_field))))))]

     ;; [set]
     [reduction
      cj-lang
      (side-condition
       (program_ store_ (inhole context_
                                (set value_obj
                                     id_class
                                     id_field
                                     value_rhs)))
       (not (eq? (term value_obj) 'null)))
      (let ([instance (store-ref (term store_) (term value_obj))])
        (term (program_
               ,(store-update (term store_) (term value_obj)
                              (update-field instance
                                            (term id_class)
                                            (term id_field)
                                            (term value_rhs)))
               ,(replace (term context_) (term hole)
                         (term value_rhs)))))]

     ;; [call]
     [reduction
      cj-lang
      (side-condition
       (program_ store_ (inhole context_
                                (send value_obj id_meth value_arg ...)))
       (not (eq? (term value_obj) 'null)))
      (let* ([inst (store-ref (term store_) (term value_obj))]
             [class-type (instance-class inst)]
             [class (find-class (term program_) class-type)]
             [method (find-method class (term id_meth))])
        (term (program_
               store_
               ,(replace (term context_) (term hole)
                         (subst-args (method-body method)
                                     (cons (term value_obj)
                                           (term (value_arg ...)))
                                     (cons 'this
                                           (method-arg-names method)))))))]

     ;; [super]
     (reduction
      cj-lang
      (program_ store_ (inhole context_
                               (super value_obj
                                      class-name_
                                      id_method
                                      value_arg ...)))
      (let* ([class (find-class (term program_)
                                (make-class-type (term class-name_)))]
             [method (find-method class (term id_method))])
      (term (program_
             store_
             ,(replace (term context_) (term hole)
                       (subst-args (method-body method)
                                   (cons (term value_obj)
                                         (term (value_arg ...)))
                                   (cons 'this
                                         (method-arg-names method))))))))

     ;; [cast]
     (reduction
      cj-lang
      (side-condition
       (program_ store_ (inhole context_ (cast class-name_ value_obj)))
       (and (not (eq? (term value_obj) 'null))
            (let ([instance (store-ref (term store_) (term value_obj))])
              (type<=? (term program_)
                       (instance-class instance)
                       (make-class-type (term class-name_))))))
      (term (program_
             store_
             ,(replace (term context_) (term hole) (term value_obj)))))

     ;; [let]
     (reduction
      cj-lang
      (program_ store_ (inhole context_ (let id_ value_rhs expr_body)))
      (term (program_
             store_
             ,(replace (term context_) (term hole)
                       (cj-subst (term id_) (term value_rhs)
                                 (term expr_body))))))

     ;; [xcast]
     (reduction
      cj-lang
      (side-condition
       (program_ store_ (inhole context_ (cast class-name_ value_obj)))
       (and (not (eq? (term value_obj) 'null))
            (let ([instance (store-ref (term store_) (term value_obj))])
              (not (type<=? (term program_)
                            (instance-class instance)
                            (make-class-type (term class-name_)))))))
      (term (program_
             store_
             "error: bad cast")))

     ;; [ncast]
     (reduction
      cj-lang
      (program_ store_ (inhole context_ (cast class-name_ null)))
      (term (program_
             store_
             (replace (term context_) (term hole) 'null))))

     ;; [nget]
     (reduction
      cj-lang
      (program_ store_ (inhole context_ (ref null id id)))
      (term (program_
             store_
             "error: dereferenced null")))

     ;; [nset]
     (reduction
      cj-lang
      (program_ store_ (inhole context_ (set null id id value)))
      (term (program_
             store_
             "error: dereferenced null")))

     ;; FIXME: need if, primop reductions
     ))



  ;; create-instance :: Program Class-Name -> Instance
  (define create-instance
    (lambda (p cn)
      (let* ([ctype (make-class-type cn)]
             [c (find-class p ctype)]
             [fields (find-all-fields c)])
        (make-instance ctype
                       (map
                        (lambda (fd)
                          (make-ivar (field-class fd)
                                     (field-name fd)
                                     (match (field-type fd)
                                       [(struct ground-type ('int)) 0]
                                       [(struct ground-type ('bool)) #f]
                                       [else 'null])))
                        fields)))))

  ;; get-field-val :: Instance Class-Name Field-Name -> Value
  (define get-field-val
    (lambda (inst class fd)
      (let ([c (make-class-type class)])
        (recur loop ([ivars (instance-fields inst)])
          (if (null? ivars)
              (error 'get-field-val "cdn't find field ~a.~a" class fd)
              (let ([ivar (car ivars)])
                (if (and (eq? (ivar-name ivar) fd)
                         (type=? (ivar-class ivar) c))
                    (ivar-value ivar)
                    (loop (cdr ivars)))))))))

  ;; update-field :: Instance Class-Name Field-Name Value -> Instance
  (define update-field
    (lambda (inst class fd rhs)
      (let ([c (make-class-type class)])
        (make-instance
         (instance-class inst)
         (recur loop ([ivars (instance-fields inst)])
           (if (null? ivars)
               (error 'update-field "cdn't find field ~a.~a" class fd)
               (let ([ivar (car ivars)])
                 (if (and (eq? (ivar-name ivar) fd)
                          (type=? (ivar-class ivar) c))
                     (cons (make-ivar (ivar-class ivar)
                                      (ivar-name ivar)
                                      rhs)
                           (cdr ivars))
                     (cons (car ivars)
                           (loop (cdr ivars)))))))))))

  )
