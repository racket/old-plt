;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; reduction.ss
;; Richard Cobbe
;; $Id: reduction.ss,v 1.31 2004/05/24 21:51:40 cobbe Exp $
;;
;; Contains the definition of Acquired Java for Robby's reduction semantics
;; engine.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module reduction mzscheme

  (require (lib "reduction-semantics.ss" "reduction-semantics")
           (lib "subst.ss" "reduction-semantics")
           (lib "list.ss")
           (lib "plt-match.ss")

           "utils.ss"
           "ast.ss"
           "program.ss"
           "store.ss")

  (with-public-inspector
   (define-struct instance (class parent fields)))
  ;; Instance ::= (make-instance Type[Class] Address (Alist Field-Name Value))
  ;; (Alist X Y) ::= (listof (list X Y))
  ;; Address ::= (Union (list 'addr Number) 'null)

  (set! make-instance
        (let ([orig-ctor make-instance])
          (lambda (c p f)
            (unless (class-type? c)
              (error 'make-instance "expected class-type, got ~a" c))
            (unless (match p
                      [(list 'addr (? number? _)) #t]
                      ['null #t]
                      [else #f])
              (error 'make-instance "expected address, got ~a" p))
            (unless (match f
                      [(list (list (? field-name? _) _) ...) #t]
                      [else #f])
              (error 'make-instance "expected field alist, got ~a" f))
            (orig-ctor c p f))))

  (define box-addr (lambda (addr)
                     (unless (and (integer? addr) (>= addr 0))
                       (error 'box-addr "expected positive integer, got ~a"
                              addr))
                     (list 'addr addr)))

  (define unbox-addr
    (match-lambda
      [(list 'addr (? number? n)) n]
      [bogus (error 'unbox-addr "expected address, got ~a" bogus)]))

  (define aj-syntax
    (language [id (variable-except class contain acquire any new ivar send
                                   super this cast let if + - * == and or not
                                   null? zero? int bool null true false
                                   Object addr)]
              [class-name Object id]
              [binop + - * == and or]
              [unop not null? zero?]

              [address (addr number)]
              ;; have to have unique representation for store addresses,
              ;; otherwise we get ambiguous parses for values.  <sigh>
              ;; probably don't *really* need to mark ADDR as a keyword, but
              ;; I'm not taking any more chances with ambiguous parses.

              [value null
                     true
                     false
                     number
                     address]

              [expr value
                    id
                    this
                    (new class-name expr ...)
                    (ivar expr id)
                    (send expr id expr ...)
                    (super expr class-name id expr ...)
                    (cast class-name expr)
                    (let id expr expr)
                    (binop expr expr)
                    (unop expr)
                    (if expr expr expr)]

              [store any]
              [program any]

              [configuration (program expr store)]

              [context hole
                       (new class-name value ... context expr ...)
                       (ivar context id)
                       (send context id expr ...)
                       (send value id value ... context expr ...)
                       (super value class-name id value ... context expr ...)
                       (cast id context)
                       (let id context expr)
                       (binop context expr)
                       (binop value context)
                       (unop context)
                       (if context expr expr)]))

  (define address? (language->predicate aj-syntax 'address))

  ;; purely functional reduction within a context.  Here, `purely functional'
  ;; means that the reduction neither allocates to nor reads from the store.
  (define-syntax f-reduction/context
    (syntax-rules ()
      [(_ redex-pat body)
       (reduction aj-syntax
                  ((name program program)
                   (in-hole (name ctxt context) redex-pat)
                   (name store store))
                  (term (program
                         ,(replace (term ctxt)
                                   (term hole)
                                   body)
                         store)))]))

  ;; purely functional reduction, as above, but allows access to the context.
  #;(define-syntax f-reduction
    (lambda (stx)
      (syntax-case stx ()
        [(_ ctxt redex-pat body)
         (identifier? #'ctxt)
         #'(reduction aj-syntax
                      ((name program program)
                       (in-hole (name ctxt context) redex-pat)
                       (name store store))
                      (term (program body store)))])))

  (define aj-reductions
    (list
     ;; [new]
     ;; [err-new]
     (reduction
      aj-syntax
      ((name program program)
       (in-hole (name ctxt context)
                (new (name c class-name)
                     (name arg value) ...))
       (name store store))
      (let ([fields (init-fields (find-class (term program)
                                             (make-class-type (term c))))])
        (if (ormap (lambda (field val)
                     (and (address? val)
                          (let ([obj (store-ref (term store)
                                                (unbox-addr val))])
                            (and (eq? (field-status field) 'contained)
                                 (not (eq? (instance-parent obj) 'null))))))
                   fields
                   (term (arg ...)))
            (term (program
                   "error: container violation"
                   store))
            (let*-values ([(new-obj)
                           (make-instance (make-class-type (term c))
                                          'null
                                          (map (lambda (fd val)
                                                 (list (field-name fd) val))
                                               fields
                                               (term (arg ...))))]
                          [(new-addr store-0)
                           (store-alloc (term store) new-obj)]
                          [(new-store)
                           (foldl (update-parent (box-addr new-addr)) store-0
                                  fields
                                  (term (arg ...)))])
              (term (program
                     ,(replace (term ctxt) (term hole) (box-addr new-addr))
                     ,new-store))))))

     ;; [ivar]
     ;; [ivar-acq]
     ;; [err-ivar-acq]
     ;; err-ivar-acq deliberately *not* tested: hard to create an object with a
     ;; null parent pointer with current system.  Would have to weaken type
     ;; rules for ctor calls, but we'll have to do that anyway when we add
     ;; side-effects.  So, this will become much easier to test before it
     ;; becomes important.
     (reduction
      aj-syntax
      ((name program program)
       (in-hole (name ctxt context)
                (ivar (name addr address) (name fd id)))
       (name store store))
      (let* ([obj (store-ref (term store) (unbox-addr (term addr)))]
             [class (find-class (term program) (instance-class obj))]
             [field (find-field class (term fd))]
             [field-val (if (eq? (field-status field) 'acquired)
                            (get-acq-field (field-name field) (term store)
                                           obj)
                            (cadr (assq (term fd) (instance-fields obj))))])
        (if field-val
            (term (program
                   ,(replace (term ctxt) (term hole) field-val)
                   store))
            (term (program "error: incomplete context" store)))))

     ;; [ivar-null]
     (reduction aj-syntax
                ((name program program)
                 (in-hole (name ctxt context) (ivar null id))
                 (name store store))
                (term (program "error: dereferenced null" store)))

     ;; [send]
     (reduction
      aj-syntax
      ((name program program)
       (in-hole (name ctxt context)
                (send (name addr address)
                      (name md id)
                      (name arg value) ...))
       (name store store))
      (let* ([obj (store-ref (term store) (unbox-addr (term addr)))]
             [class
               (find-class (term program) (instance-class obj))]
             [method (find-method class (term md))])
        (term (program
               ,(replace (term ctxt) (term hole)
                         (subst-args (method-body method)
                                     (term (addr arg ...))
                                     (cons 'this
                                           (method-arg-names method))))
               store))))

     ;; [send-null]
     (reduction aj-syntax
                ((name program program)
                 (in-hole (name ctxt context) (send null id value ...))
                 (name store store))
                (term (program "error: dereferenced null" store)))

     ;; [super]
     (reduction
      aj-syntax
      ((name program program)
       (in-hole (name ctxt context)
                (super (name addr address)
                       (name c id)
                       (name m id)
                       (name arg value) ...))
       (name store store))
      (let* ([obj (store-ref (term store) (unbox-addr (term addr)))]
             [class (find-class (term program) (make-class-type (term c)))]
             [method (find-method class (term m))])
        (term (program
               ,(replace (term ctxt) (term hole)
                         (subst-args (method-body method)
                                     (term (addr arg ...))
                                     (cons 'this (method-arg-names method))))
               store))))

     ;; [cast], [err-cast]
     (reduction aj-syntax
                ((name program program)
                 (in-hole (name ctxt context)
                          (cast (name c class-name) (name addr address)))
                 (name store store))
                (list (term program)
                      (if (type<=? (term program)
                                   (instance-class
                                    (store-ref (term store)
                                               (unbox-addr (term addr))))
                                   (make-class-type (term c)))
                          (replace (term ctxt) (term hole) (term addr))
                          "error: bad cast")
                      (term store)))

     ;; [null-cast]
     (f-reduction/context (cast class-name null) 'null)

     ;; [let]
     (f-reduction/context (let id_1 value_1 expr_1)
                          (aj-subst (term id_1)
                                    (term value_1)
                                    (term expr_1)))

     ;; [unary-op]
     (f-reduction/context (unop_1 value_1)
                          (delta-1 (term unop_1) (term value_1)))

     ;; [binary-op]
     (f-reduction/context (binop_1 value_1 value_2)
                          (delta-2 (term binop_1) (term value_1)
                                   (term value_2)))

     ;; [if-true]
     (f-reduction/context (if true expr_1 expr_2)
                          (term expr_1))

     ;; [if-false]
     (f-reduction/context (if false expr_1 expr_2)
                          (term expr_2))))

  (define delta-1
    (lambda (rator rand)
      (case rator
        [(null?) (bool->r (eq? rand 'null))]
        [(zero?) (bool->r (= rand 0))]
        [(not) (bool->r (not (r->bool rand)))])))

  (define delta-2
    (lambda (op r1 r2)
      (case op
        [(+) (+ r1 r2)]
        [(-) (- r1 r2)]
        [(*) (* r1 r2)]
        [(==) (bool->r (= r1 r2))]
        [(and) (bool->r (and (r->bool r1) (r->bool r2)))]
        [(or) (bool->r (or (r->bool r1) (r->bool r2)))])))

  (define bool->r (lambda (b) (if b 'true 'false)))
  (define r->bool (lambda (r) (eq? r 'true)))

  ;; subst-args :: Tagged-Expr (Listof Value) (Listof ID) -> RExpr
  (define subst-args
    (lambda (expr vals ids)
      (foldl aj-subst (texpr->rexpr expr) ids vals)))

  ;; aj-subst :: (Union ID 'this) Reduction-Expr Reduction-Expr
  ;;          -> Reduction-Expr
  ;; substitutes arg2 for arg1 in arg3.
  (define aj-subst
    (subst
     ['null (constant)]
     ['true (constant)]
     ['false (constant)]
     [(? number?) (constant)]
     [('addr (? number?)) (constant)]
     [(? id?) (variable)]
     ['this (variable)]
     [('new class arg ...)
      (all-vars '())
      (build (lambda (vars . new-args) `(new ,class ,@new-args)))
      (subterms '() arg)]
     [('ivar expr fd)
      (all-vars '())
      (build (lambda (vars new-expr) `(ivar ,new-expr ,fd)))
      (subterm '() expr)]
     [('send obj md arg ...)
      (all-vars '())
      (build (lambda (vars new-obj . new-args)
               `(send ,new-obj ,md ,@new-args)))
      (subterm '() obj)
      (subterms '() arg)]
     [('super obj c md arg ...)
      (all-vars '())
      (build (lambda (vars new-obj . new-args)
               `(super ,new-obj ,c ,md ,@new-args)))
      (subterm '() obj)
      (subterms '() arg)]
     [('cast c expr)
      (all-vars '())
      (build (lambda (vars new-expr) `(cast ,c ,new-expr)))
      (subterm '() expr)]
     [('let id expr1 expr2)
      (all-vars (list id))
      (build (lambda (vars new-expr1 new-expr2)
               `(let ,@vars ,new-expr1 ,new-expr2)))
      (subterm '() expr1)
      (subterm (list id) expr2)]
     [((? binary-prim-name? prim) arg1 arg2)
      (all-vars '())
      (build (lambda (vars new-arg1 new-arg2) `(,prim ,new-arg1 ,new-arg2)))
      (subterm '() arg1)
      (subterm '() arg2)]
     [((? unary-prim-name? prim) arg)
      (all-vars '())
      (build (lambda (vars new-arg) `(,prim ,new-arg)))
      (subterm '() arg)]
     [('if e1 e2 e3)
      (all-vars '())
      (build (lambda (vars new-e1 new-e2 new-e3)
               `(if ,new-e1 ,new-e2 ,new-e3)))
      (subterm '() e1)
      (subterm '() e2)
      (subterm '() e3)]))

  ;; texpr->rexpr :: Tagged-Expr -> Reduction-Expr
  ;; Converts a tagged expression AST into a reduction-expr that the reduction
  ;; rules can parse.
  (define texpr->rexpr
    (match-lambda
      [(struct new (type args)) `(new ,(class-type-name type)
                                      ,@(map texpr->rexpr args))]
      [(struct var-ref (var)) var]
      [(struct nil ()) 'null]
      [(struct ivar (obj field)) `(ivar ,(texpr->rexpr obj) ,field)]
      [(struct send (obj md args))
       `(send ,(texpr->rexpr obj) ,md ,@(map texpr->rexpr args))]
      [(struct tagged-super (c md args))
       `(super this ,(class-type-name c) ,md ,@(map texpr->rexpr args))]
      [(struct cast (c obj)) `(cast ,(class-type-name c) ,(texpr->rexpr obj))]
      [(struct aj-let (lhs rhs body))
       `(let ,lhs ,(texpr->rexpr rhs) ,(texpr->rexpr body))]
      [(struct num-lit (val)) val]
      [(struct bool-lit (val)) (bool->r val)]
      [(struct unary-prim (rator rand)) `(,rator ,(texpr->rexpr rand))]
      [(struct binary-prim (rator rand1 rand2))
       `(,rator ,(texpr->rexpr rand1) ,(texpr->rexpr rand2))]
      [(struct if-expr (test then else))
       `(if ,(texpr->rexpr test)
            ,(texpr->rexpr then)
            ,(texpr->rexpr else))]))

  ;; get-acq-field :: Field-Name (Store Value) Instance -> (Union Value #f)
  (define get-acq-field
    (lambda (fd store obj)
      (if (eq? (instance-parent obj) 'null)
          #f
          (let* ([parent (store-ref store (unbox-addr (instance-parent obj)))]
                 [field-val (assq fd (instance-fields parent))])
            (if field-val
                (cadr field-val)
                (get-acq-field fd store parent))))))

  ;; update-parent :: Address -> Field Value[address] Store -> Store
  (define update-parent
    (lambda (parent-addr)
      (lambda (field val store)
        (if (and (eq? 'contained (field-status field))
                 (not (eq? val 'null)))
            (let ([old-obj (store-ref store (unbox-addr val))])
              (store-update store (unbox-addr val)
                            (make-instance (instance-class old-obj)
                                           parent-addr
                                           (instance-fields old-obj))))
            store))))
  )
