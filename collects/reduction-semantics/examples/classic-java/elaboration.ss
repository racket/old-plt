;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; elaboration.ss
;;
;; Richard Cobbe
;; $Id: elaboration.ss,v 1.3 2005/01/03 12:44:03 cobbe Exp $
;;
;; Code to type-check and elaborate the program.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module elaboration mzscheme

  (require (lib "contract.ss")
           (lib "match.ss")
           (lib "helper.ss" "reduction-semantics")

           "utils.ss"
           "environment.ss"
           "program.ss"
           "ast.ss")

  (provide/contract (elab-program (-> program? program?))
                    (struct (exn:cj:elab exn:fail:contract)
                            ([message string?]
                             [continuation-marks continuation-mark-set?]
                             [obj any/c])))

  (with-public-inspector
   (define-struct (exn:cj:elab exn:fail:contract) (obj)))

  ;; elab-program :: Program -> Program
  ;; ClassesOnce, CompleteClasses, WellFoundedClasses ensured during parsing.
  (define elab-program
    (lambda (p)
      (methods-once p)
      (fields-once p)
      (methods-ok p)
      (let ([new-table (make-hash-table)])
        (hash-table-for-each
         (program-classes p)
         (lambda (n c) (elab-class p new-table c)))
        (let-values ([(new-main type)
                      (elab-expr p (make-empty-env) (program-main p))])
          (make-program new-table new-main)))))

  ;; ensure-unique :: (Class -> (Listof ID)) String -> Program -> ()
  ;; applies selector to each class in p in turn and ensures that the list of
  ;; names for each class contains no duplicates.  Throws an elaboration
  ;; exception upon duplicate detection.
  (define ensure-unique
    (lambda (selector err-msg)
      (lambda (p)
        (hash-table-for-each
         (program-classes p)
         (lambda (_ c)
           (unless (unique-names? (selector c))
             (raise (make-exn:cj:elab err-msg
                                      (current-continuation-marks)
                                      c))))))))

  ;; methods-once :: Program -> ()
  ;; ensures that no class defines two methods with the same name.
  (define methods-once
    (ensure-unique (lambda (c) (map method-name (class-methods c)))
                   "duplicate method definition"))

  ;; fields-once :: Program -> ()
  ;; ensures that no class defines two fields with the same name.
  (define fields-once
    (ensure-unique (lambda (c) (map field-name (class-fields c)))
                   "duplicate field definition"))

  ;; methods-ok :: Program -> ()
  ;; ensures that all method declarations are OK (i.e., overriding methods
  ;; preserves type).
  (define methods-ok
    (lambda (p)
      (hash-table-for-each
       (program-classes p)
       (lambda (name class)
         (when (class-superclass class)
           (class-methods-ok class))))))

  ;; class-methods-ok :: Class[!Object] -> ()
  ;; Ensures that all method declarations within class are OK
  ;; NOT DIRECTLY COVERED BY TEST SUITE
  (define class-methods-ok
    (lambda (class)
      (let ([superclass (class-superclass class)])
        (for-each
         (lambda (md)
           (let ([md2 (find-method superclass (method-name md))])
             (unless (or (not md2)
                         (and (equal? (method-type md) (method-type md2))
                              (equal? (method-arg-types md)
                                      (method-arg-types md2))))
               (raise (make-exn:cj:elab
                       "method override doesn't preserve type"
                       (current-continuation-marks)
                       class)))))
         (class-methods class)))))

  ;; elab-class :: Program (Hash-Table Class-Name Class) Class -> Class
  ;; type-checks and elaborates a class definition; stores elaborated
  ;; class definition in new-table.
  (define elab-class
    (lambda (p new-table c)
      (cond
       [(hash-table-get new-table (class-type-name (class-name c))
                        (lambda () #f)) =>
        (lambda (x) x)]
       [else
        (let ([new-superclass
               (if (class-superclass c)
                   (elab-class p new-table (class-superclass c))
                   #f)])

          ;; P |-t t_i, for all field types t_i
          (unless
              (andmap (type-exists? p) (map field-type (class-fields c)))
            (raise (make-exn:cj:elab "bad field type"
                                     (current-continuation-marks)
                                     c)))

          (let ([result (make-class (class-name c)
                                    new-superclass
                                    (class-fields c)
                                    (map (elab-method p c)
                                         (class-methods c)))])
            (hash-table-put! new-table (class-type-name (class-name c)) result)
            result))])))
  ;; FIXME: add test cases to ensure that elab-class preserves graph structure
  ;; of original class table!

  ;; elab-method :: Program Class -> Method -> Method
  ;; type-checks and elaborates a method definition
  (define elab-method
    (lambda (p c)
      (lambda (m)
        (unless ((type-exists? p) (method-type m))
          (raise (make-exn:cj:elab "method return type doesn't exist"
                                   (current-continuation-marks)
                                   (list c m))))
        (unless (andmap (type-exists? p) (method-arg-types m))
          (raise (make-exn:cj:elab "method arg type doesn't exist"
                                   (current-continuation-marks)
                                   (list c m))))
        (let-values ([(new-body type)
                      (elab-expr p
                                 (extend-env (make-empty-env)
                                             (cons 'this (method-arg-names m))
                                             (cons (class-name c)
                                                   (method-arg-types m)))
                                 (method-body m))])
          (unless (type<=? p type (method-type m))
            (raise (make-exn:cj:elab "method return type incompatible w/ body"
                                     (current-continuation-marks)
                                     (list c m))))
          (make-method (method-type m)
                       (method-name m)
                       (method-arg-names m)
                       (method-arg-types m)
                       new-body)))))

  ;; elab-expr :: Program (Env Type) Expr -> Expr Type
  ;; type-checks and elaborates an expression.
  (define elab-expr
    (lambda (p tenv e)
      (match e
        [($ new type) (elab-ctor p tenv type)]
        [($ var-ref var)
         (values e
                 (lookup tenv var
                         (lambda ()
                           (raise (make-exn:cj:elab
                                   "unbound identifier"
                                   (current-continuation-marks)
                                   var)))))]
        [($ nil) (values e (make-any-type))]
        [($ ref obj field) (elab-ref p tenv obj field)]
        [($ set obj field rhs) (elab-set p tenv obj field rhs)]
        [($ send obj md args) (elab-send p tenv obj md args)]
        [($ super md args) (elab-super p tenv md args)]
        [($ cast t obj) (elab-cast p tenv t obj)]
        [($ cj-let id rhs body) (elab-let p tenv id rhs body)]
        [($ num-lit val) (values e (make-ground-type 'int))]
        [($ bool-lit val) (values e (make-ground-type 'bool))]
        [($ unary-prim rator rand) (elab-unary-prim p tenv rator rand)]
        [($ binary-prim rator rand1 rand2)
         (elab-binary-prim p tenv rator rand1 rand2)]
        [($ if-expr test then else) (elab-if p tenv test then else)])))

  ;; elab-ctor :: Program (Env Type) Type[Class] -> Expr Type[Class]
  (define elab-ctor
    (lambda (p tenv type)
      (unless ((type-exists? p) type)
        (raise (make-exn:cj:elab "constructor for nonexistent type"
                                 (current-continuation-marks)
                                 type)))
      (values (make-new type) type)))

  ;; elab-ref :: Program (Env Type) Expr Field-Name -> Expr[Tagged-Ref] Type
  (define elab-ref
    (lambda (p tenv obj fd)
      (let-values ([(new-obj type) (elab-expr p tenv obj)])
        (unless (class-type? type)
          (raise (make-exn:cj:elab
                  "ref: subexpr not of object type (possibly null)"
                  (current-continuation-marks)
                  (make-ref obj fd))))
        (let* ([obj-class (find-class p type)]
               [field (find-field obj-class fd)])
          (if field
              (values (make-tagged-ref new-obj (field-class field) fd)
                      (field-type field))
              (raise (make-exn:cj:elab "ref: field doesn't exist"
                                       (current-continuation-marks)
                                       (make-ref obj fd))))))))

  ;; elab-set :: Program (Env Type) Expr Field-Name Expr
  ;;          -> Expr[Tagged-Set] Type
  (define elab-set
    (lambda (p tenv obj fd rhs)
      (let-values ([(new-obj obj-type) (elab-expr p tenv obj)]
                   [(new-rhs rhs-type) (elab-expr p tenv rhs)])
        (unless (class-type? obj-type)
          (raise (make-exn:cj:elab
                  "set: first subexpr not of object type (maybe null)"
                  (current-continuation-marks)
                  (make-ref obj fd))))
        (let* ([obj-class (find-class p obj-type)]
               [field (find-field obj-class fd)])
          (cond
           [(and field (type<=? p rhs-type (field-type field)))
            (make-tagged-set new-obj (field-class field)
                             fd new-rhs)]
           [(not field) (raise (make-exn:cj:elab
                                "set: field doesn't exist"
                                (current-continuation-marks)
                                (make-set obj fd rhs)))]
           [else (raise (make-exn:cj:elab
                         "set: rhs not of good type"
                         (current-continuation-marks)
                         (make-set obj fd rhs)))])))))

  ;; elab-send :: Program (Env Type) Expr Method-Name (Listof Expr)
  ;;           -> Expr[Send] Type
  (define elab-send
    (lambda (p tenv obj md args)
      (let-values ([(new-obj obj-type) (elab-expr p tenv obj)])
        (unless (class-type? obj-type)
          (raise (make-exn:cj:elab
                  "send: subexpr not of object type (possibly null)"
                  (current-continuation-marks)
                  (make-send obj md args))))
        (let* ([obj-class (find-class p obj-type)]
               [method (find-method obj-class md)])
          (if method
              (values (make-send new-obj md
                                 (elab-args p tenv (make-send obj md args)
                                            (method-arg-types method)
                                            args))
                      (method-type method))
              (raise (make-exn:cj:elab "send: method doesn't exist"
                                       (current-continuation-marks)
                                       (make-send obj md args))))))))

  ;; elab-args :: Program (Env Type) Expr (Listof Type) (Listof Expr)
  ;;           -> (Listof Expr)
  ;; elaborates a list of expressions as part of an argument list
  ;; (i.e., subtyping is allowed)
  ;; NOT DIRECTLY COVERED BY TEST SUITE
  (define elab-args
    (lambda (p tenv expr arg-types args)
      (let loop ([arg-types arg-types] [args args])
        (cond
         [(and (null? arg-types) (null? args)) null]
         [(or (null? arg-types) (null? args))
          (raise (make-exn:cj:elab "elab-args: arity mismatch"
                                   (current-continuation-marks)
                                   expr))]
         [else
          (let-values ([(new-arg type) (elab-expr p tenv (car args))])
            (if (type<=? p type (car arg-types))
                (cons new-arg (loop (cdr arg-types) (cdr args)))
                (raise (make-exn:cj:elab "elab-args: arg type mismatch"
                                         (current-continuation-marks)
                                         expr))))]))))

  ;; elab-super :: Program (Env Type) Method-Name (Listof Expr) -> Expr Type
  (define elab-super
    (lambda (p tenv md args)
      (let* ([static-type (lookup tenv 'this (lambda () (error 'elab-super)))]
             [static-class (find-class p static-type)]
             [superclass (class-superclass static-class)]
             ;; superclass can only be null if static-type is Object, which
             ;; can't happen -- static-type is the name of the class in
             ;; which the currently-executing method is defined.
             [method (find-method superclass md)])
        (if method
            (values
             (make-tagged-super (class-name superclass)
                                md
                                (elab-args p tenv (make-super md args)
                                           (method-arg-types method)
                                           args))
             (method-type method))
            (raise (make-exn:cj:elab "super method doesn't exist"
                                     (current-continuation-marks)
                                     (make-super md args)))))))

  ;; elab-cast :: Program (Env Type) Type[Class] Expr -> Expr Type[Class]
  (define elab-cast
    (lambda (p tenv t obj)
      (let-values ([(new-obj obj-type) (elab-expr p tenv obj)])
        (cond
         [(type<=? p obj-type t)        ;; widening cast
          (values new-obj t)]
         [(type<=? p t obj-type) (values (make-cast t new-obj) t)]
         [else (raise (make-exn:cj:elab "cast between unrelated types"
                                        (current-continuation-marks)
                                        (make-cast t obj)))]))))

  ;; elab-let :: Program (Env Type) ID Expr Expr -> Expr Type
  (define elab-let
    (lambda (p tenv id lhs body)
      (let*-values ([(new-lhs lhs-type) (elab-expr p tenv lhs)]
                    [(new-body body-type)
                     (elab-expr p (extend-env tenv
                                              (list id)
                                              (list lhs-type))
                                body)])
        (values (make-cj-let id new-lhs new-body) body-type))))

  ;; elab-unary-prim :: Program (Env Type) Unary-Prim Expr -> Expr Type
  (define elab-unary-prim
    (lambda (p tenv rator rand)
      (let-values ([(new-rand rand-type) (elab-expr p tenv rand)]
                   [(arg-type result-type) (type-of-prim rator)])
        (if (type<=? p rand-type arg-type)
            (values (make-unary-prim rator new-rand) result-type)
            (raise (make-exn:cj:elab "unary primitive: bad arg type"
                                     (current-continuation-marks)
                                     (make-unary-prim rator rand)))))))

  ;; elab-binary-prim :: Program (Env Type) Binary-Prim Expr Expr
  ;;                  -> Expr Type
  (define elab-binary-prim
    (lambda (p tenv rator rand1 rand2)
      (let-values ([(new-rand1 rand1-type) (elab-expr p tenv rand1)]
                   [(new-rand2 rand2-type) (elab-expr p tenv rand2)]
                   [(arg1-type arg2-type result-type) (type-of-prim rator)])
        (if (and (type<=? p rand1-type arg1-type)
                 (type<=? p rand2-type arg2-type))
            (values (make-binary-prim rator new-rand1 new-rand2) result-type)
            (raise (make-exn:cj:elab "binary primitive: bad arg type"
                                     (current-continuation-marks)
                                     (make-binary-prim rator rand1 rand2)))))))

  ;; elab-if :: Program (Env Type) Expr Expr Expr -> Expr Type
  (define elab-if
    (lambda (p tenv e1 e2 e3)
      (let-values ([(new-e1 e1-type) (elab-expr p tenv e1)]
                   [(new-e2 e2-type) (elab-expr p tenv e2)]
                   [(new-e3 e3-type) (elab-expr p tenv e3)])
        (unless (type<=? p e1-type (make-ground-type 'bool))
          (raise (make-exn:cj:elab "if: conditional must have boolean type"
                                   (current-continuation-marks)
                                   (make-if-expr e1 e2 e3))))
        (cond
         [(type-lub p e2-type e3-type) =>
          (lambda (x) (values (make-if-expr new-e1 new-e2 new-e3) x))]
         [else (raise (make-exn:cj:elab "if: branches have unrelated types"
                                        (current-continuation-marks)
                                        (make-if-expr e1 e2 e3)))]))))

  ;; type-of-prim :: (Union (Unary-Prim -> Type Type)
  ;;                        (Binary-Prim -> Type Type Type))
  ;; returns the type of the supplied primitive operation.
  ;; NOT DIRECTLY COVERED BY TEST SUITE
  (define type-of-prim
    (let ([int (make-ground-type 'int)]
          [bool (make-ground-type 'bool)])
      (lambda (op)
        (case op
          [(null?) (values (make-class-type 'Object) bool)]
          [(zero?) (values int bool)]
          [(not) (values bool bool)]
          [(+ - *) (values int int int)]
          [(==) (values int int bool)]
          [(and or) (values bool bool bool)])))))
