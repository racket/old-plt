;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; elaboration.ss
;;
;; Richard Cobbe
;; $Id: elaboration.ss,v 1.47 2004/04/23 20:41:02 cobbe Exp $
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
                    (struct exn:aj:elab ()))

  (with-public-inspector
   (define-struct (exn:aj:elab exn:application) ()))

  ;; elab-program :: Program -> Program
  ;; ClassesOnce and CompleteClasses ensured during parsing.
  ;; We don't verify WellFoundedClasses: too much work, not very interesting.
  ;; (Besides, if this property doesn't hold, we'll diverge during parsing.)
  (define elab-program
    (lambda (p)
      (methods-once p)
      (fields-once p)
      (methods-ok p)
      (fields-ok p)
      (let ([new-table (make-hash-table)])
        (hash-table-for-each
         (program-classes p)
         (lambda (n c) (elab-class p new-table c)))
        (let-values ([(new-main type)
                      (elab-expr p (make-empty-env) null (program-main p))])
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
             (raise (make-exn:aj:elab err-msg
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
    (ensure-unique (lambda (c) (map field-name
                                    (append (class-fields c)
                                            (class-contained-fields c)
                                            (class-acquired-fields c))))
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
               (raise (make-exn:aj:elab
                       "method override doesn't preserve type"
                       (current-continuation-marks)
                       class)))))
         (class-methods class)))))

  ;; fields-ok :: Program -> ()
  ;; ensures that no field declarations are shadowed.
  (define fields-ok
    (lambda (p)
      (hash-table-for-each
       (program-classes p)
       (lambda (name class)
         (when (class-superclass class)
           (class-fields-ok class))))))

  ;; class-fields-ok :: Class[!Program] -> ()
  ;; ensures that class does not shadow any inherited fields.
  ;; NOT DIRECTLY COVERED BY TEST SUITE
  (define class-fields-ok
    (lambda (class)
      (let ([superclass (class-superclass class)])
        (for-each
         (lambda (fd)
           (when (find-field superclass (field-name fd))
             (raise (make-exn:aj:elab "shadowed field"
                                      (current-continuation-marks)
                                      class))))
         (append (class-fields class)
                 (class-contained-fields class)
                 (class-acquired-fields class))))))

  ;; elab-class :: Program (Hash-Table Class-Name Class) Class -> Class
  ;; type-checks and elaborates a class definition; stores elaborated
  ;; class definition in new-table.
  (define elab-class
    (lambda (p new-table c)
      (cond
       [(hash-table-get new-table (class-name c) (lambda () #f)) =>
        (lambda (x) x)]
       [else
        (let ([new-superclass
               (if (class-superclass c)
                   (elab-class p new-table (class-superclass c))
                   #f)]
              [contained-field-types
               (map field-type (class-contained-fields c))])

          ;; P |-t t_i, for all field types t_i
          (unless
              (andmap (type-exists? p)
                      (map field-type (append (class-fields c)
                                              (class-contained-fields c)
                                              (class-acquired-fields c))))
            (raise (make-exn:aj:elab "bad field type"
                                     (current-continuation-marks)
                                     c)))

          ;; t_i <= Object, for all contained field types t_i
          (unless (andmap class-type? contained-field-types)
            (raise (make-exn:aj:elab "contained field not object type"
                                     (current-continuation-marks)
                                     c)))

          ;; t_i sq< c for all contained field types t_i
          (unless (andmap
                   (lambda (ftype)
                     (can-be-contained-in? p (find-class p ftype) c))
                   contained-field-types)
            (raise (make-exn:aj:elab
                    "class not container for contained field"
                    (current-continuation-marks)
                    c)))

          (if (eq? 'any (class-containers c))
              (check-any-class p c)
              (check-contained-class p c))

          (let ([result (make-class (class-name c)
                                    new-superclass
                                    (class-containers c)
                                    (class-fields c)
                                    (class-contained-fields c)
                                    (class-acquired-fields c)
                                    (map (elab-method p c)
                                         (class-methods c)))])
            (hash-table-put! new-table (class-type-name (class-name c)) result)
            result))])))

  ;; check-any-class :: Program Class -> ()
  ;; performs those checks applicable only to classes with ANY container.
  (define check-any-class
    (lambda (p c)
      (unless (null? (acquired-fields c))
        (raise (make-exn:aj:elab "class with 'any container cannot acquire"
                                 (current-continuation-marks)
                                 c)))))

  ;; check-contained-class :: Program Class -> ()
  ;; performs those checks applicable only to those classes which explictly
  ;; specify their containers.
  (define check-contained-class
    (lambda (p c)
      (let ([superclass (class-superclass c)])
        (when superclass
          ;; superclass's containers can't be ANY
          (when (eq? (class-containers superclass) 'any)
            (raise (make-exn:aj:elab
                    "cannot specify containers for subclass of ANY"
                    (current-continuation-marks)
                    c)))
          ;; superclass's containers must be subset of ours
          (unless (subset? (class-containers superclass)
                           (class-containers c))
            (raise (make-exn:aj:elab
                    "class has fewer containers than superclass"
                    (current-continuation-marks)
                    c))))
        ;; container types must exist
        (unless (andmap (type-exists? p)
                        (class-containers c))
          (raise (make-exn:aj:elab
                  "nonexistent container type"
                  (current-continuation-marks)
                  c)))
        ;; if we have no containers, we can't acquire any fields
        (when (and (null? (class-containers c))
                   (not (null? (acquired-fields c))))
          (raise (make-exn:aj:elab
                  "class acquires fields with no containers"
                  (current-continuation-marks)
                  c)))
        ;; all of our containers can contain us
        (unless (andmap (lambda (ct) (can-contain? p ct c))
                        (class-containers c))
          (raise (make-exn:aj:elab
                  "class cannot be contained in container"
                  (current-continuation-marks)
                  c)))
        ;; our containers provide all of our acquired fields
        (unless (andmap (lambda (fd)
                          (andmap (lambda (container)
                                    (provides-field? p container fd))
                                  (class-containers c)))
                        (acquired-fields c))
          (raise (make-exn:aj:elab
                  "class acquires field from invalid context"
                  (current-continuation-marks)
                  c))))))

  ;; subset? :: (Listof X) (Listof X) -> Boolean
  ;; returns #t if all elements in l1 are present in l2.
  ;; Comparisons with equal?
  (define subset?
    (lambda (l1 l2)
      (if (null? l1)
          #t
          (and (member (car l1) l2)
               (subset? (cdr l1) l2)))))

  ;; elab-method :: Program Class -> Method -> Method
  ;; type-checks and elaborates a method definition
  (define elab-method
    (lambda (p c)
      (lambda (m)
        (unless ((type-exists? p) (method-type m))
          (raise (make-exn:aj:elab "method return type doesn't exist"
                                   (current-continuation-marks)
                                   (list c m))))
        (unless (andmap (type-exists? P)
                        (method-arg-types m))
          (raise (make-exn:aj:elab "method arg type doesn't exist"
                                   (current-continuation-marks)
                                   (list c m))))
        (let-values ([(new-body type)
                      (elab-expr p
                                  (extend-env (make-empty-env)
                                              (cons 'this (method-arg-names m))
                                              (cons (class-name c)
                                                    (method-arg-types m)))
                                  null
                                  (method-body m))])
          (unless (type<=? p type (method-type m))
            (raise (make-exn:aj:elab "method return type incompatible w/ body"
                                     (current-continuation-marks)
                                     (list c m))))
          (make-method (method-type m)
                       (method-name m)
                       (method-arg-names m)
                       (method-arg-types m)
                       new-body)))))

  ;; elab-expr :: Program (Env Type) (Listof Type[Class]) Expr -> Expr Type
  ;; type-checks and elaborates an expression.
  (define elab-expr
    (lambda (p tenv cs e)
      (match e
        [($ new type args) (elab-ctor p tenv cs type args)]
        [($ var-ref var)
         (values e
                 (lookup tenv var
                         (lambda ()
                           (raise (make-exn:aj:elab
                                   "unbound identifier"
                                   (current-continuation-marks)
                                   var)))))]
        [($ nil) (values e (make-any-type))]
        [($ ivar obj field) (elab-ivar p tenv obj field)]
        [($ send obj md args) (elab-send p tenv obj md args)]
        [($ super md args) (elab-super p tenv md args)]
        [($ cast t obj) (elab-cast p tenv cs t obj)]
        [($ aj-let id rhs body) (elab-let p tenv cs id rhs body)]
        [($ num-lit val) (values e (make-ground-type 'int))]
        [($ bool-lit val) (values e (make-ground-type 'bool))]
        [($ unary-prim rator rand) (elab-unary-prim p tenv rator rand)]
        [($ binary-prim rator rand1 rand2)
         (elab-binary-prim p tenv rator rand1 rand2)]
        [($ if-expr test then else)
         (elab-if p tenv cs test then else)])))

  ;; elab-ctor :: Program (Env Type) (Listof Type[Class]) Type[Class]
  ;;              (Listof Expr)
  ;;           -> Expr Type[Class]
  (define elab-ctor
    (lambda (p tenv containers type args)
      (unless ((type-exists? p) type)
        (raise (make-exn:aj:elab "constructor for nonexistent type"
                                 (current-continuation-marks)
                                 type)))
      (let ([c (find-class p type)])
        (if (null? containers)
            (unless (or (null? (class-containers c))
                        (eq? 'any (class-containers c)))
              (raise (make-exn:aj:elab "object must be constructed in context"
                                       (current-continuation-marks)
                                       (make-new type args))))
            (unless (and (list? (class-containers c))
                         (ormap (lambda (c)
                                  (type<=? p (car containers) c))
                                (class-containers c)))
              (raise (make-exn:aj:elab "object constructed in bad context"
                                       (current-continuation-marks)
                                       (make-new type args)))))
        (let ([fields (init-fields c)])
          (unless (= (length fields) (length args))
            (raise (make-exn:aj:elab "arity mismatch in ctor"
                                     (current-continuation-marks)
                                     (make-new type args))))
          (values
           (make-new
            type
            (map
             (lambda (field arg)
               (let ([containers (if (eq? 'contained (field-status field))
                                     (list (class-name c))
                                     null)])
                 (let-values ([(new-arg arg-type)
                               (elab-expr p tenv containers arg)])
                   (if (type<=? p arg-type (field-type field))
                       new-arg
                       (raise (make-exn:aj:elab "arg type mismatch in ctor"
                                                (current-continuation-marks)
                                                (make-new type args)))))))
             fields args))
           type)))))

  ;; elab-ivar :: Program (Env Type) Expr Field-Name -> Expr[Ivar] Type
  (define elab-ivar
    (lambda (p tenv obj fd)
      (let-values ([(new-obj type) (elab-expr p tenv null obj)])
        (unless (class-type? type)
          (raise (make-exn:aj:elab
                  "ivar: subexpr not of object type (possibly null)"
                  (current-continuation-marks)
                  (make-ivar obj fd))))
        (let* ([obj-class (find-class p type)]
               [field (find-field obj-class fd)])
          (if field
              (values (make-ivar new-obj fd) (field-type field))
              (raise (make-exn:aj:elab "ivar: field doesn't exist"
                                       (current-continuation-marks)
                                       (make-ivar obj fd))))))))

  ;; elab-send :: Program (Env Type) Expr Method-Name (Listof Expr)
  ;;           -> Expr[Send] Type
  (define elab-send
    (lambda (p tenv obj md args)
      (let-values ([(new-obj obj-type) (elab-expr p tenv null obj)])
        (unless (class-type? obj-type)
          (raise (make-exn:aj:elab
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
              (raise (make-exn:aj:elab "send: method doesn't exist"
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
          (raise (make-exn:aj:elab "elab-args: arity mismatch"
                                   (current-continuation-marks)
                                   expr))]
         [else
          (let-values ([(new-arg type) (elab-expr p tenv null (car args))])
            (if (type<=? p type (car arg-types))
                (cons new-arg (loop (cdr arg-types) (cdr args)))
                (raise (make-exn:aj:elab "elab-args: arg type mismatch"
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
            (raise (make-exn:aj:elab "super method doesn't exist"
                                     (current-continuation-marks)
                                     (make-super md args)))))))

  ;; elab-cast :: Program (Env Type) (listof Type[Class]) Type[Class] Expr
  ;;           -> Expr Type[Class]
  (define elab-cast
    (lambda (p tenv cs t obj)
      (let-values ([(new-obj obj-type) (elab-expr p tenv cs obj)])
        (cond
         [(type<=? p obj-type t)        ;; widening cast
          (values new-obj t)]
         [(type<=? p t obj-type) (values (make-cast t new-obj) t)]
         [else (raise (make-exn:aj:elab "cast between unrelated types"
                                        (current-continuation-marks)
                                        (make-cast t obj)))]))))

  ;; elab-let :: Program (Env Type) (Listof Type[Class])
  ;;             ID Expr Expr -> Expr Type
  (define elab-let
    (lambda (p tenv cs id lhs body)
      (let*-values ([(new-lhs lhs-type) (elab-expr p tenv null lhs)]
                    [(new-body body-type)
                     (elab-expr p (extend-env tenv
                                              (list id)
                                              (list lhs-type))
                                cs body)])
        (values (make-aj-let id new-lhs new-body) body-type))))

  ;; elab-unary-prim :: Program (Env Type) Unary-Prim Expr -> Expr Type
  (define elab-unary-prim
    (lambda (p tenv rator rand)
      (let-values ([(new-rand rand-type) (elab-expr p tenv null rand)]
                   [(arg-type result-type) (type-of-prim rator)])
        (if (type<=? p rand-type arg-type)
            (values (make-unary-prim rator new-rand) result-type)
            (raise (make-exn:aj:elab "unary primitive: bad arg type"
                                     (current-continuation-marks)
                                     (make-unary-prim rator rand)))))))

  ;; elab-binary-prim :: Program (Env Type) Binary-Prim Expr Expr
  ;;                  -> Expr Type
  (define elab-binary-prim
    (lambda (p tenv rator rand1 rand2)
      (let-values ([(new-rand1 rand1-type) (elab-expr p tenv null rand1)]
                   [(new-rand2 rand2-type) (elab-expr p tenv null rand2)]
                   [(arg1-type arg2-type result-type) (type-of-prim rator)])
        (if (and (type<=? p rand1-type arg1-type)
                 (type<=? p rand2-type arg2-type))
            (values (make-binary-prim rator new-rand1 new-rand2) result-type)
            (raise (make-exn:aj:elab "binary primitive: bad arg type"
                                     (current-continuation-marks)
                                     (make-binary-prim rator rand1 rand2)))))))

  ;; elab-if :: Program (Env Type) (Listof Type[Class]) Expr Expr Expr
  ;;         -> Expr Type
  (define elab-if
    (lambda (p tenv cs e1 e2 e3)
      (let-values ([(new-e1 e1-type) (elab-expr p tenv null e1)]
                   [(new-e2 e2-type) (elab-expr p tenv cs e2)]
                   [(new-e3 e3-type) (elab-expr p tenv cs e3)])
        (unless (type<=? p e1-type (make-ground-type 'bool))
          (raise (make-exn:aj:elab "if: conditional must have boolean type"
                                   (current-continuation-marks)
                                   (make-if-expr e1 e2 e3))))
        (cond
         [(type-lub p e2-type e3-type) =>
          (lambda (x) (values (make-if-expr new-e1 new-e2 new-e3) x))]
         [else (raise (make-exn:aj:elab "if: branches have unrelated types"
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
