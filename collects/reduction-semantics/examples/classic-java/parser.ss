;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; parser.ss
;; Richard Cobbe
;; $Id: parser.ss,v 1.37 2004/05/21 23:01:06 cobbe Exp $
;;
;; Implements the parser for the S-Expression based source syntax for
;; Acquired Java.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module parser mzscheme

  (require (lib "list.ss" "lib")
           (lib "etc.ss")
           (lib "match.ss")
           (lib "contract.ss")
           "utils.ss"
           "ast.ss")

  (provide/contract (parse-program (-> sexp? program?))
                    (struct exn:aj:parse ()))

  (provide expand-parse-exn)

  (with-public-inspector
   (define-struct (exn:aj:parse exn:application) ())
   (define-struct temp-class
                  (name superclass containers fields contained-fields
                        acquired-fields methods)))

  #;(set! make-temp-class
        (let ([old-ctor make-temp-class])
          (lambda (n s c f cf af m)
            (unless (class-name? n)
              (error 'make-temp-class "expected class-name, got ~a" n))
            (unless (or (class-name? s) (not s))
              (error 'make-temp-class
                     "expected class-name or false, got ~a" s))
            (unless (or (eq? c 'any)
                        (and (list? c)
                             (andmap class-type? c)))
              (error 'make-temp-class
                     "expected 'any or list of class-types, got ~a" c))
            (unless (and (list? f) (andmap field? f))
              (error 'make-temp-class "expected list of field, got ~a" f))
            (unless (and (list? cf) (andmap field? cf))
              (error 'make-temp-class "expected list of field, got ~a" cf))
            (unless (and (list? af) (andmap field? af))
              (error 'make-temp-class "expected list of field, got ~a" af))
            (unless (and (list? m) (andmap method? m))
              (error 'make-temp-class "expected list of method, got ~a" m))
            (old-ctor n s c f cf af m))))

  ;; Temp-Class ::= (make-temp-class Class-Name (Union Class-Name #f)
  ;;                                 (Union 'any (Listof Type[Class])
  ;;                                 (Listof Field) (Listof Field)
  ;;                                 (Listof Field) (Listof Method))

  (define-syntax expand-parse-exn
    (syntax-rules ()
      [(_ expr)
       (with-handlers ([exn:aj:parse? struct->vector])
         expr)]))

  ;; parse-init-program :: SExpr -> (Hash-Table ID Temp-Class) Src-Expr
  (define parse-init-program
    (lambda (src)
      (unless (and (list? src)
                   (not (null? src)))
        (raise (make-exn:aj:parse "bad program" (current-continuation-marks)
                                  src)))
      (let ([table (make-hash-table)])
        (hash-table-put! table 'Object
                         (make-temp-class 'Object #f null null null null null))
        (let loop ([src src])
          (cond
           [(null? (cdr src)) (values table (parse-expr (car src)))]
           [else
            (add-to-table! (parse-defn (car src)) table)
            (loop (cdr src))])))))

  ;; add-to-table! :: Temp-Class (Hash-Table Class-Name Temp-Class) -> ()
  ;; adds cdefn to table; raises exn:aj:parse if already present
  (define add-to-table!
    (lambda (cdefn table)
      (when (hash-table-get table
                            (temp-class-name cdefn)
                            (lambda () #f))
        (raise (make-exn:aj:parse "duplicate class definition"
                                  (current-continuation-marks)
                                  cdefn)))
      (hash-table-put! table (temp-class-name cdefn) cdefn)))

  ;; parse-program :: SExpr -> Program[Src-Expr]
  (define parse-program
    (lambda (src)
      (let-values ([(temp-table main) (parse-init-program src)])
        (make-program (make-final-classes temp-table) main))))

  (define make-final-classes
    (lambda (temp-table)
      (let ([final-table (make-hash-table)])
        (hash-table-for-each temp-table
                             (patch-superclass temp-table final-table))
        final-table)))

  ;; patch-superclass :: (Hash-Table Class-Name Temp-Class)
  ;;                     (Hash-Table Class-Name Class)
  ;;                  -> Class-Name Class
  ;;                  -> ()
  (define patch-superclass
    (lambda (temp-table final-table)
      (rec loop
        (lambda (name class)
          (unless (hash-table-get final-table name (lambda () #f))
            (let* ([parent-name (temp-class-superclass class)]
                   [parent
                    (if parent-name
                        (hash-table-get temp-table parent-name
                                        (lambda ()
                                          (raise (make-exn:aj:parse
                                                  "parent class doesn't exist"
                                                  (current-continuation-marks)
                                                  class))))
                        #f)])
              (when parent (loop parent-name parent))
              (let ([final-parent (if parent-name
                                      (hash-table-get final-table
                                                      parent-name)
                                      #f)])
                (hash-table-put!
                 final-table name
                 (make-class (make-class-type name)
                             final-parent
                             (temp-class-containers class)
                             (temp-class-fields class)
                             (temp-class-contained-fields class)
                             (temp-class-acquired-fields class)
                             (temp-class-methods class))))))))))

  ;; parse-expr :: SExpr -> Src-Expr
  (define parse-expr
    (match-lambda
      ['null (make-nil)]
      [(? integer? i) (make-num-lit i)]
      ['true (make-bool-lit #t)]
      ['false (make-bool-lit #f)]
      [(? id? x) (make-var-ref x)]
      ['this (make-var-ref 'this)]
      [('new (? class-name? cname) args ...)
       (make-new (make-class-type cname) (map parse-expr args))]
      [('ivar obj (? field-name? fd)) (make-ivar (parse-expr obj) fd)]
      [('send obj (? method-name? md) args ...)
       (make-send (parse-expr obj) md (map parse-expr args))]
      [('super (? method-name? md) args ...)
       (make-super md (map parse-expr args))]
      [('cast (? class-name? cname) obj)
       (make-cast (make-class-type cname) (parse-expr obj))]
      [('let (? id? id) rhs body)
       (make-aj-let id (parse-expr rhs) (parse-expr body))]
      [((? binary-prim-name? op) rand1 rand2)
       (make-binary-prim op (parse-expr rand1) (parse-expr rand2))]
      [((? unary-prim-name? op) rand)
       (make-unary-prim op (parse-expr rand))]
      [('if e1 e2 e3) (make-if-expr (parse-expr e1)
                                    (parse-expr e2)
                                    (parse-expr e3))]
      [bogus (raise (make-exn:aj:parse "bad expression"
                                       (current-continuation-marks)
                                       bogus))]))

  ;; parses a raw definition into a temp definition---that is, a full
  ;; definition except we have a symbol instead of a class reference for the
  ;; superclass.  (Patch those up later.)
  ;; parse-defn :: SExpr -> Temp-Class
  (define parse-defn
    (match-lambda
      [('class
         (? defn-name? name)
         (? class-name? superclass)
         'any
         (fields ...)
         ('contain cfields ...)
         ('acquire afields ...)
         methods ...)
       (make-temp-class name superclass 'any
                        (map (parse-field 'normal) fields)
                        (map (parse-field 'contained) cfields)
                        (map (parse-field 'acquired ) afields)
                        (map parse-method methods))]
      [('class
         (? defn-name? name)
         (? class-name? superclass)
         ((? class-name? containers) ...)
         (fields ...)
         ('contain cfields ...)
         ('acquire afields ...)
         methods ...)
       (make-temp-class name superclass
                        (map make-class-type containers)
                        (map (parse-field 'normal) fields)
                        (map (parse-field 'contained) cfields)
                        (map (parse-field 'acquired) afields)
                        (map parse-method methods))]
      [bogus (raise (make-exn:aj:parse "bad definition"
                                       (current-continuation-marks)
                                       bogus))]))

  ;; Parses a raw field definition.
  ;; parse-field :: Status -> SExpr -> Field
  (define parse-field
    (lambda (status)
      (match-lambda
        [((? type-name? type) (? field-name? fd))
         (make-field (parse-type type) fd status)]
        [bogus (raise (make-exn:aj:parse "bad field definition"
                                         (current-continuation-marks)
                                         bogus))])))

  ;; parses the raw representation of a type
  ;; parse-type :: SExpr -> Type
  (define parse-type
    (match-lambda
      ['int (make-ground-type 'int)]
      ['bool (make-ground-type 'bool)]
      [(? class-name? cname) (make-class-type cname)]
      [bogus (raise (make-exn:aj:parse "bad type" (current-continuation-marks)
                                       bogus))]))

  ;; parses a method definition
  ;; parse-method :: SExpr -> Method
  (define parse-method
    (match-lambda
      [((? type-name? type) (? method-name? name) (args ...) body)
       (let-values ([(names types) (mv-map parse-arg args)])
         (make-method (parse-type type) name names types
                      (parse-expr body)))]
      [bogus (raise (make-exn:aj:parse "bad method definition"
                                       (current-continuation-marks)
                                       bogus))]))

  ;; parses a method argument declaration
  ;; parse-arg :: SExpr -> Arg-Name Type
  (define parse-arg
    (match-lambda
      [((? type-name? type) (? arg-name? name))
       (values name (parse-type type))]
      [bogus (raise (make-exn:aj:parse "bad argument definition"
                                       (current-continuation-marks)
                                       bogus))]))
  )
