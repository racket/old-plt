;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; parser.ss
;; Richard Cobbe
;; $Id: parser.ss,v 1.8 2004/12/31 18:05:04 cobbe Exp $
;;
;; Implements the parser for the S-Expression based source syntax for
;; ClassicJava.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module parser mzscheme

  (require (lib "etc.ss")
           (lib "match.ss")
           (lib "contract.ss")
           "utils.ss"
           "ast.ss")

  (provide/contract (parse-program (-> sexp/c program?))
                    (struct (exn:cj:parse exn:fail:contract) 
                            ([message string?]
                             [continuation-marks continuation-mark-set?]
                             [src any/c])))

  (provide expand-parse-exn)

  (with-public-inspector
   (define-struct (exn:cj:parse exn:fail:contract) (src))
   (define-struct temp-class (name superclass fields methods)))

  #;
  ;; Wraps temp-class ctor with assertions on arguments.  Can't use contracts
  ;; here, since temp-class not exported from this module.
  (set! make-temp-class
        (let ([old-ctor make-temp-class])
          (lambda (n s f m)
            (unless (class-name? n)
              (error 'make-temp-class "expected class-name, got ~a" n))
            (unless (or (class-name? s) (not s))
              (error 'make-temp-class
                     "expected class-name or false, got ~a" s))
            (unless (and (list? f) (andmap field? f))
              (error 'make-temp-class "expected list of field, got ~a" f))
            (unless (and (list? m) (andmap method? m))
              (error 'make-temp-class "expected list of method, got ~a" m))
            (old-ctor n s f m))))

  ;; Temp-Class ::= (make-temp-class Class-Name (Union Class-Name #f)
  ;;                                 (Listof Field) (Listof Method))

  ;; display the parse exception in human-readable form (for debugging)
  ;; USAGE: (expand-parse-exn body)
  ;;    Evaluates to result of body.  If body throws an exn:cj:parse, evaluates
  ;;    to structure describing exception.
  (define-syntax expand-parse-exn
    (syntax-rules ()
      [(_ expr)
       (with-handlers ([exn:cj:parse? struct->vector])
         expr)]))

  ;; parse-init-program :: SExpr -> (Hash-Table ID Temp-Class) Src-Expr
  ;; Creates initial class hierarchy; classes have superclass names rather
  ;; than direct references.
  (define parse-init-program
    (lambda (src)
      (unless (and (list? src)
                   (not (null? src)))
        (raise (make-exn:cj:parse "bad program" (current-continuation-marks)
                                  src)))
      (let ([table (make-hash-table)])
        (hash-table-put! table 'Object
                         (make-temp-class 'Object #f null null))
        (recur loop ([src src])
          (cond
           [(null? (cdr src)) (values table (parse-expr (car src)))]
           [else
            (add-to-table! (parse-defn (car src)) table)
            (loop (cdr src))])))))

  ;; add-to-table! :: Temp-Class (Hash-Table Class-Name Temp-Class) -> ()
  ;; adds temp cdefn to table; raises exn:cj:parse if already present
  (define add-to-table!
    (lambda (cdefn table)
      (when (hash-table-get table
                            (temp-class-name cdefn)
                            (lambda () #f))
        (raise (make-exn:cj:parse "duplicate class definition"
                                  (current-continuation-marks)
                                  cdefn)))
      (hash-table-put! table (temp-class-name cdefn) cdefn)))

  ;; parse-program :: SExpr -> Program[Src-Expr]
  ;; parses the SExpression source into an (unelaborated) program
  (define parse-program
    (lambda (src)
      (let-values ([(temp-table main) (parse-init-program src)])
        (make-program (make-final-classes temp-table) main))))

  ;; make-final-classes :: (Hash-Table Class-Name Temp-Class)
  ;;                    -> (Hash-Table Class-Name Class[Src-Expr])
  ;; patches up parent links in class inheritance hierarchy.
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
  ;; ensures that class and all its ancestors are in final-table with
  ;;   valid superclass refs
  (define patch-superclass
    (lambda (temp-table final-table)
      (lambda (name class0)
        (recur loop ([name name]
                     [class class0]
                     [history null])
          (when (memq class history)
            (raise (make-exn:cj:parse "inheritance cycle"
                                      (current-continuation-marks)
                                      class0)))
          (unless (hash-table-get final-table name (lambda () #f))
            (let* ([parent-name (temp-class-superclass class)]
                   [parent
                    (if parent-name
                        (hash-table-get temp-table parent-name
                                        (lambda ()
                                          (raise (make-exn:cj:parse
                                                  "parent class doesn't exist"
                                                  (current-continuation-marks)
                                                  class))))
                        #f)])
              (when parent (loop parent-name parent (cons class history)))
              (let ([final-parent (if parent-name
                                      (hash-table-get final-table
                                                      parent-name)
                                      #f)])
                (hash-table-put!
                 final-table name
                 (make-class (make-class-type name)
                             final-parent
                             (temp-class-fields class)
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
      [('new (? class-name? cname))
       (make-new (make-class-type cname))]
      [('ref obj (? field-name? fd)) (make-ref (parse-expr obj) fd)]
      [('set obj (? field-name? fd) rhs)
       (make-set (parse-expr obj) fd (parse-expr rhs))]
      [('send obj (? method-name? md) args ...)
       (make-send (parse-expr obj) md (map parse-expr args))]
      [('super (? method-name? md) args ...)
       (make-super md (map parse-expr args))]
      [('cast (? class-name? cname) obj)
       (make-cast (make-class-type cname) (parse-expr obj))]
      [('let (? id? id) rhs body)
       (make-cj-let id (parse-expr rhs) (parse-expr body))]
      [((? binary-prim-name? op) rand1 rand2)
       (make-binary-prim op (parse-expr rand1) (parse-expr rand2))]
      [((? unary-prim-name? op) rand)
       (make-unary-prim op (parse-expr rand))]
      [('if e1 e2 e3) (make-if-expr (parse-expr e1)
                                    (parse-expr e2)
                                    (parse-expr e3))]
      [bogus (raise (make-exn:cj:parse "bad expression"
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
         (fields ...)
         methods ...)
       (make-temp-class name superclass
                        (map (parse-field (make-class-type name)) fields)
                        (map parse-method methods))]
      [bogus (raise (make-exn:cj:parse "bad definition"
                                       (current-continuation-marks)
                                       bogus))]))

  ;; Parses a raw field definition.
  ;; parse-field :: Type[Class] -> SExpr -> Field
  (define parse-field
    (lambda (declaring-class)
      (match-lambda
        [((? type-name? type) (? field-name? fd))
         (make-field (parse-type type) declaring-class fd)]
        [bogus (raise (make-exn:cj:parse "bad field definition"
                                         (current-continuation-marks)
                                         bogus))])))

  ;; parses the raw representation of a type
  ;; parse-type :: SExpr -> Type
  (define parse-type
    (match-lambda
      ['int (make-ground-type 'int)]
      ['bool (make-ground-type 'bool)]
      [(? class-name? cname) (make-class-type cname)]
      [bogus (raise (make-exn:cj:parse "bad type" (current-continuation-marks)
                                       bogus))]))

  ;; parses a method definition
  ;; parse-method :: SExpr -> Method
  (define parse-method
    (match-lambda
      [((? type-name? type) (? method-name? name) (args ...) body)
       (let-values ([(names types) (mv-map parse-arg args)])
         (make-method (parse-type type) name names types
                      (parse-expr body)))]
      [bogus (raise (make-exn:cj:parse "bad method definition"
                                       (current-continuation-marks)
                                       bogus))]))

  ;; parses a method argument declaration
  ;; parse-arg :: SExpr -> Arg-Name Type
  (define parse-arg
    (match-lambda
      [((? type-name? type) (? arg-name? name))
       (values name (parse-type type))]
      [bogus (raise (make-exn:cj:parse "bad argument definition"
                                       (current-continuation-marks)
                                       bogus))]))
  )
