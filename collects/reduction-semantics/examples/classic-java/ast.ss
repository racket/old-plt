;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ast.ss
;; $Id: ast.ss,v 1.18 2004/04/12 18:00:58 cobbe Exp $
;;
;; Defines the AST types used for the Acquired Java system.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ast mzscheme

  (require (lib "contract.ss")
           "utils.ss")

  #| ID ::= any symbol except 'class 'Object 'any 'contain 'acquire 'new
                              'ivar 'send 'super 'this 'cast 'let
                              '+ '- '* '== 'and 'or 'zero? 'null? 'not
                              'int 'bool 'null 'true 'false

     Class-Name ::= ID | 'Object
     Defn-Name ::= ID
     Type-Name ::= Class-Name | 'int | 'bool
     Method-Name ::= ID
     Field-Name ::= ID
     Arg-Name ::= ID

     Unary-Prim ::= 'zero? | 'null? | 'not
     Binary-Prim ::= '+ '- '* 'and 'or '=
  |#

  (define BINARY-PRIMS '(+ - * == and or))
  (define UNARY-PRIMS '(zero? null? not))
  (define RESERVED-WORDS '(class Object any contain acquire new ivar
                            send super this cast let int bool null
                            true false))

  (define reserved?
    (lambda (x)
      (or (and (memq x RESERVED-WORDS) #t)
          (binary-prim-name? x)
          (unary-prim-name? x))))

  (define binary-prim-name? (lambda (x) (and (memq x BINARY-PRIMS) #t)))
  (define unary-prim-name? (lambda (x) (and (memq x UNARY-PRIMS) #t)))

  (define id? (lambda (x) (and (symbol? x) (not (reserved? x)))))

  (define type-name?
    (lambda (x)
      (or (eq? x 'int)
          (eq? x 'bool)
          (class-name? x))))

  (define field-name? id?)
  (define method-name? id?)

  (define defn-name? id?)
  (define class-name?
    (lambda (x)
      (or (eq? x 'Object) (defn-name? x))))

  (define arg-name? id?)

  (with-public-inspector
   (define-struct program (classes main))
   ;; Program ::= (make-program (Hash-Table Class-Name Class) Expr)

   (define-struct class (name superclass containers
                              fields contained-fields acquired-fields methods))
   ;; Class ::= (make-class Type[Class] (Union Class #f)
   ;;                          (Union (Listof Type[Class]) 'any)
   ;;                          (Listof Field) (Listof Field) (Listof Field)
   ;;                          (Listof Method))

   (define-struct field (type name status))
   ;; Field ::= (make-field Type Field-Name
   ;;                       (Union 'normal 'contained 'acquired))

   (define-struct method (type name arg-names arg-types body))
   ;; Method ::= (make-method Type Method-Name (Listof Arg-Name) (Listof Type)
   ;;                         Expr)

   (define-struct expr ())
   (define-struct (new expr) (type args))
   (define-struct (var-ref expr) (var))
   (define-struct (nil expr) ())
   (define-struct (ivar expr) (object field))
   (define-struct (send expr) (object method args))
   (define-struct (super expr) (method args))
   (define-struct (tagged-super expr) (type method args))
   (define-struct (cast expr) (type object))
   (define-struct (aj-let expr) (lhs rhs body))
   (define-struct (num-lit expr) (val))
   (define-struct (bool-lit expr) (val))
   (define-struct (unary-prim expr) (rator rand))
   (define-struct (binary-prim expr) (rator rand1 rand2))
   (define-struct (if-expr expr) (test then else))
   ;; Src-Expr ::= (make-new Type[Class] (Listof Src-Expr))
   ;;            | (make-var-ref Arg-Name)
   ;;            | (make-nil)
   ;;            | (make-ivar Src-Expr Field-Name)
   ;;            | (make-send Src-Expr Method-Name (Listof Src-Expr))
   ;;            | (make-super Method-Name (Listof Src-Expr))
   ;;            | (make-cast Type[Class] Src-Expr)
   ;;            | (make-aj-let ID Src-Expr Src-Expr)
   ;;            | (make-num-lit Integer)
   ;;            | (make-bool-lit Boolean)
   ;;            | (make-unary-prim Unary-Prim Src-Expr)
   ;;            | (make-binary-prim Binary-Prim Src-Expr Src-Expr)
   ;;            | (make-if-expr Src-Expr Src-Expr Src-Expr)

   ;; Tagged-Expr ::= (make-new Type[Class] (Listof Tagged-Expr))
   ;;               | (make-var-ref Arg-Name)
   ;;               | (make-nil)
   ;;               | (make-ivar Tagged-Expr Field-Name)
   ;;               | (make-send Tagged-Expr Method-Name
   ;;                            (Listof Tagged-Expr))
   ;;               | (make-tagged-super Type[Class] Method-Name
   ;;                                    (Listof Tagged-Expr))
   ;;               | (make-cast Type[Class] Tagged-Expr)
   ;;               | (make-aj-let ID Tagged-Expr Tagged-Expr)
   ;;               | (make-num-lit Integer)
   ;;               | (make-bool-lit Boolean)
   ;;               | (make-unary-prim Unary-Prim Tagged-Expr)
   ;;               | (make-binary-prim Binary-Prim Tagged-Expr Tagged-Expr)
   ;;               | (make-if-expr Tagged-Expr Tagged-Expr Tagged-Expr)

   (define-struct type ())
   (define-struct (ground-type type) (name))
   (define-struct (class-type type) (name))
   (define-struct (any-type type) ())
   ;; Type ::= (make-ground-type (Union 'int 'bool))
   ;;        | (make-class-type Class-Name)
   ;;        | (make-any-type)
   )

  (define src-expr?
    (lambda (x)
      (and (expr? x)
           (not (tagged-super? x)))))

  (define tagged-expr?
    (lambda (x)
      (and (expr? x)
           (not (super? x)))))

  (define (type->sexpr type)
    (cond
      [(ground-type? type) (ground-type-name type)]
      [(class-type? type) (class-type-name type)]
      [(any-type? type) '_!_]))

  (define type=?
    (lambda (t1 t2)
      (cond
        [(and (ground-type? t1) (ground-type? t2))
         (eq? (ground-type-name t1) (ground-type-name t2))]
        [(and (class-type? t1) (class-type? t2))
         (eq? (class-type-name t1) (class-type-name t2))]
        [(and (any-type? t1) (any-type? t2)) #t]
        [else #f])))

  (provide/contract (struct program ([classes hash-table?]
                                     [main expr?]))
                    (struct class ([name class-type?]
                                   [superclass (union false?
                                                      class?)]
                                   [containers
                                    (union (symbols 'any)
                                           (listof class-type?))]
                                   [fields (listof field?)]
                                   [contained-fields (listof field?)]
                                   [acquired-fields (listof field?)]
                                   [methods (listof method?)]))

                    (struct field ([type type?]
                                   [name field-name?]
                                   [status (symbols 'normal
                                                    'contained
                                                    'acquired)]))
                    (struct method ([type type?]
                                    [name method-name?]
                                    [arg-names (listof arg-name?)]
                                    [arg-types (listof type?)]
                                    [body expr?]))

                    (type? predicate?)
                    (struct ground-type ([name (symbols 'int 'bool)]))
                    (struct class-type ([name class-name?]))
                    (struct any-type ())

                    (expr? predicate?)
                    (struct new ([type class-type?]
                                 [args (listof expr?)]))
                    (struct var-ref ([var (union id? (symbols 'this))]))
                    (struct nil ())
                    (struct ivar ([object expr?]
                                  [field field-name?]))
                    (struct send ([object expr?]
                                  [method method-name?]
                                  [args (listof expr?)]))
                    (struct super ([method method-name?]
                                   [args (listof expr?)]))
                    (struct tagged-super ([type class-type?]
                                          [method method-name?]
                                          [args (listof expr?)]))
                    (struct cast ([type class-type?]
                                  [object expr?]))
                    (struct aj-let ([lhs id?]
                                    [rhs expr?]
                                    [body expr?]))
                    (struct num-lit ([val integer?]))
                    (struct bool-lit ([val boolean?]))
                    (struct unary-prim ([rator unary-prim-name?]
                                        [rand expr?]))
                    (struct binary-prim ([rator binary-prim-name?]
                                         [rand1 expr?]
                                         [rand2 expr?]))
                    (struct if-expr ([test expr?]
                                     [then expr?]
                                     [else expr?]))

                    (class-name? predicate?)
                    (defn-name? predicate?)
                    (type-name? predicate?)
                    (field-name? predicate?)
                    (method-name? predicate?)
                    (arg-name? predicate?)
                    (binary-prim-name? predicate?)
                    (unary-prim-name? predicate?)
                    (id? predicate?)))
