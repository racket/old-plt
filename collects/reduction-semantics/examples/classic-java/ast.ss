;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ast.ss
;; $Id: ast.ss,v 1.9 2005/01/03 12:42:20 cobbe Exp $
;;
;; Defines the AST types used for the ClassicJava system.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ast mzscheme

  (require (lib "contract.ss")
           "utils.ss")

  #| ID ::= any symbol except those in RESERVED-WORDS, BINARY-PRIMS, and
            UNARY-PRIMS below

     Class-Name ::= ID | 'Object
     Defn-Name ::= ID     ;; those names legal for user-defined classes
     Type-Name ::= Class-Name | 'int | 'bool
     Method-Name ::= ID
     Field-Name ::= ID
     Arg-Name ::= ID

     Unary-Prim ::= 'zero? | 'null? | 'not
     Binary-Prim ::= '+ '- '* 'and 'or '=
  |#

  (define BINARY-PRIMS '(+ - * == and or))
  (define UNARY-PRIMS '(zero? null? not))
  (define RESERVED-WORDS '(class Object new ref set send super this cast let
                            int bool null true false))

  ;; reserved? :: x -> Boolean
  ;; recognizes ClassicJava reserved words
  (define reserved?
    (lambda (x)
      (or (and (memq x RESERVED-WORDS) #t)
          (binary-prim-name? x)
          (unary-prim-name? x))))

  ;; binary-prim-name? unary-prim-name? :: x -> Boolean
  ;; recognize ClassicJava binary and unary primitives
  (define binary-prim-name? (lambda (x) (and (memq x BINARY-PRIMS) #t)))
  (define unary-prim-name? (lambda (x) (and (memq x UNARY-PRIMS) #t)))

  ;; id? :: x -> Boolean
  ;; recognizes legal ClassicJava identifiers
  (define id? (lambda (x) (and (symbol? x) (not (reserved? x)))))

  ;; type-name? :: x -> Boolean
  ;; recognizes legal ClassicJava type names
  (define type-name?
    (lambda (x)
      (or (eq? x 'int)
          (eq? x 'bool)
          (class-name? x))))

  ;; field-name? method-name? :: x -> Boolean
  ;; recognize legal ClassicJava field names, method names
  (define field-name? id?)
  (define method-name? id?)

  ;; defn-name? class-name? :: x -> Boolean
  ;; recognizes legal ClassicJava definition names and class names
  (define defn-name? id?)
  (define class-name?
    (lambda (x)
      (or (eq? x 'Object) (defn-name? x))))

  ;; arg-name? :: x -> Boolean
  ;; recognizes legal ClassicJava method argument names
  (define arg-name? id?)

  (with-public-inspector
   (define-struct program (classes main))
   ;; Program ::= (make-program (Hash-Table Class-Name Class) Expr)

   (define-struct class (name superclass fields methods))
   ;; Class ::= (make-class Type[Class] (Union Class #f)
   ;;                       (Listof Field) (Listof Method))

   (define-struct field (type class name))
   ;; Field ::= (make-field Type Type[Class] Field-Name)
   ;;   class is class in which field is declared.

   (define-struct method (type name arg-names arg-types body))
   ;; Method ::= (make-method Type Method-Name (Listof Arg-Name)
   ;;                         (Listof Type) Expr)

   (define-struct expr ())
   (define-struct (new expr) (type))
   (define-struct (var-ref expr) (var))
   (define-struct (nil expr) ())
   (define-struct (ref expr) (object field))
   (define-struct (tagged-ref expr) (object class field))
   (define-struct (set expr) (object field rhs))
   (define-struct (tagged-set expr) (object class field rhs))
   (define-struct (send expr) (object method args))
   (define-struct (super expr) (method args))
   (define-struct (tagged-super expr) (type method args))
   (define-struct (cast expr) (type object))
   (define-struct (cj-let expr) (lhs rhs body))
   (define-struct (num-lit expr) (val))
   (define-struct (bool-lit expr) (val))
   (define-struct (unary-prim expr) (rator rand))
   (define-struct (binary-prim expr) (rator rand1 rand2))
   (define-struct (if-expr expr) (test then else))
   ;; Src-Expr ::= (make-new Type[Class])
   ;;            | (make-var-ref Arg-Name)
   ;;            | (make-nil)
   ;;            | (make-ref Src-Expr Field-Name)
   ;;            | (make-set Src-Expr Field-Name Src-Expr)
   ;;            | (make-send Src-Expr Method-Name (Listof Src-Expr))
   ;;            | (make-super Method-Name (Listof Src-Expr))
   ;;            | (make-cast Type[Class] Src-Expr)
   ;;            | (make-cj-let ID Src-Expr Src-Expr)
   ;;            | (make-num-lit Integer)
   ;;            | (make-bool-lit Boolean)
   ;;            | (make-unary-prim Unary-Prim Src-Expr)
   ;;            | (make-binary-prim Binary-Prim Src-Expr Src-Expr)
   ;;            | (make-if-expr Src-Expr Src-Expr Src-Expr)

   ;; Tagged-Expr ::= (make-new Type[Class])
   ;;               | (make-var-ref Arg-Name)
   ;;               | (make-var-ref 'this)
   ;;               | (make-nil)
   ;;               | (make-tagged-ref Src-Expr Type[Class] Field-Name)
   ;;               | (make-tagged-set Src-Expr Type[Class] Field-Name
   ;;                                  Src-Expr)
   ;;               | (make-send Tagged-Expr Method-Name (Listof Tagged-Expr))
   ;;               | (make-tagged-super Type[Class] Method-Name
   ;;                                    (Listof Tagged-Expr))
   ;;               | (make-cast Type[Class] Tagged-Expr)
   ;;               | (make-cj-let ID Tagged-Expr Tagged-Expr)
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

  ;; type=? :: Type Type -> Boolean
  ;; compares two types for (nominal) equality
  (define type=? equal?)

  ;; src-expr? :: x -> Boolean
  ;; recognizes source (i.e., unelaborated) expressions
  (define src-expr?
    (lambda (x)
      (and (expr? x)
           (not (tagged-ref? x))
           (not (tagged-set? x))
           (not (tagged-super? x)))))

  ;; tagged-expr? :: x -> Boolean
  ;; recognizes tagged (i.e., elaborated) expressions
  (define tagged-expr?
    (lambda (x)
      (and (expr? x)
           (not (ref? x))
           (not (set? x))
           (not (super? x)))))

  ;; type->sexpr :: Type -> Sexp
  ;; formats a type for easy manipulation
  (define (type->sexpr type)
    (cond
      [(ground-type? type) (ground-type-name type)]
      [(class-type? type) (class-type-name type)]
      [(any-type? type) '_!_]))

  (provide/contract
   [struct program         ([classes hash-table?]
                            [main expr?])]
   [struct class           ([name class-type?]
                            [superclass (union false/c class?)]
                            [fields (listof field?)]
                            [methods (listof method?)])]

   [struct field           ([type type?]
                            [class class-type?]
                            [name field-name?])]
   [struct method          ([type type?]
                            [name method-name?]
                            [arg-names (listof arg-name?)]
                            [arg-types (listof type?)]
                            [body expr?])]

   [type?                  predicate/c]
   [struct ground-type     ([name (symbols 'int 'bool)])]
   [struct class-type      ([name class-name?])]
   [struct any-type        ()]

   [expr?                  predicate/c]
   [struct new             ([type class-type?])]
   [struct var-ref         ([var (union id? (symbols 'this))])]
   [struct nil             ()]
   [struct ref             ([object expr?]
                            [field field-name?])]
   [struct tagged-ref      ([object expr?]
                            [class class-type?]
                            [field field-name?])]
   [struct set             ([object expr?]
                            [field field-name?]
                            [rhs expr?])]
   [struct tagged-set      ([object expr?]
                            [class class-type?]
                            [field field-name?]
                            [rhs expr?])]
   [struct send            ([object expr?]
                            [method method-name?]
                            [args (listof expr?)])]
   [struct super           ([method method-name?]
                            [args (listof expr?)])]
   [struct tagged-super    ([type class-type?]
                            [method method-name?]
                            [args (listof expr?)])]
   [struct cast            ([type class-type?]
                            [object expr?])]
   [struct cj-let          ([lhs id?]
                            [rhs expr?]
                            [body expr?])]
   [struct num-lit         ([val integer?])]
   [struct bool-lit        ([val boolean?])]
   [struct unary-prim      ([rator unary-prim-name?]
                            [rand expr?])]
   [struct binary-prim     ([rator binary-prim-name?]
                            [rand1 expr?]
                            [rand2 expr?])]
   [struct if-expr         ([test expr?]
                            [then expr?]
                            [else expr?])]

   [type=?                 (-> type? type? boolean?)]

   [src-expr?              predicate/c]
   [tagged-expr?           predicate/c]

   [type->sexpr            (-> type? sexp/c)]

   [class-name?            predicate/c]
   [defn-name?             predicate/c]
   [type-name?             predicate/c]
   [field-name?            predicate/c]
   [method-name?           predicate/c]
   [arg-name?              predicate/c]
   [binary-prim-name?      predicate/c]
   [unary-prim-name?       predicate/c]
   [id?                    predicate/c]))
