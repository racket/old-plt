;;Kathy Gray, December 2001
;;Abstract syntax tree for Java

#cs
(module ast mzscheme
  
  ;Macro to allow structure definition and provision
  (define-syntax p-define-struct
    (syntax-rules ()
	          [(_ (name inherit) fields)
		   (begin
		     (provide (struct name fields))
                     (define-struct (name inherit) fields (make-inspector)))]
                  [(_ name fields)
                   (begin
                     (provide (struct name fields))
                     (define-struct name fields (make-inspector)))]))

  
  ;(make-src int int int)
  (p-define-struct src (line col pos span))
  
  ;;(make-package (U #f name) (list import) (list (U class-def interface-def)))
  (p-define-struct package (name imports defs))

  ;;(make-name id (list id) src)
  (p-define-struct name (id path src))
  
  ;;(make-id string src)
  (p-define-struct id (string src))
  
  ;; import java.lang.* -> (make-import (make-name "lang" `("java") ...) #t ...)
  ;;(make-import name bool src src string)
  (p-define-struct import (name star key-src src file))
  
  (provide def-header def-file def-uses def-name def-level)
  (define (def-header d)
    (if (class-def? d)
        (class-def-info d)
        (interface-def-info d)))
  (define (def-file d)
    (if (class-def? d)
        (class-def-file d)
        (interface-def-file d)))
  (define (def-uses d)
    (if (class-def? d)
        (class-def-uses d)
        (interface-def-uses d)))
  (define (def-level d)
    (if (class-def? d)
        (class-def-level d)
        (interface-def-level d)))
  (define (def-name d)
    (header-id (def-header d)))
  
  ;;(make-class-def header (list member) src src string symbol (list req))
  ;; members in file order
  (p-define-struct class-def (info members key-src src file level uses))

  ;;(make-interface-def header (list member) src src string symbol (list req))
  (p-define-struct interface-def (info members key-src src file level uses))
  
  ;;(make-require string (list string))
  (p-define-struct req (class path))
  
  ;;(make-header id (list modifier) (list name) (list name) (list gj-info) src)
  (p-define-struct header (id modifiers extends implements type-parms src))
  
  ;;(make-modifier symbol src)
  (p-define-struct modifier (kind src))
    
  ;;member = var-decl
  ;;       | var-init
  ;;       | method
  ;;       | initialize
  ;;       | class-def
  ;;       | interface-def
  
  ;;(make-type-spec (U name type-var symbol) int src)
  ;; dim is for array dimensions
  (p-define-struct type-spec (name dim src))
  
  ;;(make-type-var symbol (U #f type-spec) src)
  (p-define-struct type-var (name bound src))

  ;;Code for accessing fields: var-decl and var-init
  (provide field? field-name field-modifiers field-type field-src)
  (define (field? v) (or (var-decl? v) (var-init? v)))
  (define (field-name v) (var-decl-name (if (var-init? v) (var-init-var-decl v) v)))
  (define (field-modifiers v) (var-decl-modifiers (if (var-init? v) (var-init-var-decl v) v)))
  (define (field-type v) (var-decl-type (if (var-init? v) (var-init-var-decl v) v)))
  (define (field-src v) (var-decl-src (if (var-init? v) (var-init-var-decl v) v)))
  
  ;;(make-var-decl id (list modifier) type-spec src)
  (p-define-struct var-decl (name modifiers type src))

  ;;(make-var-init var-decl (U array-init expression) src)
  (p-define-struct var-init (var-decl init src))
  
  ;;(make-array-init (list (U expression array-init)) src)
  (p-define-struct array-init (vals src))
  
  ;;(make-method (list modifier) type-spec (list gj-info) id (list var-decl) (list name) Statement src)
  (p-define-struct method (modifiers type type-parms name parms throws body src))
  
  ;;(make-initialize bool block src)
  (p-define-struct initialize (static block src))

  (provide statement?)
  
  ;statement? 'a -> bool
  (define (statement? stmt)
    (or (ifS? stmt) (throw? stmt) (return? stmt) (while? stmt) (doS? stmt)
        (for? stmt) (try? stmt) (switch? stmt) (block? stmt) (break? stmt)
        (continue? stmt) (label? stmt) (synchronized? stmt) (statement-expression? stmt)))
    
  ;statement => if
  ;           | throw
  ;           | return 
  ;           | while
  ;           | do
  ;           | for
  ;           | try
  ;           | switch
  ;           | block
  ;           | break
  ;           | continue
  ;           | label
  ;           | synchronized
  ;           | StatementExpression
  
  ;StatementExpression => call
  ;                     |  post-expr
  ;                     |  preExpr
  ;                     |  assignment
  ;                     |  class-alloc

  ;(make-ifS Expression Statement Statement src src)
  (p-define-struct ifS (cond then else key-src src))
  
  ;(make-throw Expression src src)
  (p-define-struct throw (expr key-src src))

  ;(make-return Expression src)
  (p-define-struct return (expr src))
  
  ;(make-while Expression Statement src)
  (p-define-struct while (cond loop src))
  
  ;(make-do Statement Expression src)
  (p-define-struct doS (loop cond src))
  
  ;(make-for forInit Expression (list Expression) Statement src)
  (p-define-struct for (init cond incr loop src))  

  ;forInit => (list (U var-init var-decl))
  ;         | (list StatementExpression)
  
  ;(make-try Block (list Catch) (U #f statement) src src)
  (p-define-struct try (body catches finally key-src src))
  
  ;(make-catch var-decl statement src)
  (p-define-struct catch (cond body src))
  
  ;(make-switch Expression CaseStatements src)
  (p-define-struct switch (expr cases src))
  
  ;CaseStatements = (list case)
  ;(make-case (list (U ConstantExpression `default)) (list (U var-decl var-init Statement)) src)
  (p-define-struct caseS (constant body src))
  
  ;(make-block (list (U var-decl var-init Statement)) src)
  (p-define-struct block (stmts src))
  
  ;(make-break (U #f id) src)
  (p-define-struct break (label src))

  ;(make-continue (U #f id) src)
  (p-define-struct continue (label src))

  ;(make-label id statement src)
  (p-define-struct label (label stmt src))
  
  ;(make-synchronized expression statement src)
  (p-define-struct synchronized (expr stmt src))

  (provide statement-expression?)
  ;statement-expression?: StatementExpression -> bool
  (define statement-expression?
    (lambda (stmt)
      (or (call? stmt)
          (post-expr? stmt)
          (pre-expr? stmt)
          (unary? stmt)
          (assignment? stmt))))
  
  ;(make-expr (U symbol ??) src)
  (p-define-struct expr (types src)) 

  ;Expression => literal
  ;           |  bin-op
  ;           |  access
  ;           |  special-name
  ;           |  call
  ;           |  class-alloc
  ;           |  array-alloc
  ;           |  cond-expression
  ;           |  array-access
  ;           |  post-expr
  ;           |  pre-expr
  ;           |  unary
  ;           |  cast
  ;           |  instanceof
  ;           |  assignment

  ;(make-literal symbol src value)
  (p-define-struct (literal expr) (val))
  
  ;value => number | string
  
  ;(make-bin-op ?? src binary-op Expression Expression src)
  (p-define-struct (bin-op expr) (op left right key-src))
  
  ;binary-op => + - * / % << >> >>> < > <= >= == != & ^ or && oror 
  
  ;(make-access ?? src (U (list id) field-access local-access))
  ;Types before check
  ;After check, (list id) -> (U field-access local-access)
  (p-define-struct (access expr) (name))
  
  ;(make-field-access (U Expression #f) id var-access)
  (p-define-struct field-access (object field access))
  
  ;;(make-var-access bool bool bool string)
  (p-define-struct var-access (static? final? init? class))
  
  ;(make-local-access id)
  (p-define-struct local-access (name))
  
  ;(make-special-name ?? src string)
  (p-define-struct (special-name expr) (name))
  
  ;(make-call ?? src (U #f expression) MethodName (list Expression) (U #f method-record))
  (p-define-struct (call expr) (expr method-name args method-record))
  
  ;MethodName => special-name
  ;           |  id
  
  ;(make-class-alloc ?? src name (list Expression) (U #f method-record))
  (p-define-struct (class-alloc expr) (name args ctor-record))
  
  ;(make-array-alloc ?? src type-spec (list Expression) int)
  (p-define-struct (array-alloc expr) (name size dim))

  ;;(make-array-alloc-init ?? src type-spec int array-init)
  (p-define-struct (array-alloc-init expr) (name dim init))

  
  ;(make-cond-expression ?? src Expression Expression Expression src)
  (p-define-struct (cond-expression expr) (cond then else key-src))
  
  ;(make-array-access ?? src expression Expression)
  (p-define-struct (array-access expr) (name index))
  
  ;(make-post-expr ?? src Expression PrePost src)
  (p-define-struct (post-expr expr) (expr op key-src))
  
  ;PrePost => ++ --
  
  ;(make-pre-expr ?? src PrePost Expression src)
  (p-define-struct (pre-expr expr) (op expr key-src))
  
  ;(make-unary ?? src UnaryOp Expression src)
  (p-define-struct (unary expr) (op expr key-src))
  
  ;UnaryOp => + - ~ !
  
  ;(make-cast ?? src type-spec Expression)
  (p-define-struct (cast expr) (type expr))
  
  ;(make-instanceof ?? src Expression type-spec src)
  (p-define-struct (instanceof expr) (expr type key))
  
  ;Note: lefthand side might be incorrect
  ;(make-assignment ?? src (U access array-access) symbol Expression src)
  (p-define-struct (assignment expr) (left op right key-src))
  
  ;Op -> = *= /= %= += -= <<= >>= >>>= &= ^= or=  
 
  
  
)
