(begin-elaboration-time 

 (require-library "functios.ss")
 (require-library "files.ss")
 (require-library "strings.ss")
 (require-library "prettys.ss")

 (require-library "compiles.ss" "dynext")
 (require-library "links.ss" "dynext")
 (require-library "files.ss" "dynext")

 (require-library "zsigs.ss" "zodiac")
 (require-library "sigs.ss" "zodiac")

 (require-library "sparams.ss" "backward")

 (require-relative-library "sig.ss") )

(define-signature compiler:library^
  (logical-inverse
   one-of
   all-of
   none-of
   vector-map
   improper-map
   begin-map!
   begin-map
   map!
   list-index
   list-last

   set?
   empty-set
   make-singleton-set
   list->set
   set->list
   improper-list->set
   set-list-predicate
   set-member?
   set-memv?
   set-memq?
   set-empty?
   set-find
   set-union
   set-union-singleton
   set-minus
   set-remove
   set-intersect
   set-subset?
   set-map
   set-filter
   remove-duplicates
   symbol-append
   compiler:formals->arity
   compiler:paroptformals->arity
   compiler:formals->arity*
   compiler:gensym
   compiler:genlabel
   compiler:clean-string
   protect-comment
   global-defined-value*

   compiler:label-number
   compiler:reset-label-number!
   compiler:bad-chars))


(define-signature compiler:cstructs^
  (varref:empty-attributes
   varref:add-attribute!
   varref:has-attribute?
   
   varref:static
   varref:per-load-static
   varref:primitive
   varref:symbol
   varref:inexact
   varref:env
   
   (struct compiler:make-closure (lambda free-vars args name))
   
   (struct binding (rec?       ; part of a letrec recursive binding set
		    mutable?   ; set!ed?
		    unit-i/e?  ; is imported/exported (including uses by invoke)
		    anchor     ; anchor binding for this binding
		    letrec-set?; set! for a letrec definition
		    ivar?      ; is a class ivar?
		    known? val ; has known value?
		    known-but-used? ; known value used in an improper way?
		    rep))      ; reprsentation
   copy-binding

   copy-binding-for-light-closures

   binder:empty-anno 

   (struct code (free-vars local-vars global-vars used-vars captured-vars
			   parent case-parent children))

   (struct closure-code (rep alloc-rep label vehicle
				     max-arity
				     return-multi ; #f, #t, or 'possible
				     name))

   (struct procedure-code (case-codes case-arities))

   (struct case-code (has-continue?))

   (struct unit-code (defines   ; a list of lexical-bindings
		       exports   ; a list of lexical-bindings
		       import-anchors   ; a list of lexical-bindings for anchors
		       export-anchors   ; a list of lexical-bindings for anchors
		       export-list-offset))

   (struct class-code (public-lookup-bindings
		       public-define-bindings
		       override-lookup-bindings
		       override-define-bindings
		       private-bindings
		       inherit-bindings
		       rename-bindings
		       assembly))

   (struct invoke-info (anchors))

   (struct compound-info (assembly ; assembly number
			  imports  ; constant reference for import spec
			  exports  ; constant reference for export spec
			  links))  ; constant reference for link spec

   (struct interface-info (assembly name))

   (struct app (tail? prim? prim-name))

   compiler:bound-varref->binding 

   (struct compiler:message (ast message))
   (struct compiler:error ())
   (struct compiler:fatal-error ())
   (struct compiler:internal-error ())
   (struct compiler:warning ())))

(define-signature compiler:zlayer^
  (static-error
   dynamic-error
   internal-error
   analysis-error
   analysis-internal-error
   
   compiler:empty-annotation
   make-empty-box
   get-annotation
   set-annotation!
   annotated?
   remove-annotation!

   compiler:escape-on-error

   zodiac:begin0-form-first
   zodiac:begin0-form-rest
   zodiac:set-begin0-form-first!
   zodiac:set-begin0-form-rest!

   class-init-defaults-map!

   zodiac:invoke-form?
   zodiac:invoke-form-unit
   zodiac:set-invoke-form-unit!
   zodiac:invoke-form-variables
   zodiac:set-invoke-form-variables!
   
   (struct zodiac:void ())
   (struct zodiac:undefined ())

   zodiac:make-void
   zodiac:make-undefined

   zodiac:make-special-constant
 
   zodiac:binding->lexical-varref

   main-source-file
   zodiac:print-start!

   zodiac->sexp/annotate))


(define-signature compiler:prephase^
  (prephase:init-binding-properties!
   prephase:set-mutable!
   prephase:set-binding-anchor!
   prephase:set-unit-i/e?!
   prephase:is-mutable?
   prephase:is-unit-i/e?
   prephase:is-ivar?
   prephase:binding-anchor

   (struct binding-properties (unit-i/e?))

   prephase:set!-is-unit-definition?

   prephase!))


(define-signature compiler:anorm^
  (a-normalize))

(define-signature compiler:const^
  (const:init-tables!
   const:the-per-load-statics-table
   const:per-load-statics-table?

   const:symbol-counter
   const:symbol-table

   const:inexact-counter
   const:inexact-table

   compiler:add-const!
   compiler:get-symbol-const!
   compiler:construct-const-code!

   compiler:static-list
   compiler:per-load-static-list
   
   compiler:add-per-load-static-list!

   compiler:make-const-constructor))

(define-signature compiler:rep^
  ((struct rep:atomic (type))
   (struct rep:pointer (to))
   (struct rep:struct (name orig-name fields))
   (struct rep:struct-field (name orig-name rep))

   compiler:structs
   compiler:init-structs!

   rep:find-field

   choose-binding-representations!
   choose-closure-representation!))

(define-signature compiler:known^
  (make-unknown-letbound-binding
   analyze-knowns!))

(define-signature compiler:analyze^
  (compiler:global-symbols
   compiler:primitive-refs
   compiler:compounds
   compiler:interfaces

   compiler:define-list
   compiler:per-load-define-list
   compiler:local-define-list
   compiler:local-per-load-define-list

   compiler:init-define-lists!

   compiler:add-global-varref!
   compiler:add-primitive-varref!

   compiler:add-local-define-list!
   compiler:add-local-per-load-define-list!
   
   (struct case-info (body case-code global-vars used-vars captured-vars max-arity))

   extract-varref-known-val

   analyze-expression!))

(define-signature compiler:lightweight^
  (lightweight-analyze-and-transform))

(define-signature compiler:closure^
  (compiler:closure-list
   compiler:once-closures-list
   compiler:once-closures-globals-list
   compiler:init-closure-lists!
   compiler:init-once-closure-lists!
   closure-expression!))

(define-signature compiler:vehicle^
  ((struct vehicle (total-labels lambdas max-arity))
   (struct procedure-vehicle (max-args))
   (struct unit-vehicle ())
   (struct class-vehicle ())

   compiler:vehicles
   compiler:total-vehicles
   compiler:total-unit-exports
   compiler:case-lambdas
   compiler:classes

   compiler:init-vehicles!

   get-vehicle
   relate-lambdas!

   vehicle:only-code-in-vehicle?

   choose-vehicles!))

(define-signature compiler:vmstructs^
  ((struct vm:sequence (vals))
   (struct vm:if (test then else))
   
   (struct vm:void (val))
   (struct vm:return (val))
   (struct vm:tail-apply (closure argc prim))
   (struct vm:tail-call (label closure set-env?))
   (struct vm:continue ())
   
   (struct vm:set! (vars val mode))
   (struct vm:generic-args (closure tail? prim vals))
   (struct vm:register-args (vars vals))
   (struct vm:args (type vals))
   (struct vm:begin0-mark! (var val))
   (struct vm:begin0-setup! (var))
   
   (struct vm:alloc (type))
   (struct vm:build-constant (text))
   (struct vm:make-closure (closure))
   (struct vm:make-procedure-closure (vehicle min-arity max-arity name empty?))
   (struct vm:make-case-procedure-closure (vehicle num-cases case-arities name empty?))
   (struct vm:make-unit-closure (vehicle num-imports num-exports exports-offset name empty?))
   (struct vm:make-class-closure (assembly))
   (struct vm:apply (closure argc known? multi? prim simple-tail-prim?))
   (struct vm:macro-apply (name primitive args tail? bool?))
   (struct vm:struct (type super fields multi?)) ; multi? = #f => always run-time error
   (struct vm:compound (assembly))
   (struct vm:invoke (num-variables open? multi? tail? name-specifier))
   (struct vm:interface (assembly))
   (struct vm:call (label closure))
   (struct vm:begin0-extract (var))

   (struct vm:global-varref (var))
   (struct vm:bucket (var))
   (struct vm:per-load-statics-table ())
   (struct vm:cast (val rep)) ; last resort

   (struct vm:local-varref (var binding))
   (struct vm:static-varref (var))
   (struct vm:per-load-static-varref ())
   (struct vm:primitive-varref (var))
   (struct vm:symbol-varref (var))
   (struct vm:inexact-varref (var))
   (struct vm:struct-ref (field var))
   (struct vm:deref (var))
   (struct vm:ref (var))

   (struct vm:immediate (text))
   
   (struct vm:struct-type (fields))

   vm:box-struct-type

   arg-type:register
   arg-type:arg
   arg-type:tail-arg

   target-type:global
   target-type:lexical
   target-type:static

   vm:control-return?

   vm:fixnum?
   
   vm:literal-constant?))


(define-signature compiler:vmphase^
  (vm:convert-bound-varref
   vm-phase))

(define-signature compiler:vmopt^
  (vm-optimize!))

(define-signature compiler:driver^
  ((open compiler:inner^)

   compiler:error
   compiler:fatal-error
   compiler:internal-error
   compiler:warning

   s:file-block
   s:register-max-arity!
   compiler:setup-suffix

   compiler:multi-o-constant-pool?

   debug
   debug:port))

(define-signature compiler:top-level^
  ((struct block (source codes max-arity))
   make-empty-block
   block:register-max-arity!

   add-code-local+used-vars!
   remove-code-free-vars!))

(define-signature compiler:vm2c^
  (vm->c:indent-by
   vm->c:indent-spaces

   vm->c:extract-inferred-name

   vm->c:make-bucket-names!
   vm->c:emit-symbol-list!
   vm->c:emit-symbol-declarations!
   vm->c:emit-symbol-definitions!
   vm->c:emit-inexact-declarations!
   vm->c:emit-inexact-definitions!
   vm->c:emit-export-symbol-definitions!
   vm->c:emit-prim-ref-declarations!
   vm->c:emit-prim-ref-definitions!
   vm->c:emit-struct-definitions!
   vm->c:emit-static-declarations!
   vm->c:emit-registration!
   vm->c:emit-case-arities-definitions!
   vm->c:emit-compound-definitions!
   vm->c:emit-class-definitions!
   vm->c:emit-interface-definitions!
   vm->c:emit-top-levels!
   vm->c:emit-vehicle-prototype
   vm->c:emit-vehicle-declaration
   vm->c:emit-vehicle-header
   vm->c:emit-vehicle-prologue
   vm->c:emit-vehicle-epilogue
   vm->c:convert-type-definition
   vm->c:emit-function-prologue
   vm->c:emit-case-prologue
   vm->c:emit-case-epilogue
   vm->c:emit-function-epilogue
   vm->c:emit-unit-prologue
   vm->c:emit-unit-epilogue
   vm->c:emit-class-prologue
   vm->c:emit-class-epilogue
   vm->c-expression))

(define-signature compiler:mrspidey^
  (copy-annotations!
   analyze-program-sexps
   binding-mutated
   constant-value
   SDL-type
   parsed-ftype
   Tvar-objs
   Tvar?
   fo-FlowType?
   FlowType->Tvar
   prim-av?
   fo-ftype->AVs
   ast->AVs
   AV->AVs))

(define-signature compiler:basic-link^
  ((unit ZODIAC : zodiac:system^)
   (unit ZLAYER : compiler:zlayer^)
   (unit DRIVER : compiler:driver^)
   (unit LIBRARY : compiler:library^)))


