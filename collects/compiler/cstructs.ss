;; Compiler structures
;; (c) 1996-7 Sebastian Good

(unit/sig
 compiler:cstructs^
 (import compiler:library^
	 (zodiac : zodiac:system^)
	 compiler:zlayer^
	 mzlib:function^)

;;----------------------------------------------------------------------------
;; VARREF ATTRIBUTES
;;

(define (varref:empty-attributes) empty-set)
(define (varref:add-attribute! ast attr)
  (set-annotation! ast (set-union (make-singleton-set attr)
				  (get-annotation ast))))
(define (varref:has-attribute? ast attr)
  (set-memq? attr (get-annotation ast)))

(define varref:static 'varref:static)
(define varref:per-load-static 'varref:per-load-static)
(define varref:primitive 'varref:primitive)
(define varref:symbol 'varref:symbol)
(define varref:env 'varref:env)

;;----------------------------------------------------------------------------
;; AST NODES
;;
;; until this code is unitized, we'll just make it look like it is :-)

(define-struct (compiler:env-varref zodiac:struct:lexical-varref) ())
       (define compiler:make-env-varref make-compiler:env-varref)
       (set! make-compiler:env-varref 'make-compiler:env-varref)

(define-struct (compiler:static-varref zodiac:struct:varref) ())
       (define compiler:make-static-varref make-compiler:static-varref)
       (set! make-compiler:static-varref 'make-compiler:static-varref)
		
(define-struct (compiler:make-closure zodiac:struct:zodiac) (lambda free-vars args name))

;;----------------------------------------------------------------------------
;; ANNOTATION STRUCTURES
;;
;; this defines a binding.  We can check whether the variable
;; is known or mutable (mutable? => ~known?)
;; We will add type information to this eventually
;; it may be recursive -- bound by letrec
;;
(define-struct binding (rec?       ; part of a letrec recursive binding set
			mutable?   ; set!ed?
			unit-i/e?  ; is imported/exported (including uses by invoke)
			anchor     ; anchor binding for this binding
			ivar?      ; is a class ivar?
			known? val ; has known value?
			known-but-used? ; known value used in an improper way?
			rep))      ; reprsentation

;; This is the annotations given to a body of code.
(define-struct code (free-vars local-vars global-vars captured-vars 
		     closure-rep closure-alloc-rep label vehicle
		     max-arity
		     name))

(define-struct (procedure-code struct:code) (case-codes case-arities))

(define-struct case-code (free-vars local-vars global-vars))

;; annotations given to a unit
(define-struct (unit-code struct:code) (defines   ; a list of lexical-bindings
					exports   ; a list of lexical-bindings
					import-anchors   ; a list of lexical-bindings for anchors
					export-anchors   ; a list of lexical-bindings for anchors
					export-list-offset ; integer
					))

(define-struct (class-code struct:code) (public-lookup-bindings
					 public-define-bindings
					 private-bindings
					 inherit-bindings
					 rename-bindings
					 assembly ; integer
					 ))

(define-struct invoke-info (anchors))

(define-struct compound-info (assembly ; assembly number
			      imports  ; constant reference for import spec
			      exports  ; constant reference for export spec
			      links))  ; constant reference for link spec

(define-struct interface-info (assembly name))

;; this defines annotations given to applications
(define-struct app (tail? prim?))

;;----------------------------------------------------------------------------
;; ACCESSOR
;;

; gets the binding information from a lexical-varref
(define compiler:bound-varref->binding 
  (compose get-annotation zodiac:bound-varref-binding))

;;----------------------------------------------------------------------------
;; error/warning structures
;;
(define-struct compiler:message (ast message))
(define-struct (compiler:error struct:compiler:message) ())
(define-struct (compiler:fatal-error struct:compiler:message) ())
(define-struct (compiler:internal-error struct:compiler:message) ())
(define-struct (compiler:warning struct:compiler:message) ())

)
