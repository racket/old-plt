;; VM-Scheme
;; (c) 1996-7 Sebastian Good

;; Mostly structure definitions for VM AST nodes.

(unit/sig compiler:vmstructs^
  (import compiler:library^
	  compiler:cstructs^
	  (zodiac : zodiac:system^)
	  compiler:zlayer^
	  compiler:driver^
	  mzlib:function^)

  ;; Block statements
  (define-struct (vm:sequence zodiac:struct:zodiac) (vals))
  (define-struct (vm:if zodiac:struct:zodiac) (test then else))

  ;; Tail position statements
  (define-struct (vm:void zodiac:struct:zodiac) (val))
  (define-struct (vm:return zodiac:struct:zodiac) (val))
  (define-struct (vm:tail-apply zodiac:struct:zodiac) (closure argc prim))
  (define-struct (vm:tail-call zodiac:struct:zodiac) (label closure set-env?))
  (define-struct (vm:continue zodiac:struct:zodiac) ())

  ;; non-tail imperative statements
  (define-struct (vm:set! zodiac:struct:zodiac) (vars val mode))
  (define-struct (vm:generic-args zodiac:struct:zodiac) (closure tail? prim vals))
  (define-struct (vm:register-args zodiac:struct:zodiac) (vars vals))
  (define-struct (vm:args zodiac:struct:zodiac) (type vals))
  (define-struct (vm:begin0-mark! zodiac:struct:zodiac) (var val))
  (define-struct (vm:begin0-setup! zodiac:struct:zodiac) (var))

  ;; r-values (1 step computations)
  (define-struct (vm:alloc zodiac:struct:zodiac) (type))
  (define-struct (vm:build-constant zodiac:struct:zodiac) (text))
  (define-struct (vm:make-closure zodiac:struct:zodiac) (closure))
  (define-struct (vm:make-procedure-closure struct:vm:make-closure)
    (vehicle min-arity max-arity name empty?))
  (define-struct (vm:make-case-procedure-closure struct:vm:make-closure)
    (vehicle num-cases case-arities name empty?))
  (define-struct (vm:make-unit-closure struct:vm:make-closure)
    (vehicle num-imports num-exports exports-offset name empty?))
  (define-struct (vm:make-class-closure struct:vm:make-closure)
    (assembly))
  (define-struct (vm:apply zodiac:struct:zodiac) 
    (closure argc known? multi? prim simple-tail-prim?))
  (define-struct (vm:macro-apply zodiac:struct:zodiac) 
    (name primitive args tail? bool?))
  (define-struct (vm:struct zodiac:struct:zodiac)
    (type super fields multi?)) ; multi? = #f => always run-time error
  (define-struct (vm:compound zodiac:struct:zodiac) (assembly))
  (define-struct (vm:invoke zodiac:struct:zodiac) (num-variables open? multi? tail? name-specifier))
  (define-struct (vm:interface zodiac:struct:zodiac) (assembly))
  (define-struct (vm:call zodiac:struct:zodiac) (label closure))
  (define-struct (vm:begin0-extract zodiac:struct:zodiac) (var))

  ;; a-values
  (define-struct (vm:global-varref zodiac:struct:zodiac) (var))
  (define-struct (vm:bucket zodiac:struct:zodiac) (var))
  (define-struct (vm:per-load-statics-table zodiac:struct:zodiac) ())
  (define-struct (vm:cast zodiac:struct:zodiac) (val rep)) ; last resort

  ;; l-values (locations in memory)
  (define-struct (vm:local-varref zodiac:struct:zodiac) (var binding))
  (define-struct (vm:static-varref zodiac:struct:zodiac) (var))
  (define-struct (vm:static-varref-from-lift struct:vm:static-varref) (lambda))
  (define-struct (vm:per-load-static-varref struct:vm:static-varref) ())
  (define-struct (vm:primitive-varref zodiac:struct:zodiac) (var))
  (define-struct (vm:symbol-varref zodiac:struct:zodiac) (var))
  (define-struct (vm:inexact-varref zodiac:struct:zodiac) (var))
  (define-struct (vm:struct-ref zodiac:struct:zodiac) (field var))
  (define-struct (vm:deref zodiac:struct:zodiac) (var))
  (define-struct (vm:ref zodiac:struct:zodiac) (var))

  ;; immediate values
  (define-struct (vm:immediate zodiac:struct:zodiac) (text))

  ;; defines a structure type
  ;; all structures may be indexed by number as well.
  (define-struct vm:struct-type (fields))

  (define vm:box-struct-type (make-vm:struct-type '(box)))

  ;; argument types
  (define arg-type:register 'arg-type:register)
  (define arg-type:arg 'arg-type:arg)
  (define arg-type:tail-arg 'arg-type:tail-arg)

  ;; set!-target types
  (define target-type:global 'target-type:global)
  (define target-type:lexical 'target-type:lexical)
  (define target-type:static target-type:lexical)

  ;; this is the class of statements that make control leave the block
  (define vm:control-return?
    (one-of vm:return? vm:tail-apply? vm:tail-call? vm:continue?))

  ;; Defines fixnumness in the VM Scheme.  
  ;; may change under different implementations; and will certainly
  ;; need to change as things get unwrapped...
  (define vm:fixnum?
    (lambda (n)
      (and (exact? n) (integer? n) (< n (expt 2 30)) (> n (- (expt 2 30))))))

  ;; This function defines whether a constant must be built or not.
  ;; This functions answers #t to constants that may just appear 
  ;; as constants in VM Scheme.
  (define vm:literal-constant?
    (one-of (all-of zodiac:number? (compose vm:fixnum? zodiac:read-object))
	    (all-of zodiac:list? (compose null? zodiac:read-object))
	    zodiac:boolean?
	    zodiac:char?
	    zodiac:void?
	    zodiac:undefined?))

  )

#|

(define vm:vm->sexp
  (lambda (ast)
    (cond
     [(vm:sequence? ast) `(sequence ,@(map vm:vm->sexp (vm:sequence-vals ast)))]
     [(vm:if? ast) `(if ,(vm:vm->sexp (vm:if-test ast))
			,(vm:vm->sexp (vm:if-then ast))
			,(vm:vm->sexp (vm:if-else ast)))]
     [(vm:void? ast) `(void ,(vm:vm->sexp (vm:void-val ast)))]
     [(vm:return? ast) `(return ,(vm:vm->sexp (vm:return-val ast)))]
     [(vm:tail-apply? ast) `(tail-apply ,(vm:vm->sexp (vm:tail-apply-closure ast))
					(argc ,(vm:tail-apply-argc ast))
					(prim ,(vm:tail-apply-prim ast)))]
     [(vm:tail-call? ast) `(tail-call ,label (set-env? ,(vm:tail-call-set-env? ast)))]
     [(vm:continue? ast) '(continue)]
     [(vm:set!? ast) `(set! ,@(map (lambda (v)
				     `(,(car v) ,(vm:vm->sexp (cdr v))))
				   (vm:set!-vars ast))
			    ,(vm:vm->sexp (vm:set!-val ast)))]
     [(vm:generic-args? ast) 
      `(generic-args (tail? ,(vm:generic-args-tail? ast))
		     (prim ,(vm:generic-args-prim ast))
		     (vals ,@(map vm:vm->sexp (vm:generic-args-vals ast))))]
     [(vm:args? ast)
      `(args (type ,(vm:args-type ast)) (vals ,@(map vm:vm->sexp (vm:args-vals ast))))]
     [(vm:struct? ast)
      (let ([super (vm:struct-super ast)])
	`(struct ,(vm:struct-type ast)
		 ,(if super (vm:vm->sexp (vm:struct-super ast)) #f)
		 ,(vm:struct-fields ast)))]
     [(vm:alloc? ast)
      `(alloc ,(rep->sexp (vm:alloc-type ast)))]
     
     [(vm:build-constant? ast)
      `(build-constant ,(zodiac:read-object (vm:build-constant-text ast)))]
     [(vm:make-procedure-closure? ast)
      `(make-procedure-closure ,(vm:vm->sexp (vm:make-closure-closure ast))
			       ,(vm:make-closure-min-arity ast)
			       ,(vm:make-closure-max-arity ast))]
     [(vm:apply? ast)
      `(apply ,(vm:vm->sexp (vm:apply-closure ast))
	      (argc ,(vm:apply-argc ast))
	      (known? ,(vm:apply-known? ast))
	      (multi? ,(vm:apply-multi? ast))
	      (prim ,(vm:apply-prim ast)))]
     [(vm:call? ast)
      `(call ,(vm:call-label ast))]
     [(vm:global-varref? ast)
      `(global-varref ,(vm:global-varref-var ast))]
     [(vm:local-varref? ast)
      `(local-varref ,(vm:local-varref-var ast))]
     [(vm:static-varref? ast)
      `(static-varref ,(vm:static-varref-var ast))]
     [(vm:primitive-varref? ast) 
      `(primitive-varref ,(vm:primitive-varref-var ast))]
     [(vm:struct-ref? ast)
      `(struct-ref ,(vm:struct-ref-field ast)
		   ,(vm:vm->sexp (vm:struct-ref-var ast)))]
     [(vm:deref? ast)
      `(deref ,(vm:vm->sexp (vm:deref-var ast)))]
     [(vm:ref? ast)   
      `(ref ,(vm:vm->sexp (vm:ref-var ast)))] 
     [(vm:immediate? ast)
      (let ([text (vm:immediate-text ast)])
	`(immediate ,(cond [(number? text)
			    `(label ,text)]
			   [(zodiac:read? text)
			    (zodiac:read-object text)]
			   [else (error 'vm:vm->sexp "~a bad immediate text" text)])))]
     [else
      (error 'vm:vm->sexp "~a not supported" ast)])))
|#
