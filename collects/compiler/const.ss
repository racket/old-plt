;; const.ss
;; constant construction code generator
;; (c) 1996-7 Sebastian Good

(define const:symbol-table (make-hash-table))
(define const:symbol-counter 0)
(define const:number-table (make-hash-table))

(define (const:init-tables)
  (set! const:symbol-table (make-hash-table))
  (set! const:symbol-counter 0)
  (set! const:number-table (make-hash-table)))

(define-values (const:the-per-load-statics-table
		const:per-load-statics-table?)
  (let-struct const:per-load-statics-table ()
     (values (make-const:per-load-statics-table)
	     const:per-load-statics-table?)))

;; we need to make this in a-normalized, analyzed form from the beginning
(define compiler:add-const!
  (lambda (code attr)
    (let* ([var (gensym 'const)]
	   [sv (zodiac:make-top-level-varref/bind 
		(zodiac:zodiac-origin code)
		(zodiac:zodiac-start code)
		(zodiac:zodiac-finish code)
		(make-empty-box) 
		var
		(box '()))]
	   [def (zodiac:make-define-values-form 
		 (zodiac:zodiac-origin code)
		 (zodiac:zodiac-start code)
		 (zodiac:zodiac-finish code)
		 (make-empty-box) (list sv) code)])
	  	   
      (set-annotation! sv (varref:empty-attributes))
      (varref:add-attribute! sv varref:static)
      (varref:add-attribute! sv attr)
      (if (eq? attr varref:per-load-static)
	  (begin
	    (set! compiler:per-load-static-list
		  (cons var compiler:per-load-static-list)) 
	    (set! compiler:local-per-load-define-list 
		  (cons def compiler:local-per-load-define-list)))
	  (begin
	    (set! compiler:static-list (cons var compiler:static-list))
	    (set! compiler:local-define-list (cons def compiler:local-define-list))))
      sv)))

(define compiler:get-symbol-const!
  (lambda (ast sym)
    (hash-table-get 
     const:symbol-table
     sym
     (lambda ()
       (let ([sv (zodiac:make-top-level-varref/bind 
		  (and ast (zodiac:zodiac-origin ast))
		  (and ast (zodiac:zodiac-start ast))
		  (and ast (zodiac:zodiac-finish ast))
		  (make-empty-box) 
		  (string->symbol (number->string const:symbol-counter))
		  (box '()))])

	 (set! const:symbol-counter (add1 const:symbol-counter))

	 (set-annotation! sv (varref:empty-attributes))
	 (varref:add-attribute! sv varref:symbol)

	 (hash-table-put! const:symbol-table sym sv)
	 sv)))))

(define compiler:re-quote 
  (lambda (ast)
    (zodiac:make-quote-form (zodiac:zodiac-origin ast)
			    (zodiac:zodiac-start ast)
			    (zodiac:zodiac-finish ast)
			    (make-empty-box)
			    ast)))

; [make this in analyzed form...]
(define compiler:make-const-constructor
  (lambda (ast constructor-name args)
    (let* ([v (zodiac:make-top-level-varref/bind
	       (zodiac:zodiac-origin ast)
	       (zodiac:zodiac-start ast)
	       (zodiac:zodiac-finish ast)
	       (make-empty-box) 
	       constructor-name
	       (box '()))]
	   [app  (zodiac:make-app 
		  (zodiac:zodiac-origin ast)
		  (zodiac:zodiac-start ast)
		  (zodiac:zodiac-finish ast)
		  (make-empty-box)
		  v
		  args)])
      (set-annotation! v (varref:empty-attributes))
      (varref:add-attribute! v varref:primitive)
      (set-annotation! app (make-app #f #t))
      (block:register-max-arity! s:file-block (length args))
      (compiler:add-global-varref! v)
      (compiler:add-primitive-varref! v)
      app)))

(define compiler:construct-const-code!
  (lambda (ast known-immutable?)
    (cond
      ; base case - constant does not have to be built
      [(vm:literal-constant? ast) (compiler:re-quote ast)]
      
      ; a box has a constant inside it to mess with, yet it's
      ; still a scalar
      [(zodiac:box? ast)
       (compiler:add-const! (compiler:make-const-constructor
			     ast
			     '#%box
			     (list (compiler:construct-const-code!
				    (zodiac:read-object ast)
				    known-immutable?)))
			    (if known-immutable?
				varref:static
				varref:per-load-static))]

      ; Do symbols at most once:
      [(zodiac:symbol? ast)
       (let ([sym (zodiac:read-object ast)])
	 (compiler:get-symbol-const! ast sym))]
      
      ; Numbers that must be built
      [(zodiac:number? ast)
       (let ([sym (string->symbol (number->string (zodiac:read-object ast)))])
	 (hash-table-get const:number-table
			 sym
			 (lambda ()
			   (let ([num (compiler:add-const! 
				       (compiler:re-quote ast) 
				       varref:static)])
			     (hash-table-put! const:number-table sym num)
			     num))))]

      ; atomic constants that must be built
      [(zodiac:scalar? ast)
       (compiler:add-const! (compiler:re-quote ast) 
			    (if (and (zodiac:string? ast)
				     (not  known-immutable?))
				varref:per-load-static 
				varref:static))]
      
      ; lists
      [(zodiac:list? ast)
       (compiler:add-const! (compiler:make-const-constructor 
			     ast
			     '#%list
			     (map (lambda (x)
				    (compiler:construct-const-code! x known-immutable?))
				  (zodiac:read-object ast)))
			    (if  known-immutable?
				 varref:static
				 varref:per-load-static))]
      
      ; improper lists
      [(zodiac:improper-list? ast)
       (compiler:add-const! (compiler:make-const-constructor 
			     ast
			     '#%list*
			     (map (lambda (x)
				    (compiler:construct-const-code! x known-immutable?))
				  (zodiac:read-object ast)))
			    (if known-immutable?
				varref:static
				varref:per-load-static))]

      ; vectors
      [(zodiac:vector? ast)
       (compiler:add-const! (compiler:make-const-constructor 
			     ast
			     '#%vector
			     (map (lambda (x)
				    (compiler:construct-const-code! x known-immutable?))
				  (zodiac:read-object ast)))
			    (if known-immutable?
				varref:static
				varref:per-load-static))]

      [(void? ast) ; elaboration may return #<void> - should it?
       (zodiac:make-special-constant 'void)]

      [else
       (compiler:internal-error
	ast
	(format "unknown constant kind: ~a" ast))])))
