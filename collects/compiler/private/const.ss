;; constant construction code generator
;; (c) 1996-7 Sebastian Good
;; (c) 1997-8 PLT, Rice University

; Handles code-generation for constructing constants.

; Symbols and floating point numbers are handled specially,
;  in a way that allows the generated C code to be both 
;  efficient and small. 
; Other kinds of constants are constrcted by generating code
;  that is prefixed onto the beginning of the program.

(module const mzscheme
  (import (lib "unitsig.ss")
	  (lib "list.ss")
	  (lib "etc.ss"))

  (import (lib "zodiac-sig.ss" "syntax"))

  (import "sig.ss")
  (import "../sig.ss")

  (export const@)
  (define const@
    (unit/sig compiler:const^
      (import (compiler:option : compiler:option^)
	      compiler:library^
	      compiler:cstructs^
	      (zodiac : zodiac^)
	      compiler:analyze^
	      compiler:zlayer^
	      compiler:vmstructs^
	      compiler:top-level^
	      compiler:driver^)
      
      (define const:symbol-table (make-hash-table))
      (define const:symbol-counter 0)
      (define const:inexact-table (make-hash-table))
      (define const:inexact-counter 0)
      (define const:number-table (make-hash-table))

      (define (const:get-symbol-table) const:symbol-table)
      (define (const:get-symbol-counter) const:symbol-counter)
      (define (const:get-inexact-table) const:inexact-table)
      (define (const:get-inexact-counter) const:inexact-counter)

      (define vector-table (make-hash-table))

      (define compiler:static-list null)
      (define compiler:per-load-static-list null)

      (define (compiler:get-static-list) compiler:static-list)
      (define (compiler:get-per-load-static-list) compiler:per-load-static-list)

      (define (const:init-tables!)
	(set! const:symbol-table (make-hash-table))
	(set! const:symbol-counter 0)
	(set! const:inexact-table (make-hash-table))
	(set! const:inexact-counter 0)
	(set! const:number-table (make-hash-table))
	(set! compiler:static-list null)
	(set! compiler:per-load-static-list null)
	(set! vector-table (make-hash-table)))

      (define (compiler:add-per-load-static-list! var)
	(set! compiler:per-load-static-list
	      (cons var compiler:per-load-static-list)))

      (define-values (const:the-per-load-statics-table
		      const:per-load-statics-table?)
	(let-struct const:per-load-statics-table ()
		    (values (make-const:per-load-statics-table)
			    const:per-load-statics-table?)))

      ;; we need to make this in a-normalized, analyzed form from the beginning
      (define compiler:add-const!
	(lambda (code attr)
	  (let* ([var (gensym 'const)]
		 [sv (zodiac:make-top-level-varref 
		      (zodiac:zodiac-stx code)
		      (make-empty-box) 
		      var
		      (box '()))]
		 [def (zodiac:make-define-values-form 
		       (zodiac:zodiac-stx code)
		       (make-empty-box) (list sv) code)])
	    
	    (set-annotation! sv (varref:empty-attributes))
	    (varref:add-attribute! sv varref:static)
	    (varref:add-attribute! sv attr)
	    (if (eq? attr varref:per-load-static)
		(begin
		  (set! compiler:per-load-static-list
			(cons var compiler:per-load-static-list)) 
		  (compiler:add-local-per-load-define-list! def))
		(begin
		  (set! compiler:static-list (cons var compiler:static-list))
		  (compiler:add-local-define-list! def)))
	    sv)))

      (define compiler:get-special-const!
	(lambda (ast sym attrib table counter)
	  (let ([v (hash-table-get table sym (lambda () #f))])
	    (if v
		(values v counter)
		(let ([sv (zodiac:make-top-level-varref 
			   (and ast (zodiac:zodiac-stx ast))
			   (make-empty-box) 
			   (string->symbol (number->string counter))
			   (box '()))])
		  
		  (set-annotation! sv (varref:empty-attributes))
		  (varref:add-attribute! sv attrib)
		  (varref:add-attribute! sv varref:static)

		  (hash-table-put! table sym sv)
		  (values sv (add1 counter)))))))

      (define compiler:get-symbol-const!
	(lambda (ast sym)
	  (let-values ([(sv c) (compiler:get-special-const! ast sym varref:symbol
							    const:symbol-table
							    const:symbol-counter)])
	    (set! const:symbol-counter c)
	    sv)))

      (define compiler:get-inexact-real-const!
	(lambda (v ast)
	  (let ([sym (string->symbol (number->string v))])
	    (let-values ([(sv c) (compiler:get-special-const! ast sym varref:inexact
							      const:inexact-table
							      const:inexact-counter)])
	      (set! const:inexact-counter c)
	      sv))))

      (define compiler:re-quote 
	(lambda (ast)
	  (zodiac:make-quote-form (zodiac:zodiac-stx ast)
				  (make-empty-box)
				  ast)))

      ;; [make this in analyzed form...]
      (define compiler:make-const-constructor
	(lambda (ast constructor-name args)
	  (let* ([v (zodiac:make-top-level-varref
		     (zodiac:zodiac-stx ast)
		     (make-empty-box) 
		     constructor-name
		     (box '()))]
		 [app  (zodiac:make-app 
			(zodiac:zodiac-stx ast)
			(make-empty-box)
			v
			args)])
	    (set-annotation! v (varref:empty-attributes))
	    (varref:add-attribute! v varref:primitive)
	    (set-annotation! app (make-app #f #t constructor-name))
	    (block:register-max-arity! (get-s:file-block) (length args))
	    (compiler:add-global-varref! v)
	    (compiler:add-primitive-varref! v)
	    app)))

      (define ht-eol (gensym))

      (define (get-hash-id elem)
	(cond
	 [(zodiac:quote-form? elem) (let ([o (zodiac:quote-form-expr elem)])
				      (if (number? (zodiac:read-object o))
					  (zodiac:read-object o)
					  o))]
	 [else elem]))

      (define (find-immutable-vector constructor elems)
	(let ([ht (hash-table-get vector-table constructor (lambda () #f))])
	  (and ht
	       (let loop ([ht ht][l elems])
		 (if (null? l)
		     (hash-table-get ht ht-eol (lambda () #f))
		     (let ([ht (hash-table-get ht (get-hash-id (car l)) (lambda () #f))])
		       (and ht (loop ht (cdr l)))))))))
      
      (define (remember-immutable-vector constructor elems const)
	(let ([ht (hash-table-get vector-table constructor make-hash-table)])
	  (hash-table-put! vector-table constructor ht)
	  (let loop ([ht ht][l elems])
	    (if (null? l)
		(hash-table-put! ht ht-eol const)
		(let* ([hash-id (get-hash-id (car l))]
		       [htn (hash-table-get ht hash-id make-hash-table)])
		  (hash-table-put! ht hash-id htn)
		  (loop htn (cdr l)))))))

      (define (construct-vector-constant ast constructor known-immutable?)
	(let* ([elems (map (lambda (x)
			     (compiler:construct-const-code! x known-immutable?))
			   (zodiac:read-object ast))]
	       [known-immutable? (or known-immutable? (null? elems))])
	  (or (and known-immutable?
		   (find-immutable-vector constructor elems))
	      (let ([const (compiler:add-const! 
			    (compiler:make-const-constructor 
			     ast
			     constructor
			     elems)
			    (if known-immutable?
				varref:static
				varref:per-load-static))])
		(when known-immutable?
		  (remember-immutable-vector constructor elems const))
		const))))

      (define compiler:construct-const-code!
	(lambda (ast known-immutable?)
	  (cond
	   ;; base case - constant does not have to be built
	   [(vm:literal-constant? ast) (compiler:re-quote ast)]
	   
	   ;; a box has a constant inside it to mess with, yet it's
	   ;; still a scalar
	   [(box? (zodiac:read-object ast))
	    (compiler:add-const! (compiler:make-const-constructor
				  ast
				  '#%box
				  (list (compiler:construct-const-code!
					 (zodiac:read-object ast)
					 known-immutable?)))
				 (if known-immutable?
				     varref:static
				     varref:per-load-static))]

	   ;; Do symbols at most once:
	   [(symbol? (zodiac:read-object ast))
	    (let ([sym (zodiac:read-object ast)])
	      (compiler:get-symbol-const! ast sym))]
	   
	   ;; Numbers that must be built
	   [(number? (zodiac:read-object ast))
	    (let ([n (zodiac:read-object ast)])
	      (if (and (inexact? n) (eqv? 0 (imag-part n))
		       (not (member n '(+inf.0 -inf.0 +nan.0))))
		  (compiler:get-inexact-real-const! n ast)
		  (let ([sym (string->symbol (number->string n))])
		    (hash-table-get const:number-table
				    sym
				    (lambda ()
				      (let ([num (compiler:add-const! 
						  (compiler:re-quote ast) 
						  varref:static)])
					(hash-table-put! const:number-table sym num)
					num))))))]

	   ;; lists
	   [(stx-list? (zodiac:zodiac-stx ast))
	    (construct-vector-constant ast 'list known-immutable?)]
	   
	   ;; improper lists
	   [(pair? (zodiac:read-object ast))
	    (construct-vector-constant ast 'list* known-immutable?)]

	   ;; vectors
	   [(vector? (zodiac:read-object ast))
	    (construct-vector-constant ast 'vector known-immutable?)]

	   ;; elaboration may return #<void> - should it?
	   [(or (void? ast)
		(void? (zodiac:read-object ast)))
	    (zodiac:make-special-constant 'void)]

	   ;; other atomic constants that must be built
	   [else
	    (compiler:add-const! (compiler:re-quote ast) 
				 (if (and (string? (zodiac:read-object ast))
					  (not  known-immutable?))
				     varref:per-load-static 
				     varref:static))]))))))
