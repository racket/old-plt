;; Zodiac compatibility layer,
;;  for programs that used to manipulate the
;;  output of zodiac elaboration.

(module zodiac-unit mzscheme
  (import (lib "unitsig.ss"))
  (import "kerncase.ss")

  (import "zodiac-sig.ss")

  (export zodiac@)

  (define zodiac@
    (unit/sig zodiac^
      (import)

      (define (stx-bound-assq ssym l)
	(ormap (lambda (p)
		 (and (bound-identifier=? ssym (car p))
		      p))
	       l))

      ;; Back boxes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define-struct secure-box (value))

      (define init-value-list '())

      (define register-initial-value
	(lambda (index value-thunk)
	  (set! init-value-list
		(append init-value-list
			(list value-thunk)))))

      (define make-initial-value-vector
	(lambda ()
	  (let ((v (make-vector current-vector-size uninitialized-flag)))
	    (let loop ((index 0) (inits init-value-list))
	      (unless (null? inits)
		(vector-set! v index ((car inits)))
		(loop (add1 index) (cdr inits))))
	    v)))

      (define make-empty-back-box
	(lambda ()
	  (make-secure-box (make-initial-value-vector))))

      (define current-vector-size 2)
      
      (define next-client-count
	(let ((count -1))
	  (lambda ()
	    (set! count (add1 count))
	    (when (>= count current-vector-size)
	      (set! current-vector-size (* 2 current-vector-size)))
	    count)))

      (define-struct uninitialized-back ())
      (define uninitialized-flag (make-uninitialized-back))
      
      (define getters-setters
	(lambda (index)
	  (values
	   (lambda (back)		; getter
	     (let ((v (secure-box-value back)))
	       (with-handlers
		   ((exn:application:mismatch?
		     (lambda (exception)
		       (vector-ref (extend-back-vector back) index))))
		 (let ((value (vector-ref v index)))
		   (if (uninitialized-back? value)
		       (let ((correct-value
			      ((list-ref init-value-list index))))
			 (vector-set! v index correct-value)
			 correct-value)
		       value)))))
	   (lambda (back value)		; setter
	     (let ((v (secure-box-value back)))
	       (with-handlers
		   ((exn:application:mismatch?
		     (lambda (exception)
		       (vector-set! (extend-back-vector back) index value))))
		 (vector-set! v index value)))))))
  
      (define register-client
	(lambda (client-name default-initial-value-thunk)
	  (let ((index (next-client-count)))
	    (register-initial-value index default-initial-value-thunk)
	    (getters-setters index))))
      
      (define extend-back-vector
	(lambda (back-box)
	  (let ((v (secure-box-value back-box)))
	    (let ((new-v (make-initial-value-vector)))
	      (let loop ((n (sub1 (vector-length v))))
		(when (>= n 0)
		  (vector-set! new-v n (vector-ref v n))
		  (loop (sub1 n))))
	      (set-secure-box-value! back-box new-v)
	      new-v))))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (mk-back) (make-empty-back-box))

      (define (get-slot stx table)
	(let ([l (hash-table-get table (syntax-e stx) (lambda () null))])
	  (let ([s (ormap (lambda (b)
			    (and (module-identifier=? stx (car b))
				 (cdr b)))
			  l)])
	    (if s
		s
		(let ([s (box #f)])
		  (hash-table-put! table (syntax-e stx) (cons (cons stx s) l))
		  s)))))

      (define (let-s->z mk-let rec? stx env loop)
	(syntax-case stx ()
	  [(_ ([vars rhs] ...) . body)
	   (let* ([varses (syntax->list (syntax (vars ...)))]
		  [rhses (syntax->list (syntax (rhs ...)))]
		  [z:varses (apply
			     append
			     (map (lambda (vars)
				    (map (lambda (var)
					   (make-binding
					    stx
					    (mk-back)
					    (gensym)
					    (syntax-e var)))
					 (syntax->list vars)))
				  varses))]
		  [body-env (apply
			     append
			     (map (lambda (z:vars vars)
				    (map (lambda (z:var var)
					   (cons
					    var
					    z:var))
					 z:vars
					 (syntax->list vars)))
				  z:varses
				  varses))])
	     (mk-let
	      stx
	      (mk-back)
	      z:varses
	      (map (lambda (rhs)
		     (loop rhs (if rec? body-env env)))
		   rhses)
	      (loop (syntax body) body-env)))]))
      
      (define (args-s->z env args)
	(let-values ([(maker ids)
		      (syntax-case args ()
			[id
			 (identifier? (syntax id))
			 (values make-sym-arglist
				 (list (syntax id)))]
			[(id ...)
			 (values make-list-arglist (syntax->list args))]
			[_else (let loop ([args args])
				 (values make-ilist-arglist
					 (syntax-case args ()
					   [id (identifier? args) (list args)]
					   [(id . rest)
					    (cons (syntax id) (loop (syntax rest)))])))])])
	  (let ([bindings
		 (map (lambda (id)
			(make-binding
			 id
			 (mk-back)
			 (gensym)
			 (syntax-e id)))
		      ids)])
	    (values
	     (append (map cons ids bindings) env)
	     (maker bindings)))))

      (define (syntax->zodiac stx)
	(define slot-table (make-hash-table))
	
	(if (eof-object? stx)
	    stx
	    (let loop ([stx stx][env null][trans? #f])
	      (kernel-syntax-case stx trans?
		[id
		 (identifier? stx)
		 (let ([a (stx-bound-assq stx env)])
		   (if a
		       ;; Lexical reference:
		       (make-bound-varref
			stx
			(mk-back)
			(syntax-e stx)
			(cdr a))
		       ;; Top-level (or module) reference:
		       (make-top-level-varref
			stx
			(mk-back)
			(syntax-e stx)
			(get-slot stx slot-table))))]
		

		[(#%unbound . id)
		 ;; Top-level (or module) reference:
		 (make-top-level-varref
		  stx
		  (mk-back)
		  (syntax-e (syntax id))
		  (get-slot (syntax id) slot-table))]

		[(#%datum . val)
		 (let ([val (syntax val)])
		   (make-quote-form
		    stx
		    (mk-back)
		    (make-read
		     val)))]

		[(define-values names rhs)
		 (make-define-values-form
		  stx
		  (mk-back)
		  (map (lambda (stx)
			 (make-top-level-varref
			  stx
			  (mk-back)
			  (syntax-e stx)
			  (get-slot stx slot-table)))
		       (syntax->list (syntax names)))
		  (loop (syntax rhs) null #f))]
		
		[(define-syntax name rhs)
		 (make-define-syntax-form
		  stx
		  (mk-back)
		  (syntax name)
		  (loop (syntax rhs) null #t))]
		
		[(module name init-import . body)
		 (make-module-form
		  stx
		  (mk-back)
		  (syntax name)
		  (syntax init-import)
		  (map (lambda (x)
			 (loop x env trans?))
		       (syntax->list (syntax body))))]

		[(import i)
		 (make-import/export-form
		  stx
		  (mk-back))]
		[(import-for-syntax i ...)
		 (make-import/export-form
		  stx
		  (mk-back))]
		[(export i ...)
		 (make-import/export-form
		  stx
		  (mk-back))]
		[(export-indirect i ...)
		 (make-import/export-form
		  stx
		  (mk-back))]

		[(quote expr)
		 (make-quote-form
		  stx
		  (mk-back)
		  (make-read (syntax expr)))]

		[(quote-syntax expr)
		 (make-quote-syntax-form
		  stx
		  (mk-back)
		  (syntax expr))]
		
		[(lambda args . body)
		 (let-values ([(env args) (args-s->z env (syntax args))])
		   (make-case-lambda-form
		    stx
		    (mk-back)
		    (list args)
		    (list (loop (syntax (begin . body)) env trans?))))]
		[(case-lambda [args . body] ...)
		 (let-values ([(envs argses)
			       (let ([es+as
				      (map
				       (lambda (args)
					 (let-values ([(env args) (args-s->z env args)])
					   (cons env args)))
				       (syntax->list (syntax (args ...))))])
				 (values
				  (map car es+as)
				  (map cdr es+as)))])
		   (make-case-lambda-form
		    stx
		    (mk-back)
		    argses
		    (map (lambda (env body)
			   (with-syntax ([body body])
			     (loop (syntax (begin . body)) env trans?)))
			 envs
			 (syntax->list (syntax (body ...))))))]

		[(let-values . _)
		 (let-s->z make-let-values-form #f stx env
			   (lambda (b env) (loop b env trans?)))]
		[(letrec-values . _)
		 (let-s->z make-letrec-values-form #t stx env
			   (lambda (b env) (loop b env trans?)))]
		
		[(set! var rhs)
		 (make-set!-form
		  stx
		  (mk-back)
		  (loop (syntax var) env trans?)
		  (loop (syntax rhs) env trans?))]
		
		[(begin . exprs)
		 (make-begin-form
		  stx
		  (mk-back)
		  (map (lambda (x)
			 (loop x env trans?))
		       (syntax->list (syntax exprs))))]
		
		[(begin0 . exprs)
		 (make-begin0-form
		  stx
		  (mk-back)
		  (map (lambda (x)
			 (loop x env trans?))
		       (syntax->list (syntax exprs))))]

		[(if test then else)
		 (make-if-form
		  stx
		  (mk-back)
		  (loop (syntax test) env trans?)
		  (loop (syntax then) env trans?)
		  (loop (syntax else) env trans?))]

		[(with-continuation-mark k v body)
		 (make-with-continuation-mark-form
		  stx
		  (mk-back)
		  (loop (syntax k) env trans?)
		  (loop (syntax v) env trans?)
		  (loop (syntax body) env trans?))]

		[(#%app func arg ...)
		 (make-app
		  stx
		  (mk-back)
		  (loop (syntax func) env trans?)
		  (map
		   (lambda (arg)
		     (loop arg env trans?))
		   (syntax->list (syntax (arg ...)))))]
		
		[(struct (name sup) fields)
		 (make-struct-form
		  stx
		  (mk-back)
		  (syntax name)
		  (loop (syntax sup) env trans?)
		  (map syntax-e (syntax->list (syntax fields))))]
		[(struct name fields)
		 (make-struct-form
		  stx
		  (mk-back)
		  (syntax name)
		  #f
		  (map syntax-e (syntax->list (syntax fields))))]
		
		[_else
		 (error 'syntax->zodiac
			"unrecognized expression form: ~e"
			(syntax->datum stx))]))))

      
      (define (zodiac->syntax x)
	(let loop ([x x])
	  (cond
	   [(read? x)
	    (zodiac-stx x)]

	   [(top-level-varref? x)
	    (zodiac-stx x)]
	   [(bound-varref? x)
	    ;; An stx object is getting gensymmed here!
	    (datum->syntax (binding-var (bound-varref-binding x)) #f #f)]
	   
	   [(app? x)
	    (with-syntax ([fun (loop (app-fun x))]
			  [args (map loop (app-args x))])
	      (syntax (#%app fun . args)))]

	   [(struct-form? x)
	    (let ([super (and (struct-form-super x)
			      (loop (struct-form-super x)))])
	      (with-syntax ([name (datum->syntax (struct-form-type x) #f #f)]
			    [fields (map (lambda (x)
					   (datum->syntax x #f #f))
					 (struct-form-fields x))])
		(with-syntax ([name+super (if (syntax-e (syntax super))
					      (with-syntax ([super super])
						(syntax (name super)))
					      (syntax name))])
		  (syntax (struct name+super fields)))))]

	   [(if-form? x)
	    (with-syntax ([test (loop (if-form-test x))]
			  [then (loop (if-form-then x))]
			  [else (loop (if-form-else x))])
	      (syntax (if test then else)))]

	   [(quote-form? x)
	    (with-syntax ([v (zodiac-stx (quote-form-expr x))])
	      (syntax (quote v)))]
	   [(quote-syntax-form? x)
	    (with-syntax ([v (quote-syntax-form-expr x)])
	      (syntax (quote-syntax v)))]

	   [(begin-form? x)
	    (with-syntax ([body (map loop (begin-form-bodies))])
	      (syntax (begin . body)))]
	   [(begin0-form? x)
	    (with-syntax ([body (map loop (begin-form-bodies))])
	      (syntax (begin0 . body)))]

	   [(let-values-form? x)
	    (with-syntax ([(vars ...)
			   (map (lambda (vars)
				  (map binding-var vars))
				(let-values-form-vars x))]
			  [(val ...)
			   (map loop (let-values-form-vals x))]
			  [body (loop (let-values-form-body x))])
	      (syntax (let-values ([vars val] ...) body)))]
	   [(letrec-values-form? x)
	    (with-syntax ([(vars ...)
			   (map (lambda (vars)
				  (map binding-var vars))
				(letrec-values-form-vars x))]
			  [(val ...)
			   (map loop (letrec-values-form-vals x))]
			  [body (loop (letrec-values-form-body x))])
	      (syntax (letrec-values ([vars val] ...) body)))]
	   
	   [(define-values-form? x)
	    (with-syntax ([vars (map zodiac-stx (define-values-form-vars x))]
			  [val (loop (define-values-form-val x))])
	      (syntax (define-values vars val)))]
	   
	   [(set!-form? x)
	    (with-syntax ([var (loop (set!-form-var x))]
			  [val (loop (set!-form-val x))])
	      (syntax (set! var val)))]
	   
	   [(case-lambda-form? x)
	    (with-syntax ([(args ...)
			   (map (lambda (args)
				  (cond
				   [(sym-arglist? args)
				    (datum->syntax (binding-var (car (arglist-vars args)))
						   #f #f)]
				   [(list-arglist? args)
				    (map (lambda (var)
					   (datum->syntax (binding-var var) #f #f))
					 (arglist-vars args))]
				   [(ilist-arglist? args)
				    (let loop ([vars (arglist-vars args)])
				      (let ([id (datum->syntax (binding-var (car vars)) #f #f)])
					(if (null? (cdr vars))
					    id
					    (cons id (loop (cdr vars))))))]))
				(case-lambda-form-args x))]
			  [(body ...)
			   (map loop (case-lambda-form-bodies x))])
	      (syntax (case-lambda [args body] ...)))]

	   [(with-continuation-mark-form? x)
	    (with-syntax ([key (loop (with-continuation-mark-form-key x))]
			  [val (loop (with-continuation-mark-form-val x))]
			  [body (loop (with-continuation-mark-form-body x))])
	      (syntax (with-continuation-mark key val body)))]

	   [else (error 'zodiac->syntax
			"unknown zodiac record type: ~e"
			x)])))

      (define (zodiac-origin z) z)

      (define (origin-who z)
	(if (syntax-original? (zodiac-stx z))
	    'source
	    'macro))

      (define (origin-how z)
	(syntax-property (zodiac-stx z) 'origin))
      
      (define (zodiac-start z) z)
      (define (zodiac-finish z) z)

      (define (location-line z)
	(syntax-line (zodiac-stx z)))
      
      (define (location-column z)
	(syntax-column (zodiac-stx z)))

      (define (location-file z)
	(syntax-source (zodiac-stx z)))

      (define (read-object z)
	(syntax-e (zodiac-stx z)))

      (define (structurize-syntax sexp)
	(make-read (datum->syntax sexp #f #f)))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define eof? eof-object?)

      (define-struct zodiac (stx))
      (define-struct (read struct:zodiac) ())

      (define-struct (parsed struct:zodiac) (back))

      (define-struct (varref struct:parsed) (var))

      (define-struct (top-level-varref struct:varref) (slot))
      (define (create-top-level-varref z var slot)
	(make-top-level-varref (zodiac-stx z) var slot))

      (define-struct (bound-varref struct:varref) (binding))
      (define (create-bound-varref z var binding)
	(make-bound-varref (zodiac-stx z) var binding))

      (define lexical-varref? bound-varref?)
      (define make-lexical-varref make-bound-varref)
      (define create-lexical-varref create-bound-varref)

      (define-struct (binding struct:parsed) (var orig-name))
      (define (create-binding z var orig-name)
	(make-binding (zodiac-stx z) var orig-name))

      (define lexical-binding? binding?)
      (define make-lexical-binding make-binding)
      (define create-lexical-binding create-binding)


      (define-struct (app struct:parsed) (fun args))
      (define (create-app z fun args)
	(make-app (zodiac-stx z) fun args))

      (define-struct struct-form (type super fields))
      (define (create-struct-form z type super fields)
	(make-struct-form (zodiac-stx z) type super fields))

      
      (define-struct (if-form struct:parsed) (test then else))
      (define (create-if-form z test then else)
	(make-if-form (zodiac-stx z) test then else))

      (define-struct (quote-form struct:parsed) (expr))
      (define (create-quote-form z expr)
	(make-quote-form (zodiac-stx z) expr))

      (define-struct (begin-form struct:parsed) (bodies))
      (define (create-begin-form z bodies)
	(make-begin-form (zodiac-stx z) bodies))

      (define-struct (begin0-form struct:parsed) (bodies))
      (define (create-begin0-form z bodies)
	(make-begin0-form (zodiac-stx z) bodies))

      (define-struct (let-values-form struct:parsed) (vars vals body))
      (define (create-let-values-form z vars vals body)
	(make-let-values-form (zodiac-stx z) vars vals body))

      (define-struct (letrec-values-form struct:parsed) (vars vals body))
      (define (create-letrec-values-form z vars vals body)
	(make-letrec-values-form (zodiac-stx z) vars vals body))

      (define-struct (define-values-form struct:parsed) (vars val))
      (define (create-define-values-form z vars val)
	(make-define-values-form (zodiac-stx z) vars val))

      (define-struct (set!-form struct:parsed) (var val))
      (define (create-set!-form z var val)
	(make-set!-form (zodiac-stx z) var val))

      (define-struct (case-lambda-form struct:parsed) (args bodies))
      (define (create-case-lambda-form z args bodies)
	(make-case-lambda-form (zodiac-stx z) args bodies))

      (define-struct (with-continuation-mark-form struct:parsed) (key val body))
      (define (create-with-continuation-mark-form z key val body)
	(make-with-continuation-mark-form (zodiac-stx z) key val body))

      (define-struct (quote-syntax-form struct:parsed) (expr))
      (define (create-quote-syntax-form z expr)
	(make-quote-syntax-form (zodiac-stx z) expr))

      (define-struct (define-syntax-form struct:parsed) (name expr))
      (define (create-define-syntax-form z name expr)
	(make-define-syntax-form (zodiac-stx z) name expr))

      (define-struct (module-form struct:parsed) (name init-import body))
      (define (create-module-form z name init-import body)
	(make-module-form (zodiac-stx z) name init-import body))

      (define-struct (import/export-form struct:parsed) ())
      (define (create-import/export-form z)
	(make-import/export-form (zodiac-stx z)))

      (define-struct arglist (vars))
      (define-struct sym-arglist ())
      (define-struct list-arglist ())
      (define-struct ilist-arglist ()))))
