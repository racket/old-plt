
;; Implements a source-to-source optimizer

;; The src-to-src transformation currently drops
;;  properties, which is bad. The 'mzc-cffi,
;;  'method-arity-error, and 'inferred-name properties are
;;  specially preserved for `lambda' expressions.

(module src2src mzscheme
  (require (lib "class.ss")
	   (lib "class100.ss")
	   (lib "kerncase.ss" "syntax")
	   (lib "primitives.ss" "syntax")
	   (lib "etc.ss")
	   (lib "list.ss"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Optimizer
  ;; classes representing syntax with methods for optimization steps
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define foldable-prims '(void
			   + - * / arithmetic-shift
			   < <= = > >=
			   number? positive? negative? zero?
			   real? complex?
			   string-ref))

  (define effectless-prims '(list list* cons vector))

  ;; The following primitives either invoke functions, or
  ;;  install functions that can be used later.
  (define (non-valueable-prims) (procedure-calling-prims))

  (define (keep-mzc-property stx-out stx)
    (let ([v (syntax-property stx 'mzc-cffi)]
	  [v2 (syntax-property stx 'method-arity-error)]
	  [v3 (syntax-property stx 'inferred-name)])
      (let ([stx-out2 (if v
			  (syntax-property stx-out 'mzc-cffi v)
			  stx-out)])
	(let ([stx-out3 (if v2
			    (syntax-property stx-out2 'method-arity-error v2)
			    stx-out2)])
	  (if v3
	      (syntax-property stx-out3 'inferred-name v3)
	      stx-out3)))))
  
  (define-struct context (need indef))
  ;; need = #f => don't need  the value
  ;; need = 'bool => need bool only
  ;; need = 'all => need exact result

  ;; indef = list of binding%s

  (define (need-all ctx)
    (if (eq? 'all (context-need ctx))
	ctx
	(make-context 'all (context-indef ctx))))
  (define (need-none ctx)
    (if (eq? 'none (context-need ctx))
	ctx
	(make-context 'none (context-indef ctx))))
  (define (need-bool ctx)
    (make-context 'bool (context-indef ctx)))

  (define exp%
    (class100 object% (stx)
      (private-field
       [src-stx stx] ;; generally only useful for src loc info
       [known-value #f])
      (public
	;; resets known-value computation, use counts, etc.
	[reset-varflags
	 (lambda ()
	   (set! known-value #f)
	   (for-each (lambda (e) (send e reset-varflags)) (sub-exprs)))]

	;; accumulates known-value mappings, use counts on bindings, etc.;
	;; assumes varflags are reset
	[set-known-values
	 (lambda ()
	   (for-each (lambda (e) (send e set-known-values)) (nonbind-sub-exprs)))]

	;; for each reference of a binding in the exp, drop one use
	[drop-uses
	 (lambda ()
	   (for-each (lambda (e) (send e drop-uses)) (nonbind-sub-exprs)))]

	;; any side-effects might be in this expression?
	;; (return #t if unsure)
	[no-side-effect? (lambda () (andmap (lambda (e) (send e no-side-effect?))
					    (nonbind-sub-exprs)))]

	;; arity is a number or 'unknown
	[get-result-arity (lambda () 'unknown)]

	;; gets all subexpressions, including binding%s for lambda, etc.
	[sub-exprs (lambda () (append (bind-sub-exprs) (nonbind-sub-exprs)))]
	;; just the binding%s
	[bind-sub-exprs (lambda () null)]
	;; all subexpressions that aren't binding%s
	[nonbind-sub-exprs (lambda () null)]

	;; some default implementations map over nonbind-sub-exprs, 
	;; the install the results with this method
	[set-nonbind-sub-exprs (lambda (x) (void))]

	;; valueable means that evaluating the expression can't access
	;;  a variable before it is initialized or mutate a
	;;  variable. It's used, for example, on the RHSs of a letrec
	;;  to determine known bindings.
	[valueable? (lambda () (andmap (lambda (x) (send x valueable?)) (nonbind-sub-exprs)))]

	;; ok to duplicate or move the expression?
	;; (return #f if unsure)
	[can-dup/move? (lambda () #f)]

	;; known value is an exp%; usually only binding% objects
	;; get known-value settings
	[set-known-value (lambda (x) (set! known-value x))]

	;; finds the most-specific exp% whose value is the
	;; same this this expression's value
	[get-value (lambda () (or known-value this))])

      (private
	;; helper:
	[subexp-map!
	 (lambda (f)
	   (set-nonbind-sub-exprs (map f (nonbind-sub-exprs)))
	   this)])

      (public
	;; main optimization method:
	[simplify (lambda (ctx) 
		    (subexp-map! (lambda (x) 
				   (send x simplify (need-all ctx)))))]
	
	;; not an optimizations, but exposes info (epsecially to mzc)
	[reorganize (lambda ()
		      (subexp-map! (lambda (x)
				     (send x reorganize))))]
	;; reverses reorganize
	[deorganize (lambda ()
		      (subexp-map! (lambda (x)
				     (send x deorganize))))]

	;; substitution of lexical refs for global variables
	[global->local (lambda (env)
			 (subexp-map! (lambda (x)
					(send x global->local env))))]

	;; substitution of lexical refs for either lex or global vars
	[substitute (lambda (env)
		      (subexp-map! (lambda (x)
				     (send x substitute env))))]

	;; creates a copy, used for inling; don't try to preserve
	;;  analysis, because we'll just re-compute it
	[clone 
	 (lambda (env) 
	   (error 'clone "unimplemented: ~a" this))]

	;; gets stx object, usually for src info
	[get-stx (lambda () src-stx)]

	;; convert back to a syntax object
	[sexpr
	 (lambda ()
	   src-stx)]

	;; list of body exprs (avoids redundant `begin', just for
	;; readability)
	[body-sexpr
	 (lambda ()
	   (list (sexpr)))])
      (sequence (super-init))))

  (define (get-sexpr o) (send o sexpr))
  (define (get-body-sexpr o) (send o body-sexpr))

  (define-struct bucket (mutated? inited-before-use?))

  (define (global-bucket table stx)
    (let ([l (hash-table-get table (syntax-e stx) (lambda () null))])
      (let ([s (ormap (lambda (b)
			(and (module-identifier=? stx (car b))
			     (cdr b)))
		      l)])
	(if s
	    s
	    (let ([s (make-bucket #f #f)])
	      (hash-table-put! table (syntax-e stx) (cons (cons stx s) l))
	      s)))))

  (define-struct tables (global-ht et-global-ht))

  (define global%
    (class100 exp% (-stx -trans? -tables -needs-top?)
      (private-field
       [stx -stx]
       [trans? -trans?]
       [tables -tables]
       [needs-top? -needs-top?]
       [mbind #f]
       [bucket (global-bucket ((if trans? tables-et-global-ht tables-global-ht) tables) stx)])
      (private
	[get-mbind!
	 (lambda ()
	   (unless mbind
	     (set! mbind ((if trans?
			      identifier-transformer-binding 
			      identifier-binding)
			  stx))))])
      (public
	[orig-name
	 (lambda ()
	   (get-mbind!)
	   (if (pair? mbind)
	       (cadr mbind)
	       (syntax-e stx)))]

	[is-kernel?
	 (lambda ()
	   (get-mbind!)
	   (and (pair? mbind)
		(eq? (car mbind) '#%kernel)))]

	[is-trans? (lambda () trans?)]

	[is-mutated? (lambda () (bucket-mutated? bucket))])

      (override
	[no-side-effect?
	 ;; If not built in, could raise exn
	 (lambda () (is-kernel?))]

	[get-result-arity (lambda () 1)]

	[valueable? (lambda () (or (bucket-inited-before-use? bucket)
				   (is-kernel?)))]

	[can-dup/move? (lambda () (valueable?))]

	[clone (lambda (env) (make-object global% stx trans? tables needs-top?))]

	[global->local (lambda (env)
			 (or (ormap (lambda (e)
				      (and (module-identifier=? (car e) stx)
					   (make-object ref% stx (cdr e))))
				    env)
			     this))]

	[sexpr
	 (lambda ()
	   (if needs-top?
	       (with-syntax ([stx stx])
		 (syntax (#%top . stx)))
	       stx))])
      (public
	[set-mutated (lambda () (set-bucket-mutated?! bucket #t))]
	[set-inited (lambda () (set-bucket-inited-before-use?! bucket #t))])

      (sequence
	(super-init stx))))

  (define binding% 
    (class100 exp% (-name -always-inited?)
      (private-field
       [name -name]
       [always-inited? -always-inited?]

       [value #f]
       [used 0]
       [mutated? #f]
       [inited? always-inited?])

      (public
	[is-used? (lambda () (positive? used))]
	[is-mutated? (lambda () mutated?)]
	[is-inited? (lambda () inited?)]

	[get-use-count (lambda () used)]

	[set-mutated
	 (lambda ()
	   (set! mutated? #t))]
	[set-inited
	 (lambda ()
	   (set! inited? #t))]
	[set-value (lambda (v) (set! value v))]

	[clone-binder (lambda (env) 
			(make-object binding% (datum->syntax-object
					       #f
					       (gensym (syntax-e name))
					       name
					       always-inited?)))])

      (override
	[reset-varflags
	 (lambda ()
	   (set! used 0)
	   (set! mutated? #f)
	   (set! inited? always-inited?))]
	[set-known-values
	 (lambda ()
	   (set! used (add1 used))
	   (unless inited?
	     (set! mutated? #t)))]

	[valueable? (lambda () (and inited? (not mutated?)))]

	[drop-uses (lambda () (set! used (sub1 used)))]

	[get-value (lambda () 
		     (and (not mutated?)
			  value
			  (send value get-value)))]

	[sexpr
	 (lambda ()
	   ;; `(==lexical== ,name ,used ,mutated? ,inited? ,(get-value))
	   name
	   )])
      (public
	[orig-name
	 (lambda ()
	   (syntax-e name))])
      (sequence
	(super-init name))))

  (define ref% 
    (class100 exp% (-stx lexical-var)
      (private-field
       [stx -stx]
       [binding lexical-var]) 
      (public
	[is-used? (lambda () (send binding is-used?))]
	[is-mutated? (lambda () (send binding is-mutated?))]
	[is-inited? (lambda () (send binding is-inited?))]

	[get-use-count (lambda () (send binding get-use-count))]

	[set-mutated (lambda () (send binding set-mutated))]
	[set-inited (lambda () (send binding set-inited))]
	[set-value (lambda (v) (send binding set-value v))])

      (override
	[set-known-values
	 (lambda () (send binding set-known-values))]

	[valueable? (lambda () (send binding valueable?))]
	[can-dup/move? (lambda () (valueable?))]

	[drop-uses (lambda () (send binding drop-uses))]

	[get-result-arity (lambda () 1)]

	[get-value (lambda () (send binding get-value))]

	[simplify (lambda (ctx)
		    (if (context-need ctx)
			(let ([v (get-value)])
			  (if (and v (send v can-dup/move?))
			      (begin
				(drop-uses)
				(send v simplify ctx))
			      this))
			(begin
			  (drop-uses)
			  (make-object void% stx))))]

	[clone (lambda (env) (lookup-clone binding this env))]
	[substitute (lambda (env) (lookup-clone binding this env))]

	[sexpr (lambda () 
		 (let ([x (send binding sexpr)])
		   (datum->syntax-object
		    x
		    (syntax-e x)
		    stx)))])
      (public
	[get-binding (lambda () binding)]
	[orig-name
	 (lambda ()
	   (send binding orig-name))])
      (sequence
	(super-init stx))))

  (define begin% 
    (class100 exp% (-stx -subs)
      (private-field
       [stx -stx]
       [subs -subs])
      (override
	[nonbind-sub-exprs (lambda () subs)]
	[set-nonbind-sub-exprs (lambda (s) (set! subs s))]

	[get-result-arity (lambda ()
			    (if (null? subs)
				'unknown
				(let loop ([subs subs])
				  (if (null? (cdr subs))
				      (send (car subs) get-result-arity)
				      (loop (cdr subs))))))]

	[simplify (lambda (ctx)
		    (set! subs
			  (let loop ([subs subs])
			    (cond
			     [(null? subs) null]
			     [(null? (cdr subs))
			      (list (send (car subs) simplify ctx))]
			     [else
			      (let ([r (send (car subs) simplify (need-none ctx))]
				    [rest (loop (cdr subs))])
				(cond
				 [(send r no-side-effect?)
				  (send r drop-uses)
				  rest]
				 [(is-a? r begin%)
				  (append (send r nonbind-sub-exprs)
					  rest)]
				 [else (cons r rest)]))])))
		    (if (and (pair? subs)
			     (null? (cdr subs)))
			(car subs)
			this))]

	[clone (lambda (env) (make-object begin% 
					  stx
					  (map (lambda (x) (send x clone env)) 
					       subs)))]

	[sexpr
	 (lambda ()
	   (with-syntax ([(body ...) (body-sexpr)])
	     (syntax/loc stx (begin body ...))))]
	[body-sexpr
	 (lambda ()
	   (map get-sexpr subs))])
      (sequence
	(super-init stx))))

  (define top-def% 
    (class100 exp% (-stx -formname -varnames -expr -tables)
      (private-field
       [stx -stx]
       [formname -formname]
       [varnames -varnames]
       [expr -expr]
       [tables -tables]
       [globals #f])
      (override
	[nonbind-sub-exprs (lambda () (list expr))]
	[set-nonbind-sub-exprs (lambda (s) (set! expr (car s)))]

	[get-result-arity (lambda () 1)]

	[no-side-effect? (lambda () #f)]
	[valueable? (lambda () #f)]

	[clone (lambda (env) (make-object top-def% stx varnames 
					  (send expr clone env)))]

	[sexpr
	 (lambda ()
	   (with-syntax ([formname formname]
			 [(varname ...) varnames]
			 [rhs (get-sexpr expr)])
	     (syntax/loc stx (formname (varname ...) rhs))))])
      (public
	[get-vars (lambda () varnames)]
	[get-rhs (lambda () expr)]

	;; Like get-vars, but return global% objects, instead.
	;; Useful because the global% object has the global variable bucket info.
	[get-globals (lambda ()
		       (unless globals
			 (set! globals
			       (map (lambda (v)
				      (make-object global% v #f tables #f))
				    varnames)))
		       globals)])
      (sequence
	(super-init stx))))

  (define variable-def% 
    (class100 top-def% (-stx -varnames -expr -tables)
      (sequence
	(super-init -stx (quote-syntax define-values) -varnames -expr -tables))))

  (define syntax-def% 
    (class100 top-def% (-stx -varnames -expr -tables)
      (sequence
	(super-init -stx (quote-syntax define-syntaxes) -varnames -expr -tables))))

  (define (install-values vars expr)
    (when (= 1 (length vars))
      (send (car vars) set-value expr)))

  (define constant% 
    (class100 exp% (-stx -val)
      (private-field
       [stx -stx]
       [val -val])
      (public
	[get-const-val (lambda () val)])
      (override
	[get-value (lambda () this)]

	[valueable? (lambda () #t)]

	[can-dup/move? (lambda ()
			 (or (number? val)
			     (boolean? val)
			     (char? val)
			     (symbol? val)
			     (void? val)))]

	[get-result-arity (lambda () 1)]

	[simplify (lambda (ctx)
		    (cond
		     [(eq? 'bool (context-need ctx))
		      (if (boolean? val)
			  this
			  (make-object constant% stx #t))]
		     [(context-need ctx)
		      (cond
		       [(eq? val (void))
			(make-object void% stx)]
		       [else this])]
		     [else (make-object void% stx)]))]

	[clone (lambda (env) (make-object constant% stx val))]

	[sexpr
	 (lambda ()
	   (let ([vstx (datum->syntax-object (quote-syntax here) val stx)])
	     (cond
	      [(or (number? val)
		   (string? val)
		   (boolean? val)
		   (char? val))
	       vstx]
	      [(syntax? val)
	       (with-syntax ([vstx vstx])
		 (syntax (quote-syntax vstx)))]
	      [else
	       (with-syntax ([vstx vstx])
		 (syntax (quote vstx)))])))])
      (sequence
	(super-init stx))))

  (define void%
    (class100 constant% (-stx)
      (private-field
       [stx -stx])
      (override
	[sexpr (lambda () (quote-syntax (void)))]

	[simplify (lambda (ctx)
		    (if (eq? 'bool (context-need ctx))
			(make-object constant% stx #t)
			this))]

	[clone (lambda (env) (make-object void% stx))])

      (sequence
	(super-init stx (void)))))

  (define app%
    (class100 exp% (-stx -rator -rands -tables)
      (private-field
       [stx -stx]
       [rator -rator]
       [rands -rands]
       [tables -tables]
       [known-single-result? #f])
      (rename [super-simplify simplify]
	      [super-valueable? valueable?])
      (inherit set-known-value)
      (private
	[known-single-result (lambda (v)
			       (set! known-single-result? #t)
			       (set-known-value v)
			       v)])
      (override
	[nonbind-sub-exprs (lambda () (cons rator rands))]
	[set-nonbind-sub-exprs (lambda (s) 
				 (set! rator (car s))
				 (set! rands (cdr s)))]

	[no-side-effect? (lambda ()
			   ;; Note: get-result-arity assumes #t result => single value
			   ;;
			   ;; Some prims are known to be side-effect-free (including no errors)
			   ;; get-result-arity assumes 1 when this returns #t
			   (or known-single-result?
			       (and (rator . is-a? . global%)
				    (send rator is-kernel?)
				    (memq (send rator orig-name) effectless-prims)
				    (andmap (lambda (rand) (send rand no-side-effect?))
					    rands))))]

	[valueable? (lambda ()
		      (and (rator . is-a? . global%)
			   (send rator is-kernel?)
			   (not (memq (send rator orig-name)
				      (non-valueable-prims)))
			   (super-valueable?)))]

	[get-result-arity (lambda ()
			    (if (or known-single-result? (no-side-effect?))
				1
				'unknown))]

	[simplify
	 (lambda (ctx)
	   (super-simplify ctx)
	   (cond
	    ;; ((lambda (a ...) ...) v ...) => (let ([a v] ...) ...)
	    [(and (is-a? rator lambda%)
		  (send rator arg-body-exists? (length rands)))
	     (send rator drop-other-uses (length rands))
	     (let-values ([(vars body) (send rator arg-vars-and-body (length rands))])
	       (for-each (lambda (var rand)
			   (install-values (list var) rand))
			 vars rands)
	       (send (make-object let%
				  stx
				  (map list vars)
				  rands
				  body)
		     simplify ctx))]

	    ;; constant folding
	    [(and (is-a? rator global%)
		  (memq (send rator orig-name) foldable-prims)
		  (send rator is-kernel?)
		  (andmap (lambda (x) (is-a? x constant%)) rands))
	     (if (eq? (send rator orig-name) 'void)
		 (make-object void% stx)
		 (let ([vals (map (lambda (x) (send x get-const-val)) rands)]
		       [f (dynamic-require 'mzscheme (send rator orig-name))])
		   (with-handlers ([not-break-exn? (lambda (x) 
						     (fprintf (current-error-port)
							      "constant calculation error: ~a~n"
							      (exn-message x))
						     this)])
		     (known-single-result
		      (send (make-object constant% stx (apply f vals))
			    simplify ctx)))))]

	    ;; (+ x 1) => (add1 x)
	    [(and (is-a? rator global%)
		  (send rator is-kernel?)
		  (eq? (send rator orig-name) '+)
		  (= 2 (length rands))
		  (or (and (is-a? (car rands) constant%)
			   (eq? 1 (send (car rands) get-const-val)))
		      (and (is-a? (cadr rands) constant%)
			   (eq? 1 (send (cadr rands) get-const-val)))))
	     (make-object app% 
			  stx
			  (make-object global% (quote-syntax add1) (send rator is-trans?) tables #f)
			  (list
			   (if (and (is-a? (car rands) constant%)
				    (eq? 1 (send (car rands) get-const-val)))
			       (cadr rands)
			       (car rands)))
			  tables)]
	    ;; (- x 1) => (sub1 x)
	    [(and (is-a? rator global%)
		  (send rator is-kernel?)
		  (eq? (send rator orig-name) '-)
		  (= 2 (length rands))
		  (and (is-a? (cadr rands) constant%)
		       (eq? 1 (send (cadr rands) get-const-val))))
	     (make-object app% 
			  stx
			  (make-object global% (quote-syntax sub1)  (send rator is-trans?) tables #f)
			  (list (car rands))
			  tables)]

	    ;; (car x) where x is known to be a list construction
	    [(and (is-a? rator global%)
		  (send rator is-kernel?)
		  (let-values ([(pos len) (case (send rator orig-name) 
					    [(car) (values 0 1)]
					    [(cadr) (values 1 1)]
					    [(caddr) (values 2 1)]
					    [(cadddr) (values 3 1)]
					    [(list-ref) (values (and (= 2 (length rands))
								     (let ([v (send (cadr rands) get-value)])
								       (and (v . is-a? . constant%)
									    (send v get-const-val))))
								2)]
					    [else (values #f #f)])])
		    (and (number? pos)
			 (= len (length rands))
			 (and ((car rands) . is-a? . ref%)
			      (let ([val (send (car rands) get-value)])
				(and (val . is-a? . app%)
				     (send val get-list-ref pos)))))))
	     =>
	     (lambda (val)
	       (send (car rands) drop-uses)
	       (known-single-result val))]

	    ;; (memv x '(c ...)) in a boolean context => (if (eq[v]? x 'c) ...)
	    ;; relevant to the output of `case'
	    [(and (eq? (context-need ctx) 'bool) 
		  (is-a? rator global%)
		  (send rator is-kernel?)
		  (eq? (send rator orig-name) 'memv)
		  (= 2 (length rands))
		  (is-a? (car rands) ref%)
		  (is-a? (cadr rands) constant%)
		  (list? (send (cadr rands) get-const-val)))
	     (let ([xformed
		    (let ([l (send (cadr rands) get-const-val)]
			  [l-stx (send (cadr rands) get-stx)]
			  [false (make-object constant% (datum->syntax-object #f #f) #f)]
			  [true (make-object constant% (datum->syntax-object #f #t) #t)])
		      (if (null? l)
			  false
			  (let loop ([l l])
			    (let ([test
				   (make-object app%
						stx
						(make-object global% 
							     (let ([a (car l)])
							       (if (or (symbol? a)
								       (and (number? a)
									    (exact? a)
									    (integer? a)
									    ;; fixnums:
									    (<= (- (expt 2 29))
										a
										(expt 2 29))))
								   (quote-syntax eq?)
								   (quote-syntax eqv?)))
							     (send rator is-trans?)
							     tables
							     #f)
						(list
						 (car rands)
						 (make-object constant% 
							      l-stx
							      (car l)))
						tables)])
			      (cond
			       [(null? (cdr l)) test]
			       [else (let ([rest (loop (cdr l))])
				       ;; increment use count:
				       (send (car rands) set-known-values)
				       (make-object if%
						    stx
						    test
						    true
						    rest))])))))])
	       (send xformed simplify ctx))]

	    ;; (values e) where e has result arity 1
	    [(and (is-a? rator global%)
		  (send rator is-kernel?)
		  (eq? 'values (send rator orig-name))
		  (= 1 (length rands))
		  (equal? 1 (send (car rands) get-result-arity)))
	     (known-single-result (car rands))]

	    ;; inlining: currently hacked for testing to only inline on two special names
	    [(and (is-a? rator binding%)
		  (is-a? (send rator get-value) lambda%)
		  (or 
		   (eq? (send rator orig-name) '*++scan)
		   (eq? (send rator orig-name) '*++match)
		   ;; Only use and not in defn context? Always inline
		   (and (= (send rator get-use-count) 1)
			(not (memq rator (context-indef ctx))))))
	     (let ([f (send (send rator get-value) clone null)])
	       (send rator drop-uses)
	       (set! rator f)
	       (send f set-known-values)
	       ;; Now we have ((lambda ...) ...). Go again.
	       (simplify ctx))]

	    [else this]))]

	[clone (lambda (env) (make-object app%
					  stx
					  (send rator clone env)
					  (map (lambda (rand)
						 (send rand clone env))
					       rands)
					  tables))]

	[sexpr
	 (lambda ()
	   (keep-mzc-property
	    (with-syntax ([rator (get-sexpr rator)]
			  [(rand ...) (map get-sexpr rands)])
	      (syntax/loc stx (rator rand ...)))
	    stx))])

      (public
	;; Checks whether the expression is an app of `values'
	;; to a particular set of bindings.
	[is-values-of?
	 (lambda (args)
	   (and (rator . is-a? . global%)
		(send rator is-kernel?)
		(eq? (send rator orig-name) 'values)
		(= (length rands) (length args))
		(andmap
		 (lambda (rand arg)
		   (and (rand . is-a? . ref%)
			(eq? arg (send rand get-binding))))
		 rands args)))]

	;; If app constructs a list and the nth element can be
	;;  safely extracted, then extract it.
	[get-list-ref
	 (lambda (n)
	   (and (rator . is-a? . global%)
		(send rator is-kernel?)
		(eq? 'list (send rator orig-name))
		((length rands) . > . n)
		(let ([i (list-ref rands n)])
		  (if (send i can-dup/move?)
		      i
		      #f))))])
      (sequence
	(super-init stx))))

  (define lambda% 
    (class100 exp% (-stx -varss -normal?s -bodys)
      (private-field
       [stx -stx]
       [varss -varss]
       [normal?s -normal?s]
       [bodys -bodys])
      (rename [super-simplify simplify])
      (inherit drop-uses)
      (private
	[multarity-ize (lambda (l)
			 (if (null? (cdr l))
			     (car l)
			     (cons (car l)
				   (multarity-ize (cdr l)))))])

      (public
	[multi? (lambda () (or (null? bodys)
			       (pair? (cdr bodys))))]

	[arg-body-exists?
	 (lambda (n)
	   (ormap (lambda (vs n?) (and n? (= n (length vs))))
		  varss normal?s))]
	[arg-vars-and-body
	 (lambda (n)
	   (let loop ([varss varss][normal?s normal?s][bodys bodys])
	     (if (and (car normal?s)
		      (= (length (car varss)) n))
		 (values (car varss) (car bodys))
		 (loop (cdr varss) (cdr normal?s) (cdr bodys)))))]

	[drop-other-uses
	 (lambda (n)
	   (let loop ([n n][varss varss][normal?s normal?s][bodys bodys])
	     (unless (null? varss)
	       (let ([n (if (and (car normal?s)
				 (= (length (car varss)) n))
			    -1
			    (begin
			      (send (car bodys) drop-uses)
			      n))])
		 (loop n (cdr varss) (cdr normal?s) (cdr bodys))))))])

      (override
	[bind-sub-exprs (lambda () (apply append varss))]
	[nonbind-sub-exprs (lambda () bodys)]
	[set-nonbind-sub-exprs (lambda (s) (set! bodys s))]

	[no-side-effect? (lambda () #t)]
	[get-result-arity (lambda () 1)]

	[valueable? (lambda () #t)]
	
	[simplify (lambda (ctx)
		    (if (eq? 'bool (context-need ctx))
			(begin
			  (drop-uses)
			  (make-object constant% stx #t))
			(super-simplify ctx)))]
	
	[clone (lambda (env)
		 (let ([varss+bodys
			(let loop ([varss varss][bodys bodys])
			  (if (null? varss)
			      null
			      (let* ([vars (car varss)]
				     [new-vars (map (lambda (v) (send v clone-binder env))
						    vars)])
				(cons
				 (cons new-vars
				       (send (car bodys)
					     clone (append (map cons vars new-vars)
							   env)))
				 (loop (cdr varss) (cdr bodys))))))])
		   (make-object lambda%
				stx
				(map car varss+bodys)
				normal?s
				(map cdr varss+bodys))))]

	[sexpr
	 (lambda ()
	   (with-syntax ([(vars ...)
			  (map (lambda (vars normal?)
				 (let ([vs (map get-sexpr vars)])
				   (if normal?
				       vs
				       (multarity-ize vs))))
			       varss normal?s)]
			 [(body ...)
			  (map (lambda (body)
				 (get-body-sexpr body))
			       bodys)])
	     (keep-mzc-property
	      (if (multi?)
		  (syntax/loc stx
		      (case-lambda
		       [vars . body] ...))
		  (with-syntax ([body (car (syntax->list (syntax (body ...))))])
		    (syntax/loc stx
			(lambda vars ... . body))))
	      stx)))])
      (sequence
	(super-init stx))))

  (define local% 
    (class100 exp% (-stx -form -varss -rhss -body)
      (private-field
       [stx -stx]
       [form -form]
       [varss -varss]
       [rhss -rhss]
       [body -body])
      (public
	[get-rhss (lambda () rhss)]
	[get-varss (lambda () varss)]
	[get-body (lambda () body)])
      (override
	[bind-sub-exprs (lambda () (apply append varss))]
	[nonbind-sub-exprs (lambda () (cons body rhss))]
	[set-nonbind-sub-exprs (lambda (s) 
				 (set! body (car s))
				 (set! rhss (cdr s)))]

	[get-result-arity (lambda () (send body get-result-arity))]

	[simplify (lambda (ctx)
		    (set! rhss (map (lambda (rhs vars) 
				      (send rhs simplify 
					    (make-context 'all
							  (append vars (context-indef ctx)))))
				    rhss varss))
		    (set! body (send body simplify ctx))

		    ;; Drop unused constant bindings
		    (set!-values (varss rhss)
				 (let loop ([varss varss][rhss rhss])
				   (cond
				    [(null? varss) (values null null)]
				    [else (let-values ([(rest-vss rest-rhss)
							(loop (cdr varss) (cdr rhss))])
					    (if (and (andmap (lambda (var) (not (send var is-used?)))
							     (car varss))
						     (equal? (send (car rhss) get-result-arity)
							     (length (car varss)))
						     (send (car rhss) no-side-effect?))
						(begin
						  (send (car rhss) drop-uses)
						  (values rest-vss rest-rhss))
						(values (cons (car varss) rest-vss)
							(cons (car rhss) rest-rhss))))])))

		    (cond
		     ;; (let-values ([(x) e]) (if e ... ...))
		     ;;  is a pattern created by `or'
		     [(and (is-a? body if%)
			   (let ([t (send body get-if-test)])
			     (and (is-a? t binding%)
				  (= 1 (length varss))
				  (= 1 (length (car varss)))
				  (eq? (caar varss) t)
				  (= 1 (send t get-use-count)))))
		      (make-object if%
				   stx
				   (car rhss)
				   (send body get-if-then)
				   (send body get-if-else))]
		     [(null? varss)
		      (send body simplify ctx)]
		     ;; (let-values [(x) y] ...) whether y is inited, and
		     ;;  neither x nor y is mutated => replace x by y
		     [(and (andmap (lambda (vars) (= 1 (length vars))) varss)
			   (send (caar varss) valueable?)
			   (andmap (lambda (rhs) (and (or (rhs . is-a? . ref%)
							  (rhs . is-a? . global%))
						      (send rhs valueable?)))
				   rhss))
		      (send body substitute
			    (map (lambda (vars rhs) (cons (car vars) 
							  (if (rhs . is-a? . ref%)
							      (send rhs get-binding)
							      rhs)))
				 varss rhss))]
		     
		     [else
		      this]))]

	[clone (lambda (env)
		 (let* ([new-varss
			 (map (lambda (vs)
				(map (lambda (v) (send v clone-binder env))
				     vs))
			      varss)]
			[body-env (append
				   (map cons
					(apply append varss)
					(apply append new-varss))
				   env)]
			[letrec? (eq? form 'letrec-values)])
		   (make-object (if letrec? letrec% let%)
				new-varss
				(map (lambda (rhs) 
				       (send rhs clone (if letrec? body-env env)))
				     rhss)
				(send body clone body-env))))]

	[get-value (lambda () (send body get-value))]
	
	[sexpr
	 (lambda ()
	   (with-syntax ([form form]
			 [(vars ...)
			  (map (lambda (vars)
				 (map get-sexpr vars))
			       varss)]
			 [(rhs ...)
			  (map get-sexpr rhss)]
			 [(body ...) (get-body-sexpr body)])
	     (syntax/loc stx
	       (form ([vars rhs] ...) 
		     body ...))))])
      (sequence
	(super-init -stx))))

  (define let%
    (class100 local% (-stx -varss -rhss -body)
      (inherit get-varss get-rhss get-body)
      (rename [super-set-known-values set-known-values])
      
      (override
	[set-known-values
	 (lambda ()
	   (for-each (lambda (vars rhs) (install-values vars rhs))
		     (get-varss) (get-rhss))
	   (super-set-known-values))])
      (sequence
	(super-init -stx (quote-syntax let-values) -varss -rhss -body))))

  (define letrec%
    (class100 local% (-stx -varss -rhss -body)
      (inherit get-varss get-rhss)
      (rename [super-set-known-values set-known-values])
      
      (override
	[set-known-values
	 (lambda ()
	   (let loop ([varss (get-varss)][rhss (get-rhss)])
	     (unless (null? varss)
	       (when (send (car rhss) valueable?)
		 (for-each (lambda (var) (send var set-inited))
			   (car varss))
		 (loop (cdr varss) (cdr rhss)))))
	   (for-each install-values (get-varss) (get-rhss))
	   (super-set-known-values))])

      (sequence
	(super-init -stx (quote-syntax letrec-values) -varss -rhss -body))))

  (define set!%
    (class100 exp% (-stx -var -val)
      (private-field
       [stx -stx]
       [var -var]
       [val -val])
      (override
	[nonbind-sub-exprs (lambda () (list var val))]
	[set-nonbind-sub-exprs (lambda (s) 
				 (set! var (car s))
				 (set! val (cadr s)))]

	[no-side-effect? (lambda () #f)]
	[valueable? (lambda () #f)]
	[get-result-arity (lambda () 1)]
	
	[set-known-values (lambda ()
			    (send var set-mutated)
			    (send var set-known-values) ; increments use
			    (send val set-known-values))]
	
	[clone (lambda (env)
		 (make-object set!% 
			      (send var clone env)
			      (send val clone env)))]

	[sexpr
	 (lambda ()
	   (with-syntax ([var (get-sexpr var)]
			 [val (get-sexpr val)])
	     (syntax/loc stx
	       (set! var val))))])
      (sequence
	(super-init -stx))))

  (define if%
    (class100 exp% (-stx -test -then -else)
      (private-field
       [stx -stx]
       [test -test]
       [then -then]
       [else -else])
      (public
	[get-if-test (lambda () test)]
	[get-if-then (lambda () then)]
	[get-if-else (lambda () else)])
      (override
	[nonbind-sub-exprs (lambda () (list test then else))]
	[set-nonbind-sub-exprs (lambda (s)
				 (set! test (car s))
				 (set! then (cadr s))
				 (set! else (caddr s)))]
	
	[get-result-arity (lambda ()
			    (let ([t (send then get-result-arity)]
				  [e (send else get-result-arity)])
			      (if (equal? t e)
				  t
				  'unknown)))]

	[simplify (lambda (ctx)
		    (set! test (send test simplify (need-bool ctx)))
		    (set! then (send then simplify ctx))
		    (set! else (send else simplify ctx))
		    
		    ;; (if xvar xvar y) when need bool
		    ;;   => (if xvar #t y)
		    (when (and (eq? 'bool (context-need ctx))
			       (is-a? test binding%)
			       (eq? test then))
		      (send then drop-uses)
		      (set! then (make-object constant% stx #t)))
		    (when (and (eq? 'bool (context-need ctx))
			       (eq? test else)
			       (is-a? test binding%))
		      (send else drop-uses)
		      (set! else (make-object constant% stx #f)))
		    

		    (cond
		     ;; Constant switch
		     [(is-a? test constant%)
		      (if (eq? (send test get-const-val) #f)
			  (begin
			    (send test drop-uses)
			    (send then drop-uses)
			    else)
			  (begin
			    (send test drop-uses)
			    (send else drop-uses)
			    then))]

		     ;; (if (if x y #f) a (void))
		     ;;    => (if x (if y a (void)) (void))
		     [(and (is-a? test if%)
			   (is-a? else void%)
			   (let ([c (send test get-if-else)])
			     (and (is-a? c constant%)
				  (eq? #f (send c get-const-val)))))
		      (send
		       (make-object if%
				    stx
				    (send test get-if-test)
				    (make-object if%
						 stx
						 (send test get-if-then)
						 then
						 (make-object void% stx))
				    (make-object void% stx))
		       simplify ctx)]
		     
		     [else this]))]

	[clone (lambda (env) 
		 (make-object if%
			      stx
			      (send test clone env)
			      (send then clone env)
			      (send else clone env)))]

	[sexpr
	 (lambda ()
	   (with-syntax ([test (get-sexpr test)]
			 [then (get-sexpr then)])
	     (if (else . is-a? . void%)
		 (syntax/loc stx
		   (if test then))
		 (with-syntax ([else (get-sexpr else)])
		   (syntax/loc stx
		     (if test then else))))))])
      (sequence
	(super-init -stx))))

  (define begin0%
    (class100 exp% (-stx -first -rest)
      (private-field
       [stx -stx]
       [first -first]
       [rest -rest])
      (override
	[nonbind-sub-exprs (lambda () (list first rest))]
	[set-nonbind-sub-exprs (lambda (s)
				 (set! first (car s))
				 (set! rest (cadr s)))]
	
	[get-result-arity (lambda () (send first get-result-arity))]

	[simplify (lambda (ctx)
		    (set! first (send first simplify ctx))
		    (set! rest (send rest simplify (need-none ctx)))
		    (if (send rest no-side-effect?)
			(begin
			  (send rest drop-uses)
			  first)
			this))]
	
	[clone (lambda (env) 
		 (make-object begin0%
			      (send first clone env)
			      (send rest clone env)))]
	
	[sexpr
	 (lambda ()
	   (with-syntax ([first (get-sexpr first)]
			 [(rest ...) (get-body-sexpr rest)])
	     (syntax/loc stx
	       (begin0 first rest ...))))])
      (sequence
	(super-init -stx))))

  (define wcm%
    (class100 exp% (-stx -key -val -body)
      (private-field
       [stx -stx]
       [key -key]
       [val -val]
       [body -body])
      (override
	[nonbind-sub-exprs (lambda () (list key val body))]
	[set-nonbind-sub-exprs (lambda (s)
				 (set! key (car s))
				 (set! val (cadr s))
				 (set! body (caddr s)))]

	[get-result-arity (lambda () (send body get-result-arity))]
	
	[clone (lambda (env) 
		 (make-object wcm%
			      (send key clone env)
			      (send val clone env)
			      (send body clone env)))]

	[sexpr
	 (lambda ()
	   (with-syntax ([key (get-sexpr key)]
			 [val (get-sexpr val)]
			 [body (get-sexpr body)])
	     (syntax/loc stx
	       (with-continuation-mark key val body))))])
      (sequence
	(super-init -stx))))


  (define module%
    (class100 exp% (-stx -body -et-body -name -init-req -req-prov -tables)
      (private-field
       [stx -stx]
       [body -body]
       [et-body -et-body]
       [req-prov -req-prov]
       [name -name]
       [init-req -init-req]
       [tables -tables])
      (rename
       [super-deorganize deorganize])
      (override
	[reset-varflags
	 (lambda ()
	   (for-each (lambda (e) (send e reset-varflags)) body)
	   (for-each (lambda (e) (send e reset-varflags)) et-body))]
	[set-known-values
	 ;; Assumes varflags are reset
	 (lambda ()
	   (for-each (lambda (e) (send e set-known-values)) (nonbind-sub-exprs)))]

	[drop-uses
	 ;; Assumes varflags are reset
	 (lambda ()
	   (for-each (lambda (e) (send e drop-uses)) (nonbind-sub-exprs)))]
	
	[no-side-effect? (lambda () #f)]
	[valueable? (lambda () #f)]

	[get-result-arity (lambda () 'unknown)]

	[sub-exprs (lambda () (append (append et-body body)))]
	[bind-sub-exprs (lambda () null)]
	[nonbind-sub-exprs (lambda () (sub-exprs))]
	[set-nonbind-sub-exprs (lambda (l)
				 (let-values ([(etb b)
					       (let loop ([l l][etb et-body][accum null])
						 (cond
						  [(null? etb)
						   (values (reverse! accum) l)]
						  [else (loop (cdr l) (cdr etb) (cons (car l)
										      accum))]))])
				   (set! body body)
				   (set! et-body etb)))]

	;; expose known bindings by converting a sequence of top-level
	;;  expressions into a letrec:
	;;   (define-values (a ...) body) ...
	;;   => (define-values (a ... ...) 
        ;;          (letrec-values ([(a ...) body] ...) (values a ... ...)))
	[reorganize
	 (lambda ()
	   (let ([-body (map (lambda (x) (send x reorganize)) body)]
		 [-et-body (map (lambda (x) (send x reorganize)) et-body)])
	     (let loop ([l -body][defs null])
	       (cond
		[(and (pair? l) 
		      ((car l) . is-a? . variable-def%)
		      (not (ormap (lambda (v) (send v is-mutated?))
				  (send (car l) get-globals)))
		      (send (send (car l) get-rhs) valueable?))
		 (for-each (lambda (g) (send g set-inited))
			   (send (car l) get-globals))
		 (loop (cdr l)
		       (cons (car l) defs))]
		[else
		 (if (null? defs)
		     (void) ; no reorganization
		     (let* ([defs (reverse defs)]
			    [varss
			     (map (lambda (def) (send def get-vars)) defs)]
			    [rhss
			     (map (lambda (def) (send def get-rhs)) defs)]
			    [lex-varss (map (lambda (vars)
					      (map (lambda (var)
						     (make-object binding%
								  (datum->syntax-object
								   #f
								   (syntax-e var)
								   var)
								  #t))
						   vars))
					    varss)]
			    [vars (apply append varss)]
			    [lex-vars (apply append lex-varss)]
			    [env (map cons vars lex-vars)])
		       (set! -body
			     (cons
			      (make-object variable-def%
					   (send (car defs) get-stx)
					   vars
					   (make-object letrec%
							(send (car defs) get-stx)
							lex-varss
							(map (lambda (rhs)
							       (send rhs global->local env))
							     rhss)
							(make-object app%
								     (send (car defs) get-stx)
								     (make-object global%
										  (quote-syntax values)
										  #f
										  tables
										  #f)
								     (map (lambda (var lex-var)
									    (make-object ref% var lex-var))
									  vars
									  lex-vars)
								     tables))
					   tables)
			      l))))])
	       (set! body -body)
	       (set! et-body -et-body)))
	   this)]

	[deorganize
	 (lambda ()
	   ;; Check for
	   ;;   (define-values (a ... ...) 
	   ;;       (letrec-values ([(a ...) body] ...) (values a ... ...)))
	   ;;   => (define-values (a ...) body) ...
	   (when (and (pair? body)
		      (let ([first (car body)])
			(and (first . is-a? . variable-def%)
			     (let ([rhs (send first get-rhs)])
			       (and (rhs . is-a? . letrec%)
				    (let ([lbody (send rhs get-body)]
					  [lvarss (send rhs get-varss)])
				      (and (lbody . is-a? . app%)
					   (send lbody is-values-of? 
						 (apply append lvarss)))))))))
	     (let ([vars (send (car body) get-vars)]
		   [bindingss (send (send (car body) get-rhs) get-varss)]
		   [bodys (send (send (car body) get-rhs) get-rhss)])
	       ;; split vars into varss:
	       (let ([varss (let loop ([bindingss bindingss][vars vars])
			      (if (null? bindingss)
				  null
				  (let loop2 ([bindings (car bindingss)][vars vars][accum null])
				    (if (null? bindings)
					(cons (reverse! accum)
					      (loop (cdr bindingss) vars))
					(loop2 (cdr bindings) (cdr vars) (cons (car vars) accum))))))]
		     [bindings (apply append bindingss)])
		 (let ([env (map cons bindings 
				 (map (lambda (var) 
					(make-object global% var #f tables #f)) 
				      vars))])
		   (set! body
			 (append
			  (map (lambda (vars body)
				 (make-object variable-def%
					      stx
					      vars
					      (send body substitute env)
					      tables))
			       varss bodys)
			  (cdr body)))))))
	   (super-deorganize))]

	[sexpr
	 (lambda ()
	   (with-syntax ([name name]
			 [init-req init-req]
			 [(body ...) (map get-sexpr body)]
			 [(et-body ...) (map get-sexpr et-body)]
			 [(req-prov ...) (map get-sexpr req-prov)])
	     (syntax/loc stx
	       (module name init-req
		 (#%plain-module-begin
		  req-prov ...
		  body ...
		  et-body ...)))))]
	[body-sexpr
	 (lambda ()
	   (list (sexpr)))])
      (sequence (super-init stx))))

  ;; requires and provides should really be ignored:
  (define require/provide%
    (class100 exp% (stx)
      (override
	[valueable? (lambda () #f)]
	[no-side-effect? (lambda () #f)])
      (sequence
	(super-init stx))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Parser
  ;; converts a syntax object to an exp%
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (parse-args env args)
    (let-values ([(norm? ids)
		  (syntax-case args ()
		    [id
		     (identifier? (syntax id))
		     (values #f (list (syntax id)))]
		    [(id ...)
		     (values #t (syntax->list args))]
		    [_else (values #f
				   (let loop ([args args])
				     (syntax-case args ()
				       [id (identifier? args) (list args)]
				       [(id . rest)
					(cons (syntax id) (loop (syntax rest)))])))])])
      (let ([bindings (map (lambda (id) (make-object binding% id #t)) ids)])
	(values
	 (append (map cons ids bindings) env)
	 bindings
	 norm?))))

  (define (parse-let % rec? stx env loop)
    (syntax-case stx ()
      [(_ ([vars rhs] ...) . body)
       (let* ([varses (syntax->list (syntax (vars ...)))]
	      [rhses (syntax->list (syntax (rhs ...)))]
	      [var-objses (map (lambda (vars)
				 (map (lambda (var)
					(make-object binding% var (not rec?)))
				      (syntax->list vars)))
			       varses)]
	      [body-env (append
			 (apply
			  append
			  (map (lambda (var-objs vars)
				 (map cons
				      (syntax->list vars)
				      var-objs))
			       var-objses
			       varses))
			 env)])
	 (make-object
	  %
	  stx
	  var-objses
	  (map (lambda (rhs)
		 (loop rhs (if rec? body-env env)))
	       rhses)
	  (loop (syntax (begin . body)) body-env)))]))

  (define (stx-bound-assq ssym l)
    (ormap (lambda (p)
	     (and (bound-identifier=? ssym (car p))
		  p))
	   l))

  (define (lookup-clone binding var env)
    (let ([s (assq binding env)])
      (if s
	  (let ([b (cdr s)])
	    (if (b . is-a? . binding%)
		(make-object ref% (send var get-stx) b)
		;; it's a global%:
		b))
	  var)))

  (define (make-parse top?)
    (lambda (stx env trans? in-module? tables)
      (kernel-syntax-case stx trans?
	[id
	 (identifier? stx)
	 (let ([a (stx-bound-assq stx env)])
	   (if a
	       (make-object ref% stx (cdr a))
	       (make-object global% stx trans? tables #f)))]

	[(#%top . id)
	 (make-object global% (syntax id) trans? tables #t)]
	
	[(#%datum . val)
	 (make-object constant% stx (syntax-object->datum (syntax val)))]

	[(define-values names rhs)
	 (make-object variable-def% 
		      stx
		      (syntax->list (syntax names))
		      (parse (syntax rhs) env #f in-module? tables)
		      tables)]
	
	[(define-syntaxes names rhs)
	 (make-object syntax-def% 
		      stx
		      (syntax->list (syntax names))
		      (parse (syntax rhs) env #t in-module? tables)
		      tables)]
	
	[(begin . exprs)
	 (make-object begin%
		      stx
		      (map (lambda (e) ((if top? parse-top parse) e env trans? in-module? tables))
			   (syntax->list (syntax exprs))))]

	[(begin0 expr . exprs)
	 (make-object begin0%
		      stx
		      (parse (syntax expr) env trans? in-module? tables)
		      (parse (syntax (begin . exprs)) env trans? in-module? tables))]

	[(quote expr)
	 (make-object constant% stx (syntax-object->datum (syntax expr)))]

	[(quote-syntax expr)
	 (make-object constant% stx (syntax expr))]

	[(lambda args . body)
	 (let-values ([(env args norm?) (parse-args env (syntax args))])
	   (make-object lambda%
			stx
			(list args)
			(list norm?)
			(list (parse (syntax (begin . body)) env trans? in-module? tables))))]

	[(case-lambda [args . body] ...)
	 (let-values ([(envs argses norm?s)
		       (let ([es+as+n?s
			      (map
			       (lambda (args)
				 (let-values ([(env args norm?) (parse-args env args)])
				   (cons env (cons args norm?))))
			       (syntax->list (syntax (args ...))))])
			 (values
			  (map car es+as+n?s)
			  (map cadr es+as+n?s)
			  (map cddr es+as+n?s)))])
	   (make-object lambda%
			stx
			argses
			norm?s
			(map (lambda (env body)
			       (with-syntax ([body body])
				 (parse (syntax (begin . body)) env trans? in-module? tables)))
			     envs
			     (syntax->list (syntax (body ...))))))]
	
	[(let-values . _)
	 (parse-let let% #f stx env
		    (lambda (b env) (parse b env trans? in-module? tables)))]
	[(letrec-values . _)
	 (parse-let letrec% #t stx env
		    (lambda (b env) (parse b env trans? in-module? tables)))]

	[(set! var rhs)
	 (make-object set!% 
		      stx
		      (parse (syntax var) env trans? in-module? tables)
		      (parse (syntax rhs) env trans? in-module? tables))]

	[(if test then . else)
	 (make-object if%
		      stx
		      (parse (syntax test) env trans? in-module? tables)
		      (parse (syntax then) env trans? in-module? tables)
		      (if (null? (syntax-e (syntax else)))
			  (parse (quote-syntax (#%app void)) env trans? in-module? tables)
			  (parse (car (syntax-e (syntax else))) env trans? in-module? tables)))]
	
	[(with-continuation-mark k v body)
	 (make-object wcm% 
		      stx
		      (parse (syntax k) env trans? in-module? tables)
		      (parse (syntax v) env trans? in-module? tables)
		      (parse (syntax body) env trans? in-module? tables))]
	
	[(#%app)
	 (make-object constant% stx null)]
	
	[(#%app func . args)
	 (make-object app% 
		      stx
		      (parse (syntax func) env trans? in-module? tables)
		      (map (lambda (v) (parse v env trans? in-module? tables)) (syntax->list (syntax args)))
		      tables)]

	[(module name init-require (#%plain-module-begin . body))
	 (let* ([body (map (lambda (x)
			     (parse x env #f #t tables))
			   (syntax->list (syntax body)))]
		[et-body
		 (filter (lambda (x) (x . is-a? . syntax-def%)) body)]
		[rt-body
		 (filter (lambda (x) (not (or (x . is-a? . syntax-def%)
					      (x . is-a? . require/provide%))))
			 body)]
		[req-prov
		 (filter (lambda (x) (x . is-a? . require/provide%))
			 body)])
	   (make-object module%
			stx
			rt-body
			et-body
			(syntax name)
			(syntax init-require)
			req-prov
			tables))]

	[(require . i) (make-object require/provide% stx)]
	[(require-for-syntax . i) (make-object require/provide% stx)]
	[(provide i ...) (make-object require/provide% stx)]

	[else (error 'parse "unknown expression: ~a" (syntax-object->datum stx))])))

  (define parse (make-parse #f))
  (define parse-top (make-parse #t))

  (define (create-tables)
    (make-tables (make-hash-table) (make-hash-table)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Optimizer
  ;; the driver function
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define optimize 
    (opt-lambda (e [for-mzc? #f])
      (let ([p (parse-top e null #f #f (create-tables))])
	(send p reorganize)
	(send p set-known-values)
	(let ([p (send p simplify (make-context 'all null))])
	  (get-sexpr (if for-mzc?
			 p
			 (send p deorganize)))))))

  (provide optimize))
