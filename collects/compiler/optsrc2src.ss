
;; Source-to-source optimizer

(module opt2 mzscheme
  (require (lib "class.ss")
	   (lib "class100.ss")
	   (lib "kerncase.ss" "syntax")
	   (lib "list.ss"))

  (define foldable '(void
		     + - * / arithmetic-shift
		     < <= = > >=
		     number? positive? negative? zero?
		     real? complex?
		     string-ref))

  (define-struct context (need indef))
  ;; need = #f => don't need  the value
  ;; need = 'bool => need bool only
  ;; need = 'all => need exact result

  ;; indef = list of lexical%s

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
	[src-stx stx]
	[known-value #f])
      (public
	[reset-varflags
	 (lambda ()
	   (set! known-value #f)
	   (for-each (lambda (e) (send e reset-varflags)) (sub-exprs)))]
	[set-known-values
	 ;; Assumes varflags are reset
	 (lambda ()
	   (for-each (lambda (e) (send e set-known-values)) (nonbind-sub-exprs)))]

	[drop-uses
	 ;; Assumes varflags are reset
	 (lambda ()
	   (for-each (lambda (e) (send e drop-uses)) (nonbind-sub-exprs)))]
	
	[no-side-effect? (lambda () (andmap (lambda (e) (send e no-side-effect?))
					    (nonbind-sub-exprs)))]

	[get-result-arity (lambda () 'unknown)]

	[sub-exprs (lambda () (append (bind-sub-exprs) (nonbind-sub-exprs)))]
	[bind-sub-exprs (lambda () null)]
	[nonbind-sub-exprs (lambda () null)]
	[set-nonbind-sub-exprs (lambda (x) (void))]

	[valueable? (lambda () #f)]

	[set-known-value (lambda (x) (set! known-value x))]
	[get-value (lambda () (or known-value this))])
      
      (private
	[subexp-map!
	 (lambda (f)
	   (set-nonbind-sub-exprs (map f (nonbind-sub-exprs)))
	   this)])

      (public
	[simplify (lambda (ctx) 
		    (subexp-map! (lambda (x) 
				   (send x simplify (need-all ctx)))))]	
	[reorganize (lambda ()
		      (subexp-map! (lambda (x)
				     (send x reorganize))))]
	[global->local (lambda (env)
			 (subexp-map! (lambda (x)
					(send x global->local env))))]
	
	[clone 
	 ;; Creates a copy, used for inling. We don't try
	 ;;  to preserve analysis; we'll just re-do it.
	 (lambda (env) 
	   (error 'clone "unimplemented: ~a" this))]

	[get-stx (lambda () src-stx)]

	[sexpr
	 (lambda ()
	   src-stx)]
	[body-sexpr
	 (lambda ()
	   (list (sexpr)))])
      (sequence (super-init))))
  
  (define (get-sexpr o) (send o sexpr))
  (define (get-body-sexpr o) (send o body-sexpr))

  (define-struct bucket (mutated?))

  (define (global-bucket table stx)
    (let ([l (hash-table-get table (syntax-e stx) (lambda () null))])
      (let ([s (ormap (lambda (b)
			(and (module-identifier=? stx (car b))
			     (cdr b)))
		      l)])
	(if s
	    s
	    (let ([s (make-bucket #f)])
	      (hash-table-put! table (syntax-e stx) (cons (cons stx s) l))
	      s)))))

  (define global-ht (make-hash-table))
  (define et-global-ht (make-hash-table))

  (define global%
    (class100 exp% (-stx -trans?)
      (private-field
        [stx -stx]
	[trans? -trans?]
	[mbind #f]
	[bucket (global-bucket (if trans? et-global-ht global-ht) stx)])
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
	       (cdr mbind)
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

	[valueable? (lambda () (is-kernel?))]
	
	[clone (lambda (env) (make-object global% stx trans?))]

	[global->local (lambda (env)
			 (or (ormap (lambda (e)
				      (and (module-identifier=? (car e) stx)
					   (make-object ref% stx (cdr e))))
				    env)
			     this))])
      (public
	[set-mutated (lambda () (set-bucket-mutated?! bucket #t))])

      (sequence
	(super-init stx))))

  (define lexical% 
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
			(make-object lexical% (datum->syntax-object
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

	[valueable? (lambda () inited?)]

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

	[drop-uses (lambda () (send binding drop-uses))]
	
	[get-result-arity (lambda () 1)]
	
	[get-value (lambda () (send binding get-value))]

	[simplify (lambda (ctx)
		    (if (context-need ctx)
			(let ([v (get-value)])
			  (if (or (is-a? v constant%)
				  (and (is-a? v global%)
				       (send v is-kernel?)))
			      (begin
				(drop-uses)
				(send v simplify ctx))
			      this))
			(begin
			  (drop-uses)
			  (make-object void% stx))))]

	[clone (lambda (env) (lookup-clone this env))]

	[sexpr (lambda () (send binding sexpr))])
      (public
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
				  (send (car subs) drop-uses)
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
    (class100 exp% (-stx -formname -varnames -expr)
      (private-field
       [stx -stx]
       [formname -formname]
       [varnames -varnames]
       [expr -expr])
      (override
	[nonbind-sub-exprs (lambda () (list expr))]
	[set-nonbind-sub-exprs (lambda (s) (set! expr (car s)))]

	[get-result-arity (lambda () 1)]
	
	[no-side-effect? (lambda () #f)]

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
	[get-rhs (lambda () expr)])
      (sequence
	(super-init stx))))

  (define variable-def% 
    (class100 top-def% (-stx -varnames -expr)
      (sequence
	(super-init -stx (quote-syntax define-values) -varnames -expr))))

  (define syntax-def% 
    (class100 top-def% (-stx -varnames -expr)
      (sequence
	(super-init -stx (quote-syntax define-syntaxes) -varnames -expr))))

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
    (class100 exp% (-stx -rator -rands)
      (private-field
       [stx -stx]
       [rator -rator]
       [rands -rands])
      (rename [super-simplify simplify])
      (override
	[nonbind-sub-exprs (lambda () (cons rator rands))]
	[set-nonbind-sub-exprs (lambda (s) 
				 (set! rator (car s))
				 (set! rands (cdr s)))]

	[no-side-effect? (lambda () #f)]

	[get-result-arity (lambda () 'unknown)]
	
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
		  (memq (send rator orig-name) foldable)
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
		     (send (make-object constant% stx (apply f vals))
			   simplify ctx))))]

	    ;; (+ x 1) => (add1 x)
	    [(and (is-a? rator global%)
		  (eq? (send rator orig-name) '+)
		  (= 2 (length rands))
		  (or (and (is-a? (car rands) constant%)
			   (eq? 1 (send (car rands) get-const-val)))
		      (and (is-a? (cadr rands) constant%)
			   (eq? 1 (send (cadr rands) get-const-val)))))
	     (make-object app% (make-object global% (quote-syntax add1) (send rator is-trans?))
			  (list
			   (if (and (is-a? (car rands) constant%)
				    (eq? 1 (send (car rands) get-const-val)))
			       (cadr rands)
			       (car rands))))]
	    ;; (- x 1) => (sub1 x)
	    [(and (is-a? rator global%)
		  (eq? (send rator orig-name) '-)
		  (= 2 (length rands))
		  (and (is-a? (cadr rands) constant%)
		       (eq? 1 (send (cadr rands) get-const-val))))
	     (make-object app% (make-object global% (quote-syntax sub1)  (send rator is-trans?))
			  (list (car rands)))]

	    ;; inlining
	    [(and (is-a? rator lexical%)
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
					  (send rator clone env)
					  (map (lambda (rand)
						 (send rand clone env))
					       rands)))]

	[sexpr
	 (lambda ()
	   (with-syntax ([rator (get-sexpr rator)]
			 [(rand ...) (map get-sexpr rands)])
	     (syntax/loc stx (rator rand ...))))])
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
	[multi (lambda () (pair? (cdr bodys)))]

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
	   (with-syntax ([([vars body] ...)
			  (map (lambda (vars normal? body)
				 (with-syntax ([vars (let ([vs (map get-sexpr vars)])
						       (if normal?
							   vs
							   (multarity-ize vs)))]
					       [body (get-body-sexpr body)])
				   (syntax [vars body])))
			       varss normal?s bodys)])
	     (if (multi)
		 (syntax/loc stx
		   (case-lambda
		    [vars body] ...))
		 (syntax/loc stx
		   (lambda vars ... body ...)))))])
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
			     (and (is-a? t lexical%)
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
	[get-result-arity (lambda () 1)]
	
	[set-known-values (lambda ()
			    (send var set-mutated)
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
			       (is-a? test lexical%)
			       (eq? test then))
		      (send then drop-uses)
		      (set! then (make-object constant% stx #t)))
		    (when (and (eq? 'bool (context-need ctx))
			       (eq? test else)
			       (is-a? test lexical%))
		      (send else drop-uses)
		      (set! else (make-object constant% stx #f)))
		      

		    (cond
		     ;; Constant switch
		     [(is-a? test constant%)
		      (if (eq? (send test get-const-val) #f)
			  (begin
			    (send then drop-uses)
			    else)
			  (begin
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
			 [then (get-sexpr then)]
			 [else (get-sexpr else)])
	     (syntax/loc stx
	       (if test then else))))])
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
				 (set! val (car s))
				 (set! body (car s)))]

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
	       (with-continuation-marks key val body))))])
      (sequence
	(super-init -stx))))


  (define module%
    (class100 exp% (-stx -body -et-body -name -init-req -req-prov)
      (private-field
	[stx -stx]
	[body -body]
	[et-body -et-body]
	[req-prov -req-prov]
	[name -name]
	[init-req -init-req])
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
	
	[no-side-effect? (lambda () (andmap (lambda (e) (send e no-side-effect?))
					    (nonbind-sub-exprs)))]

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
	[reorganize
	 (lambda ()
	   (let ([-body (map (lambda (x) (send x reorganize)) body)]
		 [-et-body (map (lambda (x) (send x reorganize)) et-body)])
	     (let loop ([l -body][defs null])
	       (cond
		[(and (pair? l) 
		      ((car l) . is-a? . variable-def%)
		      (not (ormap (lambda (v) (send v mutated?))
				  (send (car l) get-vars)))
		      (send (send (car l) get-rhs) valueable?))
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
						     (make-object lexical%
								  (datum->syntax-object
								   #f
								   (string->symbol (format "m_~a" (syntax-e var)))
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
										  #f)
								     (map (lambda (var)
									    (make-object ref% var var))
									  lex-vars))))
			      l))))])
	       (set! body -body)
	       (set! et-body -et-body)))
	   this)]

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
		  body ...
		  et-body ...
		  req-prov ...)))))]
	[body-sexpr
	 (lambda ()
	   (list (sexpr)))])
      (sequence (super-init stx))))

  (define require/provide%
    (class100 exp% (stx)
       (sequence
	 (super-init stx))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (let ([bindings (map (lambda (id) (make-object lexical% id #t)) ids)])
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
					(make-object lexical% var (not rec?)))
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

  (define (lookup-clone var env)
    (let ([s (assq var env)])
      (if s
	  (cdr s)
	  var)))

  (define (make-parse top?)
    (lambda (stx env trans? in-module?)
      (kernel-syntax-case stx trans?
	[id
	 (identifier? stx)
	 (let ([a (stx-bound-assq stx env)])
	   (if a
	       (make-object ref% stx (cdr a))
	       (make-object global% stx trans?)))]

	[(#%top . id)
	 (make-object global% (syntax id) trans?)]
	
	[(#%datum . val)
	 (make-object constant% stx (syntax-object->datum (syntax val)))]

	[(define-values names rhs)
	 (make-object variable-def% 
		      stx
		      (syntax->list (syntax names))
		      (parse (syntax rhs) env #f in-module?))]
	
	[(define-syntaxes names rhs)
	 (make-object syntax-def% 
		      stx
		      (syntax->list (syntax names))
		      (parse (syntax rhs) env #t in-module?))]
	
	[(begin . exprs)
	 (make-object begin%
		      stx
		      (map (lambda (e) ((if top? parse-top parse) e env trans? in-module?))
			   (syntax->list (syntax exprs))))]

	[(begin0 expr . exprs)
	 (make-object begin0%
		      stx
		      (parse (syntax expr) env trans? in-module?)
		      (parse (syntax (begin . exprs)) env trans? in-module?))]

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
			(list (parse (syntax (begin . body)) env trans? in-module?))))]

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
				 (parse (syntax (begin . body)) env trans? in-module?)))
			     envs
			     (syntax->list (syntax (body ...))))))]
	
	[(let-values . _)
	 (parse-let let% #f stx env
		    (lambda (b env) (parse b env trans? in-module?)))]
	[(letrec-values . _)
	 (parse-let letrec% #t stx env
		    (lambda (b env) (parse b env trans? in-module?)))]

	[(set! var rhs)
	 (make-object set!% 
		      stx
		      (parse (syntax var) env trans? in-module?)
		      (parse (syntax rhs) env trans? in-module?))]

	[(if test then . else)
	 (make-object if%
		      stx
		      (parse (syntax test) env trans? in-module?)
		      (parse (syntax then) env trans? in-module?)
		      (if (null? (syntax-e (syntax else)))
			  (parse (quote-syntax (#%app void)) env trans? in-module?)
			  (parse (car (syntax-e (syntax else))) env trans? in-module?)))]
	
	[(with-continuation-mark k v body)
	 (make-object wcm% 
		      stx
		      (parse (syntax k) env in-module?)
		      (parse (syntax v) env in-module?)
		      (parse (syntax body) env in-module?))]
	   
	[(#%app)
	 (make-object constant% stx null)]
	
	[(#%app func . args)
	 (make-object app% 
		      stx
		      (parse (syntax func) env trans? in-module?)
		      (map (lambda (v) (parse v env trans? in-module?)) (syntax->list (syntax args))))]

	[(module name init-require (#%plain-module-begin . body))
	 (let* ([body (map (lambda (x)
			     (parse x env #f #t))
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
			req-prov))]

	[(require . i) (make-object require/provide% stx)]
	[(require-for-syntax . i) (make-object require/provide% stx)]
	[(provide i ...) (make-object require/provide% stx)]

	[else (error 'parse "unknown expression: ~a" (syntax-object->datum stx))])))

  (define parse (make-parse #f))
  (define parse-top (make-parse #t))

  (define (optimize e)
    (let ([p (parse-top e null #f #f)])
      (send p reorganize)
      (send p set-known-values)
      (printf "simplify~n")
      (let ([p (send p simplify (make-context 'all null))])

	(get-sexpr p))))

  (provide optimize))

(require opt2)

(define (no-optimize x) x)

(require (lib "pretty.ss"))

(pretty-print
 (syntax-object->datum
  (optimize
   (parameterize ([current-directory "/home/mflatt/proj/plt/collects/mzlib/"])
     (parameterize ([current-load-relative-directory (current-directory)])
       (expand 
	(with-input-from-file "awk.ss" 
	  (lambda () (read-syntax "awk.ss")))))))))
