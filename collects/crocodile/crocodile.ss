(module crocodile mzscheme

  (require-for-syntax (lib "stx.ss" "syntax")
		      "private/ops.ss"
		      "private/util.ss"
		      (lib "kerncase.ss" "syntax"))
  
  (begin-for-syntax

   (define kernel-forms (kernel-form-identifier-list #'here))

   (define (top-block-context? ctx) (memq ctx '(top-block)))
   (define (block-context? ctx) (memq ctx '(top-block block)))
   (define (expression-context? ctx) (memq ctx '(expression)))
   (define (type-context? ctx) (memq ctx '(type)))

   (define block-context 'block)
   (define top-block-context 'top-block)
   (define expression-context 'expression)
   (define type-context 'type)

   ;; --------------------------------------------------------
   ;; Transformer procedure property and basic struct

   (define-values (prop:croc-transformer croc-transformer? croc-transformer-ref)
     (make-struct-type-property 'crocodile-transformer))


   (define-values (struct:croc-trans make-croc-trans croc-trans? croc-trans-ref croc-trans-set!)
     (make-struct-type 'croc-trans #f 1 0 #f 
		       (list (list prop:croc-transformer #t))
		       (current-inspector) 0))

   (define (make-crocodile-transformer proc)
     (unless (and (procedure? proc)
		  (procedure-arity-includes? proc 2))
       (raise-type-error
	'define-crocodile-syntax
	"procedure (arity 2)"
	proc))
     (make-croc-trans proc))

   ;; --------------------------------------------------------
   ;; Type

   (define-values (struct:croc-type make-croc-type croc-type? croc-type-ref croc-type-set!)
     (make-struct-type 'croc-type #f 3 0 #f null (current-inspector) 0))

   (define (croc-type-stx v) (croc-type-ref v 0))
   (define (croc-type-pred-def-stx v) (croc-type-ref v 1))
   (define (croc-type-pred-stx v) (croc-type-ref v 2))

   ;; --------------------------------------------------------
   ;; Parsing blocks

   (define operator? 
     (let ([sym-chars (string->list "+-_=?:<>.!%^&*/~|")])
       (lambda (stx)
	 (and (identifier? stx)
	      (let ([str (symbol->string (syntax-e stx))])
		(and (positive? (string-length str))
		     (memq (string-ref str 0) sym-chars)))))))
   
   (define (get-transformer stx)
     (or (and (stx-pair? stx)
	      (identifier? (stx-car stx))
	      (let ([v (syntax-local-value (stx-car stx) (lambda () #f))])
		(and (croc-transformer? v) v)))
	 (and (stx-pair? stx)
	      (let ([first (stx-car stx)])
		(and (stx-pair? first)
		     (identifier? (stx-car first))
		     (module-identifier=? #'#%parens (stx-car first))
		     ;; If the stx-car is a list with just one operator symbol,
		     ;;  try using the operator as a transformer
		     (let ([l (cdr (stx->list first))])
		       (let loop ([l l])
			 (cond
			  [(null? l) #f]
			  [(operator? (car l))
			   (if (ormap operator? (cdr l))
			       #f
			       (let ([v (syntax-local-value (car l) (lambda () #f))])
				 (and (croc-transformer? v)
				      v)))]
			  [else (loop (cdr l))]))))))))
   
   ;; --------------------------------------------------------
   ;; Parsing blocks

   (define (parse-block-one ctx body k done-k)
     (cond
      [(stx-null? body) (done-k)]
      [(get-transformer body)
       => (lambda (transformer)
	    (let-values ([(code rest) (transformer body ctx)])
	      (k code rest)))]
      [else
       (raise-syntax-error 
	'block
	"unknown form" 
	(stx-car body))]))

   (define (parse-block stx)
     (let loop ([stx stx])
       (parse-block-one block-context
			stx 
			(lambda (code rest)
			  (cons code (loop rest)))
			(lambda ()
			  null))))

   ;; --------------------------------------------------------
   ;; Parsing expressions

   (define parse-expr
     (let ()
       (define (parse-expr-seq stx)
	 (define (start-expr stx) 
	   (let ([trans (get-transformer stx)])
	     (if trans
		 (let-values ([(expr rest) (trans stx expression-context)])
		   (if (stx-null? rest)
		       (list expr)
		       (cons expr (start-operator rest))))
		 (syntax-case stx (#%parens)
		   [(v)
		    (or (number? (syntax-e #'v))
			(identifier? #'v)
			(string? (syntax-e #'v)))
		    (if (operator? #'v)
			(raise-syntax-error
			 #f
			 "operator alone is not an expression"
			 #'v)
			(list #'v))]
		   [((#%parens . pexpr))
		    (list (parse-expr #'pexpr))]
		   [(op . more)
		    (and (identifier? #'op)
			 (ormap (lambda (uop)
				  (module-identifier=? #'op uop))
				unary-prefix-ops))
		    (cons (make-prefix (stx-car stx)) (start-expr #'more))]
		   [(expr then . more)
		    (append (start-expr (list #'expr))
			    (start-operator #'(then . more)))]
		   [(bad . rest)
		    (raise-syntax-error
		     'expression
		     "unknown expression form"
		     #'bad)]))))
	 (define (start-operator stx)
	   (unless (or (and (stx-pair? (stx-car stx))
			    (or (module-identifier=? #'#%brackets (stx-car (stx-car stx)))
				(module-identifier=? #'#%parens (stx-car (stx-car stx)))))
		       (and (identifier? (stx-car stx))
			    (hash-table-get op-table
					    (syntax-e (stx-car stx))
					    (lambda () #f))))
	     (raise-syntax-error
	      'expression
	      "expected an operator, but found something else"
	      (stx-car stx)))
	   ;; Check for postfix operator, first
	   (cond
	    [(stx-pair? (stx-car stx))
	     ;; Convert vector index or application to a binary operator:
	     (let ([opl (if (module-identifier=? #'#%brackets (stx-car (stx-car stx)))
			    (let ([index-expr (parse-expr (stx-cdr (stx-car stx)))])
			      (list (make-infix (stx-car (stx-car stx)))
				    index-expr))
			    (let ([arg-exprs (parse-arg-list (stx-cdr (stx-car stx)))])
			      (list (make-infix (stx-car (stx-car stx)))
				    arg-exprs)))])
	       (if (stx-null? (stx-cdr stx))
		   opl
		   (append opl (start-operator (stx-cdr stx)))))]
	    [(or (module-identifier=? #'++ (stx-car stx))
		 (module-identifier=? #'-- (stx-car stx)))
	     (if (null? (stx-cdr stx))
		 (list (make-postfix (stx-car stx)))
		 (cons (make-postfix (stx-car stx))
		       (start-operator (stx-cdr stx))))]
	    [else
	     ;; Otherwise, must be infix
	     (cons (make-infix (stx-car stx))
		   (start-expr (stx-cdr stx)))]))
	 (start-expr stx))

       (define (parse-expr stx)
	 (let group ([seq (parse-expr-seq stx)])
	   ;; seq is a list that mixes exprs with ops.
	   ;; Find leftmost oper with maximal precedence
	   (if (null? (cdr seq))
	       (car seq)
	       (let loop ([seq seq][before null][op #f][since null])
		 (cond
		  [(null? seq)
		   (cond
		    [(prefix? op)
		     (group (append (reverse (cdr before))
				    (list (quasisyntax/loc (op-id op)
					    (#,(op-id op) #,(car before))))
				    (reverse since)))]
		    [(postfix? op)
		     (let ([after (reverse since)])
		       (group (append (reverse before)
				      (list (quasisyntax/loc (op-id op)
					      (#,(op-id op) #,(car after))))
				      (cdr after))))]
		    [(infix? op)
		     (let ([after (reverse since)])
		       (group (append (reverse (cdr before))
				      (list (quasisyntax/loc (op-id op)
					      (#,(op-id op) #,(car before) #,(car after))))
				      (cdr after))))]
		    [else (error "not an op!: " op)])]
		  [(not (op? (stx-car seq)))
		   (loop (cdr seq) before op (cons (car seq) since))]
		  [(> (hash-table-get precedence-table (prec-key (car seq)) (lambda () 0))
		      (hash-table-get precedence-table (prec-key op) (lambda () 0)))
		   (loop (cdr seq) 
			 (if op
			     (append since (list op) before)
			     since)
			 (car seq) null)]
		  [else
		   (loop (cdr seq) before op (cons (car seq) since))])))))

       (define (parse-arg-list stxs)
	 (if (stx-null? stxs)
	     stxs
	     (let-values ([(val-stxs after-expr) (extract-until stxs (list #'\,))])
	       (when (and val-stxs
			  (stx-null? (stx-cdr after-expr)))
		 (raise-syntax-error
		  'procedure\ call
		  "missing expression after comma"
		  (stx-car after-expr)))
	       (when (null? val-stxs)
		 (raise-syntax-error
		  'procedure\ call
		  "missing expression before token"
		  (stx-car after-expr)))
	       (if val-stxs
		   (cons (parse-expr val-stxs)
			 (parse-arg-list (stx-cdr after-expr)))
		   (list (parse-expr stxs))))))
       
       parse-expr))

   ;; --------------------------------------------------------
   ;; Parsing declarations (which always start with a type)

   (define (parse-one-argument proc-id type id k)
     (cons (list id 
		 (croc-type-stx type)
		 (croc-type-pred-def-stx type)
		 (croc-type-pred-stx type))
	   (k)))

   (define (parse-arguments orig-args-stx proc-id)
     (if (stx-null? orig-args-stx)
	 null
	 (let loop ([args-stx orig-args-stx]
		    [where "at start of argument sequence"]
		    [where-stx orig-args-stx])
	   (let ([trans (get-transformer args-stx)])
	     (let-values ([(type rest-stx) (if trans
					       (trans args-stx type-context)
					       (values #f #f))])
	       (unless (croc-type? type)
		 (raise-syntax-error
		  '|procedure declaration|
		    (format "expected a type ~a" where)
		    where-stx))
	       (syntax-case rest-stx ()
		 [(id)
		  (identifier? #'id)
		  (parse-one-argument proc-id type #'id
				      (lambda () null))]
		 [(id comma . rest)
		  (and (identifier? #'id)
		       (identifier? #'comma)
		       (module-identifier=? #'comma #'\,))
		  (parse-one-argument proc-id type #'id
				      (lambda ()
					(loop #'rest
					      "after comma"
					      #'comma)))]
		 [(id something . rest)
		  (identifier? #'id)
		  (raise-syntax-error
		   'procedure\ declaration
		   "expected a comma after identifier name"
		   #'something)]
		 [_else
		  (raise-syntax-error
		   'procedure\ declaration
		   "expected an argument identifier"
		   (car rest-stx))]))))))
   
   (define (make-crocodile-type pred-id mk-pred-def)
     (make-croc-trans
      (lambda (orig-stx ctx)
	(let* ([pred-id (or pred-id
			    (car (generate-temporaries '(type-pred))))]
	       [pred-def (if mk-pred-def
			     (mk-pred-def pred-id orig-stx)
			     #'(begin))])
	  (cond
	   [(block-context? ctx)
	    (with-syntax ([pred-id pred-id]
			  [type-name (stx-car orig-stx)])
	      (let loop ([stx (stx-cdr orig-stx)]
			 [after (stx-car orig-stx)]
			 [after-what "type name"]
			 [parens-ok? #t])
		(syntax-case stx ()
		  [(id . rest)
		   (begin
		     (unless (identifier? #'id)
		       (raise-syntax-error 'declaration
					   (format "expected a identifier after ~a" after-what)
					   (stx-car orig-stx)
					   #'id))
		     (if (and (identifier? (stx-car #'rest))
			      (module-identifier=? #'set! (stx-car #'rest)))
			 ;; -- Non-procedure declaration
			 (let-values ([(val-stxs after-expr) (extract-until (stx-cdr #'rest)
									    (list #'\; #'\,))])
			   (unless val-stxs
			     (raise-syntax-error 
			      'declaration
			      "missing semicolon or comma after initializing assignment"
			      (stx-car #'rest)))
			   (when (null? val-stxs)
			     (raise-syntax-error 
			      'declaration
			      "missing expression initializing assignment"
			      (stx-car #'rest)))
			   (let ([def #`(define-typed id #f type-name pred-id 
					  (check-expr #f 'id type-name pred-id 
						      #,(parse-expr val-stxs)))])
			     (if (module-identifier=? #'\; (stx-car after-expr))
				 (values #`(begin #,pred-def #,def) (stx-cdr after-expr))
				 (let-values ([(defs remainder kind) (loop (stx-cdr after-expr) (stx-car after-expr) "comma" #f)])
				   (values #`(begin #,pred-def #,def #,defs) remainder)))))
			 ;; -- Procedure declaration
			 (syntax-case #'rest (#%parens \;)
			   [((#%parens . prest) (#%braces . body) . rest)
			    parens-ok?
			    (let ([args (parse-arguments #'prest #'id)])
			      (with-syntax ([((arg arg-type arg-pred-def arg-pred-id) ...) args]
					    [(temp-id ...) (generate-temporaries (map car args))])
				(values #`(begin
					    #,pred-def
					    arg-pred-def ...
					    (define-typed-procedure id 
					      ((arg arg-type arg-pred-id) ...)
					      (lambda (temp-id ...)
						(define-typed arg id arg-type arg-pred-id temp-id) ...
						(crocodile-unparsed-block id type-name pred-id . body))))
					#'rest)))]
			   ;; --- Error handling ---
			   [((#%parens . prest) . bad-rest)
			    parens-ok?
			    (begin
			      (parse-arguments #'prest #'id)
			      (raise-syntax-error 
			       '|procedure declaration|
				 "braces for function body after parenthesized arguments"
				 (stx-car #'rest)
				 #'id))]
			   [_else
			    (raise-syntax-error 
			     '|declaration|
			       (if parens-ok?
				   "expected either = (for variable intialization) or parens (for function arguments)"
				   "expected = (for variable initialization)")
			       #'id)])))]
		  [_else
		   (raise-syntax-error #f 
				       (format "expected a identifier after ~a" after-what)
				       after
				       #'id)])))]
	   [(type-context? ctx) 
	    (values (make-croc-type (stx-car orig-stx) pred-def pred-id) (stx-cdr orig-stx))]
	   [(expression-context? ctx)
	    (raise-syntax-error #f 
				"illegal in an expression context"
				(stx-car orig-stx))])))))

   (define (make-proc-predicate name form)
     ;; Form start with a operator-transformer sequence
     (let-values ([(args-stx -> result-stx) 
		   (let loop ([stx (stx-cdr (stx-car form))][args null])
		     (if (and (identifier? (stx-car stx))
			      (module-identifier=? #'-> (stx-car stx)))
			 (values (reverse args) (stx-car stx) (stx-cdr stx))
			 (loop (stx-cdr stx) (cons (stx-car stx) args))))])
       (when (stx-null? result-stx)
	 (raise-type-error
	  #f
	  "missing type for result"
	  ->))
       (let ([arg-types
	      (let loop ([args-stx args-stx])
		(if (stx-null? args-stx)
		    null
		    (let ([trans (get-transformer args-stx)])
		      (unless trans
			(raise-type-error '->
					  "non-type within a procedure-type construction"
					  (stx-car args-stx)))
		      (let-values ([(type rest-stx) (trans args-stx type-context)])
			(cons type (loop rest-stx))))))]
	     [result-type 
	      (let ([trans (get-transformer result-stx)])
		(unless trans
		  (raise-type-error '->
				    "non-type in result position for procedure-type construction"
				    (stx-car result-stx)))
		(let-values ([(type rest-stx) (trans result-stx type-context)])
		  (unless (stx-null? rest-stx)
		    (raise-type-error '->
				      "extra tokens following result for procedure-type construction"
				      (stx-car rest-stx)))
		  type))])
	 (with-syntax ([(arg ...) (generate-temporaries arg-types)]
		       [(arg-type ...) (map croc-type-stx arg-types)]
		       [(arg-pred-def ...) (map croc-type-pred-def-stx arg-types)]
		       [(arg-pred-id ...) (map croc-type-pred-stx arg-types)]
		       [result-type (croc-type-stx result-type)]
		       [result-pred-def (croc-type-pred-def-stx result-type)]
		       [result-pred-id (croc-type-pred-stx result-type)]
		       [n (length arg-types)])
	   #`(begin
	       arg-pred-def ...
	       result-pred-def
	       (define (#,name v)
		 (if (and (procedure? v)
			  (procedure-arity-includes? v n))
		     (values #t (lambda (arg ...)
				  (check-expr
				   #f #t result-type result-pred-id
				   (v (check-expr #f #f arg-type arg-pred-id arg) ...))))
		     (values #f #f))))))))
       
   (define (compatible-type? val-expr val-type target-type)
     (and (identifier? target-type)
	  (identifier? val-type)
	  (or (module-identifier=? val-type target-type)
	      (module-identifier=? #'obj target-type)
	      (and (number? (syntax-e val-expr))
		   (module-identifier=? #'num target-type))
	      (and (integer? (syntax-e val-expr))
		   (exact? (syntax-e val-expr))
		   (module-identifier=? #'int target-type))
	      (and (string? (syntax-e val-expr))
		   (module-identifier=? #'string-type target-type))))))
      
   (define (check proc who type-name pred val)
     (let-values ([(tst new-val) (pred val)])
       (unless tst
	 (raise
	  (make-exn:fail:contract
	   (string->immutable-string
	    (format "~a: expected ~a value for ~a, got something else: ~e"
		    (or proc (if (eq? who #t) #f who) "procedure")
		    type-name
		    (cond
		     [(eq? who #t) "result"]
		     [else (if proc 
			       (format "~a argument" who)
			       (if who
				   "initialization"
				   "argument"))])
		    val))
	   (current-continuation-marks))))
       new-val))

  (define-syntax (check-expr stx)
    (syntax-case stx ()
      [(_ proc who type-name pred val)
       ;; Avoid the check if the static types are consistent
       (let ([v (local-expand
		 #'val
		 'expression
		 (cons #'croc-typed
		       kernel-forms))])
	 (syntax-case v (croc-typed)
	   [(croc-typed val val-type)
	    (compatible-type? #'val #'val-type #'type-name)
	    ;; No run-time check:
	    #'val]
	   [_else
	    ;; Even without a type for v, we might see a literal,
	    ;;  or maybe the declaration is simply obj
	    (if (compatible-type? v #'obj #'type-name)
		;; No run-time check:
		#'val
		;; Run-time check:
		#'(check proc who 'type-name pred val))]))]))

  (define-syntax (define-typed stx)
    (syntax-case stx ()
      [(_ id proc-name type-name pred-id val)
       (with-syntax ([gen-id (car (generate-temporaries (list #'id)))])
	 #'(begin
	     (define gen-id val)
	     (define-syntax id
	       (make-set!-transformer
		(lambda (stx)
		  (syntax-case stx (set!)
		    [(set! id rhs)
		     #'(set! gen-id (check-expr set! id type-name pred-id rhs))]
		    [(id arg (... ...))
		     #'(#%app (croc-typed gen-id type-name) arg (... ...))]
		    [id
		     #'(croc-typed gen-id type-name)]))))))]))

  (define-syntax (define-typed-procedure stx)
    (syntax-case stx ()
      [(_ id arg-spec val)
       (with-syntax ([gen-id (car (generate-temporaries (list #'id)))])
	 #'(begin
	     (define gen-id val)
	     (define-syntax id
	       (with-syntax ([((arg arg-type pred-id) (... ...)) (quote-syntax arg-spec)])
		 (make-set!-transformer
		  (lambda (stx)
		    (syntax-case stx (set!)
		      [(set! id rhs)
		       (raise-syntax-error #f
					   "cannot assign to procedure name"
					   stx
					   #'id)]
		      [(id actual-arg (... ...))
		       (let ([actual-args (syntax->list #'(actual-arg (... ...)))]
			     [formal-args (syntax->list #'(arg (... ...)))])
			 (unless (= (length actual-args)
				    (length formal-args))
			   (raise-syntax-error
			    'id
			    (format "expects ~a arguments, provided ~a"
				    (length formal-args)
				    (length actual-args))
			    stx))
			 #'(#%app (croc-typed gen-id type-name) 
				  (check-expr 'id 'arg arg-type pred-id actual-arg) 
				  (... ...)))]
		      [id
		       #'(croc-typed (let ([id (lambda (arg (... ...))
						 (id arg (... ...)))])
				       id)
				     type-name)])))))))]))

  (define-syntax croc-typed
    (syntax-rules ()
      [(_ expr type) expr]))

  (require-for-syntax (lib "context.ss" "syntax"))
  (define-syntax (crocodile-block stx)
    ;; A block can have mixed exprs and defns. Wrap expressions with
    ;; `(define-values () ... (values))' as needed, and add a (void)
    ;; at the end if needed. Also, wrap the final expression with
    ;; a type check as needed.
    (let ([proc-id (stx-car (stx-cdr stx))]
	  [result-type-name (stx-car (stx-cdr (stx-cdr stx)))]
	  [result-pred-id (stx-car (stx-cdr (stx-cdr (stx-cdr stx))))]
	  [exprs (let loop ([exprs (cddddr (syntax->list stx))])
		   (apply 
		    append
		    (map (lambda (expr)
			   (let ([expr (local-expand
					expr
					(generate-expand-context)
					kernel-forms)])
			     (syntax-case expr (begin)
			       [(begin . rest)
				(loop (syntax->list #'rest))]
			       [else
				(list expr)])))
			 exprs)))])
      #`(let ()
	  #,@(let loop ([exprs exprs][prev-defns null][prev-exprs null])
	       (cond
		[(null? exprs) (append 
				(reverse prev-defns)
				(if (pair? prev-exprs)
				    (reverse (cons
					      #`(check-expr '#,proc-id #t
							    #,result-type-name 
							    #,result-pred-id 
							    #,(car prev-exprs))
					      (cdr prev-exprs)))
				    (begin
				      (unless (module-identifier=? #'type-name #'void-type)
					(error "no expression for type check; should have been "
					       "caught earlier"))
				      (reverse prev-exprs)))
				(if (null? prev-exprs)
				    (list #'(void))
				    null))]
		[(and (stx-pair? (car exprs))
		      (or (module-identifier=? #'define-values (stx-car (car exprs)))
			  (module-identifier=? #'define-syntaxes (stx-car (car exprs)))))
		 (loop (cdr exprs)
		       (cons (car exprs)
			     (append
			      (map (lambda (expr)
				     #`(define-values () (begin #,expr (values))))
				   prev-exprs)
			      prev-defns))
		       null)]
		[else
		 (loop (cdr exprs) prev-defns (cons (car exprs) prev-exprs))])))))

  (define-syntax (crocodile-unparsed-block stx)
    (syntax-case stx (void)
      [(_ proc-id result-type-name result-pred-id . body) 
       #`(crocodile-block proc-id result-type-name result-pred-id #,@(parse-block #'body))]))

  (define-syntax (crocodile-return stx)
    (syntax-case stx ()
      [(_ expr) #'expr]))

  (define-syntax (#%parens stx)
    (syntax-case stx ()
      [(_ rator (rand ...)) (syntax/loc #'rator (rator rand ...))]))

  ;; --------------------------------------------------------
  ;; Defining a new transformer or new type

  (require-for-syntax (lib "define.ss" "syntax"))
  (define-syntax (define-crocodile-syntax stx)
    (let-values ([(id rhs) (normalize-definition stx #'lambda #f)])
      (with-syntax ([id id]
		    [rhs rhs])
	#'(define-syntax id (make-crocodile-transformer rhs)))))

  (define-syntax (define-type stx)
    (syntax-case stx ()
      [(_ id pred-expr)
       (identifier? #'id)
       (with-syntax ([pred-id (car (generate-temporaries '(pred)))])
	 #'(begin
	     (define pred-id (let ([pred pred-expr])
				(lambda (v)
				  (values (pred v) v))))
	     (define-syntax id (make-crocodile-type #'pred-id #f))))]))

  (define-syntax (define-type-constructor stx)
    (syntax-case stx ()
      [(_ id generator-expr)
       (identifier? #'id)
       #'(define-syntax id (make-crocodile-type #f generator-expr))]))

  ;; ----------------------------------------
  ;;  Pre-defined types and forms

  (define (exact-integer? v)
    (and (integer? v) (exact? v)))

  (define-type int exact-integer?)
  (define-type num number?)
  (define-type obj (lambda (x) #t))
  (define-type string-type string?)

  (define-type-constructor -> make-proc-predicate)

  (define-crocodile-syntax croc-provide
    (lambda (body ctx)
      (unless (top-block-context? ctx)
	(raise-syntax-error #f "not allowed outside the top level" (stx-car body)))
      (let loop ([body (stx-cdr body)][prev-comma? #f])
	(syntax-case body (\, \;)
	  [(\; . rest)
	   (not prev-comma?)
	   (values #`(begin) #'rest)]
	  [(id \, . rest)
	   (identifier? #'id)
	   (let-values ([(decls rest) (loop #'rest #t)])
	     (values #`(begin (provide id) #,decls) rest))]
	  [(id \; . rest)
	   (identifier? #'id)
	   (values #'(provide id) #'rest)]))))

  (define-crocodile-syntax croc-return
    (lambda (stx ctx)
      (unless (block-context? ctx)
	(raise-syntax-error #f "allowed only in a block context" (stx-car stx)))
      (let-values ([(val-stxs after-expr) (extract-until (stx-cdr stx)
							 (list #'\;))])
	(unless val-stxs
	  (raise-syntax-error 
	   #f
	   "missing semicolon"
	   (stx-car stx)))
	(when (null? val-stxs)
	  (raise-syntax-error 
	   #f
	   "missing expression"
	   (stx-car stx)))
	(with-syntax ([expr (parse-expr val-stxs)])
	  (unless (stx-null? (stx-cdr after-expr))
	    (raise-syntax-error 
	     #f
	     "not at a block end"
	     (stx-car stx)))
	  (values
	   (syntax/loc (stx-car stx)
	     (crocodile-return expr))
	   null)))))

  ;; ----------------------------------------
  ;; Main compiler loop

  (define-syntax (croc-unparsed-begin stx)
    (syntax-case stx ()
      [(_) #'(begin)]
      [(_ . body) (let-values ([(code rest) (parse-block-one top-block-context
							     #'body 
							     values
							     (lambda ()
							       (values #'(void) null)))])
		    #`(begin
			#,code
			(croc-unparsed-begin #,@rest)))]))

  (define-syntax (croc-module-begin stx)
    #`(#%module-begin
       (croc-unparsed-begin #,@(stx-cdr stx))))
  
  (provide int obj (rename string-type string) ->
	   (rename set! =)
	   (rename croc-return return)
	   + - * / (rename modulo %)
	   #%datum
	   #%top
	   #%parens
	   (rename croc-module-begin #%module-begin)
	   (rename croc-provide provide)))
