(module crocodile mzscheme

  (require-for-syntax (lib "stx.ss" "syntax")
		      "private/ops.ss"
		      "private/util.ss")
  
  (begin-for-syntax

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
		  (procedure-arity-includes? proc 1))
       (raise-type-error
	'define-crocodile-syntax
	"procedure (arity 1)"
	proc))
     (make-croc-trans proc))

   ;; --------------------------------------------------------
   ;; Type --- has transformer and id for a predicate

   (define-values (struct:croc-type make-croc-type croc-type? croc-type-ref croc-type-set!)
     (make-struct-type 'croc-type #f 2 0 #f 
		       (list (list prop:croc-transformer #t))
		       (current-inspector) 0))

   (define (croc-type-pred-id v) (croc-type-ref v 1))

   ;; --------------------------------------------------------
   ;; Parsing blocks

   (define (parse-block stx)
     (let loop ([body stx])
       (cond
	[(stx-null? body) null]
	[(and (identifier? (stx-car body))
	      (let ([v (syntax-local-value (stx-car body) (lambda () #f))])
		(and (croc-transformer? v) v)))
	 => (lambda (transformer)
	      (let-values ([(code rest) (transformer body)])
		(cons code  (loop rest))))]
	[else
	 (raise-syntax-error 
	  'block
	  "unknown form" 
	  (stx-car body))])))

   ;; --------------------------------------------------------
   ;; Parsing expressions

   (define parse-expr
     (let ()
       (define (parse-expr-seq stx)
	 (define (start-expr stx) 
	   (let ([trans (and (identifier? (stx-car stx))
			     (let ([v (syntax-local-value (stx-car stx) (lambda () #f))])
			       (and (croc-transformer? v) v)))])
	     (if trans
		 (let-values ([(expr rest) (trans stx)])
		   (if (stx-null? rest)
		       (list expr)
		       (cons expr (start-operator rest))))
		 (syntax-case stx (#%parens)
		   [(v)
		    (or (number? (syntax-e #'v))
			(identifier? #'v)
			(string? (syntax-e #'v)))
		    (list #'v)]
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
     ;; Defer checking whether type is a valid type until later
     (cons (list id type)
	   (k)))

   (define (parse-arguments orig-args-stx proc-id)
     (if (stx-null? orig-args-stx)
	 null
	 (let loop ([args-stx orig-args-stx]
		    [where "at start of argument sequence"]
		    [where-stx orig-args-stx])
	   (syntax-case args-stx ()
	     [(type id)
	      (and (identifier? #'id)
		   (identifier? #'type))
	      (parse-one-argument proc-id #'type #'id
				  (lambda () null))]
	     [(type id comma . rest)
	      (and (identifier? #'id)
		   (identifier? #'type)
		   (module-identifier=? #'comma #'\,))
	      (parse-one-argument proc-id #'type #'id
				  (lambda ()
				    (loop #'rest
					  "after comma"
					  #'comma)))]
	     [_else
	      (raise-syntax-error
	       '|procedure declaration|
	       (format "expected a type name and identifier ~a" where)
	       where-stx)]))))

   (define (make-crocodile-type pred-id)
     (make-croc-type
      (lambda (orig-stx)
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
		       (let ([def #`(define id (check-expr #f 'id 'type-name pred-id
							   #,(parse-expr val-stxs)))])
			 (if (module-identifier=? #'\; (stx-car after-expr))
			     (values def (stx-cdr after-expr))
			     (let-values ([(defs remainder kind) (loop (stx-cdr after-expr) (stx-car after-expr) "comma" #f)])
			       (values #`(begin #,def #,defs) remainder)))))
		     ;; -- Procedure declaration
		     (syntax-case #'rest (#%parens \;)
		       [((#%parens . prest) (#%braces . body) . rest)
			parens-ok?
			(with-syntax ([((arg arg-type) ...) (parse-arguments #'prest #'id)])
			  (values #`(define (id arg ...)
				      (let ([arg (check-expr-for-type id arg arg-type)] ...)
					(crocodile-block #,@(parse-block #'body))))
				  #'rest))]
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
				   #'id)]))))
      pred-id)))

  (define (check proc who type-name pred val)
    (unless (pred val)
      (raise
       (make-exn:fail:contract
	(string->immutable-string
	 (format "~a: expected ~a value for ~a, got something else: ~e"
		 (or proc who)
		 type-name
		 (if proc (format "~a argument" who) "initialization")
		 val))
	(current-continuation-marks))))
    val)

  (define-syntax (check-expr stx)
    (syntax-case stx ()
      [(_ proc who type-name pred val)
       #'(check proc who type-name pred val)]))

  (define-syntax (check-expr-for-type stx)
    (syntax-case stx ()
      [(_ proc-id arg-id type)
       (let ([v (syntax-local-value #'type (lambda () #f))])
	 (unless (croc-type? v)
	   (raise-syntax-error
	    #f
	    (format "not a type name for the argument ~a of ~a"
		    (syntax-e #'arg-id) (syntax-e #'proc-id))
	    #'type))
	 #`(check-expr 'proc-id 'arg-id 'type #,(croc-type-pred-id v) arg-id))]))

  (require-for-syntax (lib "context.ss" "syntax")
		      (lib "kerncase.ss" "syntax"))
  (define-for-syntax kernel-forms (kernel-form-identifier-list #'here))
  (define-syntax (crocodile-block stx)
    ;; A block can have mixed exprs and defns. Wrap expressions with
    ;; `(define-values () ... (values))' as needed, and add a (void)
    ;; at the end if needed.
    (let ([exprs (let loop ([exprs (cdr (syntax->list stx))])
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
				(reverse prev-exprs)
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

  (define-syntax (crocodile-return stx)
    (syntax-case stx ()
      [(_ expr) #'expr]))

  (define-syntax (#%parens stx)
    (syntax-case stx ()
      [(_ rator (rand ...)) #'(rator rand ...)]))

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
	     (define pred-id pred-expr)
	     (define-syntax id (make-crocodile-type #'pred-id))))]))

  ;; ----------------------------------------
  ;;  Pre-defined types and forms

  (define-type int integer?)
  (define-type obj (lambda (x) #t))
  (define-type string-type string?)

  (define-crocodile-syntax croc-provide
    (lambda (body)
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
    (lambda (stx)
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

  (define-syntax (croc-module-begin stx)
    #`(#%module-begin
       #,@(let ([v (parse-block (stx-cdr stx))])
	    #;(printf "~a~n" (syntax-object->datum (datum->syntax-object #f v)))
	    v)))
  
  (provide int obj (rename string-type string)
	   (rename set! =)
	   (rename croc-return return)
	   + - * / (rename modulo %)
	   #%datum
	   #%top
	   #%parens
	   (rename croc-module-begin #%module-begin)
	   (rename croc-provide provide)))
