;; This file is converted to [c]startup.inc and evaluated by
;; MzScheme's scheme_basic_env().

;; It implements, in a non-bootstrapping way, all of the MzScheme syntax
;; and "primitives" that are not implemented in the kernel.

;; Replace the content of this file to get a different set of initial
;; module definitions and initial imports. The top-level `require'
;; works in a special way for this file: for each variable
;; (non-syntax) import, it copies the binding to a fresh top-level
;; variable.

;; Do not use block comments (with #| and |#) in this file. The 
;; pre-processing script can't handle them.

;;----------------------------------------------------------------------
;; basic syntax utilities

(module #%stx #%kernel

  ;; These utilities facilitate operations on syntax objects.
  ;; A syntax object that represents a parenthesized sequence
  ;; can contain a mixture of cons cells and syntax objects,
  ;; hence the need for `stx-null?', `stx-car', etc.

  ;; a syntax identifier?
  (define-values (identifier?)
    (lambda (p)
      (if (syntax? p)
	  (symbol? (syntax-e p))
	  #f)))

  ;; a syntax null?
  (define-values (stx-null?)
    (lambda (p)
      (if (null? p)
	  #t
	  (if (syntax? p) 
	      (null? (syntax-e p))
	      #f))))

  ;; null if a syntax null?, else escape
  (define-values (stx-null/esc)
    (lambda (p esc)
      (if (null? p)
	  null
	  (if (syntax? p) 
	      (if (null? (syntax-e p))
		  null
		  (esc #f))
	      (esc #f)))))

  ;; a syntax pair?
  (define-values (stx-pair?)
    (lambda (p)
      (if (pair? p)
	  #t
	  (if (syntax? p)
	      (pair? (syntax-e p))
	      #f))))

  ;; a syntax list?
  (define-values (stx-list?)
    (lambda (p)
      (if (list? p)
	  #t
	  (if (syntax? p) 
	      (if (list? (syntax-e p))
		  #t
		  (let loop ([l (syntax-e p)])
		    (if (pair? l)
			(loop (cdr l))
			(stx-list? l))))
	      (if (pair? p)
		  (stx-list? (cdr p))
		  #f)))))

  ;; car of a syntax pair
  (define-values (stx-car)
    (lambda (p)
      (if (pair? p)
	  (car p)
	  (car (syntax-e p)))))

  ;; car of a syntax pair or escape
  (define-values (stx-car/esc)
    (lambda (p esc)
      (if (pair? p)
	  (car p)
	  (if (syntax? p)
	      (let ([v (syntax-e p)])
		(if (pair? v)
		    (car v)
		    (esc #f)))
	      (esc #f)))))

  ;; cdr of a syntax pair
  (define-values (stx-cdr)
    (lambda (p)
      (if (pair? p)
	  (cdr p)
	  (cdr (syntax-e p)))))

  ;; Flattens a syntax list into a list
  (define-values (stx->list)
    (lambda (e)
      (if (syntax? e)
	  (syntax->list e)
	  (let ([flat-end
		 (let loop ([l e])
		   (if (null? l) 
		       #f
		       (if (pair? l)
			   (loop (cdr l))
			   (if (syntax? l) 
			       (syntax->list l)
			       #f))))])
	    (if flat-end
		;; flatten
		(let loop ([l e])
		  (if (null? l) 
		      null
		      (if (pair? l) 
			  (cons (car l) (loop (cdr l)))
			  (if (syntax? l) 
			      flat-end))))
		e)))))

  (provide identifier? stx-null? stx-null/esc stx-pair? stx-list?
	   stx-car stx-car/esc stx-cdr stx->list))

;;----------------------------------------------------------------------
;; quasiquote

(module #%qq-and-or #%kernel
  (require-for-syntax #%stx #%kernel)

  (define-syntaxes (quasiquote)
    (let ([here (quote-syntax here)] ; id with module bindings, but not lexical
	  [unquote-stx (quote-syntax unquote)]
	  [unquote-splicing-stx (quote-syntax unquote-splicing)])
      (lambda (in-form)
	(if (identifier? in-form)
	    (raise-syntax-error #f "bad syntax" in-form))
	(let-values
	    (((form) (if (stx-pair? (stx-cdr in-form))
			 (if (stx-null? (stx-cdr (stx-cdr in-form)))
			     (stx-car (stx-cdr in-form))
			     (raise-syntax-error #f "bad syntax" in-form))
			 (raise-syntax-error #f "bad syntax" in-form)))
	     ((normal)
	      (lambda (x old)
		(if (eq? x old)
		    (if (stx-null? x) 
			(quote-syntax ())
			(list (quote-syntax quote) x))
		    x))))
	  (datum->syntax-object
	   here
	   (normal
	    (letrec-values
		(((qq)
		  (lambda (x level)
		    (let-values
			(((qq-list)
			  (lambda (x level)
			    (let-values
				(((old-first) (stx-car x)))
			      (let-values
				  (((old-second) (stx-cdr x)))
				(let-values
				    (((first) (qq old-first level)))
				  (let-values
				      (((second) (qq old-second level)))
				    (let-values
					()
				      (if (if (eq? first old-first)
					      (eq? second old-second)
					      #f)
					  x
					  (list
					   (quote-syntax cons)
					   (normal first old-first)
					   (normal second old-second)))))))))))
		      (if (stx-pair? x)
			  (let-values
			      (((first) (stx-car x)))
			    (if (if (if (identifier? first)
					(module-identifier=? first unquote-stx)
					#f)
				    (stx-list? x)
				    #f)
				(let-values
				    (((rest) (stx-cdr x)))
				  (if (let-values
					  (((g35) (not (stx-pair? rest))))
					(if g35 g35 (not (stx-null? (stx-cdr rest)))))
				      (raise-syntax-error
				       unquote-stx
				       "takes exactly one expression"
				       in-form))
				  (if (zero? level)
				      (stx-car rest)
				      (qq-list x (sub1 level))))
				(if (if (if (identifier? first)
					    (module-identifier=? first (quote-syntax quasiquote))
					    #f)
					(stx-list? x)
					#f)
				    (qq-list x (add1 level))
				    (if (if (if (identifier? first)
						(module-identifier=? first unquote-splicing-stx)
						#f)
					    (stx-list? x)
					    #f)
					(raise-syntax-error
					 unquote-splicing-stx
					 "invalid context within quasiquote"
					 in-form)
					(if (if (stx-pair? first)
						(if (identifier? (stx-car first))
						    (if (module-identifier=? (stx-car first)
									     unquote-splicing-stx)
							(stx-list? first)
							#F)
						    #f)
						#f)
					    (let-values
						(((rest) (stx-cdr first)))
					      (if (let-values
						      (((g34) (not (stx-pair? rest))))
						    (if g34
							g34
							(not (stx-null? (stx-cdr rest)))))
						  (raise-syntax-error
						   unquote-splicing-stx
						   "takes exactly one expression"
						   in-form))
					      (let-values
						  (((uqsd) (stx-car rest))
						   ((old-l) (stx-cdr x))
						   ((l) (qq (stx-cdr x) level)))
						(if (zero? level)
						    (let-values
							(((l) (normal l old-l)))
						      (let-values
							  ()
							(list (quote-syntax append) uqsd l)))
						    (let-values
							(((restx) (qq-list rest (sub1 level))))
						      (let-values
							  ()
							(if (if (eq? l old-l)
								(eq? restx rest)
								#f)
							    x
							    (list
							     (quote-syntax cons)
							     (list
							      (quote-syntax cons)
							      (quote-syntax (quote unquote-splicing))
							      (normal restx rest))
							     (normal l old-l))))))))
					    (qq-list x level))))))
			  (if (if (syntax? x) 
				  (vector? (syntax-e x))
				  #f)
			      (let-values
				  (((l) (vector->list (syntax-e x))))
				(let-values
				    (((l2) (qq l level)))
				  (let-values
				      ()
				    (if (eq? l l2)
					x
					(list (quote-syntax list->vector) l2)))))
			      (if (if (syntax? x) (box? (syntax-e x)) #f)
				  (let-values
				      (((v) (unbox (syntax-e x))))
				    (let-values
					(((qv) (qq v level)))
				      (let-values
					  ()
					(if (eq? v qv)
					    x
					    (list (quote-syntax box) qv)))))
				  x)))))))
	      (qq form 0))
	    form)
	   in-form)))))

  (define-syntaxes (and)
    (let ([here (quote-syntax here)])
      (lambda (x)
	(if (not (stx-list? x))
	    (raise-syntax-error #f "bad syntax" x))
	(let ([e (stx-cdr x)])
	  (if (stx-null? e)
	      (quote-syntax #t)
	      (if (if (stx-pair? e)
		      (stx-null? (stx-cdr e))
		      #t)
		  (stx-car e)
		  (datum->syntax-object
		   here
		   (list (quote-syntax if)
			 (stx-car e)
			 (cons (quote-syntax and)
			       (stx-cdr e))
			 (quote-syntax #f))
		   x)))))))

  (define-syntaxes (or)
    (let ([here (quote-syntax here)])
      (lambda (x)
	(if (identifier? x)
	    (raise-syntax-error #f "bad syntax" x))
	(let ([e (stx-cdr x)])
	  (if (stx-null? e) 
	      (quote-syntax #f)
	      (if (if (stx-pair? e)
		      (stx-null? (stx-cdr e))
		      #f)
		  (stx-car e)
		  (if (stx-list? e)
		      (let ([tmp 'or-part])
			(datum->syntax-object
			 here
			 (list (quote-syntax let) (list
						   (list
						    tmp
						    (stx-car e)))
			       (list (quote-syntax if)
				     tmp
				     tmp
				     (cons (quote-syntax or)
					   (stx-cdr e))))
			 x))
		      (raise-syntax-error 
		       #f
		       "bad syntax"
		       x))))))))

  (provide quasiquote and or))

;;----------------------------------------------------------------------
;; cond

(module #%cond #%kernel
  (require-for-syntax #%stx #%qq-and-or #%kernel)

  (define-syntaxes (cond)
    (let ([here (quote-syntax here)])
      (lambda (in-form)
	(if (identifier? in-form)
	    (raise-syntax-error #f "bad syntax" in-form))
	(datum->syntax-object
	 here
	 (let ([form (stx-cdr in-form)]
	       [serror
		(lambda (msg at)
		  (raise-syntax-error #f msg in-form at))])
	   (let loop ([tests form])
	     (if (stx-null? tests)
		 (quote-syntax (void))
		 (if (not (stx-pair? tests))
		     (serror
		      "bad syntax (body must contain a list of pairs)"
		      tests)
		     (let ([line (stx-car tests)]
			   [rest (stx-cdr tests)])
		       (if (not (stx-pair? line))
			   (serror
			    "bad syntax (clause is not a test-value pair)"
			    line)
			   (let* ([test (stx-car line)]
				  [value (stx-cdr line)]
				  [else? (and (identifier? test)
					      (module-identifier=? test (quote-syntax else)))])
			     (if (and else? (stx-pair? rest))
				 (serror "bad syntax (`else' clause must be last)" line))
			     (if (and (stx-pair? value)
				      (identifier? (stx-car value))
				      (module-identifier=? (stx-car value) (quote-syntax =>)))
				 (if (and (stx-pair? (stx-cdr value))
					  (stx-null? (stx-cdr (stx-cdr value))))
				     (let ([test (if else?
						     #t 
						     test)]
					   [gen (gensym)])
				       `(,(quote-syntax let) ([,gen ,test])
					 (,(quote-syntax if) ,gen
					  (,(stx-car (stx-cdr value)) ,gen)
					  ,(loop rest))))
				     (serror
				      "bad syntax (bad clause form with =>)"
				      line))
				 (if else?
				     (cons (quote-syntax begin) value)
				     (if (stx-null? value)
					 (let ([gen (gensym)])
					   `(,(quote-syntax let) ([,gen ,test])
					     (,(quote-syntax if) ,gen ,gen ,(loop rest))))
					 (list
					  (quote-syntax if) test
					  (cons (quote-syntax begin) value)
					  (loop rest))))))))))))
	 in-form))))

  (provide cond))

;;----------------------------------------------------------------------
;; define, when, unless, let/ec, define-struct

(module #%define-et-al #%kernel
  (require-for-syntax #%kernel #%stx #%qq-and-or #%cond)

  (define-syntaxes (define define-syntax)
    (let ([here (quote-syntax here)])
      (let ([mk-define
	     (lambda (base)
	       (lambda (code)
		 (if (or (identifier? code)
			 (not (stx-pair? (stx-cdr code))))
		     (raise-syntax-error #f "bad syntax" code))
		 (let ([body (stx-cdr code)])
		   (if (stx-null? body)
		       (raise-syntax-error
			#f
			"bad syntax (no definition body)"
			code))
		   (let ([first (stx-car body)]) 
		     (cond
		      [(identifier? first)
		       (if (and (stx-pair? (stx-cdr body))
				(stx-null? (stx-cdr (stx-cdr body))))
			   (datum->syntax-object
			    here
			    `(,base (,first) ,@(stx->list (stx-cdr body)))
			    code)
			   (raise-syntax-error
			    #f
			    "bad syntax (zero or multiple expressions after identifier)"
			    code))]
		      [(stx-pair? first)
		       (let ([bad-symbol  (lambda (s) (raise-syntax-error #f
									  "bad identifier"
									  code
									  s))])
			 (let loop ([l first])
			   (cond
			    [(stx-null? l) #f]
			    [(stx-pair? l) 
			     (if (identifier? (stx-car l))
				 (loop (stx-cdr l))
				 (bad-symbol (stx-car l)))]
			    [(identifier? l) #f]
			    [else (bad-symbol l)])))
		       (datum->syntax-object
			(quote-syntax here)
			`(,base (,(stx-car first)) 
				(lambda ,(stx-cdr first) ,@(stx->list (stx-cdr body))))
			code)]
		      [else
		       (raise-syntax-error
			#f
			"not an identifier"
			code
			first)])))))])
	(values (mk-define (quote-syntax define-values))
		(mk-define (quote-syntax define-syntaxes))))))

  (define-syntax when
    (lambda (x)
      (let ([l (syntax->list x)])
	(if (and l
		 (> (length l) 2))
	    (datum->syntax-object
	     (quote-syntax here)
	     (list (quote-syntax if)
		   (stx-car (stx-cdr x))
		   (list*
		    (quote-syntax begin)
		    (stx-cdr (stx-cdr x))))
	     x)
	    (raise-syntax-error
	     #f
	     "bad syntax"
	     x)))))

  (define-syntax unless
    (lambda (x)
      (let ([l (syntax->list x)])
	(if (and l
		 (> (length l) 2))
	    (datum->syntax-object
	     (quote-syntax here)
	     (list (quote-syntax if)
		   (cadr l)
		   (quote-syntax (void))
		   (list*
		    (quote-syntax begin)
		    (cddr l)))
	     x)
	    (raise-syntax-error
	     #f
	     "bad syntax"
	     x)))))

  (define-syntax let/ec 
    (lambda (code)
      (let ([l (syntax->list code)])
	(if (and l
		 (> (length l) 2)
		 (identifier? (cadr l)))
	    (let ([var (cadr l)]
		  [exprs (stx-cdr (stx-cdr code))])
	      (datum->syntax-object
	       (quote-syntax here)
	       `(call/ec (lambda (,var) ,@(stx->list exprs)))
	       code))
	    (raise-syntax-error
	     #f
	     "bad syntax"
	     code)))))

  (define-syntax define-struct
    (lambda (stx)
      (if (identifier? stx)
	  (raise-syntax-error #f "bad syntax" stx))
      (let ([body (stx->list (stx-cdr stx))])
	(let ([syntax-error
	       (lambda (s . detail)
		 (apply
		  raise-syntax-error
		  #f
		  s
		  stx
		  detail))]
	      [build-struct-names
	       (lambda (name fields)
		 (let ([name (symbol->string (syntax-e name))]
		       [fields (map symbol->string (map syntax-e fields))]
		       [+ string-append])
		   (map string->symbol
			(append
			 (list 
			  (+ "struct:" name)
			  (+ "make-" name)
			  (+ name "?"))
			 (apply
			  append
			  (map
			   (lambda (f) 
			     (list 
			      (+ name "-" f)
			      (+ "set-" name "-" f "!")))
			   fields))))))])
	  (or (pair? body)
	      (syntax-error "empty declaration"))
	  (or (stx-list? body)
	      (syntax-error "illegal use of `.'"))
	  (or (<= 2 (length body) 3)
	      (syntax-error "wrong number of parts"))
	  (or (identifier? (car body))
	      (and (stx-pair? (car body))
		   (identifier? (stx-car (car body)))
		   (stx-pair? (stx-cdr (car body)))
		   (stx-null? (stx-cdr (stx-cdr (car body)))))
	      (syntax-error "first part must be an identifier or identifier-expression pair"))
	  (or (stx-list? (cadr body))
	      (syntax-error "illegal use of `.' in field list"))
	  (for-each (lambda (x) 
		      (or (identifier? x)
			  (syntax-error "field name not a identifier" x)))
		    (stx->list (cadr body)))
	  (let ([name (if (identifier? (car body))
			  (car body)
			  (stx-car (car body)))]
		[fields (stx->list (cadr body))]
		[inspector (if (null? (cddr body))
			       #f
			       (caddr body))])
	    (datum->syntax-object
	     (quote-syntax here)
	     `(define-values
		,(map (lambda (n) (datum->syntax-object name n name)) (build-struct-names name fields))
		,(let ([core
			`(let-values ([(type maker pred access mutate)
				       (make-struct-type ',name
							 ,(if (identifier? (car body))
							      #f
							      (stx-car (stx-cdr (car body))))
							 ,(length fields)
							 0 #f null
							 ,(if inspector
							      'inspector
							      #f))])
			   (values type maker pred
				   ,@(let loop ([fields fields][n 0])
				       (if (null? fields)
					   null
					   (list* `(make-struct-field-accessor access ,n ',(car fields))
						  `(make-struct-field-mutator mutate ,n ',(car fields))
						  (loop (cdr fields) (add1 n)))))))])
		   (if inspector
		       `(let ([inspector ,inspector])
			  (if (not (inspector? inspector))
			      (raise-type-error 'define-struct "inspector" inspector))
			  ,core)
		       core)))
	     stx))))))

  (provide define define-syntax when unless let/ec define-struct))

;;----------------------------------------------------------------------
;; #%small-scheme: assembles all basic forms we have so far

(module #%small-scheme #%kernel
  (require #%stx #%qq-and-or #%cond #%define-et-al)

  (provide (all-from #%qq-and-or)
	   (all-from #%cond)
	   (all-from #%define-et-al)))

;;----------------------------------------------------------------------
;; pattern-matching utilities
;; based on Shriram's pattern matcher for Zodiac

(module #%sc #%kernel
  (require #%stx #%small-scheme)

  ;; memq on a list of identifiers, and
  ;;  nested identifiers
  (define (stx-memq ssym l)
    (ormap (lambda (p)
	     (and (syntax? p)
		  (module-identifier=? ssym p)))
	   l))
  
  ;; memq on a list of identifiers and
  ;;  nested identifiers, returns a position
  (define (stx-memq-pos ssym l)
    (let loop ([p 0][l l])
      (cond
       [(null? l) #f]
       [(and (syntax? (car l))
	     (module-identifier=? ssym (car l)))
	p]
       [else (loop (add1 p) (cdr l))])))

  ;; Like stx-memq-pos, but goes into nestings to
  ;;  find identifiers.
  (define (stx-memq*-pos ssym l)
    (let loop ([p 0][l l])
      (cond
       [(null? l) #f]
       [(module-identifier=? ssym 
			     (let loop ([i (car l)])
			       (if (syntax? i)
				   i
				   (loop (car i)))))
	p]
       [else (loop (add1 p) (cdr l))])))

  ;; For error reporting:
  (define (pick-specificity e de)
    (if (eq? e de)
	(list e)
	(list e de)))


  (define syntax-case-stx (quote-syntax syntax-case))
  (define syntax-stx (quote-syntax syntax))

  ;;----------------------------------------------------------------------
  ;; Input matcher
  
  ;; Takes syntax pattern and a keyword list and produces a
  ;; matcher. A matcher is a function that takes a syntax input
  ;; and produces a pattern-variable environment or #f.
  ;;
  ;; If `just-vars?' is #t, produces the variables instead of a matcher.
  ;; Each variable is nested in the list corresponding to its ellipsis depth
  ;; in the pattern. We call this the "environment prototype".
  ;;
  ;; In the pattern-variable environment produced by a matcher,
  ;; a variable under a single ellipsis has a list of matches,
  ;; a variable under two ellipses has a list of list of matches, etc.
  ;;
  ;; An environment does not contain any indicication of how far a
  ;; variable is nested. Uses of the variable should be checked separately
  ;; using an environment prototype. Furthemore, the environment
  ;; does not contain the pattern variables as "keys", since the positions
  ;; can also be determined by the prototype.
  ;;
  (define (make-match&env/extract-vars p k just-vars? phase-param?)
    (define top p)
    (define (m&e p local-top use-ellipses? id-is-rest?)
      (cond
       [(and use-ellipses? (ellipsis? p))
	(unless (stx-null? (stx-cdr (stx-cdr p)))
	  (apply
	   raise-syntax-error 
	   syntax-case-stx
	   "misplaced ellipses in pattern"
	   (pick-specificity
	    top
	    local-top)))
	(let* ([p-head (stx-car p)]
	       [nestings (get-ellipsis-nestings p-head k)]
	       [match-head (m&e p-head p-head #t #f)])
	  (if just-vars?
	      (map list nestings)
	      (let ([nest-vars (flatten-nestings nestings (lambda (x) #t))])
		`(lambda (e esc)
		   (if (stx-list? e)
		       (let ([l ,(let ([b (app-e-esc match-head)])
				   (let ([f (if (equal? b '(list e))
						 'list
						 `(lambda (e) ,b))])
				     `(map ,f
					   (stx->list e))))])
			 (if (null? l)
			     (quote ,(map (lambda (v)
					    '())
					  nest-vars))
			     (apply map
				    list
				    l)))
		       (esc #f))))))]
       [(stx-pair? p)
	(let ([hd (stx-car p)])
	  (if (and use-ellipses?
		   (eq? (syntax-e hd) '...))
	      (if (and (stx-pair? (stx-cdr p))
		       (stx-null? (stx-cdr (stx-cdr p))))
		  (let ([dp (stx-car (stx-cdr p))])
		    (m&e dp dp #f #f))
		  (apply
		   raise-syntax-error 
		   syntax-case-stx
		   "misplaced ellipses in pattern"
		   (pick-specificity
		    top
		    local-top)))
	      (let ([match-head (m&e hd hd use-ellipses? #f)]
		    [match-tail (m&e (stx-cdr p) local-top use-ellipses? #t)])
		(if just-vars?
		    (append match-head match-tail)
		    `(lambda (e esc)
		       ,(app-append (app-esc match-head '(stx-car/esc e esc))
				    (app-esc match-tail '(stx-cdr e))))))))]
       [(stx-null? p)
	(if just-vars?
	    null
	    'stx-null/esc)]
       [(identifier? p)
	(if (stx-memq p k)
	    (if just-vars?
		null
		`(lambda (e esc)
		   (if (identifier? e)
		       ;; This module-identifier=? can be turned into
		       ;;  module-transformer-identifier=? by an
		       ;;  enclosing binding.
		       (if (module-identifier=? e (quote-syntax ,p))
			   null
			   (esc #f))
		       (esc #f))))
	    (if (and use-ellipses?
		     (eq? (syntax-e p) '...))
		(apply
		 raise-syntax-error 
		 syntax-case-stx
		 "misplaced ellipses in pattern"
		 (pick-specificity
		  top
		  local-top))
		(if just-vars?
		    (list p)
		    (if id-is-rest?
			`(lambda (e esc)
			   (list (datum->syntax-object #f e)))
			`(lambda (e esc)
			   (list e))))))]
       [else
	(if just-vars?
	    null
	    `(lambda (e esc)
	       (if (equal? ,(syntax-e p) (syntax-e e))
		   null
		   (esc #f))))]))
    (let ([r (m&e p p #t #t)])
      (if just-vars?
	  ;; Look for duplicate uses of variable names:
	  (let ([ht (make-hash-table)])
	    (let loop ([r r])
	      (cond
	       [(syntax? r)
		(let ([l (hash-table-get ht (syntax-e r) (lambda () null))])
		  (when (ormap (lambda (i) (module-identifier=? i r)) l)
		    (raise-syntax-error 
		     syntax-case-stx
		     "variable used twice in pattern"
		     top
		     r))
		  (hash-table-put! ht (syntax-e r) (cons r l)))]
	       [(pair? r)
		(loop (car r))
		(loop (cdr r))]
	       [else (void)]))
	    r)
	  `(lambda (e ,@(if phase-param?
			    '(module-identifier=?) 
			    null))
	     (let/ec esc
	       ,(app-e-esc r))))))

  (define (make-match&env p k phase-param?)
    (make-match&env/extract-vars p k #f phase-param?))
  
  (define (get-match-vars p k)
    (make-match&env/extract-vars p k #t #f))

  ;; Create an S-expression that applies
  ;; rest to `e' and `esc'. Optimize
  ;; ((lambda (e esc) E) V esc) to
  ;; ((lambda (esc) E) V), etc.
  (define (app-e-esc rest)
    (if (and (pair? rest)
	     (eq? (car rest) 'lambda)
	     (equal? (cadr rest) '(e esc)))
	(caddr rest)
	`(,rest e esc)))

  ;; Create an S-expression that applies
  ;; rest to e and `esc'. Optimize...
  (define (app-esc rest e)
    (if (and (pair? rest)
	     (eq? (car rest) 'lambda)
	     (equal? (cadr rest) '(e esc)))
	(let ([r (caddr rest)])
	  (if (and (pair? r)
		   (eq? (car r) 'list)
		   (pair? (cdr r))
		   (eq? (cadr r) 'e)
		   (null? (cddr r)))
	      `(list ,e)
	      `((lambda (e) ,r) ,e)))
	`(,rest ,e esc)))

  ;; Create an S-expression that appends
  ;; e1 and e2. Optimize...
  (define (app-append e1 e2)
    (if (and (pair? e1)
	     (eq? (car e1) 'list)
	     (pair? (cdr e1))
	     (null? (cddr e1)))
	`(cons ,(cadr e1) ,e2)
	`(append ,e1 ,e2)))
  
  ;; ----------------------------------------------------------------------
  ;; Output generator

  ;; Takes a syntax pattern, an environment prototype, and
  ;; a keyword symbol list, and produces an expander
  ;; that takes an environment and produces syntax.
  ;;
  ;; If the environment is #f, it produces a list of variables
  ;; used in the pattern, instead. This is useful for determining
  ;; what kind of environment (and prototype) to construct for the
  ;; pattern.
  ;;
  (define (make-pexpand p proto-r k dest)
    (define top p)
    (define (expander p proto-r local-top use-ellipses? hash!)
      (cond
       [(and use-ellipses? (ellipsis? p))
	(let* ([p-head (stx-car p)]
	       [nestings (and proto-r (get-ellipsis-nestings p-head k))])
	  (when (null? nestings)
	    (apply
	     raise-syntax-error 
	     syntax-stx
	     "no pattern variables before ellipses in template"
	     (pick-specificity
	      top
	      local-top)))
	  (let* ([proto-rr+deep?s (and proto-r
				       (map (lambda (nesting)
					      (ellipsis-sub-env nesting proto-r top local-top))
					    nestings))]
		 [proto-rr-deep (and proto-r
				     (let loop ([l proto-rr+deep?s])
				       (cond
					[(null? l) null]
					[(cdar l) (loop (cdr l))]
					[else (cons (caar l) (loop (cdr l)))])))]
		 [proto-rr-shallow (and proto-r
					(let loop ([l proto-rr+deep?s])
					  (cond
					   [(null? l) null]
					   [(cdar l) (cons (caar l) (loop (cdr l)))]
					   [else (loop (cdr l))])))]
		 [flat-nestings-deep (and proto-r (extract-vars proto-rr-deep))]
		 [flat-nestings-shallow (and proto-r (extract-vars proto-rr-shallow))]
		 [_ (unless (null? proto-rr-shallow)
		      (when (null? proto-rr-deep)
			(apply
			 raise-syntax-error 
			 syntax-stx
			 "too many ellipses in template"
			 (pick-specificity
			  top
			  local-top))))]
		 [rest (expander (stx-cdr (stx-cdr p)) proto-r local-top #t hash!)]
		 [ehead (expander p-head (and proto-r (append proto-rr-shallow proto-rr-deep)) p-head #t hash!)])
	    (if proto-r
		`(lambda (r)
		   ,(let ([pre (let ([deeps
				      `(map 
					(lambda vals (,ehead 
						      ,(if (null? flat-nestings-shallow)
							   'vals
							   '(append shallows vals))))
					,@(map (lambda (var)
						 (apply-list-ref 'r (stx-memq*-pos var proto-r)))
					       flat-nestings-deep))])
				 (if (null? flat-nestings-shallow)
				     deeps
				     `(let ([shallows
					     (list ,@(map (lambda (var)
							    (apply-list-ref 'r (stx-memq*-pos var proto-r)))
							  flat-nestings-shallow))])
					,deeps)))]
			  [post (apply-to-r rest)])
		      (if (eq? post 'null)
			  pre
			  `(append ,pre ,post))))
		;; variables were hashed
		(void))))]
       [(stx-pair? p)
	(let ([hd (stx-car p)])
	  (if (and use-ellipses?
		   (identifier? hd)
		   (eq? (syntax-e hd) '...))
	      (if (and (stx-pair? (stx-cdr p))
		       (stx-null? (stx-cdr (stx-cdr p))))
		  (let ([dp (stx-car (stx-cdr p))])
		    (expander dp proto-r dp #f hash!))
		  (apply
		   raise-syntax-error 
		   syntax-stx
		   "misplaced ellipses in template"
		   (pick-specificity
		    top
		    local-top)))
	      (let ([ehd (expander hd proto-r hd use-ellipses? hash!)]
		    [etl (expander (stx-cdr p) proto-r local-top use-ellipses? hash!)])
		(if proto-r
		    `(lambda (r)
		       ,(apply-cons (apply-to-r ehd) (apply-to-r etl) p))
		    ;; variables were hashed
		    (void)))))]
       [(identifier? p)
	(if (stx-memq p k) 
	    (if proto-r 
		`(lambda (r) (quote-syntax ,p))
		(void))
	    (if proto-r
		(let ((x (stx-memq p proto-r)))
		  (if x 
		      `(lambda (r) ,(apply-list-ref 'r (stx-memq-pos p proto-r)))
		      (begin
			(when (and use-ellipses?
				   (eq? (syntax-e p) '...))
			  (apply
			   raise-syntax-error 
			   syntax-stx
			   "misplaced ellipses in template"
			   (pick-specificity
			    top
			    local-top)))
			(check-not-pattern p proto-r)
			`(lambda (r) (quote-syntax ,p)))))
		(hash! p)))]
       [(null? p) 
	;; Not syntax, so avoid useless syntax info
	(if proto-r 
	    `(lambda (r) null)
	    (void))]
       [else (if proto-r 
		 `(lambda (r) (quote-syntax ,p))
		 (void))]))
    (let* ([ht (if proto-r
		   #f
		   (make-hash-table))]
	   [l (expander p proto-r p #t
			(if proto-r
			    #f
			    (lambda (r)
			      (let ([l (hash-table-get ht (syntax-e r) (lambda () null))])
				(unless (and (pair? l)
					     (ormap (lambda (i) (module-identifier=? i r)) l))
				  (hash-table-put! ht (syntax-e r) (cons r l)))))))])
      (if proto-r
	  `(lambda (r src)
	     ,(let ([main `(datum->syntax-object (quote-syntax ,(and dest
								     ;; In case dest has significant structure...
								     (datum->syntax-object
								      dest
								      'dest
								      #f)))
						 ,(apply-to-r l) 
						 src)])
		(if (multiple-ellipsis-vars? proto-r)
		    `(let ([exnh #f])
		       ((let/ec esc
			  (dynamic-wind
			   (lambda ()
			     (set! exnh (current-exception-handler))
			     (current-exception-handler
			      (lambda (exn)
				(esc
				 (lambda ()
				   (if (exn:break? exn)
				       (raise exn)
				       (raise-syntax-error
					'syntax
					"incompatible ellipsis match counts for template"
					(quote ,p)
					;; This is a trick to minimize the syntax structure we keep:
					(quote-syntax ,(datum->syntax-object #f '... p)))))))))
			   (lambda ()
			     (let ([v ,main])
			       (lambda () v)))
			   (lambda ()
			     (current-exception-handler exnh))))))
		    main)))
	  ;; Get list of unique vars:
	  (apply append (hash-table-map ht (lambda (k v) v))))))

  ;; apply-to-r creates an S-expression that applies
  ;; rest to `r', but it also optimizes ((lambda (r) E) r)
  ;; as simply E.
  (define (apply-to-r rest)
    (if (and (pair? rest)
	     (eq? (car rest) 'lambda)
	     (equal? (cadr rest) '(r)))
	(caddr rest)
	`(,rest r)))

  ;; creates an S-expression that conses h and t,
  ;; with optimizations. If h and t are quoted
  ;; versions of the carand cdr of p, then return
  ;; a quoted as the "optimization" --- one that
  ;; is necessary to preserve the syntax wraps
  ;; associated with p.
  (define (apply-cons h t p)
    (cond
     [(and (pair? h)
	   (eq? (car h) 'quote-syntax)
	   (eq? (cadr h) (stx-car p))
	   (or (eq? t 'null)
	       (and
		(pair? t)
		(eq? (car t) 'quote-syntax)
		(eq? (cadr t) (stx-cdr p)))))
      `(quote-syntax ,p)]
     [(eq? t 'null)
      `(list ,h)]
     [(and (pair? t)
	   (memq (car t) '(list list*)))
      `(,(car t) ,h ,@(cdr t))]
     [(and (pair? t)
	   (eq? (car t) 'cons))
      `(list* ,h ,@(cdr t))]
     [else
      `(cons ,h ,t)]))

  (define (apply-list-ref e p)
    (cond
     [(eq? p 0) `(car ,e)]
     [(eq? p 1) `(cadr ,e)]
     [(eq? p 2) `(caddr ,e)]
     [(eq? p 3) `(cadddr ,e)]
     [else `(list-ref ,e ,p)]))
  
  ;; Returns a list that nests a pattern variable as deeply as it
  ;; is ellipsed. Escaping ellipses are detected.
  (define get-ellipsis-nestings
    (lambda (p k)
      (let sub ([p p][use-ellipses? #t])
	(cond 
	 [(and use-ellipses? (ellipsis? p))
	  (let ([subs (sub (stx-car p) #t)])
	    (append (map list subs)
		    (sub (stx-cdr (stx-cdr p)) #t)))]
	 [(stx-pair? p) 
	  (let ([hd (stx-car p)])
	    (if (and use-ellipses?
		     (identifier? hd)
		     (eq? (syntax-e hd) '...)
		     (stx-pair? (stx-cdr p)))
		(sub (stx-car (stx-cdr p)) #f)
		(append! (sub (stx-car p) use-ellipses?) 
			 (sub (stx-cdr p) use-ellipses?))))]
	 [(identifier? p)
	  (if (stx-memq p k) 
	      '() 
	      (list p))]
	 [else '()]))))

  ;; Checks whether the given nesting matches a nesting in the
  ;; environment prototype, returning the prototype entry if it is
  ;; found, and signalling an error otherwise. If the prototype 
  ;; entry can be unwrapped by one, it is, and the resulting
  ;; prototype is paired with #f. Otherwise, the prototype is left
  ;; alone and paird with #t.
  (define ellipsis-sub-env
    (lambda (nesting proto-r src detail-src)
      (let ([v (ormap (lambda (proto)
			(let ([start (if (pair? proto)
					 (car proto)
					 proto)])
			  (let loop ([c start] [n nesting])
			    (cond
			     [(and (pair? c) (pair? n))
			      (loop (car c) (car n))]
			     [(pair? n)
			      (loop c (car n))]
			     [(and (syntax? c) (syntax? n))
			      (if (module-identifier=? c n)
				  (cons start (identifier? proto))
				  #f)]
			     [else #f]))))
		      proto-r)])
	(unless v
	  (apply
	   raise-syntax-error 
	   (quote-syntax syntax)
	   "too few ellipses for pattern variable in template"
	   (pick-specificity
	    src
	    (let loop ([n nesting])
	      (if (syntax? n)
		  n
		  (loop (car n)))))))
	v)))

  (define (extract-vars proto-r)
    (map (lambda (i)
	   (let loop ([i i])
	     (if (syntax? i)
		 i
		 (loop (car i)))))
	 proto-r))

  ;; Checks that a variable is not in the prototype
  ;; environment, and specifically not an ellipsed
  ;; variable.
  (define (check-not-pattern ssym proto-r)
    (for-each (lambda (p)
		(when (pair? p)
		  (let loop ([l (car p)])
		    (cond
		     [(syntax? l)
		      (when (module-identifier=? l ssym)
			(raise-syntax-error 
			 (quote-syntax syntax)
			 "missing ellipses with pattern variable in template"
			 ssym))]
		     [else (loop (car l))]))))
	      proto-r))

  ;; Tests if x is an ellipsing pattern of the form
  ;;   (blah ... . blah2)
  (define (ellipsis? x)
    (and (stx-pair? x) 
	 (let ([d (stx-cdr x)])
	   (and (stx-pair? d) 
		(eq? (syntax-e (stx-car d)) '...)
		(not (eq? (syntax-e (stx-car x)) '...))))))

  ;; Takes an environment prototype and removes
  ;; the ellipsis-nesting information.
  (define (flatten-nestings nestings filter?)
    (let loop ([nestings nestings])
      (if (null? nestings)
	  null
	  (if (filter? (car nestings))
	      (cons (let loop ([nesting (car nestings)])
		      (if (syntax? nesting)
			  nesting
			  (loop (car nesting))))
		    (loop (cdr nestings)))
	      (loop (cdr nestings))))))

  (define (multiple-ellipsis-vars? proto-r)
    (let loop ([proto-r proto-r])
      (cond
       [(null? proto-r) #f]
       [(pair? (car proto-r))
	(let loop ([proto-r (cdr proto-r)])
	  (cond
	   [(null? proto-r) #f]
	   [(pair? (car proto-r))
	    #t]
	   [else (loop (cdr proto-r))]))]
       [else (loop (cdr proto-r))])))

  ;; Structure for communicating first-order pattern variable information:
  (define-struct syntax-mapping (depth valvar))

  (provide make-match&env get-match-vars make-pexpand
	   make-syntax-mapping syntax-mapping?
	   syntax-mapping-depth syntax-mapping-valvar
	   stx-memq-pos))

;;----------------------------------------------------------------------
;; syntax-case and syntax

(module #%stxcase #%kernel
  (require #%stx #%small-scheme)
  (require-for-syntax #%stx #%small-scheme #%sc #%kernel)

  (define-syntax syntax-case*
    (lambda (x)
      (define l (and (stx-list? x) (stx->list x)))
      (unless (and (stx-list? x)
		   (> (length l) 3))
	(raise-syntax-error
	 #f
	 "bad form"
	 x))
      (let ([expr (cadr l)]
	    [kws (caddr l)]
	    [lit-comp (cadddr l)]
	    [clauses (cddddr l)])
	(for-each
	 (lambda (lit)
	   (unless (identifier? lit)
	     (raise-syntax-error
	      #f
	      "literal is not a indentifier"
	      x
	      lit)))
	 (stx->list kws))
	(for-each
	 (lambda (clause)
	   (unless (and (stx-list? clause)
			(<= 2 (length (stx->list clause)) 3))
	     (raise-syntax-error
	      #f
	      "bad clause"
	      x
	      clause)))
	 clauses)
	(let ([patterns (map stx-car clauses)]
	      [fenders (map (lambda (clause)
			      (and (stx-pair? (stx-cdr (stx-cdr clause)))
				   (stx-car (stx-cdr clause))))
			    clauses)]
	      [answers (map (lambda (clause)
			      (let ([r (stx-cdr (stx-cdr clause))])
				(if (stx-pair? r) 
				    (stx-car r)
				    (stx-car (stx-cdr clause)))))
			    clauses)])
	  (let* ([arg (quote-syntax arg)]
		 [rslt (quote-syntax rslt)]
		 [pattern-varss (map
				 (lambda (pattern)
				   (get-match-vars pattern (stx->list kws)))
				 (stx->list patterns))]
		 [lit-comp-is-mod? (and (identifier? lit-comp)
					(module-identifier=? 
					 lit-comp
					 (quote-syntax module-identifier=?)))])
	    (datum->syntax-object
	     (quote-syntax here)
	     (list (quote-syntax let) (list (list arg (list (quote-syntax datum->syntax-object)
							    (list
							     (quote-syntax quote-syntax)
							     (datum->syntax-object
							      expr
							      'here))
							    expr)))
		   (let loop ([patterns patterns]
			      [fenders fenders]
			      [unflat-pattern-varss pattern-varss]
			      [answers answers])
		     (cond
		      [(null? patterns)
		       (list
			(quote-syntax raise-syntax-error)
			#f
			"bad syntax"
			arg)]
		      [else
		       (let ([rest (loop (cdr patterns) (cdr fenders)
					 (cdr unflat-pattern-varss) (cdr answers))])
			 (let ([pattern (car patterns)]
			       [fender (car fenders)]
			       [unflat-pattern-vars (car unflat-pattern-varss)]
			       [answer (car answers)])
			   (define pattern-vars
			     (map (lambda (var)
				    (let loop ([var var])
				      (if (syntax? var)
					  var
					  (loop (car var)))))
				  unflat-pattern-vars))
			   (define temp-vars
			     (map
			      (lambda (p) (datum->syntax-object p (gensym) #f))
			      pattern-vars))
			   ;; Here's the result expression for one match:
			   (let* ([do-try-next (if (car fenders)
						   (list (quote-syntax try-next))
						   rest)]
				  [m
				   ;; Do match, bind result to rslt:
				   (list (quote-syntax let)
					 (list 
					  (list rslt
						(list* (datum->syntax-object
							(quote-syntax here)
							(make-match&env
							 pattern
							 (stx->list kws)
							 (not lit-comp-is-mod?))
							pattern)
						       arg
						       (if lit-comp-is-mod?
							   null
							   (list lit-comp)))))
					 ;; If match succeeded...
					 (list 
					  (quote-syntax if)
					  rslt
					  ;; Extract each name binding into a temp variable:
					  (list
					   (quote-syntax let) 
					   (map (lambda (pattern-var temp-var)
						  (list
						   temp-var
						   (let ([pos (stx-memq-pos pattern-var pattern-vars)])
						     (let ([accessor (cond
								      [(eq? pos 0) (quote-syntax car)]
								      [(eq? pos 1) (quote-syntax cadr)]
								      [(eq? pos 2) (quote-syntax caddr)]
								      [(eq? pos 3) (quote-syntax cadddr)]
								      [else #f])])
						       (if accessor
							   (list
							    accessor
							    rslt)
							   (list
							    (quote-syntax list-ref)
							    rslt
							    pos))))))
						pattern-vars temp-vars)
					   ;; Tell nested `syntax' forms about the
					   ;;  pattern-bound variables:
					   (list
					    (quote-syntax letrec-syntaxes) 
					    (map (lambda (pattern-var unflat-pattern-var temp-var)
						   (list (list pattern-var)
							 (list
							  (quote-syntax make-syntax-mapping)
							  ;; Tell it the shape of the variable:
							  (let loop ([var unflat-pattern-var][d 0])
							    (if (syntax? var)
								d
								(loop (car var) (add1 d))))
							  ;; Tell it the variable name:
							  (list
							   (quote-syntax quote-syntax)
							   temp-var))))
						 pattern-vars unflat-pattern-vars
						 temp-vars)
					    (if fender
						(list (quote-syntax if) fender
						      answer
						      do-try-next)
						answer)))
					  do-try-next))])
			     (if fender
				 (list
				  (quote-syntax let)
				  ;; Bind try-next to try next case
				  (list (list (quote try-next)
					      (list (quote-syntax lambda)
						    (list)
						    rest)))
				  ;; Try one match
				  m)
				 ;; Match try already embed the rest case
				 m))))])))
	     x))))))

  (define-syntax syntax
    (lambda (x)
      (unless (and (stx-pair? x)
		   (let ([rest (stx-cdr x)])
		     (and (stx-pair? rest)
			  (stx-null? (stx-cdr rest)))))
	(raise-syntax-error
	 #f
	 "bad form"
	 x))
      (datum->syntax-object
       (quote-syntax here)
       (let ([pattern (stx-car (stx-cdr x))])
	 (let ([unique-vars (make-pexpand pattern #f null #f)])
	   (let ([var-bindings
		  (map
		   (lambda (var)
		     (and (let ([v (syntax-local-value var (lambda () #f))])
			    (and (syntax-mapping? v)
				 v))))
		   unique-vars)])
	     (if (or (null? var-bindings)
		     (not (ormap (lambda (x) x) var-bindings)))
		 ;; Constant template:
		 (list (quote-syntax quote-syntax) pattern)
		 ;; Non-constant:
		 (let ([proto-r (let loop ([vars unique-vars][bindings var-bindings])
				  (if (null? bindings)
				      null
				      (let ([rest (loop (cdr vars)
							(cdr bindings))])
					(if (car bindings)
					    (cons (let loop ([v (car vars)]
							     [d (syntax-mapping-depth (car bindings))])
						    (if (zero? d)
							v
							(loop (list v) (sub1 d))))
						  rest)
					    rest))))]
		       [non-pattern-vars (let loop ([vars unique-vars][bindings var-bindings])
					   (if (null? bindings)
					       null
					       (let ([rest (loop (cdr vars)
								 (cdr bindings))])
						 (if (car bindings)
						     rest
						     (cons (car vars) rest)))))])
		   (let ([build-from-template
			  ;; Even if we don't use the builder, we need to check
			  ;; for a well-formed pattern:
			  (make-pexpand pattern proto-r non-pattern-vars pattern)]
			 [r (let loop ([vars unique-vars][bindings var-bindings])
			      (cond
			       [(null? bindings) null]
			       [(car bindings) (cons 
						(datum->syntax-object
						 (car vars)
						 (syntax-e 
						  (syntax-mapping-valvar (car bindings)))
						 x)
						(loop (cdr vars) (cdr bindings)))]
			       [else  (loop (cdr vars) (cdr bindings))]))])
		     (if (identifier? pattern)
			 ;; Simple syntax-id lookup:
			 (car r)
			 ;; General case:
			 (list (datum->syntax-object
				(quote-syntax here)
				build-from-template
				pattern)
			       (cons (quote-syntax list) r)
			       (list (quote-syntax quote-syntax)
				     (datum->syntax-object #f 'srctag x))))))))))
       x)))

  (provide syntax-case* syntax))

;;----------------------------------------------------------------------
;; syntax/loc

(module #%stxloc #%kernel
  (require #%stxcase #%define-et-al)
  (require-for-syntax #%kernel #%stxcase)

  ;; Regular syntax-case
  (define-syntax syntax-case
    (lambda (stx)
      (syntax-case* stx () module-identifier=?
	[(_ stxe kl clause ...)
	 (syntax (syntax-case* stxe kl module-identifier=? clause ...))])))

  ;; Like syntax, but also takes a syntax object
  ;; that supplies a source location for the
  ;; resulting syntax object.
  (define-syntax syntax/loc
    (lambda (stx)
      (syntax-case* stx () module-identifier=?
	[(_ loc pattern)
	 (syntax (let ([stx (syntax pattern)])
		   (datum->syntax-object
		    stx
		    (syntax-e stx)
		    loc)))])))

  (provide syntax/loc syntax-case))

;;----------------------------------------------------------------------
;; with-syntax, generate-temporaries

(module #%with-stx #%kernel
  (require #%stx #%stxloc #%small-scheme)
  (require-for-syntax #%kernel #%stxcase #%stxloc)

  ;; From Dybvig
  (define-syntax with-syntax
    (lambda (x)
      (syntax-case x ()
	((_ () e1 e2 ...)
	 (syntax/loc x (begin e1 e2 ...)))
	((_ ((out in)) e1 e2 ...)
	 (syntax/loc x (syntax-case in () (out (begin e1 e2 ...)))))
	((_ ((out in) ...) e1 e2 ...)
	 (syntax-case (map (lambda (x)
			       (datum->syntax-object
				x
				'here
				x))
			   (syntax->list (syntax (in ...)))) ()
	   [(here ...)
	    (syntax/loc x (syntax-case (list (datum->syntax-object 
					      (quote-syntax here) 
					      in) 
					     ...) ()
			    ((out ...) (begin e1 e2 ...))))])))))

  (define (generate-temporaries sl)
    (unless (stx-list? sl)
      (raise-type-error 
       'generate-temporaries
       "syntax pair"
       sl))
    (let ([l (stx->list sl)])
      (map (lambda (x) (datum->syntax-object
			#f
			(cond
			 [(or (symbol? x) (string? x))
			  (gensym x)]
			 [(identifier? x)
			  (gensym (syntax-e x))]
			 [else (gensym)])
			#f)) l)))

  (provide with-syntax generate-temporaries))

;;----------------------------------------------------------------------
;; #%stxcase-scheme: adds let-syntax, synatx-rules, and
;;  check-duplicate-identifier, and assembles everything we have so far

(module #%stxcase-scheme #%kernel
  (require #%small-scheme #%stx #%stxcase #%with-stx #%stxloc)
  (require-for-syntax #%kernel #%small-scheme #%stx #%stxcase #%with-stx #%stxloc)

  (define (check-duplicate-identifier names)
    (let/ec escape
      (let ([ht (make-hash-table)])
	(for-each
	 (lambda (defined-name)
	   (unless (identifier? defined-name)
	     (raise-type-error 'check-duplicate-identifier
			       "list of identifiers" names))
	   (let ([l (hash-table-get ht (syntax-e defined-name) (lambda () null))])
	     (when (ormap (lambda (i) (bound-identifier=? i defined-name)) l)
	       (escape defined-name))
	     (hash-table-put! ht (syntax-e defined-name) (cons defined-name l))))
	 names)
	#f)))
  
  (define-syntax letrec-syntax
    (lambda (stx)
      (syntax-case stx ()
	[(_ ([id expr] ...) body1 body ...)
	 (syntax/loc stx
	     (letrec-syntaxes ([(id) expr] ...)
	       body1 body ...))])))

  (define-syntax let-syntaxes
    (lambda (stx)
      (syntax-case stx ()
	[(_ ([(id ...) expr] ...) body1 body ...)
	 (with-syntax ([((tmp ...) ...) 
			(map
			 generate-temporaries 
			 (syntax->list (syntax ((id ...) ...))))])
	   (syntax/loc stx
	       (letrec-syntaxes ([(tmp ...) expr] ...)
		 (letrec-syntaxes ([(id ...) (values
					      (syntax-local-value (quote-syntax tmp) void)
					      ...)] ...)
		   body1 body ...))))])))

  (define-syntax let-syntax
    (lambda (stx)
      (syntax-case stx ()
	[(_ ([id expr] ...) body1 body ...)
	 (syntax/loc stx
	     (let-syntaxes ([(id) expr] ...)
	       body1 body ...))])))

  ;; From Dybvig:
  (define-syntax syntax-rules
    (lambda (x)
      (syntax-case x ()
	((_ (k ...) ((keyword . pattern) template) ...)
	 (andmap identifier? (syntax->list (syntax (k ...))))
	 (with-syntax (((dummy ...)
			(generate-temporaries (syntax (keyword ...)))))
	   (syntax (lambda (x)
		     (syntax-case x (k ...)
		       ((dummy . pattern) (syntax template))
		       ...))))))))

  (provide (all-from #%stxcase) (all-from #%small-scheme)
	   (all-from #%with-stx) (all-from #%stxloc) check-duplicate-identifier
	   letrec-syntax let-syntaxes let-syntax syntax-rules))

;;----------------------------------------------------------------------
;; #%more-scheme : case, do, etc. - remaining syntax

(module #%more-scheme #%kernel
  (require #%small-scheme)
  (require-for-syntax #%kernel #%stx #%stxcase-scheme)

  (define (check-parameter-procedure p)
    (unless (and (procedure? p)
		 (procedure-arity-includes? p 0)
		 (procedure-arity-includes? p 1))
      (raise-type-error 'parameterize "procedure (arity 0 and 1)" p))
    p)

  ;; From Dybvig:
  (define-syntax case
    (lambda (x)
      (syntax-case x (else)
	((_ v)
	 (syntax (begin v (cond))))
	((_ v (else e1 e2 ...))
	 (syntax/loc x (begin v e1 e2 ...)))
	((_ v ((k ...) e1 e2 ...))
	 (syntax/loc x (if (memv v '(k ...)) (begin e1 e2 ...))))
	((_ v ((k ...) e1 e2 ...) c1 c2 ...)
	 (syntax/loc x (let ((x v))
			 (if (memv x '(k ...))
			     (begin e1 e2 ...)
			     (case x c1 c2 ...)))))
	((_ v (bad e1 e2 ...) . rest)
	 (raise-syntax-error 
	  #f
	  "bad syntax (not a datum sequence)"
	  x
	  (syntax bad)))
	((_ v clause . rest)
	 (raise-syntax-error 
	  #f
	  "bad syntax (missing expression after datum sequence)"
	  x
	  (syntax clause)))
	((_ . v)
	 (not (null? (syntax-e (syntax v))))
	 (raise-syntax-error 
	  #f
	  "bad syntax (illegal use of `.')"
	  x)))))

  ;; From Dybvig:
  (define-syntax do
    (lambda (orig-x)
      (syntax-case orig-x ()
	((_ ((var init . step) ...) (e0 e1 ...) c ...)
	 (with-syntax (((step ...)
			(map (lambda (v s)
			       (syntax-case s ()
				 (() v)
				 ((e) (syntax e))
				 (_ (raise-syntax-error 
				     #f
				     "bad variable syntax"
				     orig-x))))
			     (syntax->list (syntax (var ...)))
			     (syntax->list (syntax (step ...))))))
	   (syntax-case (syntax (e1 ...)) ()
	     (() (syntax/loc
		  orig-x
		  (let doloop ((var init) ...)
		    (if (not e0)
			(begin c ... (doloop step ...))))))
	     ((e1 e2 ...)
	      (syntax/loc
	       orig-x
	       (let doloop ((var init) ...)
		 (if e0
		     (begin e1 e2 ...)
		     (begin c ... (doloop step ...))))))))))))
  
  ;; From Dybvig:
  (define-syntax delay
    (lambda (x)
      (syntax-case x ()
	((delay exp)
	 (syntax/loc x (make-a-promise (lambda () exp)))))))
  
  (define-struct promise (p))

  ;; From Dybvig (mostly):
  (define make-a-promise
    (lambda (thunk)
      (make-promise
       (let ([result (void)] [set? #f])
	 (lambda ()
	   (unless set?
	     (let ([v (call-with-values thunk list)])
	       (unless set?
		 (set! result v)
		 (set! set? #t))))
	   (apply values result))))))
  
  (define (force p)
    (unless (promise? p)
      (raise-type-error 'force "promise" p))
    ((promise-p p)))

  (define-syntax parameterize
    (lambda (stx)
      (syntax-case stx ()
	[(_ () expr1 expr ...)
	 (syntax (let () expr1 expr ...))]
	[(_ ([param val] ...) expr1 expr ...)
	 (with-syntax ([(pz ...) (generate-temporaries (syntax (param ...)))]
		       [(save ...) (generate-temporaries (syntax (param ...)))])
	   (syntax/loc
	    stx
	    (let ([pz (check-parameter-procedure param)] ...
		  [save val] ...)
	      (let ([swap
		     (lambda ()
		       'done
		       (let ([x save])
			 (set! save (pz)) 
			 (pz x))
		       ...)])
		(dynamic-wind
		    swap
		    (lambda () expr1 expr ...)
		    swap)))))])))

  (define-syntax with-handlers
    (lambda (stx)
      (syntax-case stx ()
	[(_ () expr1 expr ...) (syntax/loc stx (let () expr1 expr ...))]
	[(_ ([pred handler] ...) expr1 expr ...)
	 (syntax/loc
	  stx
	  ((call/ec 
	    (lambda (k)
	      (let ([l (list (cons pred handler) ...)])
		(parameterize ([current-exception-handler
				(lambda (e)
				  (k
				   (lambda ()
				     (let loop ([l l])
				       (cond
					[(null? l)
					 (raise e)]
					[((caar l) e)
					 ((cdar l) e)]
					[else
					 (loop (cdr l))])))))])
		  (call-with-values (lambda () expr1 expr ...)
		    (lambda args (lambda () (apply values args))))))))))])))

  (define-syntax set!-values
    (lambda (stx)
      (syntax-case stx ()
	[(_ () expr) (syntax (let-values ([() expr]) (void)))]
	[(_ (id) expr) (identifier? (syntax id)) (syntax (set! id expr))]
	[(_ (id ...) expr)
	 (let ([ids (stx->list (syntax (id ...)))])
	   (for-each
	    (lambda (id)
	      (unless (identifier? id)
		(raise-syntax-error #f
				    "not an identifier"
				    stx
				    id)))
	    ids)
	   (let ([dup (check-duplicate-identifier ids)])
	     (when dup
	       (raise-syntax-error #f
				   "duplicate identifier"
				   stx
				   dup))))
	 (with-syntax ([(temp ...) (generate-temporaries (syntax (id ...)))])
	   (syntax/loc
	    stx
	    (let-values ([(temp ...) expr])
	      (set! id temp) ...)))])))

  (define-syntax let/cc
    (lambda (stx)
      (syntax-case stx ()
	[(_ var body1 body ...)
	 (syntax/loc stx (call/cc (lambda (var) body1 body ...)))])))

  (define-syntax let-struct
    (lambda (stx)
      (syntax-case stx ()
	[(_ base (field ...) body1 body ...)
	 (syntax/loc stx (let ()
			   (define-struct base (field ...))
			   body1 body ...))])))

  (define-syntax fluid-let
    (lambda (stx)
      (syntax-case stx ()
	[(_ () body1 body ...) (syntax/loc stx (let () body1 body ...))]
	[(_ ([name val] ...) body1 body ...)
	 (with-syntax ([(tmp ...) (generate-temporaries (syntax (name ...)))])
	   (syntax/loc
	    stx
	    (let ([tmp val] ...)
	      (let ([swap
		     (lambda ()
		       (let ([s tmp])
			 (set! tmp name)
			 (set! name s))
		       ...)])
		(dynamic-wind
		    swap
		    (lambda () body1 body ...)
		    swap)))))])))

  (define-syntax time
    (lambda (stx)
      (syntax-case stx ()
	[(_ expr1 expr ...)
	 (syntax/loc
	  stx
	  (let-values ([(v cpu user gc) (time-apply (lambda () expr1 expr ...) null)])
	    (printf "cpu time: ~s real time: ~s gc time: ~s~n" cpu user gc)
	    (apply values v)))])))

  (provide case do delay force promise?
	   parameterize with-handlers set!-values
	   let/cc let-struct fluid-let time))

;;----------------------------------------------------------------------
;; #%misc : file utilities, etc. - remaining functions

(module #%misc #%kernel
  (require #%more-scheme #%small-scheme)
  
  (define rationalize
    (letrec ([check (lambda (x) 
                      (unless (real? x) (raise-type-error 'rationalize "real" x)))]
	     [find-between 
	      (lambda (lo hi)
		(if (integer? lo)
		    lo
		    (let ([lo-int (floor lo)]
			  [hi-int (floor hi)])
		      (if (< lo-int hi-int)
			  (add1 lo-int)
			  (+ lo-int
			     (/ (find-between (/ (- hi lo-int)) (/ (- lo lo-int)))))))))])
      (lambda (x within)
	(check x) (check within)
	(let* ([delta (abs within)]
	       [lo (- x delta)]
	       [hi (+ x delta)])
	  (cond
	   [(not (= x x)) +nan.0]
	   [(<= lo 0 hi) (if (exact? x) 0 0.0)]
	   [(negative? lo) (- (find-between (- hi) (- lo)))]
	   [else (find-between lo hi)])))))

  (define (read-eval-print-loop)
    (let* ([eeh #f]
	   [jump #f]
	   [be? #f]
	   [rep-error-escape-handler (lambda () (jump))])
      (dynamic-wind
	  (lambda () (set! eeh (error-escape-handler))
		  (set! be? (break-enabled))
		  (error-escape-handler rep-error-escape-handler)
		  (break-enabled #f))
	  (lambda ()
	    (let/ec done
	      (let loop ()
		(let/ec k
		  (dynamic-wind
		      (lambda ()
			(break-enabled be?)
			(set! jump k))
		      (lambda ()
			(let ([v ((current-prompt-read))])
			  (when (eof-object? v) (done (void)))
			  (call-with-values
			      (lambda () ((current-eval) v))
			    (lambda results (for-each (current-print) results)))))
		      (lambda () 
			(set! be? (break-enabled))
			(break-enabled #f)
			(set! jump #f))))
		(loop))))
	  (lambda () (error-escape-handler eeh)
		  (break-enabled be?)
		  (set! jump #f)
		  (set! eeh #f)))))

  (define load/cd
    (lambda (n)
      (unless (string? n)
	(raise-type-error 'load/cd "string" n))
      (let-values ([(base name dir?) (split-path n)])
	(if dir?
	    (raise
	     (make-exn:i/o:filesystem
	      (string->immutable-string
	       (format "load/cd: cannot open a directory: ~s" n))
	      (current-continuation-marks)
	      n
	      #f))
	    (if (not (string? base))
		(load n)
		(begin
		  (if (not (directory-exists? base))
		      (raise
		       (make-exn:i/o:filesystem
			(string->immutable-string
			 (format 
			  "load/cd: directory of ~s does not exist (current directory is ~s)" 
			  n (current-directory)))
			(current-continuation-marks)
			base
			#f)))
		  (let ([orig (current-directory)])
		    (dynamic-wind
			(lambda () (current-directory base))
			(lambda () (load name))
			(lambda () (current-directory orig))))))))))

  (define (-load load name path)
    (unless (and (string? path) (or (relative-path? path) (absolute-path? path)))
      (raise-type-error name "pathname string" path))
    (if (complete-path? path)
	(load path)
	(let ([dir (current-load-relative-directory)])
	  (load (if dir (path->complete-path path dir) path)))))
  (define (load-relative path) (-load load 'load-relative path))
  (define (load-relative-extension path) (-load load-extension 'load-relative-extension path))
  
  (define path-list-string->path-list
    (let ((r (regexp (let ((sep (case (system-type) 
				  ((unix beos oskit) ":")
				  ((windows macos) ";"))))
		       (format "([^~a]*)~a(.*)" sep sep))))
	  (cons-path (lambda (default s l) 
		       (if (string=? s "")
			   (append default l)
			   (if (or (relative-path? s) (absolute-path? s)) (cons s l) l)))))
      (lambda (s default)
	(unless (string? s) (raise-type-error 'path-list-string->path-list "string" s))
	(unless (list? default) (raise-type-error 'path-list-string->path-list "list" default))
	(let loop ([s s])
	  (let ([m (regexp-match r s)])
	    (if m
		(cons-path default (cadr m) (loop (caddr m)))
		(cons-path default s null)))))))

  (define find-executable-path
    (lambda (program libpath)
      (unless (and (string? program) 
		   (or (relative-path? program)
		       (absolute-path? program)))
	(raise-type-error 'find-executable-path "path string" program))
      (unless (or (not libpath) (and (string? libpath) (relative-path? libpath)))
	(raise-type-error 'find-executable-path "relative-path string or #f" libpath))
      (letrec ([found-exec
		(lambda (exec-name)
                  (if libpath
		      (let-values ([(base name isdir?) (split-path exec-name)])
			(if (string? base)
			    (let ([lib (build-path base libpath)])
			      (if (or (directory-exists? lib) 
				      (file-exists? lib))
				  lib
				  (let ([resolved (resolve-path exec-name)])
				    (cond
				     [(string=? resolved exec-name) #f]
				     [(relative-path? resolved)
				      (found-exec (build-path base resolved))]
			             [else (found-exec resolved)]))))
			    #f))
		      exec-name))])
	(if (and (relative-path? program)
		 (let-values ([(base name dir?) (split-path program)])
		   (eq? base 'relative)))
	    (let ([paths-str (getenv "PATH")]
		  [win-add (lambda (s) (if (eq? (system-type) 'windows) (cons "." s) s))])
	      (let loop ([paths (if paths-str 
				    (win-add (path-list-string->path-list paths-str null))
				    null)])
		(if (null? paths)
		    #f
		    (let* ([base (path->complete-path (car paths))]
			   [name (build-path base program)])
		      (if (file-exists? name)
			  (found-exec name)
			  (loop (cdr paths)))))))
	    (let ([p (path->complete-path program)])
	      (and (file-exists? p) (found-exec p)))))))

  ;; ------------------------------ Collections ------------------------------

  (define (-check-relpath who s)
    (unless (string? s)
      (raise-type-error who "string" s))
    (unless (relative-path? s)
      (raise (make-exn:i/o:filesystem
	      (string->immutable-string
	       (format "~a: invalid relative path: ~s" who s))
	      (current-continuation-marks) s 'ill-formed-path))))

  (define (-check-collection who collection collection-path)
    (-check-relpath who collection) 
    (for-each (lambda (p) (-check-relpath who p)) collection-path))
  
  (define (-find-col who collection collection-path)
    (let ([all-paths (current-library-collection-paths)])
      (let loop ([paths all-paths])
	(if (null? paths)
	    (raise
	     (make-exn:i/o:filesystem
	      (string->immutable-string
	       (format "~a: collection not found: ~s in any of: ~s" 
		       who collection all-paths))
	      (current-continuation-marks)
	      collection
	      #f))
	    (let ([dir (build-path (car paths) collection)])
	      (if (directory-exists? dir)
		  (let* ([cpath (apply build-path dir collection-path)])
		    (if (directory-exists? cpath)
			cpath
			(let loop ([p dir][l collection-path][c collection])
			  (let ([np (build-path p (car l))]
				[nc (build-path c (car l))])
			    (if (directory-exists? np)
				(loop np (cdr l) nc)
				(raise
				 (make-exn:i/o:filesystem
				  (string->immutable-string
				   (format "~a: collection ~s does not have sub-collection: ~s in: ~s"
					   who c (car l) p))
				  (current-continuation-marks)
				  nc
				  #f)))))))
		  (loop (cdr paths))))))))

  (define -re:suffix (regexp "\\..?.?.?$"))
	  
  (define -core-load/use-compiled
    (let ([resolve (lambda (s)
		     (if (complete-path? s)
			 s
			 (let ([d (current-load-relative-directory)])
			   (if d (path->complete-path s d) s))))]
	  [date>=?
	   (lambda (a bm)
	     (let ([am (with-handlers ([not-break-exn? (lambda (x) #f)])
			 (file-or-directory-modify-seconds a))])
	       (or (and (not bm) am) (and am bm (>= am bm)))))])
      (case-lambda 
       [(path) (-core-load/use-compiled path #f)]
       [(path none-there)
	(unless (and (string? path) (or (relative-path? path) (absolute-path? path)))
	  (raise-type-error 'load/use-compiled "pathname string" path))
	(let*-values ([(path) (resolve path)]
		      [(base file dir?) (split-path path)]
		      [(base) (if (eq? base 'relative) 'same base)]
		      [(mode) (use-compiled-file-kinds)]
		      [(comp?) (not (eq? mode 'none))])
	  (let* ([get-so (lambda (file)
			   (if comp?
			       (build-path base
					   "compiled"
					   "native"
					   (system-library-subpath)
					   (regexp-replace 
					    -re:suffix file
					    (case (system-type)
					      [(windows) ".dll"]
					      [else ".so"])))
			       #f))]
		 [ok-kind? (lambda (file)
			     (or (eq? mode 'all)
				 (with-handlers ([not-break-exn? void])
				   (let-values ([(p) (open-input-file file)])
				     (dynamic-wind
					 void
					 (lambda ()
					   (not (and (char=? #\' (read-char p))
						     (char=? #\e (read-char p))
						     (char=? #\space (read-char p)))))
					 (lambda () (close-input-port p)))))))]
		 [zo (and comp?
			  (build-path base
				      "compiled"
				      (regexp-replace -re:suffix file ".zo")))]
		 [so (get-so file)]
		 [_loader-so (get-so "_loader.ss")]
		 [path-d (with-handlers ([not-break-exn? (lambda (x) #f)])
			   (file-or-directory-modify-seconds path))]
		 [with-dir (lambda (t) 
			     (parameterize ([current-load-relative-directory 
					     (if (string? base) base (current-directory))])
			       (t)))])
	    (cond
	     [(and (date>=? _loader-so path-d)
		   (let ([getter (load-extension _loader-so)])
		     (getter (string->symbol (regexp-replace -re:suffix file "")))))
	      => (lambda (loader) (with-dir loader))]
	     [(date>=? so path-d)
	      (with-dir (lambda () ((current-load-extension) so)))]
	     [(and (date>=? zo path-d) (ok-kind? zo))
	      (with-dir (lambda () ((current-load) zo)))]
	     [(and (not path-d) none-there)
	      (none-there)]
	     [else (load path)])))])))

  (define (collection-path collection . collection-path) 
    (-check-collection 'collection-path collection collection-path)
    (-find-col 'collection-path collection collection-path))

  (define (load/use-compiled f) (-core-load/use-compiled f #f))

  (define -re:dir (regexp "(.+?)/+(.*)"))
  (define -re:auto (regexp "^,"))
  (define -module-hash-table-table (make-hash-table-weak)) ; weak map from namespace to module ht
  
  (define -loading-filename (gensym))

  (define standard-module-name-resolver
    (lambda (s relto stx)
      ;; If stx is not #f, raise syntax error for ill-formed paths
      ;; If s is #f, call to resolver is a notification from namespace-attach-module
      (if s
	  (let ([get-dir (lambda ()
			   (or (and relto
				    (let ([rts (symbol->string relto)])
				      (and (regexp-match -re:auto rts)
					   (let-values ([(base n d?)
							 (split-path 
							  (substring rts 1 (string-length rts)))])
					     base))))
			       (current-load-relative-directory)
			       (current-directory)))])
	    (let ([filename
		   (cond
		    [(string? s)
		     ;; Parse Unix-style relative path string
		     (let loop ([path (get-dir)][s s])
		       (let ([prefix (regexp-match -re:dir s)])
			 (if prefix
			     (loop (build-path path 
					       (let ([p (cadr prefix)])
						 (cond
						  [(string=? p ".") 'same]
						  [(string=? p "..") 'up]
						  [else p])))
				   (caddr prefix))
			     (build-path path s))))]
		    [(or (not (pair? s))
			 (not (list? s)))
		     #f]
		    [(eq? (car s) 'lib)
		     (let ([cols (let ([len (length s)])
				   (if (= len 2)
				       (list "mzlib")
				       (if (> len 2)
					   (cddr s)
					   #f)))])
		       (and cols
			    (andmap (lambda (x) (and (string? x) (relative-path? x))) cols)
			    (string? (cadr s))
			    (relative-path? (cadr s))
			    (let ([p (-find-col 'standard-module-name-resolver (car cols) (cdr cols))])
			      (build-path p (cadr s)))))]
		    [(eq? (car s) 'file)
		     (and (= (length s) 2)
			  (let ([p (cadr s)])
			    (and (string? p)
				 (or (relative-path? p)
				     (absolute-path? p))
				 (path->complete-path p (get-dir)))))]
		    [else #f])])
	      (unless filename
		(if stx
		    (raise-syntax-error
		     (quote-syntax standard-module-name-resolver)
		     "bad module path"
		     stx)
		    (raise-type-error 
		     'standard-module-name-resolver
		     "module path"
		     s)))
	      ;; At this point, filename is a complete path
	      (let ([filename (normal-case-path (simplify-path (expand-path filename)))])
		(let-values ([(base name dir?) (split-path filename)])
		  (let ([no-sfx (regexp-replace -re:suffix name "")]
			[abase (format ",~a" base)])
		    (let ([modname (string->symbol (string-append abase no-sfx))]
			  [ht (hash-table-get
			       -module-hash-table-table
			       (current-namespace)
			       (lambda ()
				 (let ([ht (make-hash-table)])
				   (hash-table-put! -module-hash-table-table
						    (current-namespace)
						    ht)
				   ht)))])
		      ;; unless it has been loaded already...
		      (unless (hash-table-get ht modname (lambda () #f))
			;; Currently loading?
			(let ([l (continuation-mark-set->list
				  (current-continuation-marks)
				  -loading-filename)])
			  (for-each
			   (lambda (s)
			     (when (string=? s filename)
			       (error
				'standard-module-name-resolver
				"cycle in loading at ~e: ~e"
				filename
				l)))
			   l))
			(hash-table-put! ht modname #t)
			(let ([prefix (string->symbol abase)])
			  (with-continuation-mark -loading-filename filename
			    (parameterize ([current-module-name-prefix prefix])
			      (load/use-compiled filename)))))
		      ;; Result is the module name:
		      modname))))))
	  ;; Just register relto as loaded
	  (let ([ht (hash-table-get
		     -module-hash-table-table
		     (current-namespace)
		     (lambda ()
		       (let ([ht (make-hash-table)])
			 (hash-table-put! -module-hash-table-table
					  (current-namespace)
					  ht)
			 ht)))])
	    (hash-table-put! ht relto #t)))))
    
  (define (find-library-collection-paths)
    (path-list-string->path-list
     (or (getenv "PLTCOLLECTS") "")
     (or (ormap
	  (lambda (f) (let ([p (f)]) (and p (directory-exists? p) (list p))))
	  (list
	   (lambda () (let ((v (getenv "PLTHOME")))
			(and v (build-path v "collects"))))
	   (lambda () (find-executable-path (find-system-path 'exec-file) "collects")))) 
	 null)))

  ;; -------------------------------------------------------------------------

  (define (port? x) (or (input-port? x) (output-port? x)))

  (define (not-break-exn? x) (not (exn:break? x)))

  ;; -------------------------------------------------------------------------

  (define interaction-environment (lambda () (current-namespace)))

  (define (scheme-report-environment n)
    (unless (= n 5)
      (raise-type-error 'scheme-report-environment "5" n))
    (mk-r5rs #f))

  (define (null-environment n)
    (unless (= n 5)
      (raise-type-error 'null-environment "5" n))
    (mk-r5rs #t))

  (define (mk-r5rs stx-only?)
    (let ([n (make-namespace 'empty)]
	  [orig (current-namespace)])
      (parameterize ([current-namespace n])
	(namespace-attach-module orig '#%r5rs)
	(namespace-require '#%r5rs)
	(namespace-transformer-require '(rename mzscheme syntax-rules syntax-rules))
	(unless stx-only?
	  (for-each
	   (lambda (n)
	     (namespace-variable-binding n (dynamic-require 'mzscheme n)))
	   '(car 
	     cdr caar cadr cdar cddr
	     caaar caadr cadar caddr cdaar cdadr cddar cdddr
	     caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
	     cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
	     map = < > <= >= max min + - * / 
	     abs gcd lcm exp log sin cos tan not eq?
	     call-with-current-continuation make-string
	     symbol->string string->symbol make-rectangular 
	     exact->inexact inexact->exact number->string string->number 
	     rationalize output-port? current-input-port current-output-port current-error-port 
	     open-input-file open-output-file close-input-port close-output-port
	     with-output-to-file transcript-on transcript-off flush-output
	     string-length string-ci<=? string-ci>=? string-append 
	     string->list list->string string-fill! 
	     vector-length vector->list list->vector vector-fill!
	     char-alphabetic? char-numeric? char-whitespace? 
	     char-upper-case? char-lower-case? char->integer integer->char char-downcase
	     call-with-output-file call-with-input-file with-input-from-file
	     apply for-each symbol? pair? cons set-car! set-cdr! null? list? list length append reverse
	     list-tail list-ref memq memv member assq assv assoc procedure?
	     number? complex? real? rational? integer? exact? inexact? zero?
	     positive?  negative? odd? even? 
	     quotient remainder modulo floor ceiling truncate round 
	     numerator denominator asin acos atan sqrt
	     expt make-polar real-part imag-part angle magnitude input-port?
	     read read-char peek-char eof-object?
	     char-ready? write display newline write-char load 
	     string? string string-ref string-set! string=? substring string-copy
	     string-ci=? string<? string>? string<=? string>=? string-ci<? string-ci>?
	     vector? make-vector vector vector-ref vector-set! 
	     char? char=? char<? char>? char<=? char>=? 
	     char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=? 
	     char-upcase boolean? eqv? equal? force
	     call-with-values values eval port? scheme-report-environment null-environment 
	     interaction-environment dynamic-wind))))
      n))

  ;; -------------------------------------------------------------------------

  (provide rationalize 
	   read-eval-print-loop
	   load/cd
	   load-relative load-relative-extension
	   path-list-string->path-list find-executable-path
	   collection-path load/use-compiled
	   port? not-break-exn?
	   find-library-collection-paths
	   interaction-environment scheme-report-environment null-environment
	   standard-module-name-resolver))

;;----------------------------------------------------------------------
;; #%stxmz-body

(module #%stxmz-body #%kernel
  (require #%stxcase-scheme)
  (require-for-syntax #%kernel #%stx)

  (define-syntax mzscheme-in-stx-module-begin
    (lambda (stx)
      (datum->syntax-object
       (quote-syntax here)
       (list* (quote-syntax #%module-begin)
	      (quote-syntax
	       (require-for-syntax mzscheme))
	      (stx-cdr stx))
       stx)))

  (provide mzscheme-in-stx-module-begin))

;;----------------------------------------------------------------------
;; mzscheme: provide everything

(module mzscheme #%kernel
  (require #%more-scheme)
  (require #%misc)
  (require #%stxcase-scheme)
  (require #%stx)
  (require #%stxmz-body)

  (provide (all-from #%more-scheme)
	   (all-from #%misc)
	   (all-from #%stxcase-scheme)
	   identifier? ;; from #%stx
	   (all-from-except #%kernel #%module-begin)
	   (rename mzscheme-in-stx-module-begin #%module-begin)
	   (rename #%module-begin #%plain-module-begin)))

;;----------------------------------------------------------------------
;; r5rs syntax (used by null-environment and scheme-report-environment)

(module #%r5rs mzscheme
  (provide quasiquote unquote unquote-splicing 
	   if let and or cond case define delay do
	   letrec let* begin lambda quote set!
	   define-syntax

	   ;; We have to include the following MzScheme-isms to do anything,
	   ;; but they're not legal R5RS names, anyway.
	   #%app #%datum #%top))

;;----------------------------------------------------------------------
;; init namespace

(begin
  ;; Special start-up require copies bindings to top-level
  (require mzscheme)
  (require-for-syntax mzscheme))

(current-module-name-resolver standard-module-name-resolver)
