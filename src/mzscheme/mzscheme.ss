
;; Implements, in a non-bootstrapping way, all of the MzScheme syntax
;; and "primitives" that are not implemented in the kernel.

;;----------------------------------------------------------------------
;; basic syntax utilities

(module .stx-utilities .kernel

  ;; stx is an identifier?
  (define-values (stx-symbol?)
    (lambda (p)
      (if (syntax? p)
	  (symbol? (syntax-e p))
	  #f)))

  ;; stx null or null?
  (define-values (stx-null?)
    (lambda (p)
      (if (null? p)
	  #t
	  (if (syntax? p) 
	      (null? (syntax-e p))
	      #f))))

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

  ;; cdr of a syntax pair
  (define-values (stx-cdr)
    (lambda (p)
      (if (pair? p)
	  (cdr p)
	  (cdr (syntax-e p)))))

  (define-values (stx-same?)
    (lambda (a b)
      (free-identifier=? a b)))

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

  (export stx-symbol? stx-null? stx-pair? stx-list?
	  stx-car stx-cdr stx-same? stx->list))

;;----------------------------------------------------------------------
;; quasiquote

(module .qq-and-or .kernel
  (import-for-syntax .stx-utilities .kernel)

  (define-syntax quasiquote
    (lambda (in-form)
      (let-values
	  (((form) (if (stx-pair? (stx-cdr in-form))
		       (if (stx-null? (stx-cdr (stx-cdr in-form)))
			   (stx-car (stx-cdr in-form))
			   (raise-syntax-error 'quasiquote "bad syntax" in-form))
		       (raise-syntax-error 'quasiquote "bad syntax" in-form)))
	   ((normal)
	    (lambda (x old)
	      (if (eq? x old)
		  (if (stx-null? x) x (list (quote quote) x))
		  x))))
	(datum->syntax
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
					 (quote cons)
					 (normal first old-first)
					 (normal second old-second)))))))))))
		    (if (stx-pair? x)
			(let-values
			    (((first) (stx-car x)))
			  (if (if (stx-same? first (quote-syntax unquote))
				  (stx-list? x)
				  #f)
			      (let-values
				  (((rest) (stx-cdr x)))
				(if (let-values
					(((g35) (not (stx-pair? rest))))
				      (if g35 g35 (not (stx-null? (stx-cdr rest)))))
				    (raise-syntax-error
				     (quote unquote)
				     "takes exactly one expression"
				     in-form))
				(if (zero? level)
				    (stx-car rest)
				    (qq-list x (sub1 level))))
			      (if (if (stx-same? first (quote-syntax quasiquote))
				      (stx-list? x)
				      #f)
				  (qq-list x (add1 level))
				  (if (if (stx-same? first (quote-syntax unquote-splicing))
					  (stx-list? x)
					  #f)
				      (raise-syntax-error
				       (quote unquote-splicing)
				       "invalid context within quasiquote"
				       in-form)
				      (if (if (stx-pair? first)
					      (if (stx-same? (stx-car first)
							     (quote-syntax unquote-splicing))
						  (stx-list? first)
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
						 (quote unquote-splicing)
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
						      (list (quote append) uqsd l)))
						  (let-values
						      (((restx) (qq-list rest (sub1 level))))
						    (let-values
							()
						      (if (if (eq? l old-l)
							      (eq? restx rest)
							      #f)
							  x
							  (list
							   (quote cons)
							   (list
							    (quote cons)
							    (list
							     (quote quote)
							     (quote unquote-splicing))
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
				      (list (quote list->vector) l2)))))
			    (if (if (syntax? x) (box? (syntax-e x)) #f)
				(let-values
				    (((v) (unbox (syntax-e x))))
				  (let-values
				      (((qv) (qq v level)))
				    (let-values
					()
				      (if (eq? v qv)
					  x
					  (list (quote box) qv)))))
				x)))))))
	    (qq form 0))
	  form)
	 in-form
	 (quote-syntax here)))))

  (define-syntax and 
    (lambda (x)
      (let ([e (stx-cdr x)])
	(if (stx-null? e)
	    (quote-syntax #t)
	    (if (if (stx-pair? e)
		    (stx-null? (stx-cdr e))
		    #t)
		(stx-car e)
		(if (stx-list? e)
		    (datum->syntax
		     (list (quote-syntax if)
			   (stx-car e)
			   (cons (quote-syntax and)
				 (stx-cdr e))
			   (quote-syntax #f))
		     x
		     (quote-syntax here))
		    (raise-syntax-error 
		     'and
		     "bad syntax"
		     x)))))))

  (define-syntax or
    (lambda (x)
      (let ([e (stx-cdr x)])
	(if (stx-null? e) 
	    (quote-syntax #f)
	    (if (if (stx-pair? e)
		    (stx-null? (stx-cdr e))
		    #f)
		(stx-car e)
		(if (stx-list? e)
		    (let ([tmp 'or-part])
		      (datum->syntax
		       (list (quote-syntax let) (list
						 (list
						  tmp
						  (stx-car e)))
			     (list (quote-syntax if)
				   tmp
				   tmp
				   (cons (quote-syntax or)
					 (stx-cdr e))))
		       x
		       (quote-syntax here)))
		    (raise-syntax-error 
		     'or
		     "bad syntax"
		     x)))))))

  (export quasiquote and or))

;;----------------------------------------------------------------------
;; cond

(module .cond .kernel
  (import-for-syntax .stx-utilities .qq-and-or .kernel)

  (define-syntax cond
    (lambda (in-form)
      (datum->syntax
       (let ([form (stx-cdr in-form)]
	     [serror
	      (lambda (msg at)
		(raise-syntax-error 'cond msg in-form at))])
	 (let loop ([tests form])
	   (if (stx-null? tests)
	       '(void)
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
				[else? (stx-same? test (quote-syntax else))])
			   (if (and else? (stx-pair? rest))
			       (serror "bad syntax (`else' clause must be last)" line))
			   (if (and (stx-pair? value)
				    (stx-same? (stx-car value) (quote-syntax =>)))
			       (if (and (stx-pair? (stx-cdr value))
					(stx-null? (stx-cdr (stx-cdr value))))
				   (let ([test (if else?
						   #t 
						   test)]
					 [gen (gensym)])
				     `(let ([,gen ,test])
					(if ,gen
					    (,(stx-car (stx-cdr value)) ,gen)
					    ,(loop rest))))
				   (serror
				    "bad syntax (bad clause form with =>)"
				    line))
			       (if else?
				   (cons 'begin value)
				   (if (stx-null? value)
				       (let ([gen (gensym)])
					 `(let ([,gen ,test])
					    (if ,gen ,gen ,(loop rest))))
				       (list
					'if test
					(cons 'begin value)
					(loop rest))))))))))))
       in-form
       (quote-syntax here))))

  (export cond))

;;----------------------------------------------------------------------
;; define, when, unless, let/ec, define-struct

(module .define-et-al .kernel
  (import-for-syntax .kernel .stx-utilities .qq-and-or .cond)

  (define-syntax define
    (lambda (code)
      (let ([body (stx-cdr code)])
	(if (stx-null? body)
	    (raise-syntax-error
	     'define
	     "bad syntax (no definition body)"
	     code))
	(let ([first (stx-car body)]) 
	  (cond
	   [(stx-symbol? first)
	    (if (and (stx-pair? (stx-cdr body))
		     (stx-null? (stx-cdr (stx-cdr body))))
		(datum->syntax
		 `(define-values (,first) ,@(stx-cdr body))
		 code (quote-syntax here))
		(raise-syntax-error
		 'define
		 "bad syntax (zero or multiple expressions after identifier)"
		 code))]
	   [(stx-pair? first)
	    (let ([bad-symbol  (lambda (s) (raise-syntax-error 'define
							       "bad identifier"
							       code
							       s))])
	      (let loop ([l first])
		(cond
		 [(stx-null? l) #f]
		 [(stx-pair? l) 
		  (if (stx-symbol? (stx-car l))
		      (loop (stx-cdr l))
		      (bad-symbol (stx-car l)))]
		 [(stx-symbol? l) #f]
		 [else (bad-symbol l)])))
	    (datum->syntax
	     `(define-values (,(stx-car first)) 
		(lambda ,(stx-cdr first) ,@(stx-cdr body)))
	     code (quote-syntax here))]
	   [else
	    (raise-syntax-error
	     'define
	     "not an identifier"
	     code
	     first)])))))

  (define-syntax when
    (lambda (x)
      (let ([l (syntax->list x)])
	(if (and l
		 (> (length l) 2))
	    (datum->syntax
	     (list (quote-syntax if)
		   (stx-car (stx-cdr x))
		   (list*
		    (quote-syntax begin)
		    (stx-cdr (stx-cdr x))))
	     x
	     (quote-syntax here))
	    (raise-syntax-error
	     'when
	     "bad syntax"
	     x)))))

  (define-syntax unless
    (lambda (x)
      (let ([l (syntax->list x)])
	(if (and l
		 (> (length l) 2))
	    (datum->syntax
	     (list (quote-syntax if)
		   (stx-car (stx-cdr x))
		   (quote-syntax (void))
		   (list*
		    (quote-syntax begin)
		    (stx-cdr (stx-cdr x))))
	     x
	     (quote-syntax here))
	    (raise-syntax-error
	     'unless
	     "bad syntax"
	     x)))))

  (define-syntax let/ec 
    (lambda (code)
      (let ([l (syntax->list code)])
	(if (and l
		 (> (length l) 2)
		 (stx-symbol? (cadr l)))
	    (let ([var (cadr l)]
		  [exprs (stx-cdr (stx-cdr code))])
	      (datum->syntax
	       `(call/ec (lambda (,var) ,@exprs))
	       code (quote-syntax here)))
	    (raise-syntax-error
	     'let/ec
	     "bad syntax"
	     code)))))

  (define-syntax define-struct
    (lambda (stx)
      (let ([body (stx->list (stx-cdr stx))])
	(let ([syntax-error
	       (lambda (s . detail)
		 (apply
		  raise-syntax-error
		  'define-struct
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
	  (or (= 2 (length body))
	      (syntax-error "wrong number of parts"))
	  (or (stx-symbol? (car body))
	      (and (stx-pair? (car body))
		   (stx-symbol? (stx-car (car body)))
		   (stx-pair? (stx-cdr (car body)))
		   (stx-null? (stx-cdr (stx-cdr (car body)))))
	      (syntax-error "first part must be an identifier or identifier-expression pair"))
	  (or (stx-list? (cadr body))
	      (syntax-error "illegal use of `.' in field list"))
	  (for-each (lambda (x) 
		      (or (stx-symbol? x)
			  (syntax-error "field name not a identifier" x)))
		    (stx->list (cadr body)))
	  (let ([name (if (stx-symbol? (car body))
			  (car body)
			  (stx-car (car body)))]
		[fields (stx->list (cadr body))])
	    (datum->syntax
	     `(define-values
		,(map (lambda (n) (datum->syntax n name name)) (build-struct-names name fields))
		(struct ,@body))
	     stx
	     (quote-syntax here)))))))

  (export define when unless let/ec define-struct))

;;----------------------------------------------------------------------
;; .small-scheme: assembles all basic forms we have so far

(module .small-scheme .kernel
  (import .stx-utilities .qq-and-or .cond .define-et-al)

  (export (all-from .qq-and-or)
	  (all-from .cond)
	  (all-from .define-et-al)))

;;----------------------------------------------------------------------
;; pattern-matching utilities
;; based on Shriram's pattern matcher for Zodiac

(module .sc-utilities .kernel
  (import .stx-utilities .small-scheme)

  ;; memq on a list of identifiers, and
  ;;  nested identifiers
  (define (stx-memq ssym l)
    (ormap (lambda (p)
	     (and (syntax? P)
		  (free-identifier=? ssym p)))
	   l))
  
  ;; memq on a list of identifiers and
  ;;  nested identifiers, returns a position
  (define (stx-memq-pos ssym l)
    (let loop ([p 0][l l])
      (cond
       [(null? l) #f]
       [(and (syntax? (car l))
	     (free-identifier=? ssym (car l)))
	p]
       [else (loop (add1 p) (cdr l))])))

  ;; Like stx-memq-pos, but goes into nestings to
  ;;  find identifiers.
  (define (stx-memq*-pos ssym l)
    (let loop ([p 0][l l])
      (cond
       [(null? l) #f]
       [(free-identifier=? ssym 
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
  (define (make-match&env/extract-vars p k just-vars?)
    (define top p)
    (define (m&e p local-top use-ellipses?)
      (cond
       [(and use-ellipses? (ellipsis? p))
	(unless (stx-null? (stx-cdr (stx-cdr p)))
	  (apply
	   raise-syntax-error 
	   'syntax
	   "misplaced ellipses"
	   (pick-specificity
	    top
	    local-top)))
	(let* ([p-head (stx-car p)]
	       [nestings (get-ellipsis-nestings p-head k)]
	       [match-head (m&e p-head p-head #t)])
	  (if just-vars?
	      (map list nestings)
	      (let ([nest-vars (flatten-nestings nestings)])
		`(lambda (e esc)
		   (if (stx-list? e)
		       (let ([l (map (lambda (e) 
				       (,match-head e esc))
				     (stx->list e))])
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
		   (stx-symbol? hd)
		   (eq? (syntax-e hd) '...))
	      (if (and (stx-pair? (stx-cdr p))
		       (stx-null? (stx-cdr (stx-cdr p))))
		  (let ([dp (stx-car (stx-cdr p))])
		    (m&e dp dp #f))
		  (apply
		   raise-syntax-error 
		   'syntax
		   "misplaced ellipses"
		   (pick-specificity
		    top
		    local-top)))
	      (let ([match-head (m&e hd hd use-ellipses?)]
		    [match-tail (m&e (stx-cdr p) local-top use-ellipses?)])
		(if just-vars?
		    (append match-head match-tail)
		    `(lambda (e esc)
		       (if  (stx-pair? e)
			    (append (,match-head (stx-car e) esc)
				    (,match-tail (stx-cdr e) esc))
			    (esc #f)))))))]
       [(stx-null? p)
	(if just-vars?
	    null
	    `(lambda (e esc)
	       (if (stx-null? e)
		   null
		   (esc #f))))]
       [(stx-symbol? p)
	(if (stx-memq p k)
	    (if just-vars?
		null
		`(lambda (e esc)
		   (if (stx-symbol? e)
		       (if (free-identifier=? e (quote-syntax ,p))
			   null
			   (esc #f))
		       (esc #f))))
	    (if (and use-ellipses?
		     (eq? (syntax-e p) '...))
		(apply
		 raise-syntax-error 
		 'syntax
		 "misplaced ellipses"
		 (pick-specificity
		  top
		  local-top))
		(if just-vars?
		    (list p)
		    `(lambda (e esc)
		       (list e)))))]
       [else
	(if just-vars?
	    null
	    `(lambda (e esc)
	       (if (equal? ,(syntax-e p) (syntax-e e))
		   null
		   (esc #f))))]))
    (let ([r (m&e p p #t)])
      (if just-vars?
	  ;; Look for duplicate uses of variable names:
	  (let ([ht (make-hash-table)])
	    (let loop ([r r])
	      (cond
	       [(syntax? r)
		(let ([l (hash-table-get ht (syntax-e r) (lambda () null))])
		  (when (ormap (lambda (i) (free-identifier=? i r)) l)
		    (raise-syntax-error 
		     'syntax
		     "variable used twice"
		     top
		     r))
		  (hash-table-put! ht (syntax-e r) (cons r l)))]
	       [(pair? r)
		(loop (car r))
		(loop (cdr r))]
	       [else (void)]))
	    r)
	  `(lambda (e1)
	     (let/ec esc
	       (,r e1 esc))))))

  (define (make-match&env p k)
    (make-match&env/extract-vars p k #f))
  
  (define (get-match-vars p k)
    (make-match&env/extract-vars p k #t))

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
    (define (expander p proto-r local-top use-ellipses?)
      (cond
       [(and use-ellipses? (ellipsis? p))
	(let* ([p-head (stx-car p)]
	       [nestings (get-ellipsis-nestings p-head k)])
	  (when (null? nestings)
	    (apply
	     raise-syntax-error 
	     'syntax
	     "no pattern variables in ellipses"
	     (pick-specificity
	      top
	      local-top)))
	  (let* ([flat-nestings (flatten-nestings nestings)]
		 [proto-rr (and proto-r
				(map (lambda (nesting)
				       (ellipsis-sub-env nesting proto-r top local-top))
				     nestings))]
		 [rest (expander (stx-cdr (stx-cdr p)) proto-r local-top #t)]
		 [ehead (expander p-head proto-rr p-head #t)])
	    (if proto-r
		(let ([quoted-flat-nestings (cons 'list
						  (map (lambda (fn) `(quote-syntax ,fn)) 
						       flat-nestings))])
		  `(lambda (r)
		     (append
		      (map 
		       (lambda vals (,ehead vals))
		       ,@(map (lambda (var)
				`(list-ref r ,(stx-memq*-pos var proto-r)))
			      flat-nestings))
		      ,(apply-to-r rest))))
		(append! ehead rest))))]
       [(stx-pair? p)
	(let ([hd (stx-car p)])
	  (if (and use-ellipses?
		   (stx-symbol? hd)
		   (eq? (syntax-e hd) '...))
	      (if (and (stx-pair? (stx-cdr p))
		       (stx-null? (stx-cdr (stx-cdr p))))
		  (let ([dp (stx-car (stx-cdr p))])
		    (expander dp proto-r dp #f))
		  (apply
		   raise-syntax-error 
		   'syntax
		   "misplaced ellipses"
		   (pick-specificity
		    top
		    local-top)))
	      (let ([ehd (expander hd proto-r hd use-ellipses?)]
		    [etl (expander (stx-cdr p) proto-r local-top use-ellipses?)])
		(if proto-r
		    `(lambda (r)
		       ,(apply-cons (apply-to-r ehd) (apply-to-r etl) p))
		    (cons ehd etl)))))]
       [(stx-symbol? p)
	(if (stx-memq p k) 
	    (if proto-r 
		`(lambda (r) (quote-syntax ,p))
		null)
	    (if proto-r
		(let ((x (stx-memq p proto-r)))
		  (if x 
		      `(lambda (r) (list-ref r ,(stx-memq-pos p proto-r)))
		      (begin
			(when (and use-ellipses?
				   (eq? (syntax-e p) '...))
			  (apply
			   raise-syntax-error 
			   'syntax
			   "misplaced ellipses"
			   (pick-specificity
			    top
			    local-top)))
			(check-not-pattern p proto-r)
			`(lambda (r) (quote-syntax ,p)))))
		(list p)))]
       [else (if proto-r 
		 `(lambda (r) (quote-syntax ,p))
		 null)]))
    (let ([l (expander p proto-r p #t)])
      (if proto-r
	  `(lambda (r src)
	     ,(let ([main `(datum->syntax ,(apply-to-r l) src (quote-syntax ,dest))])
		(if (multiple-ellipsis-vars? proto-r)
		    `(let ([exn #f])
		       ((let/ec esc
			  (dynamic-wind
			   (lambda ()
			     (set! exn (current-exception-handler))
			     (current-exception-handler
			      (lambda (exn)
				(esc
				 (lambda ()
				   (raise-syntax-error
				    'syntax
				    "incompatible ellipsis match counts"
				    (quote-syntax ,p)))))))
			   (lambda ()
			     (let ([v ,main])
			       (lambda () v)))
			   (lambda ()
			     (current-exception-handler exn))))))
		    main)))
	  ;; Get list of unique vars:
	  (let ([ht (make-hash-table)])
	    (let loop ([r l])
	      (cond
	       [(syntax? r)
		(let ([l (hash-table-get ht (syntax-e r) (lambda () null))])
		  (unless (ormap (lambda (i) (free-identifier=? i r)) l)
		    (hash-table-put! ht (syntax-e r) (cons r l))))]
	       [(pair? r)
		(loop (car r))
		(loop (cdr r))]
	       [else (void)]))
	    (apply append (hash-table-map ht (lambda (k v) v)))))))

  (define (apply-to-r rest)
    (if (and (pair? rest)
	     (eq? (car rest) 'lambda)
	     (equal? (cadr rest) '(r)))
	(caddr rest)
	`(,rest r)))

  (define (apply-cons h t p)
    (if (and (pair? h)
	     (eq? (car h) 'quote-syntax)
	     (eq? (cadr h) (stx-car p))
	     (pair? t)
	     (eq? (car t) 'quote-syntax)
	     (eq? (cadr t) (stx-cdr p)))
	`(quote-syntax ,p)
	`(cons ,h ,t)))

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
		     (stx-symbol? hd)
		     (eq? (syntax-e hd) '...)
		     (stx-pair? (stx-cdr p)))
		(sub (stx-car (stx-cdr p)) #f)
		(append! (sub (stx-car p) use-ellipses?) 
			 (sub (stx-cdr p) use-ellipses?))))]
	 [(stx-symbol? p)
	  (if (stx-memq p k) 
	      '() 
	      (list p))]
	 [else '()]))))

  ;; Checks whether the given nesting matches a nesting in
  ;; the environment prototype, returning the prototype entry
  ;; unwrapped by one if it is found, and signallign an error
  ;; otherwise.
  (define ellipsis-sub-env
    (lambda (nesting proto-r src detail-src)
      (let ([v (ormap (lambda (proto)
			(and (pair? proto)
			     (let loop ([c (car proto)][n nesting])
			       (cond
				[(and (pair? c) (pair? n))
				 (loop (car c) (car n))]
				[(and (syntax? c) (syntax? n))
				 (if (free-identifier=? c n)
				     (car proto)
				     #f)]
				[else #f]))))
		      proto-r)])
	(unless v
	  (apply
	   raise-syntax-error 
	   'syntax
	   "wrong ellipsis depth for pattern variable"
	   (pick-specificity
	    src
	    (let loop ([n nesting])
	      (if (syntax? n)
		  n
		  (loop (cdr n)))))))
	v)))

  ;; Checks that a variable is not in the prototype
  ;; environment, and specifically not an ellipsed
  ;; variable.
  (define (check-not-pattern ssym proto-r)
    (for-each (lambda (p)
		(when (pair? p)
		  (let loop ([l (car p)])
		    (cond
		     [(syntax? l)
		      (when (free-identifier=? l ssym)
			(raise-syntax-error 
			 'syntax
			 "missing ellipses with pattern variable"
			 ssym))]
		     [else (loop (car l))]))))
	      proto-r))

  ;; Tests if x is an ellipsing pattern of the form
  ;;   (blah ... . blah2)
  (define (ellipsis? x)
    (and (stx-pair? x) (stx-pair? (stx-cdr x)) 
	 (eq? (syntax-e (stx-car (stx-cdr x))) '...)
	 (not (eq? (syntax-e (stx-car x)) '...))))

  ;; Takes an environment prototype and removes
  ;; the ellipsis-nesting information.
  (define (flatten-nestings nestings)
    (map (lambda (nesting)
	   (let loop ([nesting nesting])
	     (if (syntax? nesting)
		 nesting
		 (loop (car nesting)))))
	 nestings))

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

  (export make-match&env get-match-vars make-pexpand
	  make-syntax-mapping syntax-mapping?
	  syntax-mapping-depth syntax-mapping-valvar
	  stx-memq-pos))

;;----------------------------------------------------------------------
;; syntax-case and syntax

(module .syntax-case .kernel
  (import .stx-utilities .small-scheme)
  (import-for-syntax .stx-utilities .small-scheme .sc-utilities .kernel)

  (define-syntax syntax-case
    (lambda (x)
      (define l (and (stx-list? x) (stx->list x)))
      (unless (and (stx-list? x)
		   (> (length l) 3))
	(raise-syntax-error
	 'syntax-case
	 "bad form"
	 x))
      (let ([expr (cadr l)]
	    [kws (caddr l)]
	    [clauses (cdddr l)])
	(for-each
	 (lambda (lit)
	   (unless (stx-symbol? lit)
	     (raise-syntax-error
	      'syntax-case
	      "literal is not a indentifier"
	      x
	      lit)))
	 (stx->list kws))
	(for-each
	 (lambda (clause)
	   (unless (and (stx-list? clause)
			(<= 2 (length (stx->list clause)) 3))
	     (raise-syntax-error
	      'syntax-case
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
				 (stx->list patterns))])
	    (datum->syntax
	     (list (quote-syntax let) (list (list arg expr))
		   (let loop ([patterns patterns]
			      [fenders fenders]
			      [unflat-pattern-varss pattern-varss]
			      [answers answers])
		     (cond
		      [(null? patterns)
		       (list
			(quote-syntax raise-syntax-error)
			(list (quote-syntax quote)
			      (quote-syntax compile))
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
			      (lambda (p) (datum->syntax (gensym) #f p))
			      pattern-vars))
			   ;; Here's the result expression for one match:
			   (list
			    (quote-syntax let)
			    ;; Bind try-next to try next case
			    (list (list (quote try-next)
					(list (quote-syntax lambda)
					      (list)
					      rest)))
			    ;; Do match, bind result to rslt:
			    (list (quote-syntax let)
				  (list 
				   (list rslt
					 (list (datum->syntax
						(make-match&env
						 pattern
						 (stx->list kws))
						pattern 
						(quote-syntax here))
					       arg)))
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
					    (list
					     (quote-syntax list-ref)
					     rslt
					     (stx-memq-pos pattern-var pattern-vars))))
					 pattern-vars temp-vars)
				    ;; Tell nested `syntax' forms about the
				    ;;  pattern-bound variables:
				    (list
				     (quote-syntax letrec-syntax) 
				     (map (lambda (pattern-var unflat-pattern-var temp-var)
					    (list pattern-var 
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
					       (list (quote-syntax try-next)))
					 answer)))
				   (list (quote-syntax try-next)))))))])))
	     x
	     (quote-syntax here)))))))

  (define-syntax syntax
    (lambda (x)
      (unless (and (stx-pair? x)
		   (stx-pair? (stx-cdr x))
		   (stx-null? (stx-cdr (stx-cdr x))))
	(raise-syntax-error
	 'syntax
	 "bad form"
	 x))
      (datum->syntax
       (let ([pattern (stx-car (stx-cdr x))])
	 (let ([unique-vars (make-pexpand pattern #f null #f)])
	   (if (null? unique-vars)
	       (list (quote-syntax quote-syntax) pattern)
	       (let ([var-bindings
		      (map
		       (lambda (var)
			 (and (let ([v (syntax-local-value var (lambda () #f))])
				(and (syntax-mapping? v)
				     v))))
		       unique-vars)])
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
						     (cons (car vars) rest)))))]
		       [r (let loop ([vars unique-vars][bindings var-bindings])
			    (cond
			     [(null? bindings) null]
			     [(car bindings) (cons 
					      (datum->syntax
					       (syntax-e 
						(syntax-mapping-valvar (car bindings)))
					       #f
					       (car vars))
					      (loop (cdr vars) (cdr bindings)))]
			     [else  (loop (cdr vars) (cdr bindings))]))])
		   (list (datum->syntax
			  (make-pexpand pattern proto-r non-pattern-vars pattern)
			  pattern
			  (quote-syntax here))
			 (cons (quote-syntax list) r)
			 (list (quote-syntax quote-syntax)
			       (datum->syntax 'srctag x #f))))))))
       x
       (quote-syntax here))))

  (export syntax-case syntax))

;;----------------------------------------------------------------------
;; syntax/loc

(module .syntax-loc .kernel
  (import .syntax-case)
  (import-for-syntax .kernel .syntax-case)

  (define-syntax syntax/loc
    (lambda (stx)
      (syntax-case stx ()
	[(_ loc pattern)
	 (syntax (let ([stx (syntax pattern)])
		   (datum->syntax
		    (syntax-e stx)
		    loc
		    stx)))])))

  (export syntax/loc))

;;----------------------------------------------------------------------
;; with-syntax, generate-temporaries, identifier?

(module .with-syntax .kernel
  (import .syntax-case .stx-utilities .small-scheme)
  (import-for-syntax .kernel .syntax-case .syntax-loc)

  ;; From Dybvig
  (define-syntax with-syntax
    (lambda (x)
      (syntax-case x ()
	((_ () e1 e2 ...)
	 (syntax/loc x (begin e1 e2 ...)))
	((_ ((out in)) e1 e2 ...)
	 (syntax/loc x (syntax-case in () (out (begin e1 e2 ...)))))
	((_ ((out in) ...) e1 e2 ...)
	 (syntax/loc x (syntax-case (list in ...) ()
			 ((out ...) (begin e1 e2 ...))))))))

  (define (generate-temporaries l)
    (let ([l (stx->list l)])
      (map (lambda (x) (datum->syntax (gensym) #f #f)) l)))

  (define (identifier? x)
    (and (syntax? x)
	 (symbol? (syntax-e x))))

  (export with-syntax generate-temporaries identifier?))

;;----------------------------------------------------------------------
;; .syntax-case-scheme: adds let-syntax, synatx-rules, and
;;  check-duplicate-identifier, and assembles everything we have so far

(module .syntax-case-scheme .kernel
  (import .small-scheme .stx-utilities .syntax-case .with-syntax .syntax-loc)
  (import-for-syntax .kernel .small-scheme .syntax-case .with-syntax .syntax-loc)

  (define (check-duplicate-identifier names)
    (let/ec escape
      (let ([ht (make-hash-table)])
	(for-each
	 (lambda (defined-name)
	   (let ([l (hash-table-get ht (syntax-e defined-name) (lambda () null))])
	     (when (ormap (lambda (i) (bound-identifier=? i defined-name)) l)
	       (escape defined-name))
	     (hash-table-put! ht (syntax-e defined-name) (cons defined-name l))))
	 names)
	#f)))
  
  (define-syntax let-syntax
    (lambda (stx)
      (syntax-case stx ()
	[(_ ([id expr] ...) body)
	 (with-syntax ([(tmp ...) (generate-temporaries (syntax (id ...)))])
	   (syntax/loc
	    stx
	    (letrec-syntax ([tmp expr] ...)
	      (letrec-syntax ([id (syntax-local-value (quote tmp) void)] ...)
		body))))])))

  ;; From Dybvig:
  (define-syntax syntax-rules
    (lambda (x)
      (syntax-case x ()
	((_ (k ...) ((keyword . pattern) template) ...)
	 (with-syntax (((dummy ...)
			(generate-temporaries (syntax (keyword ...)))))
	   (syntax (lambda (x)
		     (syntax-case x (k ...)
		       ((dummy . pattern) (syntax template))
		       ...))))))))

  (export (all-from .syntax-case) (all-from .kernel) (all-from .small-scheme)
	  (all-from .with-syntax) (all-from .syntax-loc) check-duplicate-identifier
	  let-syntax syntax-rules))

;;----------------------------------------------------------------------
;; .more-scheme : case, do, etc.

(module .more-scheme .kernel
  (import .small-scheme)
  (import-for-syntax .syntax-case-scheme)

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
	((_ v (else e1 e2 ...))
	 (syntax/loc x (begin v e1 e2 ...)))
	((_ v ((k ...) e1 e2 ...))
	 (syntax/loc x (if (memv v '(k ...)) (begin e1 e2 ...))))
	((_ v ((k ...) e1 e2 ...) c1 c2 ...)
	 (syntax/loc x (let ((x v))
			 (if (memv x '(k ...))
			     (begin e1 e2 ...)
			     (case x c1 c2 ...))))))))

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
				     'do
				     "bad variable syntax"
				     orig-x))))
			     (syntax (var ...))
			     (syntax (step ...)))))
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
       (let ([value (void)] [set? #f])
	 (lambda ()
	   (unless set?
	     (let ([v (thunk)])
	       (unless set?
		 (set! value v)
		 (set! set? #t))))
	   value)))))
  
  (define (force p)
    (unless (promise? p)
      (raise-type-error 'force "promise" p))
    (promise-p p))

  (define-syntax parameterize
    (lambda (stx)
      (syntax-case stx ()
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

  (define (not-break-exn? x) (not (exn:misc:user-break? x)))

  (define-syntax set!-values
    (lambda (stx)
      (syntax-case stx ()
	[(_ (id) expr) (identifier? (syntax id)) (syntax (set! id expr))]
	[(_ (id ...) expr)
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
	 (syntax/loc (let ()
		       (define-struct base (field ...))
		       body1 body ...))])))

  (define-syntax fluid-let
    (lambda (stx)
      (syntax-case stx ()
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

  (export-indirect make-a-promise)
  (export case do delay force promise?
	  parameterize with-handlers not-break-exn? set!-values
	  let/cc let-struct fluid-let
	  (all-from .small-scheme)))
