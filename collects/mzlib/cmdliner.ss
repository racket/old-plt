
(unit/sig
 mzlib:command-line^
 (import)

 (define parse-command-line
   (lambda (program arguments table finish)
     (unless (string? program)
	     (raise-type-error 'parse-command-line "string" program))
     (unless (vector? arguments)
	     (raise-type-error 'parse-command-line "vector" arguments))
     (unless (and (list? table)
		  (let ([bad-table
			 (lambda (reason)
			   (raise-type-error 'parse-command-line 
					     (format "list of flag-list/procedure pairs (~a)" 
						     reason)
					     table))])
		    (andmap (lambda (line)
			      (and (or (and (list? line) (= (length line) 3))
				       (bad-table (format "table line must be a list of at three items: ~e" line)))
				   (or (list? (car line))
				       (bad-table (format "flags section must be a list: ~e" (car line))))
				   (andmap
				    (lambda (flag)
				      (or (string? flag)
					  (bad-table (format "flag must be a string: ~e" flag)))
				      (or (and (or (regexp-match "^-[^-]$" flag)
						   (regexp-match "^[+][^+]$" flag)
						   (regexp-match "^--." flag)
						   (regexp-match "^[+][+]." flag))
					       (not (or (regexp-match "^--help$" flag)
							(regexp-match "^-h$" flag))))
					  (bad-table (format "no ill-formed or pre-defined flags: ~e" flag))))
				    (car line))
				   (or (procedure? (cadr line))
				       (bad-table (format "second line item must be a procedure: ~e" (cadr line))))
				   (let ([a (arity (cadr line))])
				     (or (and (number? a)
					      (or (>= a 1)
						  (bad-table (format "procedure must take at least 1 argument: ~e" (cadr line)))))
					 (arity-at-least? a)
					 (bad-table (format "procedure cannot have multiple cases: ~e" (cadr line)))))
				   (or (and (list? (caddr line))
					    (andmap string? (caddr line)))
				       (bad-table (format "line help section must be a list of strings")))
				   
				   (or (let ([l (length (caddr line))]
					     [a (arity (cadr line))])
					 (if (number? a)
					     (= a l)
					     (and (>= l 1)
						  (>= l (arity-at-least-value a)))))
				       (bad-table (format "help list strings must match procedure arguments")))))
			    table)))
	     (raise-type-error 'parse-command-line "list of flag-procedure pairs (~a)" table))
     (unless (and (procedure? finish) (procedure-arity-includes? finish 2))
	     (raise-type-error 'parse-command-line "procedure of arity 2" finish))
     (let* ([done
	     (lambda (args r-acc)
	       (finish (list->vector args)
		       (reverse r-acc)))]
	    [call-handler
	     (lambda (handler flag args r-acc k)
	       (let* ([a (arity handler)]
		      [remaining (length args)]
		      [needed (if (number? a)
				  (sub1 a)
				  (sub1 (arity-at-least-value a)))]
		      [use (if (number? a)
			       (sub1 a)
			       remaining)])
		 (if (< remaining needed)
		     (error (string->symbol program)
			    "the ~s flag needs ~a arguments, but only ~a remain"
			    flag needed remaining)
		     (let ([v (apply handler 
				     flag
				     (let loop ([n use][args args])
				       (if (zero? n)
					   null
					   (cons (car args)
						 (loop (sub1 n) (cdr args))))))])
		       (k (list-tail args use)
			  (if (void? v)
			      r-acc
			      (cons v r-acc)))))))]
	    [handle-flag
	     (lambda (flag args r-acc k)
	       (let loop ([table (cons
				  (list (list "--help" "-h")
					(lambda (f)
					  (for-each
					   (lambda (line)
					     (let loop ([flags (car line)])
					       (let ([flag (car flags)])
						 (printf " ~a" flag)
						 (for-each
						  (lambda (arg) (printf " <~a>" arg))
						  (cdaddr line)))
					       (unless (null? (cdr flags))
						  (printf ",")
						  (loop (cdr flags))))
					     (printf " : ~a~n" (caaddr line)))
					   table)
					  (printf " --help, -h : Show this help~n")
					  (exit 0))
					"")
				  table)])
		 (cond
		  [(null? table)
		   (error (string->symbol program) "unknown flag: ~s" flag)]
		  [(member flag (caar table))
		   (call-handler (cadar table) flag args r-acc k)]
		  [else (loop (cdr table))])))])
       (let loop ([args (vector->list arguments)][r-acc null])
	 (if (null? args)
	     (done args (reverse! r-acc))
	     (let ([arg (car args)]
		   [rest (cdr args)])
	       (cond
		[(regexp-match "^[-+][0-9]*(|[.][0-9]*)$" arg)
		 (done args r-acc)]
		[(regexp-match "^--$" arg)
		 (done (cdr args) r-acc)]
		[(regexp-match "^[-+][-+]" arg)
		 (handle-flag arg rest r-acc loop)]
		[(regexp-match "^[-+]." arg)
		 (let a-loop ([s (string->list (substring arg 1 (string-length arg)))]
			      [rest rest]
			      [r-acc r-acc])
		   (if (null? s)
		       (loop rest r-acc)
		       (handle-flag (string (string-ref arg 0) (car s)) 
				    rest r-acc
				    (lambda (args r-acc)
				      (a-loop (cdr s) args r-acc)))))]
		[else
		 (done args r-acc)]))))))))
