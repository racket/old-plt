
(unit/sig
 mzlib:command-line^
 (import)

 (define number-regexp (regexp "^[-+][0-9]*(|[.][0-9]*)$"))

 (define parse-command-line
   (case-lambda
    [(program arguments table finish)
     (parse-command-line program arguments table finish
			 (lambda (s)
			   (display s)
			   (exit 0)))]
    [(program arguments table finish help)
     (parse-command-line program arguments table finish help
			 (lambda (flag)
			   (error (string->symbol program) "unknown flag: ~s" flag)))]
    [(program arguments table finish help unknown-flag)
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
		    (andmap (lambda (spec)
			      (and (or (and (list? spec) (pair? spec))
				       (bad-table (format "spec-set must be a non-empty list: ~a" spec)))
				   (or (memq (car spec) '(once-any once-each multi))
				       (bad-table (format "spec-set type must be 'once-any, 'once-each, or 'multi: ~a" 
							  (car spec))))
				   (andmap (lambda (line)
					     (and (or (and (list? line) (= (length line) 3))
						      (bad-table (format "spec-line must be a list of at three items: ~e" line)))
						  (or (list? (car line))
						      (bad-table (format "flags part of a spec-line must be a list: ~e" (car line))))
						  (andmap
						   (lambda (flag)
						     (or (string? flag)
							 (bad-table (format "flag must be a string: ~e" flag)))
						     (or (and (or (regexp-match "^-[^-]$" flag)
								  (regexp-match "^[+][^+]$" flag)
								  (regexp-match "^--." flag)
								  (regexp-match "^[+][+]." flag))
							      (not (or (regexp-match "^--help$" flag)
								       (regexp-match "^-h$" flag)
								       (regexp-match number-regexp flag))))
							 (bad-table (format "no ill-formed or pre-defined flags: ~e" flag))))
						   (car line))
						  (or (procedure? (cadr line))
						      (bad-table (format "second item in a spec-line must be a procedure: ~e" (cadr line))))
						  (let ([a (arity (cadr line))])
						    (or (and (number? a)
							     (or (>= a 1)
								 (bad-table (format "flag handler procedure must take at least 1 argument: ~e" 
										    (cadr line)))))
							(arity-at-least? a)
							(bad-table (format "flag handler procedure cannot have multiple cases: ~e" (cadr line)))))
						  (or (and (list? (caddr line))
							   (andmap string? (caddr line)))
						      (bad-table (format "spec-line help section must be a list of strings")))
						  
						  (or (let ([l (length (caddr line))]
							    [a (arity (cadr line))])
							(if (number? a)
							    (= a l)
							    (and (>= l 1)
								 (>= l (arity-at-least-value a)))))
						      (bad-table (format "spec-line help list strings must match procedure arguments")))))
					   (cdr spec))))
			    table)))
	     (raise-type-error 'parse-command-line "list of flag-procedure pairs (~a)" table))
     (unless (and (procedure? finish) (procedure-arity-includes? finish 2))
	     (raise-type-error 'parse-command-line "procedure of arity 2" finish))
     (unless (and (procedure? help) (procedure-arity-includes? help 1))
	     (raise-type-error 'parse-command-line "procedure of arity 1" help))
     (unless (and (procedure? unknown-flag) (procedure-arity-includes? unknown-flag 1)
		  (let ([a (arity unknown-flag)])
		    (or (number? a) (arity-at-least? a))))
	     (raise-type-error 'parse-command-line "procedure of simple arity, accepting 1 argument (at least)" unknown-flag))
     (let* ([once-spec-set
	     (lambda (lines)
	       (let ([set (cons #f (apply append (map car lines)))])
		 (map
		  (lambda (line) (cons set line))
		  lines)))]
	    [table
	     (apply
	      append
	      (list
	       (list #f (list "--help" "-h")
		     (lambda (f)
		       (let ([sp (open-output-string)])
			 (for-each
			  (lambda (line)
			    (let loop ([flags (car line)])
			      (let ([flag (car flags)])
				(fprintf sp " ~a" flag)
				(for-each
				 (lambda (arg) (fprintf sp " <~a>" arg))
				 (cdaddr line)))
			      (unless (null? (cdr flags))
				      (fprintf sp ",")
				      (loop (cdr flags))))
			    (fprintf sp " : ~a~n" (caaddr line)))
			  table) ; the original table
			 (fprintf sp " --help, -h : Show this help~n")
			 (help (get-output-string))))
		     (list "Help")))
	      (map
	       (lambda (spec)
		 (cond
		  [(eq? (car spec) 'once-each)
		   (apply
		    append
		    (map 
		     (lambda (line) (once-spec-set (list line))) 
		     (cdr spec)))]
		  [(eq? (car spec) 'once-any)
		   (once-spec-set (cdr spec))]
		  [(eq? (car spec) 'multi)
		   (map
		    (lambda (line) (cons #f line))
		    (cdr spec))]))
	       table))]
	    [done
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
	       (let loop ([table table])
		 (cond
		  [(null? table)
		   (call-handler unknown-flag flag args r-acc k)]
		  [(member flag (cadar table))
		   (when (caar table)
			 (let ([set (caar table)])
			   (if (car set)
			       (let ([flags (cdr set)])
				 (error (string->symbol program)
					(if (= 1 (length flags))
					    (format "the ~a flag can only be specified once" (car flags))
					    (format "only one instance of one flag from ~a is allowed" flags))))
			       (set-car! set #t))))
		   (call-handler (caddar table) flag args r-acc k)]
		  [else (loop (cdr table))])))])
       (let loop ([args (vector->list arguments)][r-acc null])
	 (if (null? args)
	     (done args r-acc)
	     (let ([arg (car args)]
		   [rest (cdr args)])
	       (cond
		[(regexp-match number-regexp arg)
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
		 (done args r-acc)])))))])))
