
(module cmdline mzscheme

  (define-syntax command-line
    (lambda (stx)
      (syntax-case stx ()
	[(_ program-name
	    argv
	    clause ...)
	 (let ([serror
		(lambda (msg . detail)
		  (apply
		   raise-syntax-error
		   #f
		   msg
		   stx
		   detail))])
	   (let ([extract (lambda (what args . detail)
			    (if (null? args)
				(apply serror (format "missing ~a" what) detail)
				(values (car args) (cdr args))))]
		 [formal-names
		  (lambda (l)
		    (map
		     (lambda (a)
		       (datum->syntax-object
			(quote-syntax here)
			(let ([s (symbol->string (syntax-e a))])
			  (if (char=? #\* (string-ref s (sub1 (string-length s))))
			      (substring s 0 (sub1 (string-length s)))
			      s))
			#f))
		     l))])
	     (let ([clauses
		    (let loop ([csrcs (syntax->list (syntax (clause ...)))][clauses null])
		      (with-syntax ([(clause ...) clauses])
			(if (null? csrcs)
			    (syntax ((list clause ...) (lambda (accum) (void)) null))
			    (let ([line (car csrcs)]
				  [arest (cdr csrcs)])
			      (syntax-case line (help-labels => args)
				[(help-labels s ...)
				 (begin
				   (unless (andmap (lambda (x) (string? (syntax-e x)))
						   (syntax->list (syntax (s ...))))
				     (serror "help-labels clause must contain only strings" line))
				   (loop arest
					 (syntax (clause 
						  ...
						  '(help-labels s ...)))))]
				[(tag . rest)
				 (ormap (lambda (x) (module-identifier=? (syntax tag) x))
					(syntax->list (syntax (once-each once-any multi final))))
				 (with-syntax 
				     ([sublines
				       (let slloop ([sublines (syntax->list (syntax rest))])
					 (if (null? sublines)
					     (syntax ())
					     (with-syntax 
						 ([looped (slloop (cdr sublines))]
						  [subline
						   (with-syntax 
						       ([flags 
							 (syntax-case (car sublines) ()
							   [((flag ...) . rest)
							    (begin
							      (unless (andmap 
								       (lambda (x) (string? (syntax-e x)))
								       (syntax->list (syntax (flag ...))))
								(serror 
								 "flag specification is not a string or sequence of strings" 
								 (syntax (flag ...))))
							      (syntax (flag ...)))]
							   [(flag . rest)
							    (string? (syntax-e (syntax flag)))
							    (syntax (flag))]
							   [else
							    (serror "clause does not start with flags")])])
						     (syntax-case (car sublines) (=>)
						       [(_ => a b)
							(syntax (list 'flags a b))]
						       [(_ rest ...)
							(let*-values ([(formals rest) 
								       (let loop ([a null]
										  [rest (syntax->list 
											 (syntax
											  (rest ...)))])
									 (cond
									  [(null? rest) (values a null)]
									  [(identifier? (car rest)) 
									   (loop 
									    (append a (list (car rest))) 
									    (cdr rest))]
									  [else (values a rest)]))]
								      [(help rest) 
								       (extract "help string" rest line)]
								      [(_) 
								       (unless (string? (syntax-e help))
									 (serror 
									  "help info is not a string" help))]
								      [(expr1 rest) 
								       (extract 
									"handler body expressions" rest line)])
							  (with-syntax ([formals formals]
									[formal-names (formal-names formals)]
									[help help]
									[expr1 expr1]
									[rest rest])
							    (syntax (list 'flags
									  (lambda (flag . formals)
									    expr1 . rest)
									  '(help . formal-names)))))]))])
					       (syntax (subline . looped)))))])
				   (loop arest
					 (syntax (clause
						  ...
						  (list 'tag . sublines)))))]
				[(=> finish-proc arg-help help-proc unknown-proc)
				 (begin
				   (unless (null? arest)
				     (serror "=> must be the last clause line"))
				   (syntax ((list clause
						  ...)
					    finish-proc arg-help help-proc unknown-proc)))]
				[(=> finish-proc arg-help help-proc)
				 (begin
				   (unless (null? arest)
				     (serror "=> must be the last clause line"))
				   (syntax ((list clause
						  ...)
					    finish-proc arg-help help-proc)))]
				[(=> finish-proc arg-help)
				 (begin
				   (unless (null? arest)
				     (serror "=> must be the last clause line"))
				   (syntax ((list clause
						  ...)
					    finish-proc arg-help)))]
				[(=> . _)
				 (serror "bad => line" line)]
				[(args arg-formals body1 body ...)
				 (begin
				   (unless (null? arest)
				     (serror "args must be the last clause" line))
				   (let ([formals
					  (let loop ([f (syntax arg-formals)])
					    (syntax-case f ()
					      [() null]
					      [(arg . rest)
					       (identifier? (syntax arg))
					       (cons (syntax arg) (loop (syntax rest)))]
					      [arg
					       (identifier? (syntax arg))
					       (list (syntax arg))]
					      [else
					       (serror "bad argument list" line)]))])
				     (with-syntax ([formal-names (formal-names formals)])
				       (syntax ((list clause
						      ...)
						(lambda (accume . arg-formals)
						  body1 body ...)
						'formal-names)))))]
				[(args . _)
				 (serror "bad args line" line)]
				[else (serror "not a once-each, once-any, multi, final, args, or => line" line)])))))])
	       (with-syntax ([clauses clauses])
		 (syntax
		  (parse-command-line
		   program-name argv
		   . clauses))))))])))

  (define number-regexp (regexp "^[-+][0-9]*(|[.][0-9]*)$"))
  
  (define print-args
    (lambda (port l f)
      (let loop ([l l][a (letrec ([a (procedure-arity f)]
				  [a-c (lambda (a)
					 (cond
					  [(number? a) (cons (sub1 a) (sub1 a))]
					  [(arity-at-least? a) 
					   (let ([v (sub1 (arity-at-least-value a))])
					     (cons v v))]
					  [else (let ([r (map a-c a)])
						  (cons (apply min (map car r))
							(apply max (map cdr r))))]))])
			   (a-c a))])
	(unless (null? l)
	  (fprintf port " ~a<~a>~a" 
		   (if (positive? (car a)) "" "[")
		   (car l)
		   (if (positive? (car a)) "" "]"))
	  (unless (positive? (cdr a))
	    (fprintf port " ..."))
	  (loop (cdr l) (cons (sub1 (car a)) (sub1 (cdr a))))))))

  (define (procedure-arity-includes-at-least? p n)
    (letrec ([a-c
	      (lambda (a)
		(cond
		 [(number? a) (>= a n)]
		 [(arity-at-least? a) #t]
		 [else (ormap a-c a)]))])
      (a-c (procedure-arity p))))

  (define parse-command-line
    (case-lambda
     [(program arguments table finish finish-help)
      (parse-command-line program arguments table finish finish-help
			  (lambda (s)
			    (display s)
			    (exit 0)))]
     [(program arguments table finish finish-help help)
      (parse-command-line program arguments table finish finish-help help
			  (lambda (flag)
			    (error (string->symbol program) "unknown flag: ~s" flag)))]
     [(program arguments table finish finish-help help unknown-flag)
      (unless (string? program)
	(raise-type-error 'parse-command-line "program name string" program))
      (unless (and (vector? arguments)
		   (andmap string? (vector->list arguments)))
	(raise-type-error 'parse-command-line "argument vector of strings" arguments))
      (unless (and (list? table)
		   (let ([bad-table
			  (lambda (reason)
			    (raise-type-error 'parse-command-line 
					      (format "table as a list of flag-list/procedure pairs (~a)" 
						      reason)
					      table))])
		     (andmap 
		      (lambda (spec)
			(and (or (and (list? spec) (pair? spec))
				 (bad-table (format "spec-set must be a non-empty list: ~a" spec)))
			     (or (memq (car spec) '(once-any once-each multi final help-labels))
				 (bad-table (format "spec-set type must be 'once-any, 'once-each, 'multi, 'final, or 'help-labels: ~a" 
						    (car spec))))
			     (andmap 
			      (lambda (line)
				(if (eq? (car spec) 'help-labels)

				    (or (string? line)
					(bad-table (format "help-labels line must be a string: ~e" line)))

				    (and (or (and (list? line) (= (length line) 3))
					     (bad-table (format "spec-line must be a list of at three or four items: ~e" line)))

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

					 (let ([a (procedure-arity (cadr line))])
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
						   [a (procedure-arity (cadr line))])
					       (if (number? a)
						   (= a l)
						   (and (>= l 1)
							(>= l (arity-at-least-value a)))))
					     (bad-table (format "spec-line help list strings must match procedure arguments"))))))
			      (cdr spec))))
		      table)))
	(raise-type-error 'parse-command-line "table of spec sets" table))
      (unless (and (procedure? finish)
		   (procedure-arity-includes-at-least? finish 1))
	(raise-type-error 'parse-command-line "finish procedure accepting at least 1 argument" finish))
      (unless (and (list? finish-help) (andmap string? finish-help))
	(raise-type-error 'parse-command-line "argument help list of strings" finish-help))
      (unless (and (procedure? help) (procedure-arity-includes? help 1))
	(raise-type-error 'parse-command-line "help procedure of arity 1" help))
      (unless (and (procedure? unknown-flag) (procedure-arity-includes? unknown-flag 1)
		   (let ([a (procedure-arity unknown-flag)])
		     (or (number? a) (arity-at-least? a))))
	(raise-type-error 'parse-command-line "unknown-flag procedure of simple arity, accepting 1 argument (an perhaps more)" unknown-flag))
      
      (letrec ([a (procedure-arity finish)]
	       [l (length finish-help)]
	       [a-c (lambda (a)
		      (or (and (number? a) (sub1 a))
			  (and (arity-at-least? a)
			       (max 1 (arity-at-least-value a)))
			  (and (list? a) (apply max (map a-c a)))))])
	(unless (= (a-c a) l)
	  (error 'parse-command-line "the length of the argument help string list does not match the arity of the finish procedure")))

      (let* ([finalled? #f] ; set to true when 'once-final is seen
             [once-spec-set
	      (lambda (lines)
		(let ([set (cons #f (apply append (map car lines)))])
		  (map
		   (lambda (line) (cons set line))
		   lines)))]
	     [table
              ;; list of (list <once-set> <spec-line> ...)
              ;; If <once-set> is #f, then flags in <spec-line> are allowed
              ;;  any number of times.
              ;; If <once-set> is 'final, then its like #f, and `finalled?' should
              ;;  be set.
              ;; Otherwise, <once-set> is (list <bool> <string> ...) where <bool>
              ;;  starts as #f and is mutated to #t when one of <string> is
              ;;  matched.
	      (apply
	       append
	       (list
		(list #f (list "--help" "-h")
		      (lambda (f)
			(let* ([sp (open-output-string)])
			  (fprintf sp "~a~a" program
				   (if (null? table)
				       ""
				       " [ <flag> ... ]"))
			  (print-args sp finish-help finish)
			  (fprintf sp "~n where <flag> is one of~n")
			  (for-each
			   (lambda (set)
			     (if (eq? (car set) 'help-labels)
				 (for-each
				  (lambda (line)
				    (fprintf sp " ~a~n" line))
				  (cdr set))
				 (for-each
				  (lambda (line)
				    (fprintf sp (cond
						 [(and (eq? (car set) 'once-any) 
						       (pair? (cddr set)))
						  (cond
						   [(eq? line (cadr set)) "/"]
						   [(eq? line (let loop ([l set])
								(if (pair? (cdr l))
								    (loop (cdr l))
								    (car l)))) "\\"]
						   [else "|"])]
						 [(memq (car set) '(multi final))
						  "*"]
						 [else " "]))
				    (let loop ([flags (car line)])
				      (let ([flag (car flags)])
					(fprintf sp " ~a" flag)
					(print-args sp (cdaddr line) (cadr line)))
				      (unless (null? (cdr flags))
					(fprintf sp ",")
					(loop (cdr flags))))
				    (fprintf sp " : ~a~n" (caaddr line)))
				  (cdr set))))
			   table) ; the original table
			  (fprintf sp "  --help, -h : Show this help~n")
			  (fprintf sp "  -- : Do not treat any remaining argument as a flag (at this level)~n")
			  (when (or (assq 'multi table) (assq 'final table))
			    (fprintf sp " * Asterisks indicate flags allowed multiple times.~n"))
			  (when (assq 'once-any table)
			    (fprintf sp " /|\\ Brackets indicate mutually exclusive flags.~n"))
			  (fprintf sp " Multiple single-letter flags can be combined after one `-'; for~n")
			  (fprintf sp "  example: `-h-' is the same as `-h --'~n")
			  (help (get-output-string sp))))
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
		   [(eq? (car spec) 'help-labels)
		    null]
		   [(eq? (car spec) 'multi)
		    (map
		     (lambda (line) (cons #f line))
		     (cdr spec))]
                   [(eq? (car spec) 'final)
                    (map
                     (lambda (line) (cons 'final line))
                     (cdr spec))]))
		table))]
	     [done
	      (lambda (args r-acc)
		(let ([options (reverse r-acc)]
		      [c (length args)])
		  (if (procedure-arity-includes? finish (add1 c))
		      (apply finish options args)
		      (error (string->symbol program)
			     (format "expects~a on the command line, given ~a argument~a~a"
				     (if (null? finish-help)
					 " no arguments"
					 (let ([s (open-output-string)])
					   (parameterize ([current-output-port s])
					     (print-args s finish-help finish))
					   (let ([s (get-output-string s)])
					     (if (equal? 2 (procedure-arity finish))
						 (format " 1~a" s)
						 s))))
				     c
				     (cond
				      [(zero? c) "s"]
				      [(= c 1) ": "]
				      [else "s: "])
				     (let loop ([args args])
				       (if (null? args)
					   ""
					   (string-append (car args) " " (loop (cdr args))))))))))]
	     [call-handler
	      (lambda (handler flag args r-acc k)
		(let* ([a (procedure-arity handler)]
		       [remaining (length args)]
		       [needed (if (number? a)
				   (sub1 a)
				   (sub1 (arity-at-least-value a)))]
		       [use (if (number? a)
				(sub1 a)
				remaining)])
		  (if (< remaining needed)
		      (error (string->symbol program)
			     "the ~s flag needs ~a argument~a, but ~a~a provided"
			     flag needed (if (> needed 1) "s" "")
                             (if (zero? remaining) "" "only ")
			     remaining)
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
	      (lambda (flag args r-acc orig-multi k)
		(let loop ([table table])
		  (cond
		   [(null? table)
		    (call-handler unknown-flag flag args r-acc k)]
		   [(member flag (cadar table))
		    (when (eq? 'final (caar table))
                      (set! finalled? #t))
		    (when (pair? (caar table))
		      (let ([set (caar table)])
			(if (car set)
			    (let ([flags (cdr set)])
			      (error (string->symbol program)
				     (let ([s (if (= 1 (length flags))
						  (format "the ~a flag can only be specified once" (car flags))
						  (format "only one instance of one flag from ~a is allowed" flags))])
				       (if orig-multi
					   (format "~a; note that ~s is shorthand for ~s, in contrast to ~s"
						   s
						   orig-multi
						   (let loop ([prefix (string-ref orig-multi 0)]
							      [flags (string->list (substring orig-multi 1 (string-length orig-multi)))]
							      [sep ""])
						     (if (null? flags)
							 ""
							 (format "~a~a~a~a" sep prefix (car flags)
								 (loop prefix (cdr flags) " "))))
						   (string-append (substring orig-multi 0 1) orig-multi))
					   s))))
			    (set-car! set #t))))
		    (call-handler (caddar table) flag args r-acc k)]
		   [else (loop (cdr table))])))])
	(let loop ([args (vector->list arguments)][r-acc null])
	  (if (null? args)
	      (done args r-acc)
	      (let ([arg (car args)]
		    [rest (cdr args)])
		(cond
                  [finalled?
                   (done args r-acc)]
                  [(regexp-match number-regexp arg)
                   (done args r-acc)]
                  [(regexp-match "^--$" arg)
                   (done (cdr args) r-acc)]
                  [(regexp-match "^[-+][-+]" arg)
                   (handle-flag arg rest r-acc #f loop)]
                  [(regexp-match "^[-+]." arg)
                   (let a-loop ([s (string->list (substring arg 1 (string-length arg)))]
                                [rest rest]
                                [r-acc r-acc])
                     (if (null? s)
                         (loop rest r-acc)
                         (handle-flag (string (string-ref arg 0) (car s)) 
                                      rest r-acc
                                      arg
                                      (lambda (args r-acc)
                                        (a-loop (cdr s) args r-acc)))))]
                  [else
                   (done args r-acc)])))))]))

  (provide command-line parse-command-line))


