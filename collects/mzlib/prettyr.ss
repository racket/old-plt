; Originally:
 ;"genwrite.scm" generic write used by pp.scm
 ;;copyright (c) 1991, marc feeley

; Pretty-printer for MzScheme
;  Handles structures, cycles, and graphs
;
; Procedures:
;
;  (pretty-print v) - pretty-prints v (like `write')
;  (pretty-print v port) - pretty-prints v to port
;  (pretty-print v port c) - pretty-prints v to port using c columns; if
;                            c is #f, (pretty-print-columns) is used
;  (pretty-print v port c graph?) - pretty-prints v to port using graph?
;                                   instead of (print-graph)
;  (pretty-print v port c graph? struct?) - pretty-prints v to port using 
;                                   struct? instead of (print-struct)
;  (pretty-print v port c graph? struct? depth) - pretty-prints v to port
;                                   using depth instead of 
;                                   (pretty-print-depth)
;  (pretty-print v port c graph? struct? depth size-hook) - pretty-prints 
;                                   using size-hook instead of
;                                   (pretty-print-size-hook)
;  (pretty-print v port c graph? struct? depth size-hook print-hook) - 
;                                   pretty-prints using print-hook instead
;                                   of (pretty-print-print-hook)
;
;  (pretty-display ...) - like pretty-print, but prints like `display'
;                         instead of like `write'
;
;  pretty-print-columns - parameter for the default number of columns
;    or 'infinity; initial setting: 79
;
;  pretty-print-print-line - parameter of a procedure that prints
;   to separate each line; 0 indicate before the first line, #f after the
;   last line
;
;  pretty-print-depth - parameter for the default print depth
;    initial setting: #f (= infinity)
;
;  pretty-print-size-hook - parameter for the print size hook; returns #f to
;                           let pretty-printer handle it, number otherwise
;    initial setting: (lambda (x display? port) #f)
;
;  pretty-print-print-hook - parameter for the print hook, called when the
;                              size-hook returns a non-#f value
;    initial setting: (lambda (x display? port) (void))
;
;  pretty-print-display-string-handler - parameter for the string display
;                              procedure, called to finally write text
;                              to the port
;
;  (pretty-print-handler v) - pretty-prints v if v is not #<void>
;
; TO INSTALL this pretty-printer into a MzScheme's read-eval-print loop,
;  load this file and evaluate:
;      (current-print pretty-print-handler)


;; Matthew's changes:
;; Modified original for MrEd Spring/95
;; Added check for cyclic structures 11/9/95
;; Better (correct) graph printing, support boxes and structures 11/26/95
;; Support for print depth 2/28/96
;; functor 4/22/96
;; unit/s 6/13/96
;; size- and print-hook 8/22/96
;; real parameters 9/27/96
;; print-line parameter 8/18/97

(unit/sig
   mzlib:pretty-print^
   (import)
   
   (define pretty-print-show-inexactness 
     (make-parameter #f
		     (lambda (x) (and x #t))))

   (define pretty-print-columns 
     (make-parameter 79
		     (lambda (x)
		       (unless (integer? x)
			       (raise-type-error 
				'pretty-print-columns
				"integer or 'infinity"
				x))
		       x)))

   (define pretty-print-depth
     (make-parameter #f
		     (lambda (x)
		       (unless (or (not x) (number? x))
			       (raise-type-error 
				'pretty-print-depth
				"number or #f"
				x))
		       x)))

   (define can-accept-n?
     (lambda (n x)
       (procedure-arity-includes? x n)))
   
   (define pretty-print-size-hook
     (make-parameter (lambda (x display? port) #f)
		     (lambda (x)
		       (unless (can-accept-n? 3 x)
			       (raise-type-error 
				'pretty-print-size-hook
				"procedure of 3 arguments"
				x))
		       x)))

   (define pretty-print-print-hook
     (make-parameter void
		     (lambda (x)
		       (unless (can-accept-n? 3 x)
			       (raise-type-error 
				'pretty-print-print-hook
				"procedure of 3 arguments"
				x))
		       x)))

   (define pretty-print-display-string-handler
     (make-parameter display
		     (lambda (x)
		       (unless (can-accept-n? 2 x)
			       (raise-type-error 
				'pretty-print-display-string-handler
				"procedure of 2 arguments"
				x))
		       x)))

   (define pretty-print-print-line
     (make-parameter (lambda (line port offset width)
		       (when (and (number? width)
				  (not (eq? 0 line)))
			     (newline port))
		       0)
		     (lambda (x)
		       (unless (can-accept-n? 4 x)
			       (raise-type-error 
				'pretty-print-print-line
				"procedure of 4 arguments"
				x))
		       x)))

   (define make-pretty-print
     (lambda (display?)
       (letrec ([pretty-print
		 (case-lambda 
		  ([obj port width print-graph? print-struct? depth size-hook print-hook]
		   (let ([width (if width width (pretty-print-columns))])
		     (generic-write obj display?
				    width
				    (let ([display (pretty-print-display-string-handler)])
				      (lambda (s)
					(display s port)
					#t))
				    (lambda (s l) 
				      (print-hook s display? port)
				      #t)
				    print-graph? print-struct? depth
				    (lambda (o display?)
				      (size-hook o display? port))
				    (let ([print-line (pretty-print-print-line)])
				      (lambda (line offset)
					(print-line line port offset width))))
		     (void)))
		  ([obj port width print-graph? print-struct? depth size-hook] 
		   (pretty-print obj port width print-graph? print-struct? depth 
				 size-hook (pretty-print-print-hook)))
		  ([obj port width print-graph? print-struct? depth] 
		   (pretty-print obj port width print-graph? print-struct? depth 
				 (pretty-print-size-hook)))
		  ([obj port width print-graph? print-struct?] 
		   (pretty-print obj port width print-graph? print-struct? 
				 (pretty-print-depth)))
		  ([obj port width print-graph?] 
		   (pretty-print obj port width print-graph? (print-struct)))
		  ([obj port width] 
		   (pretty-print obj port width (print-graph) (print-struct)))
		  ([obj port] (pretty-print obj port (pretty-print-columns)))
		  ([obj] (pretty-print obj (current-output-port) (pretty-print-columns))))])
	 pretty-print)))

   (define pretty-print (make-pretty-print #f))
   (define pretty-display (make-pretty-print #t))

   (define (generic-write obj display? width output output-hooked 
			  print-graph? print-struct? 
			  depth size-hook print-line)

     (define line-number 0)

     (define table (make-hash-table)) ; Hash table for looking for loops

     (define show-inexactness? (pretty-print-show-inexactness))

     (define-struct mark (str def))

     (define found-cycle
       (or print-graph?
	   (let loop ([obj obj])
	     (and (or (vector? obj)
		      (pair? obj)
		      (box? obj)
		      (and (struct? obj) print-struct?))
		  (or (hash-table-get table obj (lambda () #f))
		      (begin
			(hash-table-put! table obj #t)
			(let ([cycle
			       (cond
				[(vector? obj)
				 (ormap loop (vector->list obj))]
				[(pair? obj)
				 (or (loop (car obj))
				     (loop (cdr obj)))]
				[(box? obj) (loop (unbox obj))]
				[(struct? obj)
				 (ormap loop 
					(vector->list (struct->vector obj)))])])
			  (hash-table-remove! table obj)
			  cycle)))))))

     (define :::dummy:::
       (if found-cycle
	   (let loop ([obj obj])
	     (if (or (vector? obj)
		     (pair? obj)
		     (box? obj)
		     (and (struct? obj) print-struct?))
		 ; A little confusing: use #t for not-found
		 (let ([p (hash-table-get table obj (lambda () #t))])
		   (when (not (mark? p))
			 (if p
			     (begin
			       (hash-table-put! table obj #f)
			       (cond
				[(vector? obj)
				 (loop (vector->list obj))]
				[(pair? obj)
				 (loop (car obj))
				 (loop (cdr obj))]
				[(box? obj) (loop (unbox obj))]
				[(struct? obj)
				 (for-each loop 
					   (vector->list (struct->vector obj)))]))
			     (begin
			       (hash-table-put! table obj 
						(make-mark #f (box #f)))))))))))

     (define cycle-counter 0)

     (define found (if found-cycle
		       table 
		       #f))

     (define dsub1 (lambda (d)
		     (if d
			 (sub1 d)
			 #f)))

     (print-line
      #f
      (let generic-write ([obj obj] [display? display?] 
				    [width width] 
				    [output output] [output-hooked output-hooked]
				    [depth depth] [def-box (box #t)]
				    [startpos (print-line 0 0)])
	
	(define (read-macro? l)
	  (define (length1? l) (and (pair? l) (null? (cdr l))))
	  (let ((head (car l)) (tail (cdr l)))
	    (case head
	      ((quote quasiquote unquote unquote-splicing) (length1? tail))
	      (else                                        #f))))

       (define (read-macro-body l)
	 (cadr l))

       (define (read-macro-prefix l)
	 (let ((head (car l)))
	   (case head
	     ((quote)            "'")
	     ((quasiquote)       "`")
	     ((unquote)          ",")
	     ((unquote-splicing) ",@"))))

       (define (out str col)
	 (and col (output str) (+ col (string-length str))))

       (define expr-found
	 (lambda (ref col)
	   (let ([n cycle-counter])
	     (set! cycle-counter (add1 cycle-counter))
	     (set-mark-str! ref 
			    (string-append "#"
					   (number->string n)
					   "#"))
	     (set-mark-def! ref def-box)
	     (out (string-append "#"
				 (number->string n)
				 "=")
		  col))))
       
       (define check-expr-found
	 (lambda (obj check? col c-k d-k n-k)
	   (let ([ref (and check? 
			   found
			   (hash-table-get found obj (lambda () #f)))])
	     (if (and ref (unbox (mark-def ref)))
		 (if c-k
		     (c-k (mark-str ref) col)
		     (out (mark-str ref) col))
		 (if (and ref d-k)
		     (d-k col)
		     (let ([col (if ref
				    (expr-found ref col)
				    col)])
		       (n-k col)))))))

       (define (wr obj col depth)

	 (define (wr-expr expr col depth)
	   (if (read-macro? expr)
	       (wr (read-macro-body expr) (out (read-macro-prefix expr) col) depth)
	       (wr-lst expr col #t depth)))

	 (define (wr-lst l col check? depth)
	   (if (pair? l)
	       (check-expr-found 
		l check? col
		#f #f
		(lambda (col)
		  (if (and depth (zero? depth))
		      (out "(...)" col)
		      (let loop ((l (cdr l)) (col (wr (car l) (out "(" col) (dsub1 depth))))
			(check-expr-found
			 l (and check? (pair? l)) col
			 (lambda (s col) (out ")" (out s (out " . " col))))
			 (lambda (col)
			   (out ")" (wr-lst l (out " . " col) check? (dsub1 depth))))
			 (lambda (col)
			   (and col
				(cond 
				 ((pair? l) 
				  (if (and (eq? (car l) 'unquote)
					   (pair? (cdr l))
					   (null? (cddr l)))
				      (out ")" (wr (cadr l) (out " . ," col) (dsub1 depth)))
				      (loop (cdr l) (wr (car l) (out " " col) (dsub1 depth)))))
				 ((null? l) (out ")" col))
				 (else
				  (out ")" (wr l (out " . " col) (dsub1 depth))))))))))))
	       (out "()" col)))

	 (if (and depth (negative? depth))
	     (out "..." col)

	     (cond ((size-hook obj display?)
		                       => (lambda (len)
					    (and col
						 (output-hooked obj len)
						 (+ len col))))

		   ((pair? obj)        (wr-expr obj col depth))
		   ((null? obj)        (wr-lst obj col #f depth))
		   ((vector? obj)      (check-expr-found
					obj #t col
					#f #f
					(lambda (col)
					  (wr-lst (vector->list obj) 
						  (out "#" col) #f depth))))
		   ((box? obj)         (check-expr-found
					obj #t col
					#f #f
					(lambda (col)
					  (wr (unbox obj) (out "#&" col) 
					      (dsub1 depth)))))
		   ((struct? obj)      (if (and print-struct?
						(not (and depth
							  (zero? depth))))
					   (check-expr-found
					    obj #t col
					    #f #f
					    (lambda (col)
					      (wr-lst (vector->list 
						       (struct->vector obj))
						      (out "#" col) #f
						      depth)))
					   (out
					    (let ([p (open-output-string)]
						  [p-s (print-struct)])
					      (when p-s
						    (print-struct #f))
					      ((if display? display write) obj p)
					      (when p-s
						    (print-struct p-s))
					      (get-output-string p))
					    col)))

		   ((boolean? obj)     (out (if obj "#t" "#f") col))
		   ((number? obj)
		    (when (and show-inexactness?
			       (inexact? obj))
		      (out "#i" col))
		    (out (number->string obj) col))
		   ; Let symbol get printed by default case to get proper quoting
		   ; ((symbol? obj)      (out (symbol->string obj) col))
		   ((string? obj)      (if display?
					   (out obj col)
					   (let loop ((i 0) (j 0) (col (out "\"" col)))
					     (if (and col (< j (string-length obj)))
						 (let ((c (string-ref obj j)))
						   (if (or (char=? c #\\)
							   (char=? c #\"))
						       (loop j
							     (+ j 1)
							     (out "\\"
								  (out (substring obj i j)
								       col)))
						       (loop i (+ j 1) col)))
						 (out "\""
						      (out (substring obj i j) col))))))
		   ((char? obj)        (if display?
					   (out (make-string 1 obj) col)
					   (out (case obj
						  ((#\space)    "space")
						  ((#\newline)  "newline")
						  ((#\linefeed) "linefeed")
						  ((#\return)   "return")
						  ((#\rubout)   "rubout")
						  ((#\backspace)"backspace")
						  ((#\nul)      "nul")
						  ((#\page)     "page")
						  ((#\tab)      "tab")
						  ((#\vtab)      "vtab")
						  ((#\newline)  "newline")
						  (else        (make-string 1 obj)))
						(out "#\\" col))))

		   (else (out (let ([p (open-output-string)])
				((if display? display write) obj p)
				(get-output-string p))
			      col)))))

       (define (pp obj col depth)

	 (define (spaces n col)
	   (if (> n 0)
	       (if (> n 7)
		   (spaces (- n 8) (out "        " col))
		   (out (substring "        " 0 n) col))
	       col))

	 (define (indent to col)
	   (and col
		(if (< to col)
		    (and col
			 (begin 
			   (set! line-number (add1 line-number))
			   (let ([col (print-line line-number col)])
			     (spaces (- to col) col))))
		    (spaces (- to col) col))))

	 (define (pr obj col extra pp-pair depth)
	   ; may have to split on multiple lines
	   (let* ([can-multi (or (pair? obj) (vector? obj) 
				 (box? obj) (and (struct? obj) print-struct?))]
		  [ref (if can-multi
			   (and found (hash-table-get found obj (lambda () #f)))
			   #f)])
	     (if (and can-multi
		      (or (not ref) (not (unbox (mark-def ref)))))
		 (let* ((result '())
			(result-tail #f)
			(new-def-box (box #t))
			(left (+ (- (- width col) extra) 1))
			(snoc (lambda (s len)
				(let ([v (cons s null)])
				  (if result-tail
				      (set-cdr! result-tail v)
				      (set! result v))
				  (set! result-tail v))
				(set! left (- left len))
				(> left 0))))
		   (generic-write obj display? #f
				  (lambda (s)
				    (snoc s (string-length s)))
				  (lambda (s l)
				    (snoc (cons s l) l))
				  depth
				  new-def-box
				  0)
		   (if (> left 0) ; all can be printed on one line
		       (let loop ([result result][col col])
			 (if (null? result) 
			     col
			     (loop (cdr result)
				   (+ (let ([v (car result)])
					(if (pair? v)
					    (begin 
					      (output-hooked (car v) (cdr v))
					      (+ col (cdr v)))
					    (out (car result) col)))))))
		       (begin
			 (set-box! new-def-box #f)
			 (let ([col
				(if ref
				    (expr-found ref col)
				    col)])
			   (cond
			    [(pair? obj) (pp-pair obj col extra depth)]
			    [(vector? obj)
			     (pp-list (vector->list obj) 
				      (out "#" col) extra pp-expr #f depth)]
			    [(struct? obj)
			     (pp-list (vector->list (struct->vector obj))
				      (out "#" col) extra pp-expr #f depth)]
			    [(box? obj)
			     (pr (unbox obj) (out "#&" col) extra pp-pair depth)])))))
		 (wr obj col depth))))

	 (define (pp-expr expr col extra depth)
	   (if (read-macro? expr)
	       (pr (read-macro-body expr)
		   (out (read-macro-prefix expr) col)
		   extra
		   pp-expr
		   depth)
	       (let ((head (car expr)))
		 (if (symbol? head)
		     (let ((proc (style head)))
		       (if proc
			   (proc expr col extra depth)
			   (if (> (string-length (symbol->string head))
				  max-call-head-width)
			       (pp-general expr col extra #f #f #f pp-expr depth)
			       (pp-call expr col extra pp-expr depth))))
		     (pp-list expr col extra pp-expr #t depth)))))

	 ; (head item1
	 ;       item2
	 ;       item3)
	 (define (pp-call expr col extra pp-item depth)
	   (let ((col* (wr (car expr) (out "(" col) (dsub1 depth))))
	     (and col
		  (pp-down (cdr expr) col* (+ col* 1) extra pp-item #t #t depth))))

	 ; (head item1 item2
	 ;   item3
	 ;   item4)
	 (define (pp-two-up expr col extra pp-item depth)
	   (let ((col* (wr (car expr) (out "(" col) (dsub1 depth)))
		 (col*2 (wr (cadr expr) (out " " col) (dsub1 depth))))
	     (and col
		  (pp-down (cddr expr) (+ col 1) (+ col 2) extra pp-item #t #t depth))))

	 ; (item1
	 ;  item2
	 ;  item3)
	 (define (pp-list l col extra pp-item check? depth)
	   (let ((col (out "(" col)))
	     (pp-down l col col extra pp-item #f check? depth)))

	 (define (pp-down l col1 col2 extra pp-item check-first? check-rest? depth)
	   (let loop ((l l) (col col1) (check? check-first?))
	     (and col
		  (check-expr-found
		   l (and check? (pair? l)) col
		   (lambda (s col) 
		     (out ")" (out s (indent col2 (out "." (indent col2 col))))))
		   (lambda (col)
		     (out ")" (pr l (indent col2 (out "." (indent col2 col)))
				  extra pp-item depth)))
		   (lambda (col)
		     (cond ((pair? l)
			    (let ((rest (cdr l)))
			      (let ((extra (if (null? rest) (+ extra 1) 0)))
				(loop rest
				      (pr (car l) (indent col2 col)
					  extra pp-item
					  (dsub1 depth))
				      check-rest?))))
			   ((null? l)
			    (out ")" col))
			   (else
			    (out ")"
				 (pr l
				     (indent col2 (out "." (indent col2 col)))
				     (+ extra 1)
				     pp-item
				     (dsub1 depth))))))))))

	 (define (pp-general expr col extra named? pp-1 pp-2 pp-3 depth)

	   (define (tail1 rest col1 col2 col3)
	     (if (and pp-1 (pair? rest))
		 (let* ((val1 (car rest))
			(rest (cdr rest))
			(extra (if (null? rest) (+ extra 1) 0)))
		   (tail2 rest col1 (pr val1 (indent col3 col2) extra pp-1 depth) col3))
		 (tail2 rest col1 col2 col3)))

	   (define (tail2 rest col1 col2 col3)
	     (if (and pp-2 (pair? rest))
		 (let* ((val1 (car rest))
			(rest (cdr rest))
			(extra (if (null? rest) (+ extra 1) 0)))
		   (tail3 rest col1 (pr val1 (indent col3 col2) extra pp-2 depth)))
		 (tail3 rest col1 col2)))

	   (define (tail3 rest col1 col2)
	     (pp-down rest col2 col1 extra pp-3 #f #t depth))

	   (let* ((head (car expr))
		  (rest (cdr expr))
		  (col* (wr head (out "(" col) (dsub1 depth))))
	     (if (and named? (pair? rest))
		 (let* ((name (car rest))
			(rest (cdr rest))
			(col** (wr name (out " " col*) (dsub1 depth))))
		   (tail1 rest (+ col indent-general) col** (+ col** 1)))
		 (tail1 rest (+ col indent-general) col* (+ col* 1)))))

	 (define (pp-expr-list l col extra depth)
	   (pp-list l col extra pp-expr #t depth))

	 (define (pp-lambda expr col extra depth)
	   (pp-general expr col extra #f pp-expr-list #f pp-expr depth))

	 (define (pp-if expr col extra depth)
	   (pp-general expr col extra #f pp-expr #f pp-expr depth))

	 (define (pp-cond expr col extra depth)
	   (pp-call expr col extra pp-expr-list depth))

	 (define (pp-class expr col extra depth)
	   (pp-two-up expr col extra pp-expr-list depth))

	 (define (pp-case expr col extra depth)
	   (pp-general expr col extra #f pp-expr #f pp-expr-list depth))

	 (define (pp-and expr col extra depth)
	   (pp-call expr col extra pp-expr depth))

	 (define (pp-let expr col extra depth)
	   (let* ((rest (cdr expr))
		  (named? (and (pair? rest) (symbol? (car rest)))))
	     (pp-general expr col extra named? pp-expr-list #f pp-expr depth)))

	 (define (pp-begin expr col extra depth)
	   (pp-general expr col extra #f #f #f pp-expr depth))

	 (define (pp-do expr col extra depth)
	   (pp-general expr col extra #f pp-expr-list pp-expr-list pp-expr depth))

	 ; define formatting style (change these to suit your style)

	 (define indent-general 2)

	 (define max-call-head-width 5)

	 (define (style head)
	   (case head
	     ((lambda let* letrec define shared
		      unless #%unless
		      when #%when
		      '|$\spadesuit$|
		      #%lambda #%let* #%letrec #%define
		      define-macro #%define-macro)
	      pp-lambda)
	     ((if set! #%if #%set!)
	      pp-if)
	     ((cond #%cond public private import export)
	      pp-cond)
	     ((case #%case) 
	      pp-case)
	     ((and or #%and #%or)
	      pp-and)
	     ((let #%let)
	      pp-let)
	     ((begin #%begin)
	      pp-begin)
	     ((do #%do)
	      pp-do)

	     ((class #%class) pp-class)

	     (else #f)))

	 (pr obj col 0 pp-expr depth))

       (if (and width (not (eq? width 'infinity)))
	   (pp obj startpos depth)
	   (wr obj startpos depth)))))

   (define pretty-print-handler
     (lambda (v)
       (unless (void? v)
	       (pretty-print v))))
   )