  (unit/sig
      mzlib:print-convert^
    (import (s : mzlib:string^)
	    (f : mzlib:function^)
	    (hooks@ : mzlib:print-convert-hooks^))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; the value stored in the hash table.  Contains the name
    ;; <which is a number unless we are in donkey and it already has a name>
    ;; and whether or not it is shared in the expr.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-struct share-info (name shared?))

    (define show-sharing (make-parameter #t))
    (define constructor-style-printing (make-parameter #f))
    (define quasi-read-style-printing (make-parameter #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; share-hash is the hash-table containing info on what cons cells
    ;; of the expression are shared.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; sometimes you want to go ahead and start displaying a shared
    ;; expression rather than just showing its name.  For instance, in
    ;; the shared list, you want (shared ((-1- (list 1 2))... not
    ;; (shared ((-1- -1-) ...
    ;; expand-shared? controls this
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define-struct convert-share-info (share-hash expand-shared?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; builds the hash table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define build-share
      (lambda (expr)
	(letrec*
	    ([share-cnt 0]
	     [share-hash (make-hash-table)]
	     [csi (make-convert-share-info share-hash #f)]
	     [hash
	      (lambda (obj)
		(let ([name (hooks@:build-share-name-hook obj)])
		  (hash-table-put! share-hash obj
				   (make-share-info (if name (car name) share-cnt) #f)))
		(set! share-cnt (add1 share-cnt)))]
	     [build-sub
	      (lambda (expr)
		(let/ec k
		  (let ([val (hash-table-get share-hash expr 
					     (lambda () (hash expr) (k #f)))])
		    (when val
		      (set-share-info-shared?! val #t))
		    val)))]
	     [build
	      (lambda (expr)
		(cond
		  [(or (procedure? expr) (regexp? expr) (promise? expr) (string? expr)
		       (class? expr) (object? expr))
		   (build-sub expr)]
		  [(box? expr) (unless (build-sub expr)
				 (build (unbox expr)))]
		  [(pair? expr) (unless (build-sub expr)
				  (build (car expr))
				  (build (cdr expr)))]
		  [(vector? expr) (unless (build-sub expr)
				    (for-each build (vector->list expr)))]
		  [(struct? expr) (unless (build-sub expr)
				    (for-each build (vector->list (struct->vector expr))))]
		  [else (hooks@:build-share-hook expr build)]))])
	  (build expr)
	  csi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; creates a distinctive symbol out of a name (usually just a number)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define map-share-name
      (lambda (name)
	(string->symbol
	 (string-append "-" (s:expr->string name) "-"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; prints an expression given that it has already been hashed. This
    ;; does not include the list of shared items.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define print-convert-expr
      (lambda (csi expr unroll-once?)
	(letrec*
	    ([share-hash (convert-share-info-share-hash csi)]
	     [find-hash
	      (lambda (expr)
		(hash-table-get share-hash expr (lambda () #f)))]
	     [shared?
	      (lambda (expr)
		(let* ([info (find-hash expr)]
		       [ans (and info
				 (share-info-shared? info))])
		  ans))]
	     [make-list
	      (lambda (f n)
		(letrec ([helper
			  (lambda (n l)
			    (cond [(zero? n) l]
				  [else (helper (sub1 n) (cons (f n) l))]))])
		  (helper n null)))]
	     [make-lambda-helper
	      (lambda (arity)
		(cond
		  [(arity-at-least? arity)
		   (let ([v (arity-at-least-value arity)])
		     (if (zero? v)
			 'args
			 (append (make-lambda-helper v) 'args)))]
		  [(list? arity)
		   (map (lambda (x)
			  (list (make-lambda-helper x) '...))
			arity)]
		  [else (make-list
			 (lambda (x)
			   (string->symbol
			    (string-append "a" (number->string x))))
			 arity)]))]
	     [use-quasi-quote? (not (constructor-style-printing))]
	     [use-read-syntax (quasi-read-style-printing)]
	     [doesnt-contain-shared-conses
	      (lambda (input-expr)
		(letrec ([doesnt-contain-shared-conses
			  (lambda (expr)
			    (cond
			      [(and (pair? expr)
				    (shared? expr))
			       #f]
			      [(pair? expr)
			       (doesnt-contain-shared-conses (cdr expr))]
			      [else #t]))])
		  (let ([answer (doesnt-contain-shared-conses input-expr)])
		    answer)))]
	     [print
	      (lambda (in-quasiquote? first-time)
		(lambda (expr)
		  (letrec*
		      ([lookup (find-hash expr)]
		       [recur (print in-quasiquote? #f)]
		       [self-quoting?
			(lambda (expr)
			  (or (number? expr)
			      (and (symbol? expr)
				   (not (eq? expr 'quasiquote))
				   (not (eq? expr 'quote))
				   (not (eq? expr 'unquote)))
			      (char? expr)
			      (string? expr)
			      (not expr)
			      (eq? #t expr)))]
		       [quasi-read-style
			(lambda ()
			  (cond
			    [(or (void? expr) (regexp? expr)
				 (object? expr) (class? expr))
			     expr]
			    [(box? expr) (box (recur (unbox expr)))]
			    [(vector? expr) (apply vector (map recur (vector->list expr)))]
			    [else (quasi-style)]))]
		       [quasi-style
			(lambda ()
			  (cond
			    [(null? expr) '()]
			    [(and (list? expr)
				  (doesnt-contain-shared-conses expr))
			     (map recur expr)]
			    [(pair? expr) 
			     (cons (recur (car expr)) (recur (cdr expr)))]
			    [(self-quoting? expr) expr]
			    [else `(,'unquote ,((print #f first-time) expr))]))]
		       [guard
			(lambda (f)
			  (cond
			    [use-quasi-quote?
			     `(,'quasiquote ,(if use-read-syntax
						 ((print #t first-time) expr)
						 ((print #t first-time) expr)))]
			    [else
			     (f)]))]
		       [constructor-style
			(let* ([get-class
				(lambda (n actual)
				  (lambda (expr)
				    (let* ([str (format "~a" expr)])
				      (and (not (string=? str actual))
					   (let* ([sub (substring
							str n
							(- (string-length str) 1))]
						  [symbol (string->symbol sub)])
					     symbol)))))]
			       [get-class-from-class (get-class 8 "#<class>")]
			       [get-class-from-object (get-class 9 "#<object>")]
			       [build-class-expr
				(lambda (symbol actual-class)
				  (if symbol
				      (let ([matches?
					     (with-handlers ([void (lambda (x) #f)])
					       (eq? (eval symbol) actual-class))])
					(if matches?
					    symbol
					    `(class ,symbol ...)))
				      '(class ...)))]
			       [build-named
				(lambda (build-unnamed string-name beginning-offset)
				  (let ([answer (inferred-name expr)])
				    (if answer
					(if (eq? (with-handlers ([(lambda (x) #t)
								  (lambda (x) #f)])
						   (global-defined-value answer))
						 expr)
					    answer
					    (build-unnamed))
					(build-unnamed))))])
			  (lambda ()
			    (cond
			      [(hooks@:before-test? expr) (hooks@:before-convert expr recur)]
			      [(null? expr) (guard (lambda () 'null))]
			      [(and (list? expr)
				    (or (and first-time
					     (doesnt-contain-shared-conses (cdr expr)))
					(doesnt-contain-shared-conses expr)))
			       (guard (lambda ()
					`(list ,@(map recur expr))))]
			      [(pair? expr)
			       (guard
				(lambda ()
				  `(cons ,(recur (car expr)) ,(recur (cdr expr)))))]
			      [(weak-box? expr) `(make-weak-box ,(recur (weak-box-value expr)))]
			      [(box? expr) `(box ,(recur (unbox expr)))]
			      [(vector? expr) `(vector ,@(map recur (vector->list expr)))]
			      [(symbol? expr) `(quote ,expr)]
			      [(string? expr) expr]
			      [(primitive? expr) (string->symbol (primitive-name expr))]
			      [(procedure? expr)
			       (build-named (lambda ()
					      (let ([arity (arity expr)])
						(if (list? arity)
						    (cons 'case-lambda (make-lambda-helper arity))
						    (list 'lambda (make-lambda-helper arity) '...))))
					    "#<procedure>"
					    12)]
			      [(regexp? expr)
			       '(regexp ...)]
			      [(class? expr) 
			       (build-named (lambda () (build-class-expr (get-class-from-class expr) expr))
					    "#<class>"
					    7)]
			      [(object? expr) `(make-object ,(build-class-expr (get-class-from-object expr)
									       (object-class expr))
							    ...)]
			      [(void? expr) '(void)]
			      [(promise? expr) '(delay ...)]
			      [(struct? expr)
			       (let ([name (symbol->string
					    (vector-ref (struct->vector expr) 0))])
				 (cons (string->symbol
					(string-append
					 "make-" (substring name
							    (string-length "struct:")
							    (string-length name))))
				       (map recur (cdr (vector->list
							(struct->vector expr))))))]
			      [(unit? expr) (build-named (lambda () 
							   '(unit ...))
							 "#<unit>"
							 6)]
			      [else (hooks@:print-convert-hook
				     expr recur)])))])
		    (let ([es (convert-share-info-expand-shared? csi)])
		      (set-convert-share-info-expand-shared?! csi #f)
		      (if (and lookup
			       (not es)
			       (not first-time)
			       (share-info-shared? lookup))
			  (let ([name (map-share-name (share-info-name lookup))])
			    (if in-quasiquote?
				`(,'unquote ,name)
				name))
			  (if in-quasiquote?
			      (if use-read-syntax
				  (quasi-read-style)
				  (quasi-style))
			      (constructor-style)))))))])
	  ((print #f unroll-once?) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; these functions get the list of shared items.  If just-circular is
    ;; true, then it will modify the hash table so that the only shared
    ;; items are those that are circular.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define get-shared-helper
      (lambda (csi)
	(let ([shared '()]
	      [share-hash (convert-share-info-share-hash csi)])
	  (hash-table-for-each share-hash
			       (lambda (key val)
				 (when (share-info-shared? val)
				   (set! shared (cons (list key val) shared)))))
	  (map (lambda (s)
		 (set-convert-share-info-expand-shared?! csi #t)
		 (let* ([info (cadr s)]
			[name (share-info-name info)])
		   (list info
			 (map-share-name name)
			 (print-convert-expr csi (car s) #t))))
	       shared))))

    (define get-shared
      (case-lambda
       [(csi) (get-shared csi #f)]
       [(csi just-circular)
	(let ([shared-listss
	       (if just-circular
		   (let ([shared (get-shared-helper csi)])
		     (for-each (lambda (x)
				 (unless (member* (cadr x) (caddr x))
				   (set-share-info-shared?! (car x) #f)))
			       shared)
		     (get-shared-helper csi))
		   (get-shared-helper csi))]
	      [cmp 
	       (lambda (x y)
		 (string<? (s:expr->string (share-info-name (car x)))
			   (s:expr->string (share-info-name (car y)))))])
	  (map cdr (f:quicksort shared-listss cmp)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; helper function for determining if an item is circular.  In the
    ;; shared list: (shared ((-1- (list 1 2)) (-2- (list -2- 2 3)))), you
    ;; can tell by doing a member* of the first item on the second. In this
    ;; case, the second item in the shared list is circular because -2- appears
    ;; in the value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define member*
      (lambda (a l)
	(cond [(or (not (pair? l)) (null? l)) #f]
	      [(eq? a (car l)) #t]
	      [else (or (member* a (car l)) (member* a (cdr l)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; takes an expression and completely converts it to show sharing
    ;; (or if just-circular, just circularity) and special forms.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define print-convert
      (case-lambda
       [(expr) (print-convert expr (not (show-sharing)))]
       [(expr just-circ)
	(let* ([csi (build-share expr)])
	  (let ([shared (get-shared csi just-circ)]
		[body (print-convert-expr csi expr #f)])
	    (if (null? shared)
		body
		`(shared ,shared ,body))))]))

    (define current-read-eval-convert-print-prompt
      (let ([p "|- "])
	(case-lambda
	 [() p]
	 [(n) (set! p n)])))

    (define install-converting-printer
      (lambda ()
	(let ([print (current-print)])
	  (current-print (lambda (v)
			   (unless (void? v)
			     (print (print-convert v))))))
	(current-prompt-read (lambda ()
			       (display (current-read-eval-convert-print-prompt))
			       (read))))))

;; TEST SUITE MOVED to mzscheme command test suite area.
;; At Rice: ~mflatt/proj/mred/mzscheme/tests/pconvert.ss
