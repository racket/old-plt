
(define cmd-line (vector->list argv))

(define cpp (car cmd-line))
(define file-in (cadr cmd-line))
(define file-out (caddr cmd-line))

; (require-library "errortrace.ss" "errortrace")

(unless (system (format "~a -DMZ_PRECISE_GC ~a ~a | ctok > xtmp"
			cpp
			(if (null? (cdddr cmd-line))
			    ""
			    (cadddr cmd-line))
			file-in))
  (error 'xform "cpp failed"))

(define e-raw (parameterize ([read-case-sensitive #t])
		(with-input-from-file "xtmp" read)))

(current-output-port (open-output-file file-out 'truncate))
(let ([eh (error-escape-handler)])
  (error-escape-handler
   (lambda ()
     (close-output-port (current-output-port))
     (current-output-port (current-error-port))
     (delete-file file-out)
     (eh))))

;; Header:
(printf "#define FUNCCALL(x) x~n")
(printf "#define FUNCCALL_EMPTY(x) x~n")
(printf "#define PREPARE_VAR_STACK(size) void *__gc_var_stack__[size+2]; __gc_var_stack__[0] = GC_variable_stack; __gc_var_stack__[1] = (void *)GC_variable_count;~n")
(printf "#define SETUP(x) (GC_variable_stack = __gc_var_stack__, GC_variable_count = x)~n")
(printf "#define PUSH(v, x) (__gc_var_stack__[x+2] = (void *)&(v))~n")
(printf "#define PUSHARRAY(v, l, x) (__gc_var_stack__[x+2] = (void *)0, __gc_var_stack__[x+3] = (void *)&(v), __gc_var_stack__[x+4] = (void *)l)~n")

(define-struct tok (n line col file))
(define-struct (seq struct:tok) (close in))
(define-struct (parens struct:seq) ())
(define-struct (brackets struct:seq) ())
(define-struct (braces struct:seq) ())
(define-struct (call struct:tok) (func args live))
(define-struct (note struct:tok) (s))

(define-struct vtype ())
(define-struct (array-type struct:vtype) (count))
(define-struct (struct-type struct:vtype) (struct))
(define-struct (struct-array-type struct:struct-type) (count))
(define-struct (union-type struct:vtype) ())

(define-struct live-var-info (maxlive vars))

(define e (let ([source #f])
	    (letrec ([translate
		      (lambda (v)
			(when (cadr v)
			  (set! source (cadr v)))
			(if (pair? (car v))
			    (let ([body (map translate (cddddr v))])
			      ((cond
				[(string=? "(" (caar v)) make-parens]
				[(string=? "[" (caar v)) make-brackets]
				[(string=? "{" (caar v)) make-braces])
			       (caar v) (caddr v) (cadddr v)
			       source
			       (cond
				[(string=? "(" (caar v)) ")"]
				[(string=? "[" (caar v)) "]"]
				[(string=? "{" (caar v)) "}"])
			       body))
			    (make-tok (car v) (caddr v) (cadddr v) source)))])
	      (map translate e-raw))))

(define label? #t)

(define semi '|;|)

(define (get-constructor v)
  (cond
   [(parens? v) make-parens]
   [(brackets? v) make-brackets]
   [(braces? v) make-braces]))

(define (get-variable-size vtype)
  (cond
   [(array-type? vtype)
    3]
   [(struct-type? vtype)
    (let ([size (let ([m (assq (struct-type-struct vtype) struct-defs)])
		  (apply + (map get-variable-size
				(cdr m))))])
      (if (struct-array-type? vtype)
	  (* size (struct-array-type-count vtype))
	  size))]
   [else 1]))

(define next-indent #f)

(define (newline/indent i)
  (newline)
  (set! next-indent i))

(define (display/indent v s)
  (when next-indent
    ;; can't get pre-processor line directive to work
    '(when (and v (tok-file v) (tok-line v))
      (printf "# ~a ~s~n" (max 1 (- (tok-line v) 1)) (tok-file v)))
    (display (make-string next-indent #\space))
    (set! next-indent #f))
  (display s))

(define re:quote-or-backslash (regexp "[\\\"]"))

(define (print-it e indent semi-newlines?)
  (let loop ([e e][prev #f])
    (unless (null? e)
      (let ([v (car e)])
	(cond
	 [(seq? v)
	  (display/indent v (tok-n v))
	  (let ([subindent (if (braces? v)
			       (begin
				 (newline/indent (+ indent 2))
				 (+ indent 2))
			       indent)])
	    (print-it (seq-in v) subindent
		      (not (and (parens? v)
				prev
				(memq (tok-n prev) '(for)))))
	    (when (and next-indent (= next-indent subindent))
	      (set! next-indent indent)))
	  (display/indent #f (seq-close v))
	  (cond
	   [(braces? v)
	    (newline/indent indent)]
	   [(brackets? v)
	    (display/indent v " ")]
	   [(parens? v)
	    (if (and prev 
		     (memq (tok-n prev) '(if))
		     (or (null? (cdr e))
			 (not (braces? (cadr e)))))
		(newline/indent (+ indent 2))
		(display/indent v " "))]
	   [else (error "unknown brace: ~a" (caar v))])]
	 [(note? v)
	  (display/indent v (note-s v))
	  (newline/indent indent)]
	 [(call? v)
	  (if (null? (call-live v))
	      (display/indent v "FUNCCALL_EMPTY((")
	      (begin
		(display/indent v (format "FUNCCALL((SETUP(~a), " (length (call-live v))))
		(let loop ([l (call-live v)][n 0])
		  (unless (null? l)
		    (loop (cdr l)
			  (let push-var ([full-name (caar l)][vtype (cdar l)][n n])
			    (cond
			     [(union-type? vtype)
			      (error 'xform "Hoffa lives: can't push union ~a." full-name)]
			     [(array-type? vtype)
			      (printf "PUSHARRAY(~a, ~a, ~a), " full-name (array-type-count vtype) n)
			      (+ 3 n)]
			     [(struct-type? vtype)
			      (let aloop ([array-index 0][n n])
				;; Push each struct in array (or only struct if not an array)
				(let loop ([n n][l (cdr (assq (struct-type-struct vtype) struct-defs))])
				  (if (null? l)
				      (if (and (struct-array-type? vtype)
					       (< (add1 array-index) (struct-array-type-count vtype)))
					  ;; Next in array
					  (aloop (add1 array-index) n)
					  ;; All done
					  n)
				      (loop (push-var (format "~a~a.~a"
							      full-name 
							      (if (struct-array-type? vtype)
								  (format "[~a]" array-index)
								  "")
							      (caar l))
						      (cdar l)
						      n)
					    (cdr l)))))]
			     [else
			      (printf "PUSH(~a, ~a), " full-name n)
			      (+ n 1)])))))))
	  (print-it (append (call-func v) (list (call-args v))) indent #f)
	  (display/indent v "))")]
	 [else
	  (if (string? (tok-n v))
	      (begin
		(display/indent v "\"")
		(display (tok-n v))
		(display/indent v "\""))
	      (display/indent v (tok-n v)))
	  (display/indent v " ")
	  (when (and (eq? semi (tok-n v))
		     semi-newlines?)
	    (newline/indent indent))])
	(loop (cdr e) v)))))

(define (top-level e)
  (cond
   [(prototype? e) 
    (when label? (printf "/* PROTO */~n"))
    e]
   [(typedef? e)
    (when label? (printf "/* TYPEDEF */~n"))
    (check-pointer-type e)
    e]
   [(struct? e)
    (if (braces? (caddr e))
	(begin
	  (register-struct e)
	  (when label? (printf "/* STRUCT ~a */~n" (tok-n (cadr e)))))
	(when label? (printf "/* STRUCT DECL */~n")))
    e]
   [(function? e)
    (when label? (printf "/* FUNCTION */~n"))
    (convert-function e)]
   [(var-decl? e)
    (when label? (printf "/* VAR */~n"))
    e]
   [else (error 'xform "unknown form: ~s" e)]))

(define (prototype? e)
  (let ([l (length e)])
    (and (> l 2)
	 (eq? semi (tok-n (list-ref e (sub1 l))))
	 (let ([v (list-ref e (- l 2))])
	   (and (parens? v))))))

(define (typedef? e)
  (eq? 'typedef (tok-n (car e))))

(define (struct? e)
  (memq (tok-n (car e)) '(struct enum)))

(define (function? e)
  (let ([l (length e)])
    (and (> l 2)
	 (let* ([_n (tok-n (list-ref e (sub1 l)))]
		[ll (if (eq? _n semi)
			(- l 2)
			(sub1 l))])
	   (let ([v (list-ref e ll)])
	     (and (braces? v)
		  (let ([v (list-ref e (sub1 ll))])
		    (and (parens? v)))))))))

(define (var-decl? e)
  (let ([l (length e)])
    (and (> l 2)
	 (eq? semi (tok-n (list-ref e (sub1 l)))))))

(define pointer-types '())
(define struct-defs '())

(define (check-pointer-type e)
  (let ([vars (get-pointer-vars (cdr e) "PTRDEF" #t)])
    (set! pointer-types (append vars pointer-types))))

(define (get-pointer-vars e comment union-ok?)
  (let* ([base (tok-n (car e))]
	 [base-is-ptr?
	  (assq base pointer-types)]
	 [base-struct
	  (and (eq? base 'struct)
	       (if (or (braces? (cadr e)) (braces? (caddr e)))
		   (register-struct e)
		   (let ([m (assq (tok-n (cadr e)) struct-defs)])
		     (and m (car m)))))]
	 [minpos (if (or (eq? base 'struct)
			 (eq? base 'union))
		     1
		     0)])
    (let loop ([l (- (length e) 2)][array-size #f][results null])
      (if (<= l minpos)
	  results
	  ;; Look back for "=" before comma:
	  (let ([skip (let loop ([l (sub1 l)])
			(cond
			 [(or (<= l minpos) 
			      (eq? '|,| (tok-n (list-ref e l))))
			  #f]
			 [(eq? '= (tok-n (list-ref e l)))
			  (sub1 l)]
			 [else (loop (sub1 l))]))])
	    (if skip
		;; Skip assignment RHS:
		(loop skip #f results)
		;; Not assignment RHS:
		(let ([v (list-ref e l)])
		  (cond
		   [(seq? v)
		    ;; Array? Struct?
		    (cond
		     [(brackets? v)
		      ;; Array decl:
		      (loop (sub1 l)
			    (let ([inner (seq-in (list-ref e l))])
			      (if (null? inner)
				  'pointer
				  (tok-n (car inner))))
			    results)]
		     [(braces? v)
		      ;; No more variable declarations
		      results]
		     [else
		      ;; End of function ptr
		      ;; (and we don't care about func ptrs)
		      results])]
		   [(memq (tok-n v) '(|,| *))
		    (loop (sub1 l) #f results)]
		   [else (let* ([name (tok-n v)]
				[pointer? (or (eq? 'pointer array-size)
					      (eq? '* (tok-n (list-ref e (sub1 l)))))]
				[base-struct (or base-struct
						 (and base-is-ptr?
						      (struct-type? (cdr base-is-ptr?))
						      (struct-type-struct (cdr base-is-ptr?))))]
				[union? (eq? base 'union)]
				[struct-array? (and base-struct (not pointer?) (number? array-size))])
			   (when (and struct-array?
				      (> array-size 5))
			     (error 'xform "Large array of structures? Gimme a break. ~a in line ~a."
				    name (tok-line v)))
			   (when (and (not union-ok?)
				      (not pointer?)
				      (or union?
					  (and base-struct
					       (let has-union? ([base base-struct])
						 (let ([v (cdr (assq base struct-defs))])
						   (ormap
						    (lambda (v)
						      (or (union-type? v)
							  (and (struct-type? v)
							       (has-union? (struct-type-struct v)))))
						    v))))))
			     (fprintf (current-error-port)
				      "Warning: can't handle union or record with union. ~a in line ~a.~n"
				      name (tok-line v)))
			   (if (or pointer?
				   base-is-ptr?
				   base-struct
				   union?)
			       (begin
				 (when label?
				   (printf "/* ~a: ~a ~a*/~n" 
					   comment name
					   (cond
					    [struct-array?
					     (format "struct ~a[~a] " base-struct array-size)]
					    [(number? array-size)
					     (format "[~a] " array-size)]
					    [(and base-struct (not pointer?))
					     (format "struct ~a " base-struct)]
					    [(and union? (not pointer?)) "union "]
					    [else
					     ""])))
				 (loop (sub1 l) #f 
				       (cons (cons name
						   (cond
						    [struct-array?
						     (make-struct-array-type base-struct array-size)]
						    [(number? array-size)
						     (make-array-type array-size)]
						    [pointer? (make-vtype)]
						    [base-struct
						     (make-struct-type base-struct)]
						    [union?
						     (make-union-type)]
						    [else
						     (make-vtype)]))
					     results)))
			       (loop (sub1 l) #f results)))]))))))))

(define (register-struct e)
  (let ([body (seq-in (if (braces? (cadr e))
			  (cadr e)
			  (caddr e)))]
	[name (if (braces? (cadr e))
		  (gensym 'Anonymous)
		  (tok-n (cadr e)))])
    (let ([l (let ([el (body->lines body #f)])
	       (apply
		append
		(map (lambda (e)
		       (get-pointer-vars e "PTRFIELD" #t))
		     el)))])
      (and (not (null? l))
	   (begin
	     (set! struct-defs (cons (cons name l) struct-defs))
	     name)))))

(define (body->lines e comma-sep?)
  (reverse!
   (foldl-statement
    e
    comma-sep?
    (lambda (sube l)
      (cons sube l))
    null)))

(define (convert-function e)
  (let*-values ([(body-v len) (let* ([len (sub1 (length e))]
				     [v (list-ref e len)])
				;; Function may have trailing semicolon:
				(if (eq? semi (tok-n v))
				    (values (list-ref e (sub1 len)) (sub1 len))
				    (values v len)))]
		[(body-e) (seq-in body-v)]
		[(args-e) (seq-in (list-ref e (sub1 len)))]
		[(arg-vars) (let ([arg-decls (body->lines
					      (append
					       (map 
						(lambda (v) (if (eq? '|,| (tok-n v))
								(make-tok semi (tok-line v) (tok-col v) (tok-file v))
								v))
						args-e)
					       (list (make-tok semi #f #f #f)))
					      #f)])
			      (apply
			       append
			       (map (lambda (x) (get-pointer-vars x "PTRARG" #f)) arg-decls)))])
     (append
      (let loop ([e e][len len])
	(if (zero? len)
	    null
	    (cons (car e) (loop (cdr e) (sub1 len)))))
      (list
       (make-braces
	(tok-n body-v)
	(tok-line body-v)
	(tok-col body-v)
	(tok-file body-v)
	(seq-close body-v)
	(let-values ([(body-e live-vars)
		      (convert-body body-e arg-vars (make-live-var-info 0 null) #t)])
	  body-e))))))

(define (convert-body body-e extra-vars live-vars setup-stack?)
  (let ([el (body->lines body-e #f)])
      (let-values ([(decls body) (split-decls el)])
	(let* ([local-vars 
		(apply
		 append
		 (map (lambda (x) (get-pointer-vars x "PTRLOCAL" #f)) decls))]
	       [vars (begin
		       (ormap (lambda (var)
				(when (assq var extra-vars)
				  (error 'xform 
					 "No es bueno: pointerful variable ~a shadowed in decls at line ~a"
					 var
					 (tok-line (caar decls)))))
			      
			      local-vars)
		       (append extra-vars local-vars))])
	  ;; Convert calls and body (recusively)
	  (let-values ([(orig-maxlive) (live-var-info-maxlive live-vars)]
		       [(body-x live-vars)
			(let loop ([body body])
			  (if (null? body)
			      ;; Start with 0 maxlive in case we want to check whether anything
			      ;;  was pushed in the block
			      (values null (make-live-var-info 0 (live-var-info-vars live-vars)))
			      (let*-values ([(rest live-vars) (loop (cdr body))]
					    [(e live-vars)
					     (convert-function-calls (car body)
								     vars
								     live-vars)])
				(values (cons e rest) live-vars))))])
	    (values (apply
		     append
		     (append
		      decls
		      (list (append (if label?
					(list (make-note 'note #f #f #f (format "/* PTRVARS: ~a */" (map car vars))))
					null)
				    (if (and setup-stack? (positive? (live-var-info-maxlive live-vars)))
					(list (make-note 'note #f #f #f (format "PREPARE_VAR_STACK(~a);" 
										(live-var-info-maxlive live-vars))))
					null)))
		      body-x))
		    (make-live-var-info (max orig-maxlive
					     (live-var-info-maxlive live-vars))
					(live-var-info-vars live-vars))))))))

(define (convert-function-calls e vars live-vars)
  ;; e is a single statement
  ;; Reverse to calculate live vars as we go.
  ;; Also, it's easier to look for parens and then inspect preceeding
  ;;  to find function calls.
  (let ([e- (reverse e)])
    (let loop ([e- e-][result null][live-vars live-vars])
      (cond
       [(null? e-) (values result live-vars)]
       [(and (parens? (car e-))
	     ;; Something precedes
	     (not (null? (cdr e-)))
	     ;; Not an assignment, sizeof, if, string
	     (not (memq (tok-n (cadr e-)) '(<= < > >= == != !
					       \| \|\| & && : ? % + - * / ^ >> << 
					       = >>= <<= ^= += *= /= -= %= \|= &= ++ --
					       return sizeof if for while else switch case
					       __asm __asm__ __volatile __volatile__ __extension__
					       ;; These are functions, but they don't trigger GC:
					       strcpy strlen memcpy)))
	     (not (string? (tok-n (cadr e-))))
	     ;; Look back one more for if, etc. if preceeding is paren
	     (not (and (parens? (cadr e-))
		       (not (null? (cddr e-)))
		       (memq (tok-n (caddr e-)) '(if while for)))))
	;; Looks like a function call, although we don't know the
	;; function yet.  (The parens may be preceded by an
	;; unparenthesized expression.) And it could be a cast (which
	;; requires parens).
	(let ([pre (cadr e-)])
	  ;; Look for cast:
	  (if (and (parens? pre)
		   (let ([prel (seq-in pre)])
		     (or 
		      ;; Assume we never have (func)(args, ...)
		      (= 1 (length prel))
		      ;; trailing * is a give-away
		      (eq? '* (tok-n (list-ref prel (sub1 (length prel)))))
		      ;; leading `struct' is a giveaway:
		      (eq? 'struct (tok-n (car prel))))))
	      ;; It's a cast:
	      (let-values ([(v live-vars)
			    (convert-paren-interior (car e-) vars live-vars)])
		(loop (cddr e-)
		      (list* (cadr e-) v result)
		      live-vars))
	      ;; It's a function call; find the start
	      (let-values ([(args) (car e-)]
			   [(func rest-)
			    (let loop ([e- (cdr e-)])
			      (cond
			       [(null? (cdr e-))
				(values e- null)]
			       [(parens? (car e-))
				(values (list (car e-)) (cdr e-))]
			       [(brackets? (car e-))
				;; Array access
				(let-values ([(func rest-) (loop (cdr e-))])
				  (values (cons (car e-) func) rest-))]
			       ;; Struct reference:
			       [(memq (tok-n (cadr e-)) '(-> |.|))
				(let-values ([(func rest-) (loop (cddr e-))])
				  (values (list* (car e-) (cadr e-) func) rest-))]
			       [else (values (list (car e-)) (cdr e-))]))])
		(let*-values ([(orig-live-vars) live-vars]
			      [(args live-vars)
			       (convert-paren-interior args vars live-vars)]
			      [(func live-vars)
			       (convert-function-calls (reverse func) vars live-vars)])
		  (loop rest-
			(cons (make-call
			       "func call"
			       #f
			       #f
			       #f
			       func
			       args
			       (live-var-info-vars orig-live-vars))
			      result)
			(make-live-var-info (max (apply + (map (lambda (x)
								 (get-variable-size (cdr x)))
							       (live-var-info-vars orig-live-vars)))
						 (live-var-info-maxlive live-vars))
					    (live-var-info-vars live-vars)))))))]
       [(eq? 'goto (tok-n (car e-)))
	;; Goto - assume all vars are live
	(loop (cdr e-) (cons (car e-) result) 
	      (make-live-var-info (live-var-info-maxlive live-vars) vars))]
       [(braces? (car e-))
	(let*-values ([(v) (car e-)]
		      [(e live-vars) (convert-body (seq-in v) vars live-vars #f)])
	  (loop (cdr e-) 
		(cons (make-braces
		       (tok-n v)
		       (tok-line v)
		       (tok-col v)
		       (tok-file v)
		       (seq-close v)
		       e)
		      result)
		;; Filter live vars to drop vars no longer in scope:
		(let ([new-live-vars (let loop ([l (live-var-info-vars live-vars)])
				       (cond
					[(null? l) null]
					[(assq (caar l) vars)
					 (cons (car l) (loop (cdr l)))]
					[else (loop (cdr l))]))])
		  (make-live-var-info (live-var-info-maxlive live-vars)
				      new-live-vars))))]
       [(seq? (car e-))
	;; Do nested body:
	(let-values ([(v live-vars)
		      (convert-seq-interior (car e-) (parens? (car e-)) vars live-vars)])
	  (loop (cdr e-) (cons v result) live-vars))]
       [(and (assq (tok-n (car e-)) vars)
	     (not (assq (tok-n (car e-)) (live-var-info-vars live-vars))))
	;; Add a live variable:
	(loop (cdr e-)
	      (cons (car e-) result)
	      (make-live-var-info (live-var-info-maxlive live-vars)
				  (cons (assq (tok-n (car e-)) vars)
					(live-var-info-vars live-vars))))]
       [else (loop (cdr e-) (cons (car e-) result) live-vars)]))))

(define (convert-seq-interior v comma-sep? vars live-vars)
  (let ([e (seq-in v)])
    (let ([el (body->lines e comma-sep?)])
      (let-values ([(el live-vars)
		    (let loop ([el el])
		      (if (null? el)
			  (values null live-vars)
			  (let-values ([(rest live-vars) (loop (cdr el))])
			    (let-values ([(e live-vars)
					  (convert-function-calls (car el) vars live-vars)])
			      (values (cons e rest) live-vars)))))])
	(values ((get-constructor v)
		 (tok-n v)
		 (tok-line v)
		 (tok-col v)
		 (tok-file v)
		 (seq-close v)
		 (apply append el))
		live-vars)))))

(define (convert-paren-interior v vars live-vars)
  (convert-seq-interior v #t vars live-vars))

(define (split-decls el)
  (let loop ([el el][decls null])
    (if (null? el)
	(values (reverse! decls) null)
	(let ([e (car el)])
	  (if (or 
	       ;; These keywords appear only in decls:
	       (memq (tok-n (car e)) '(union struct static))
	       ;; Otherwise try harder:
	       (and
		;; Decl needs at least three parts:
		(< 2 (length e))
		;; Decl ends in seimicolon
		(eq? semi (tok-n (list-ref e (sub1 (length e)))))
		;; Doesn't start with a star
		(not (eq? '* (tok-n (car e))))
		;; Not an assignemnt
		(not (eq? '= (tok-n (cadr e))))
		;; Not a return
		(not (eq? 'return (tok-n (car e))))
		;; Not a label, field lookup, pointer deref
		(not (memq (tok-n (cadr e)) '(: |.| ->)))
		;; No parens/braces in first two parts
		(not (seq? (car e)))
		(not (seq? (cadr e)))))
	      ;; Looks like a decl
	      (loop (cdr el) (cons e decls))
	      ;; Not a decl
	      (values (reverse! decls) el))))))

(define (get-one e comma-sep?)
  (let loop ([e e][result null][first #f])
    (cond
     [(null? e) (values (reverse! result) null)]
     [(eq? semi (tok-n (car e)))
      (values (reverse! (cons (car e) result)) (cdr e))]
     [(and (eq? '|,| (tok-n (car e))) comma-sep?)
      (values (reverse! (cons (car e) result)) (cdr e))]
     [(and (braces? (car e))
	   (not (memq first '(typedef struct union enum))))
      (let ([rest (cdr e)])
	(if (or (null? rest)
		(not (eq? semi (tok-n (car rest)))))
	    (values (reverse! (cons (car e) result)) rest)
	    (values (reverse! (list* (car rest) (car e) result)) (cdr rest))))]
     [else (loop (cdr e) (cons (car e) result) (or first (tok-n (car e))))])))

(define (foldl-statement e comma-sep? f a-init)
  (let loop ([e e][a a-init])
    (if (null? e)
	a
	(let-values ([(sube e) (get-one e comma-sep?)])
	  (loop e (f sube a))))))

; (print-it e 0 #t) (exit)

(foldl-statement
 e
 #f
 (lambda (sube v)
   (let ([sube (top-level sube)])
     (print-it sube 0 #t)))
 (void))
