(require-library "function.ss")
(require-library "letplsrc.ss")
(require-library "match.ss")
(require-library "pretty.ss")

; why doesn't require-file work? okay, so the file didn't exist.
; I still shouldn't have gotten that error...

(load "Barbican:plt:collects:stepper:closure-storage.ss")

; annotate: takes a macro-expanded s-exp and returns an annotated 
; s-exp and a list of identifiers which occur free in that s-exp.

; language accepted:
; exp = x | b | (lambda (a ...) exp) | (exp ...) | (if exp exp exp)

; my-expand-defmacro
; the mzscheme language underlying the drscheme-jr interpreter
; does not recognize lambda as a macro, and therefore expand-defmacro
; doesn't change lambda into #%lambda.  So we do this by creating
; a new namespace which does.

(define my-expand-defmacro
  (let ([n (make-namespace)])
    (lambda (exp)
      (parameterize ([current-namespace n])
	(expand-defmacro exp)))))

; make-debug-info takes a list of variables and an expression and
; creates a thunk closed over the expression and (if bindings-needed is true) 
; the following information for each variable in kept-vars:
; 1) the name of the variable (could actually be inferred)
; 2) the value of the variable
; 3) a mutator for the variable.
; (The reason for the third of these is actually that it can be used
;  in the stepper to determine which bindings refer to the same location,
;  as per Matthew's suggestion.)
; note that the mutators are needed only for the bindings which appear in
; closures; no location ambiguity can occur in the 'currently-live' bindings,
; since at most one location can exist for any given 

(define mutator-gensym (gensym 'mutator-))

(define (make-debug-info vars mutated-vars bindings-needed sexp)
  (let+ ([val kept-vars (if bindings-needed vars null)]
	 [val var-clauses (map (lambda (x) 
				 `(cons (#%quote ,x)
					(cons ,x
					      ,(if (memq x mutated-vars)
						   `(lambda (,mutator-gensym)
						      (set! ,x ,mutator-gensym))
						   `null))))
			       kept-vars)])
  `(#%lambda () (list (#%quote ,sexp) ,@var-clauses))))

(define debug-key (gensym 'debug-key))

; wrap creates the w-c-m expression.

; NB: I don't know how to protect the 'break' routine.  It can be
; redefined as written here, which would cause major havoc.

(define (wrap debug-info expr)
  (let ([with-break `(#%begin (break) ,expr)])
    `(#%with-continuation-mark (#%quote ,debug-key) ,debug-info ,with-break)))

; set-union takes two lists where no element appears twice in one list, and 
; forms a new list which is the union of the two sets.  the elements are 
; compared using eq?

(define (set-union a b)
  (append a (remq* a b)))
 
; get-arg-symbol maintains a list of gensyms associated with the non-negative
; integers.  These symbols are used in the elaboration of applications; the nth
; in the application is evaluated and stored in a variable whose name is the nth
; gensym supplied by get-arg-symbol.

; I'm just going to implement this with a simple assq list. if this isn't good
; enough, it can always be improved later.

(define get-arg-symbol
  (let ([assoc-list null])
    (lambda (arg-num)
      (let ([entry (assq arg-num assoc-list)])
	(if entry
	    (cadr entry)
	    (begin
	      (let ([new-sym (gensym (string-append "arg" (number->string arg-num) "-"))])
		(set! assoc-list `((,arg-num ,new-sym) ,@assoc-list))
		new-sym)))))))

; test cases: (returns #t on success)
;(let ([arg3 (get-arg-symbol 3)]
;      [arg2 (get-arg-symbol 2)]
;      [arg1 (get-arg-symbol 1)]
;      [arg2p (get-arg-symbol 2)])
;  (and (not (eq? arg3 arg2))
;       (not (eq? arg3 arg1))
;       (not (eq? arg3 arg2p))
;       (not (eq? arg2 arg1))
;       (eq? arg2 arg2p)
;       (not (eq? arg1 arg2p))))

; the `unevaluated' value is another gensym:

(define *unevaluated* (gensym "unevaluated-"))

; the `closure-temp' symbol is used for the let which wraps created closures, so
; that we can stuff them into the hash table.

(define closure-temp (gensym "closure-temp-"))

; closure-key takes a closure and returns an unchanging value that we can hash on.
; In our case, (conservative GC), the value of the closure itself serves this purpose
; admirably, and can also be used with a weak-key hash table to protect the
; tail-recursive properties of the language.

(define closure-key (lambda (x) x))

; How do we know which bindings we need?  For every lambda body, there is a
; `tail-spine' of expressions which is the smallest set including:
; a) the body itself
; b) an expression in tail position relative to a member of the tail-spine.
;
; I'm using `tail position' in a slightly non-standard way here.  Under my
; definition, A is in tail position relative to B if 
; a) A is contained in B
; b) if A is evaluated, the result of evaluating A will be the result of
;    evaluating B.
;
; So, if I've defined this correctly, note that an if expression has two tail
; positions, whereas an application has none.

; annotate takes an expression to annotate, the list of bound variables, and a boolean
; indicating whether this expression lies on the evaluation spine.  It returns three things;
; an annotated expression, a list of the bound variables which occur free, and a list of the 
; bound variables which occur free and may be mutated.

(define (annotate sexp bound-vars on-spine)
  (match sexp
    
    [`(#%lambda ,arglist ,body)
     (let+
      ([val (values annotated free-vars mutated-vars) (annotate body (set-union arglist bound-vars) #t)]
       [val new-free-vars (remq* arglist free-vars)]
       [val new-mutated-vars (remq* arglist mutated-vars)]
       [val new-annotated `(#%lambda ,arglist ,annotated)]
       [val debug-info (make-debug-info new-free-vars null on-spine sexp)]
       [val closure-info (make-debug-info new-free-vars new-mutated-vars #t sexp)]
       [val hash-wrapped `(let* ([,closure-temp ,new-annotated])
			    (closure-table-put! ,closure-temp ,closure-info)
			    ,closure-temp)])
      (values (wrap debug-info hash-wrapped)
	      new-free-vars
	      new-mutated-vars))]

    [`(#%if ,test ,then ,else)
     (let+
      ([val (values annotated-test free-vars-test mutated-vars-test) (annotate test bound-vars #f)]
       [val (values annotated-then free-vars-then mutated-vars-then) (annotate then bound-vars on-spine)]
       [val (values annotated-else free-vars-else mutated-vars-else) (annotate else bound-vars on-spine)]
       [val annotated `(#%if ,annotated-test ,annotated-then ,annotated-else)]
       [val free-vars (foldl set-union free-vars-test (list free-vars-then free-vars-else))]
       [val mutated-vars (foldl set-union mutated-vars-test (list mutated-vars-then mutated-vars-else))]
       [val debug-info (make-debug-info free-vars on-spine sexp)])
      (values (wrap debug-info annotated)
	      free-vars))]
    [other
     (cond
       
       ; the application form: there are a lot of questions here about just how optimized
       ; I want to make this on the first go-round.  To simplify the first implementation,
       ; I'm just going to annotate all sub-expressions with source positions, despite 
       ; the potential elision of such marks.
       	
       [(list? other) 
	(let+
	 ([val arg-sym-list (build-list (length sexp) get-arg-symbol)]
	  [val let-clauses (map (lambda (sym) `(,sym (#%quote ,*unevaluated*))) arg-sym-list)]
	  [val multi-annotate
	       (letrec
		   ; NB: this would be shorter if fold could handle multiple values. oh well.
		   
		   ([multi-annotate
		     (lambda (expr-list)
		       (if (null? expr-list)
			   (values null null null)
			   (let-values (((annotated free-vars mutated-vars) 
					 (annotate (car expr-list) bound-vars #f))
					((ann-list fv-total mv-total) (multi-annotate (cdr expr-list))))
			     (values (cons annotated ann-list) 
				     (set-union free-vars fv-total)
				     (set-union mutated-vars mv-total)))))])
		 multi-annotate)]
	  [val (values ann-exprs free-vars mutated-vars) (multi-annotate sexp)]
	  [val set!-list (map (lambda (arg-symbol ann-expr)
				`(#%set! ,arg-symbol ,ann-expr))
			      arg-sym-list ann-exprs)]
	  [val app-debug-info (make-debug-info arg-sym-list null on-spine sexp)]
	  [val final-app (wrap app-debug-info arg-sym-list)]
	  [val debug-info (make-debug-info (set-union arg-sym-list free-vars) null on-spine sexp)]
	  [val let-body (wrap debug-info `(#%begin ,@set!-list ,final-app))])
	 (values `(let ,let-clauses ,let-body) free-vars mutated-vars))]
       
       ; the variable form 
       
       [(symbol? other)
	(let+
	 ([val free-vars (if (memq other bound-vars)
			     (list other)
			     null)]
	  [val debug-info (make-debug-info free-vars null on-spine other)])
	 (values (wrap debug-info other)
		 free-vars
		 null))]
       
       ; other constants
       
       [else
	(let ([debug-info (make-debug-info null #f sexp)])
	  (values (wrap debug-info sexp) null null))])]))

(define break-info #f)
(define break-sema (make-semaphore))
(define resume-sema (make-semaphore))

(define (break)
  (set! break-info (current-continuation-marks debug-key))
  (semaphore-post break-sema)
  (semaphore-wait resume-sema))

(define (step)
  (semaphore-post resume-sema)
  (semaphore-wait break-sema)
  (display-break-info))

(define (annotate-wrapper expr)
  (let-values (((annotated dont-care dont-care-2) (annotate (my-expand-defmacro expr) null #t)))
    annotated))

(define (display-break-info)
  (for-each (lambda (frame)
	      (let ([debug-info (frame)])
		(printf "expr: ~a~n" (car debug-info))
		(if (not (null? (cdr debug-info)))
		    (printf " bound: ~a~n" (cdr debug-info)))))
	    break-info))

(define (step-prog expr)
  (initialize-closure-table)
  (thread (lambda () 
	    (printf "result: ~a~n" (eval (annotate-wrapper expr)))
	    (semaphore-post break-sema)))
  (semaphore-wait break-sema)
  (display-break-info))

(annotate-wrapper '((a b) c))

;(step-prog '((lambda (a b) (printf "~a~n" (+ a b))) (+ 1 3) 34))

;(step-prog '((lambda (x) x) (lambda (x) x)))

(define first-prog '((lambda (x) x) (lambda (x) x)))
(define second-prog '((lambda (x) (x x)) (lambda (x) (x x))))

