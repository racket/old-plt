(require-library "function.ss")
(require-library "letplsrc.ss")
(require-library "match.ss")
(require-library "pretty.ss")

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
; creates a thunk closed over the expression and (if on-spine is true) 
; the values of the variables which occur free in the list. 

(define (make-debug-info vars on-spine sexp)
  (let+ ([val kept-vars (if on-spine vars null)]
	 [val var-clauses (map (lambda (x) `(list (#%quote ,x) ,x)) kept-vars)])
  `(#%lambda () (list (#%quote ,sexp) ,@var-clauses))))

(define debug-key (gensym))

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
	      (let ([new-sym (gensym (string-append "arg" (number->string arg-num)))])
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

(define *unevaluated* (gensym "unevaluated"))

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


(define (annotate sexp bound-vars on-spine)
  (match sexp
    [`(#%lambda ,arglist ,body)
     (let+
      ([val (values annotated free-vars) (annotate body (set-union arglist bound-vars) #t)]
       [val new-free-vars (remq* arglist free-vars)]
       [val new-annotated `(#%lambda ,arglist ,annotated)]
       [val debug-info (make-debug-info new-free-vars on-spine sexp)])
      (values (wrap debug-info new-annotated)
	      new-free-vars))]
    [`(#%if ,test ,then ,else)
     (let+
      ([val (values annotated-test free-vars-test) (annotate test bound-vars #f)]
       [val (values annotated-then free-vars-then) (annotate then bound-vars on-spine)]
       [val (values annotated-else free-vars-else) (annotate else bound-vars on-spine)]
       [val annotated `(#%if ,annotated-test ,annotated-then ,annotated-else)]
       [val free-vars (foldl set-union free-vars-test (list free-vars-then free-vars-else))]
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
	  [rec multi-annotate
	       (letrec ((multi-annotate
			 (lambda (expr-list)
			   (if (null? expr-list)
			       (values null null)
		     (let-values (((annotated free-vars) (annotate (car expr-list) bound-vars #f))
				  ((ann-list fv-total) (multi-annotate (cdr expr-list))))
		       (values (cons annotated ann-list) 
			       (set-union free-vars fv-total)))))))
		 multi-annotate)]
	  [val (values ann-exprs free-vars) (multi-annotate sexp)]
	  [val set!-list (map (lambda (arg-symbol ann-expr)
				`(#%set! ,arg-symbol ,ann-expr))
			      arg-sym-list ann-exprs)]
	  [val app-debug-info (make-debug-info arg-sym-list on-spine sexp)]
	  [val final-app (wrap app-debug-info arg-sym-list)]
	  [val debug-info (make-debug-info (set-union arg-sym-list free-vars) on-spine sexp)]
	  [val let-body (wrap debug-info `(#%begin ,@set!-list ,final-app))])
	 (values `(let ,let-clauses ,let-body) free-vars))]
	         
       ; the variable form (including some constants, yes. oh well.)
       [(symbol? other)
	(let+
	 ([val free-vars (if (memq other bound-vars)
			     (list other)
			     null)]
	  [val debug-info (make-debug-info free-vars on-spine other)])
	 (values (wrap debug-info other)
		 free-vars))]
       
       ; other constants
       [else
	(let ([debug-info (make-debug-info null #f sexp)])
	  (values (wrap debug-info sexp) null))])]))

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
  (let-values (((annotated dont-care) (annotate (my-expand-defmacro expr) null #t)))
    annotated))

(define (display-break-info)
  (for-each (lambda (frame)
	      (let ([debug-info (frame)])
		(printf "expr: ~a~n" (car debug-info))
		(if (not (null? (cdr debug-info)))
		    (printf " bound: ~a~n" (cdr debug-info)))))
	    break-info))

(define (step-prog expr)
  (thread (lambda () 
	    (printf "result: ~a~n" (eval (annotate-wrapper expr)))
	    (semaphore-post break-sema)))
  (semaphore-wait break-sema)
  (display-break-info))

(annotate-wrapper '((a b) c))

(step-prog '((lambda (a b) (printf "~a~n" (+ a b))) (+ 1 3) 34))