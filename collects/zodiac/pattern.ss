(require-library "unitsig.ss")

; Uses of memq are okay, since they look up pattern var in kwd list

; Use of equal? WILL FAIL!

(define zodiac:pattern@
  (unit/sig zodiac:pattern^
    (import zodiac:misc^ zodiac:sexp^
      (z : zodiac:reader-structs^) zodiac:scheme-core^)

    (define (andmap pred l)
      (let mapf ((l l))
	(or (null? l)
	  (and (pred (car l)) (mapf (cdr l))))))

    (define (ormap pred l)
      (let mapf ((l l))
	(and (not (null? l))
	  (or (pred (car l)) (mapf (cdr l))))))

    (define (syntax-andmap pred l)
      (andmap pred (expose-list l)))

    (define (syntax-ormap pred l)
      (ormap pred (expose-list l)))

    ; ----------------------------------------------------------------------

    (define make-match&env
      (lambda (p k)			; pattern x kwd
	(letrec
	  ((m&e
	     (lambda (p)
	       (cond
		 ((ellipsis? p)
		   (let ((p-head (car p)))
		     (let ((nestings (get-ellipsis-nestings p-head k)))
		       (let ((match-head (m&e p-head)))
			 `(lambda (e)
			    (if (,z:list? e)
			      (#%list (#%cons ',nestings
					(#%map (lambda (x)
						 (,match-head x))
					  (,expose-list e))))
			      (esc #f)))))))
		 ((pair? p)
		   (let ((match-head (m&e (car p)))
			  (match-tail (m&e (cdr p))))
		     `(lambda (e)
			(if  (or (and (,z:list? e)
				   (#%not (,syntax-null? e)))
			       (,z:improper-list? e))
			  (#%append (,match-head (,syntax-car e))
			    (,match-tail (,syntax-cdr e)))
			  (esc #f)))))
		 ((null? p)
		   `(lambda (e)
		      (if (,syntax-null? e) '() (esc #f))))
		 ((symbol? p)
		   (if (memq p k)
		     `(lambda (e)
			(if (,z:symbol? e)
			  (if (,lexically-resolved? e env)
			    (esc #f)
			    (if (,name-eq? ',p (,z:read-object e))
			      '()
			      (esc #f)))
			  (esc #f)))
		     `(lambda (e)
			(#%list (#%cons ',p e)))))
		 (else
		   `(lambda (e)
		      (if (#%equal? ,p e) '() (esc #f))))))))
	  (eval `(lambda (esc env)
		   ,(m&e p))))))

    (define match-against
      (lambda (matcher e env)
	(let/ec esc
	  ((matcher esc env) e))))

    (define penv-merge append)

    (define extend-penv
      (lambda (name output env)
	(cons (cons name output) env)))

    ; ----------------------------------------------------------------------

    (define pexpand
      (lambda (p r k)			; pattern x p-env x kwd
	(letrec
	  ((expander
	     (lambda (p r)
	       (cond
		 ((ellipsis? p)
		   (append
		     (let* ((p-head (car p))
			     (nestings (get-ellipsis-nestings p-head k))
			     (rr (ellipsis-sub-envs nestings r)))
		       (map (lambda (r1)
			      (expander p-head (append r1 r)))
			 rr))
		     (expander (cddr p) r)))
		 ((pair? p)
		   (cons (expander (car p) r)
		     (expander (cdr p) r)))
		 ((symbol? p)
		   (if (memq p k) p
		     (let ((x (assq p r)))
		       (if x (cdr x) p))))
		 (else p)))))
	  (expander p r))))

;;; returns a list that nests a pattern variable as deeply as it
;;; is ellipsed
    (define get-ellipsis-nestings
      (lambda (p k)
	(let sub ((p p))
	  (cond ((ellipsis? p) (list (sub (car p))))
	    ((pair? p) (append (sub (car p)) (sub (cdr p))))
	    ((symbol? p) (if (memq p k) '() (list p)))
	    (else '())))))

;;; finds the subenvironments in r corresponding to the ellipsed
;;; variables in nestings
    (define ellipsis-sub-envs
      (lambda (nestings r)
	(ormap (lambda (c)
		 (if (contained-in? nestings (car c)) (cdr c) #f))
	  r)))

;;; checks if nestings v and y have an intersection
    (define contained-in?
      (lambda (v y)
	(if (or (symbol? v) (symbol? y)) (eq? v y)
	  (ormap (lambda (v_i)
		   (ormap (lambda (y_j)
			    (contained-in? v_i y_j))
		     y))
	    v))))

;;; tests if x is an ellipsing pattern, i.e., of the form
;;; (blah ... . blah2)
    (define ellipsis?
      (lambda (x)
	(and (pair? x) (pair? (cdr x)) (eq? (cadr x) '...))))

    ; ----------------------------------------------------------------------

    (define match-and-rewrite
      (case-lambda
	((expr rewriter out kwd env)
	  (let ((p-env (match-against rewriter expr env)))
	    (and p-env
	      (pexpand out p-env kwd))))
	((expr rewriter out kwd succeed fail env)
	  (let ((p-env (match-against rewriter expr env)))
	    (if p-env
	      (succeed (pexpand out p-env kwd))
	      (fail))))))

    ))
