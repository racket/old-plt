(unit/sig drscheme:teachpack^
  (import [mred : mred^]
	  mzlib:function^)
  
  (define core-flat@ (require-library-unit/sig "coreflatr.ss"))
  
  ;; build-single-teachpack-unit : string -> (union #f (unit () X))
  (define (build-single-teachpack-unit v)
    (with-handlers
     ([(lambda (x) #t)
       (lambda (x)
	 (mred:message-box "Invalid Teachpack" (exn-message x))
	 #f)])
     (let ([new-unit (parameterize ([read-case-sensitive #t])
				   (load/cd v))])
       (if (unit/sig? new-unit)
					; Put the unit into a procedure that invokes it into
					;  the current namespace
	   (let* ([signature 
					; exploded -> flattened
		   (let ([sig (unit-with-signature-exports new-unit)])
		     (let loop ([l (vector->list sig)][r null])
		       (cond
			[(null? l) r]
			[(symbol? (car l)) (loop (cdr l) (cons (car l) r))]
			[else (let ([sub (loop (vector->list (cadr l)) null)]
				    [prefix (string-append (symbol->string (car l)) ":")])
				(loop (cdr l)
				      (append
				       (map (lambda (s)
					      (string->symbol
					       (string-append
						prefix
						(symbol->string s))))
					    sub))))])))])
	     (eval
	      `(unit/sig ()
		 (import plt:userspace^)
		 (with-handlers ([(lambda (x) #t)
				  (lambda (x)
				    ((error-display-handler)
				     (format
				      "Invalid Teachpack: ~a~n~a"
				      ,v
				      (if (exn? x)
					  (exn-message x)
					  x))))])
		   (global-define-values/invoke-unit/sig
		    ,signature
		    ,new-unit
		    #f
		    plt:userspace^)))))
	   (begin
	     (mred:message-box 
	      "Invalid Teachpack"
	      (format "loading Teachpack file does not result in a unit/sig, got: ~e"
		      new-unit))
	     #f)))))

  ;; build-teachpack-thunk : (listof string) -> (union #f (list (union 'mz 'mr) (-> void)))
  ;; accepts a filename and returns a thunk that invokes the corresponding teachpack and
  ;; a symbol indicating if this is a mzscheme teachpack or a mred teachpack.
  (define (build-teachpack-thunk v)
    (unless (and (list? v)
		 (andmap string? v))
	    (error 'build-teachpack-thunk "expected a list of strings, got: ~e" v))
    (let* ([tagn 0]
	   [link-clauses
	    (let loop ([units v])
	      (cond
	       [(null? units) null]
	       [else
		(let ([unit (build-single-teachpack-unit (car units))])
		  (if unit
		      (begin
			(set! tagn (+ tagn 1))
			(cons `[,(string->symbol (format "teachpack~a" tagn)) : () (,unit userspace)]
			      (loop (cdr units))))
		      (loop (cdr units))))]))]
	   [cu
	    (eval
	     `(compound-unit/sig
		(import)
		(link ,@(cons
			 `[userspace : plt:userspace^ 
				     ((compound-unit/sig 
					(import)
					(link [core : mzlib:core-flat^ (,core-flat@)]
					      [mred : mred^ (,mred:mred@)])
					(export (open core)
						(open mred))))]
			 link-clauses))
		(export)))])
      (lambda ()
	(invoke-unit/sig
	 cu)))))
      