(unit/sig drscheme:teachpack^
  (import [mred : mred^])
  
  (define core-flat@ (require-library-unit/sig "coreflatr.ss"))
  
  ;; build-single-teachpack-thunk : string -> (union #f (-> void))
  (define (build-single-teachpack-thunk v)
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
	     (printf "building thunk: ~s~n" signature)
	     (eval
	      `(lambda ()
		 (printf "in thunk~n")
		 (with-handlers ([(lambda (x) #t)
				  (lambda (x)
				    ((error-display-handler)
				     (format
				      "Invalid Teachpack: ~a~n~a"
				      ,v
				      (if (exn? x) (exn-message x) x))))])
		   (global-define-values/invoke-unit/sig
		    ,signature
		    (compound-unit/sig
		      (import)
		      (link [userspace : plt:userspace^ 
				       ((compound-unit/sig 
					  (import)
					  (link [core : mzlib:core-flat^ (,core-flat@)]
						[mred : mred^ (,mred:mred@)])
					  (export (open core)
						  (open mred))))]
			    [teachpack : ,signature (,new-unit userspace)])
		      (export (open teachpack))))))))
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
    (let loop ([names v]
	       [thnk void])
      (cond
       [(null? names) thnk]
       [else
	(let ([new-thnk (build-single-teachpack-thunk (car names))])
	  (printf "new-thnk: ~s~n" new-thnk)
	  (new-thnk)
	  (if new-thnk
	      (loop (cdr names)
		    (lambda ()
		      (new-thnk)
		      (thnk)))
	      (loop (cdr names)
		    thnk)))]))))