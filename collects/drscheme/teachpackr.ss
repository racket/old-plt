(unit/sig drscheme:teachpack^
  (import [mred : mred^])
  
  (define core-flat@ (require-library-unit/sig "coreflatr.ss"))

  ;; build-teachpack-thunk : string -> (union #f (-> void))
  ;; accepts a filename and returns a thunk that invokes the corresponding teachpack
  (define (build-teachpack-thunk v)
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
	       `(lambda ()
		  (with-handlers ([(lambda (x) #t)
				   (lambda (x)
				     ((error-display-handler)
				      (format
				       "Invalid Teachpack:~n~a"
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
	       "loading Teachpack file does not result in a unit/sig")
	      #f))))))