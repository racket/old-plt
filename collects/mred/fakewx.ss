(begin-elaboration-time
 `(define-signature restrict-wx^
    ,(let loop ([syms (vector->list (signature->symbols wx^))])
       (cond
	[(null? syms) null]
	[else (let ([sym (car syms)])
		(if (member sym (list 'begin-busy-cursor 'end-busy-cursor))
		    (loop (cdr syms))
		    (cons sym (loop (cdr syms)))))]))))

(compound-unit/sig (import)
  (link [wx : wx^ (wx@)]
	[cursor : (begin-busy-cursor end-busy-cursor)
		((unit/sig (begin-busy-cursor end-busy-cursor)
		   (import [wx : wx^])

		   (printf "hi~n")

		   (define begin-busy-cursor
		     (lambda x
		       (printf "begin-busy-cursor: ~a~n" x)
		       (apply wx:begin-busy-cursor x)))
		   (define end-busy-cursor
		     (lambda x
		       (printf "end-busy-cursor: ~a~n" x)
		       (apply wx:end-busy-cursor x))))
		 wx)])
  (export (open (wx : restrict-wx^))
	  (open cursor)))
