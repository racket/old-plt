
(module unitidmap mzscheme

  (define (make-id-mapper unbox-stx)
    (let ([set!-stx (datum->syntax-object unbox-stx 'set! #f)])
      (make-set!-transformer
       (lambda (sstx)
	 (cond
	  [(identifier? sstx) unbox-stx]
	  [(module-identifier=? set!-stx (car (syntax-e sstx)))
	   (raise-syntax-error
	    'unit
	    "cannot set! imported or exported variables"
	    sstx)]
	  [else
	   (datum->syntax-object
	    set!-stx
	    (cons unbox-stx (cdr (syntax-e sstx)))
	    sstx)])))))

  (provide make-id-mapper))

    
