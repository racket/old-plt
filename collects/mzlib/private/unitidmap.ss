
(module unitidmap mzscheme

  (define (make-id-mapper unbox-stx)
    (let ([set!-stx (datum->syntax 'set! #f unbox-stx)])
      (set!-expander
       (lambda (sstx)
	 (cond
	  [(identifier? sstx) unbox-stx]
	  [(module-identifier=? set!-stx (car (syntax-e sstx)))
	   (raise-syntax-error
	    'unit
	    "cannot set! imported or exported variables"
	    sstx)]
	  [else
	   (datum->syntax
	    (cons unbox-stx (cdr (syntax-e sstx)))
	    sstx
	    set!-stx)])))))

  (export make-id-mapper))

    
