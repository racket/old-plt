
(let ([load (current-load)]
      [load-extension (current-load-extension)]
      [tab ""])
  (let ([mk-chain
	 (lambda (load)
	   (lambda (filename)
	     (fprintf (current-error-port)
		      "~aloading ~a~n" 
		      tab filename)
	     (let ([s tab])
	       (dynamic-wind
		(lambda () (set! tab (string-append " " tab)))
		(lambda () 
		  (if (regexp-match "_loader" filename)
		      (let ([f (load filename)])
			(lambda (sym)
			  (printf "~atrying ~a~n" tab sym)
			  (f sym)))
		      (load filename)))
		(lambda () (set! tab s))))))])
    (current-load (mk-chain load))
    (current-load-extension (mk-chain load-extension))))
