(define mred:version@
  (unit/sig mred:version^
    (import mzlib:function^
	    mzlib:string^)

    (rename [-version version])

    (define specs null)

    (define -version
      (lambda ()
	(foldr (lambda (entry sofar)
		 (match entry
		   [(sep num) (string-append sofar sep num)]))
	       (version)
	       specs)))

    (define add-version-spec
      (lambda (sep num)
	(set! specs (cons (list (expr->string sep)
				(expr->string num))
			  specs))))
    
    (add-version-spec 's 7)))
