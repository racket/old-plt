(module version mzscheme
  (require (lib "unitsig.ss")
	   "sig"
	   (lib "mred-sig.ss" "mred")
	   (lib "string.ss")
	   (lib "list.ss"))

  (provide version@)

  (define version@
    (unit/sig framework:version^
      (import)

      (rename [-version version])

      (define specs null)

      (define -version
	(lambda ()
	  (foldr
	   (lambda (entry sofar)
	     (let ([sep (first entry)]
		   [num (second entry)])
	       (string-append sofar sep num)))
	   (version)
	   specs)))

      (define add-spec
	(lambda (sep num)
	  (set! specs (cons (list (expr->string sep) (expr->string num))
			    specs)))))))
