(require-library "unitsig.ss")

(require-library "pretty.ss")

(define zodiac:misc@
  (let ((pretty-print pretty-print))
    (unit/sig zodiac:misc^
      (import)
      (rename (pp pretty-print))
      (define pp pretty-print)
      (define debug-level-list '(expand expose resolve lex-res))
      (define debug-level '())
;      (define debug-level '())
      (define symbol-append
	(lambda args
	  (string->symbol
	    (apply string-append
	      (map (lambda (s)
		     (cond
		       ((string? s) s)
		       ((symbol? s) (symbol->string s))
		       ((number? s) (number->string s))))
		args)))))

      )))
