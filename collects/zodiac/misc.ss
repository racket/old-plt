; $Id$

(unit/sig zodiac:misc^
  (import (mz-pp : mzlib:pretty-print^))

  ; This is to get around an ordering problem.  Otherwise uses of
  ; pretty-print show up as #<undefined>, since this pretty-print
  ; captures the MzScheme pretty-print too soon.

  (define pretty-print
    (lambda args
      (apply mz-pp:pretty-print args)))

  (define debug-level-list '(expand expose resolve lex-res))
  (define debug-level '())

  (define symbol-append
    (lambda args
      (string->symbol
	(apply string-append
	  (map (lambda (s)
		 (cond
		   ((string? s) s)
		   ((symbol? s) (symbol->string s))
		   ((number? s) (number->string s))
		   (else
		     (error 'symbol-append "~s illegal" s))))
	    args)))))

  (define flush-printf
    (lambda (format . args)
      (apply printf format args)
      (flush-output)))

  (define print-and-return
    (lambda (v)
      (pretty-print v) (newline)
      v))

  )
