(define zodiac:default-interface@
  (unit/sig zodiac:interface^
    (import)
    (define default-error-handler
      (lambda (keyword)
	(lambda (where fmt-spec . args)
	  (printf "Error at: ~s~n" where)
	  (apply error keyword fmt-spec args))))
    (define internal-error
      (default-error-handler 'internal-error))
    (define static-error
      (default-error-handler 'syntax-error))
    (define dynamic-error
      (default-error-handler 'run-time-error))))

(define zodiac:invoke-system
  (lambda ()
    (invoke-open-unit/sig
      (compound-unit/sig
	(import)
	(link
	  (INTERFACE : zodiac:interface^
	    (zodiac:default-interface@))
	  (PARAMETERS : plt:parameters^
	    (plt:mzscheme-parameters@))
	  (SYSTEM : zodiac:system^ (zodiac:system@ INTERFACE PARAMETERS)))
	(export (open SYSTEM) (open INTERFACE)))
      zodiac)))

(define zodiac:see
  (opt-lambda ((show-raw? #t) (vocab #f))
    (zodiac:invoke-system)
    (let loop ()
      (printf "e> ")
      (flush-output)
      (let ((r ((zodiac:read))))
	(if (zodiac:eof? r)
	  (newline)
	  (begin
	    (newline)
	    (pretty-print
	      (let ((e (car (if vocab
			      (zodiac:scheme-expand-program (list r) vocab)
			      (zodiac:scheme-expand-program (list r))))))
		(if show-raw? (zodiac:parsed->raw e) e)))
	    (newline)
	    (loop)))))))
