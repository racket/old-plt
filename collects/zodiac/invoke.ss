(reference-library "coreu.ss")
(reference-library "match.ss")
(begin-elaboration-time (reference-library "match.ss"))
(begin-construction-time (reference-library "match.ss"))

(reference-library "sparams.ss" "backward")

(reference "load.ss")

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

(define plt:mzscheme-parameters@
  (reference-library-unit/sig "sparamu.ss" "backward"))

(define zodiac:system@
  (reference-unit/sig "link.ss"))

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
	  (SYSTEM : zodiac:system^
	    (zodiac:system@ INTERFACE PARAMETERS
	      (MZLIB-CORE pretty-print@)
	      (MZLIB-CORE file@)))
	  (MZLIB-CORE : mzlib:core^
	    (mzlib:core@)))
	(export (open SYSTEM) (open INTERFACE)))
      zodiac)))

(define zodiac:see
  (opt-lambda ((show-raw? #t))
    (zodiac:invoke-system)
    (let ([peval (current-eval)])
      (let ((system-params (current-parameterization)))
	(let ((new-params (make-parameterization)))
	  (with-parameterization new-params
	    (lambda ()
	      (parameterize
		((current-prompt-read
		   (lambda ()
		     (newline)
		     (display "e> ")
		     (flush-output)
		     (let ([read ((zodiac:read))])
		       (newline)
		       (flush-output)
		       (if (zodiac:eof? read)
			 eof
			 read))))
		  (current-eval
		    (lambda (in)
		      (let ((e (car
				 (with-parameterization system-params
				   (lambda ()
				     (call/nal zodiac:scheme-expand-program/nal
				       zodiac:scheme-expand-program
				       (expressions: (list in))))))))
			(if show-raw?
			  (zodiac:parsed->raw e)
			  e)))))
		(read-eval-print-loop)))))))))

(define zodiac:spidey-see
  (opt-lambda ((show-raw? #t))
    (zodiac:invoke-system)
    (let ([peval (current-eval)])
      (let ((system-params (current-parameterization)))
	(let ((new-params (make-parameterization)))
	  (with-parameterization new-params
	    (lambda ()
	      (parameterize
		((current-prompt-read
		   (lambda ()
		     (newline)
		     (display "e> ")
		     (flush-output)
		     (let ([read ((zodiac:read))])
		       (newline)
		       (flush-output)
		       (if (zodiac:eof? read)
			 eof
			 read))))
		  (current-eval
		    (lambda (in)
		      (let ((e (car
				 (with-parameterization system-params
				   (lambda ()
				     (call/nal zodiac:scheme-expand-program/nal
				       zodiac:scheme-expand-program
				       (expressions: (list in))
				       (attributes: (zodiac:make-attributes))
				       (vocabulary:
					 zodiac:mrspidey-vocabulary)))))))
			(if show-raw?
			  (zodiac:parsed->raw e)
			  e)))))
		(read-eval-print-loop)))))))))
