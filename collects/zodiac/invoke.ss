; $Id: invoke.ss,v 1.37 1998/11/04 19:52:53 mflatt Exp $

(require-library "coreu.ss")

(require-library "load.ss" "zodiac")

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
      (default-error-handler 'syntax-error))))

(define zodiac:system@
  (require-library-unit/sig "link.ss" "zodiac"))

(define zodiac:invoke-system
  (lambda ()
    (invoke-open-unit/sig
      (compound-unit/sig
	(import)
	(link
	  (INTERFACE : zodiac:interface^
	    (zodiac:default-interface@))
	  (SYSTEM : zodiac:system^
	    (zodiac:system@ INTERFACE
	      (MZLIB-CORE pretty-print)
	      (MZLIB-CORE file)))
	  (MZLIB-CORE : mzlib:core^
	    (mzlib:core@)))
	(export (open SYSTEM) (open INTERFACE)))
      zodiac)))

(define (zodiac:make-see expander)
  (opt-lambda ((show-raw? #t))
    (zodiac:invoke-system)
    (parameterize ([current-prompt-read
		    (lambda ()
		      (newline)
		      (display "e> ")
		      (flush-output)
		      (let ([read ((zodiac:read))])
			(newline)
			(flush-output)
			(if (zodiac:eof? read)
			    eof
			    read)))]
		   [current-eval
		    (lambda (in)
		      (let ((e (car (expander in))))
			(if show-raw?
			    (zodiac:parsed->raw e)
			    e)))])
      (read-eval-print-loop))))

(define zodiac:see (zodiac:make-see 
		    (lambda (in)
		      (zodiac:scheme-expand-program (list in)))))

(define zodiac:spidey-see (zodiac:make-see 
			   (lambda (in)
			     (zodiac:scheme-expand-program
			      (list in)
			      (zodiac:make-attributes)
			      zodiac:mrspidey-vocabulary))))
