; $Id: invoke.ss,v 1.32 1998/04/21 02:59:55 robby Exp $

(require-library "coreu.ss")
(require-library "match.ss")
(begin-elaboration-time (require-library "match.ss"))
(begin-construction-time (require-library "match.ss"))

(require-library "sparams.ss" "backward")

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

(define plt:mzscheme-parameters@
  (require-library-unit/sig "sparamr.ss" "backward"))

(define zodiac:mzscheme-parameters@
  (unit/sig plt:parameters^
    (import (plt : plt:parameters^))
    (define check-syntax-level 'advanced)))
; plt:check-syntax-level

; (define language-levels '(core structured side-effecting advanced))

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
	  (ACTUAL-PARAMETERS : plt:parameters^
	    (plt:mzscheme-parameters@))
	  (LOCAL-PARAMETERS : plt:parameters^
	    (zodiac:mzscheme-parameters@ ACTUAL-PARAMETERS))
	  (SYSTEM : zodiac:system^
	    (zodiac:system@ INTERFACE LOCAL-PARAMETERS
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
