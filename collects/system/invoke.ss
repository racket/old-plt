
(define mred:build-spidey-unit
  (lambda (output-file app-collection app-unit-library app-sig-library)
      (call-with-output-file output-file
	(lambda (port)
	  (pretty-print
	   `(begin-elaboration-time
	     (define mred:explicit-wx? #t)
	     (current-library-collection-paths
	      (list ,@(map (lambda (x)
			     `(build-path plt:home-directory
					  ,(find-relative-path (normalize-path plt:home-directory)
							       (normalize-path x))))
			   (current-library-collection-paths)))))
	   port)

	  (pretty-print `(reference-library "match.ss") port)
	  (pretty-print `(begin-elaboration-time (reference-library "match.ss")) port)
	  (pretty-print `(reference-library "macro.ss") port)
	  (pretty-print `(begin-elaboration-time (reference-library "macro.ss")) port)
	  (pretty-print `(reference-library "debug.ss" "system") port)
	  
	  (pretty-print `(reference-library "cores.ss") port)
	  (pretty-print `(reference-library "triggers.ss") port)
	  (pretty-print `(reference-library "sig.ss" "mred") port)
	  (when app-sig-library
		(pretty-print `(reference-library ,app-sig-library ,app-collection) port))
	  (pretty-print
	   `(invoke-unit/sig
	     (compound-unit/sig (import)
	       (link [core : mzlib:core^ ((reference-library-unit/sig "corer.ss"))]
		     [trigger : mzlib:trigger^ ((reference-library-unit/sig "triggerr.ss"))]
		     [mred : mred^ ((reference-library "link.ss" "mred")
				    core trigger application)]
		     [application : mred:application^
				  ((reference-library-unit/sig
				    ,app-unit-library
				    ,app-collection)
				   mred core)])
	       (export)))
	      port))
	'replace)))

(define mred:make-invokable-unit 
  (lambda (app-collection app-unit-library)
    (let ([app (require-library app-unit-library app-collection)])
      (unless (unit/sig? app)
	(error 'invokation "the application file didn't return a unit, got: ~a" app))
      (let ([U
	     (compound-unit/sig (import)
	       (link [core : mzlib:core^ ((reference-library-unit/sig "corer.ss"))]
		     [trigger : mzlib:trigger^ ((reference-library-unit/sig "triggerr.ss"))]
		     [mred : mred^ ((reference-library-unit/sig "link.ss" "mred")
				    core trigger application)]
		     [application : mred:application^ (app mred core)])
	       (export (open mred)
		       (open application)))])
	(compound-unit/sig (import)
	  (link [mred : ((open mred^) (open mred:application^)) (U)])
	  (export (unit mred)))))))
