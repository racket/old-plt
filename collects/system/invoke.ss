(define mred:non-unit-startup? #f)
(define mred:load-user-setup? #t)

(define mred:build-spidey-unit
  (lambda ()
    (when mred:output-spidey-file
      (call-with-output-file mred:output-spidey-file
	(lambda (port)
	  (pretty-print
	   `(begin-elaboration-time
	     (define mred:explicit-wx? #t)
	     (define plt:home-directory ,plt:home-directory)
	     (define mred:plt-home-directory ,mred:plt-home-directory)
	     (current-library-collection-paths (list ,@(current-library-collection-paths))))
	   port)

	  (pretty-print `(reference-library "match.ss") port)
	  (pretty-print `(begin-elaboration-time (reference-library "match.ss")) port)
	  (pretty-print `(reference-library "macro.ss") port)
	  (pretty-print `(begin-elaboration-time (reference-library "macro.ss")) port)
	  (pretty-print `(reference ,(build-path mred:plt-home-directory
						 "mred" "system" "debug.ss"))
			port)
	  
	  (pretty-print `(reference-library "cores.ss") port)
	  (pretty-print `(reference-library "triggers.ss") port)
	  (pretty-print `(reference ,(build-path mred:system-source-directory
						 "sig.ss"))
			port)
	  (when mred:app-sig-location
	    (pretty-print `(reference ,mred:app-sig-location) port))
	  (pretty-print
	   `(invoke-unit/sig
	     (compound-unit/sig (import)
	       (link [core : mzlib:core^ ((reference-library-unit/sig "corer.ss"))]
		     [trigger : mzlib:trigger^ ((reference-library-unit/sig "triggerr.ss"))]
		     [mred : mred^ ((reference-library-unit/sig "link.ss" "mred")
				    core trigger application)]
		     [application : mred:application^
				  ((reference-unit/sig ,(cond
							 [(complete-path? mred:app-location)
							  mred:app-location]
							 [(relative-path? mred:app-location)
							  (build-path mred:system-source-directory 
								      mred:app-location)]
							 [else (build-path (current-drive)
									   mred:app-location)]))
				   mred core)])
	       (export)))
	      port))
	'replace))))

(define mred:make-invokable-unit 
  (lambda ()
    (let ([app (load/use-compiled mred:app-location)])
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

(define mred:non-unit-startup
  (lambda ()
    (set! mred:app-location (build-path mred:system-source-directory "nuapp.ss"))
    (set! mred:non-unit-startup? #t)
    (mred:invoke)))

(define mred:invoke
  (let ([invoked? #f])
    (lambda ()
      (unless invoked?
	(set! invoked? #t)
	(unless (and (procedure? mred:make-invokable-unit)
		     (equal? 0 (arity mred:make-invokable-unit)))
	  (error 'mred:invoke "mred:make-invokable-unit is not a procedure of arity 0, it's: ~a~n"
		 mred:make-invokable-unit))
	(let ([unit/sig (mred:make-invokable-unit)])
	  (unless (unit/sig? unit/sig)
	    (error 'mred:invoke "mred:make-invokable-unit didn't return a unit/sig, returned: ~a~n"
		   unit/sig))
	  (mred:change-splash-message "Invoking...")
	  (unless mred:non-unit-startup?
	    (mred:no-more-splash-messages))
	  (invoke-open-unit/sig unit/sig))
	(mred:user-setup)))))

(define mred:startup
  (lambda ()
    (make-object mred:console-frame%)))
	     
(define mred:user-setup
  (lambda ()
    (when mred:load-user-setup?
      (set! mred:load-user-setup? #f)
      (let* ([init-file (wx:find-path 'init-file)])
	(when (file-exists? init-file)
	  (let ([orig-escape (error-escape-handler)])
	    (with-handlers ([void (lambda (e) 
				    (wx:message-box (exn-message e)
						    (format "~a Error" init-file)))])
	      (eval-string 
	       (string-append
		(expr->string `(load/cd ,init-file)))))))))))
