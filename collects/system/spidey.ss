(require-library "pretty.ss")
(require-library "file.ss")
(define mred:build-spidey-unit
  (lambda (output-file app-collection info)
    (let ([app-unit-library (info 'app-unit-library
				  (lambda ()
				    (error 'mred:build-spidey-unit
					   "no app-unit-library")))]
	  [app-sig-library (info 'app-sig-library
				 (lambda ()
				   (error 'mred:build-spidey-unit
					  "no app-sig-library")))])
      (call-with-output-file output-file
	(lambda (port)
	  (pretty-print
	   `(begin-elaboration-time
	     
	     ;; instead of this, use nowx-sig.ss and set the wx manually
	     (define mred:explicit-wx? #t)
	     
	     (current-library-collection-paths
	      (list 
	       ,@(map normalize-path
		  (current-library-collection-paths)))))
	   port)

	  (pretty-print `(require-library "match.ss") port)
	  (pretty-print `(begin-elaboration-time (require-library "match.ss")) port)
	  (pretty-print `(require-library "macro.ss") port)
	  (pretty-print `(begin-elaboration-time (require-library "macro.ss")) port)
	  (pretty-print `(require-library "debug.ss" "system") port)
	  
	  (pretty-print `(require-library "invsig.ss" "system") port)
	  (when app-sig-library
	    (pretty-print `(require-library ,app-sig-library ,app-collection) port))

	  (pretty-print `(define argv (vector)) port)
	  (pretty-print `(define mred:initialize void) port)

	  (pretty-print
	   `(invoke-unit/sig
	     (require-library-unit/sig
	      ,app-unit-library
	      ,app-collection)
	     mred:application-imports^)
	   port))
	'replace))
    (exit)))

mred:build-spidey-unit
