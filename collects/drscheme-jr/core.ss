(require-library "refer.ss")
(require-library "cores.ss")
(require-library "zsigs.ss" "zodiac")
(require-library "sigs.ss" "zodiac")
(require-library "params.ss" "userspce")
(begin-elaboration-time
 (require-library "sig.ss" "userspce"))


(define-signature prims^ (program argv))
(define-signature drscheme-jr:settings^
  (repl? show-banner? initialize-userspace setting startup-file
	 run-in-new-user-thread load-and-repl-done))

(define dr-jrU
  (unit/sig ()
    (import [zodiac : zodiac:system^]
	    [print-convert : mzlib:print-convert^]
	    [basis : plt:basis^]
	    [mzlib:pretty-print : mzlib:pretty-print^]
	    [mzlib:function : mzlib:function^]
	    [mzlib:thread : mzlib:thread^]
	    [settings : drscheme-jr:settings^])
    (define user-custodian (make-custodian))

    (define primitive-eval (current-eval))

    (define (repl)
      (let ([escape-k void]
	    [display-prompt
	     (lambda ()
	       (display "> ")
	       (flush-output))]
	    [input #f])
	(error-escape-handler (lambda () (escape-k #t)))
	(let outer-loop ()
	  (when (let/ec k
		  (display-prompt)
		  (fluid-let ([escape-k k])
		    (unless input
		      (set! input
			    (zodiac:read (current-input-port)
					 (zodiac:make-location 1 1 (file-position (current-output-port)) "stdin"))))
		    (basis:process/zodiac
		     input
		     (lambda (sexp loop)
		       (unless (basis:process-finish? sexp)
			 (mzlib:thread:dynamic-enable-break
			  (lambda ()
			    (call-with-values
			     (lambda () (primitive-eval sexp))
			     (lambda args (for-each (current-print) args)))))
			 (display-prompt)
			 (loop)))
		     #t))
		  #f)
	    (outer-loop)))))

    (define read/zodiac
      (lambda (port)
	(let ([r (zodiac:read
		  port
		  (zodiac:make-location 1 1 0 "port"))])
	  (lambda ()
	    (let ([v (r)])
	      (if (zodiac:eof? v)
		  eof
		  (zodiac:sexp->raw v)))))))

    (define drscheme-jr-print-load
      (lambda (f)
	(parameterize ([basis:intermediate-values-during-load
			(lambda values
			  (for-each basis:drscheme-print values))])
	  (load f))))

    (define (load/prompt f)
      (let/ec jump
	(let* ([eeh #f])
	  (dynamic-wind
	   (lambda () 
	     (set! eeh (error-escape-handler))
	     (error-escape-handler jump))
	   (lambda () (drscheme-jr-print-load f))
	   (lambda () 
	     (error-escape-handler eeh)
	     (set! eeh #f))))))

    (define (go)
      (let ([file settings:startup-file])
	(let loop ()
	  (let ([continue? #f])

	    (basis:initialize-parameters
	     user-custodian
	     settings:setting)

	    (mzlib:thread:dynamic-disable-break
	     (lambda ()

               (settings:initialize-userspace)
	       (global-defined-value 'read/zodiac read/zodiac)
	       (global-defined-value 'restart
				     (let ([die (lambda ()
						  (set! continue? #t)
						  (custodian-shutdown-all user-custodian))])
				       (rec restart
					    (case-lambda
					     [(new-file)
					      (when (or (not (string? new-file))
							(not (or (relative-path? new-file)
								 (absolute-path? new-file))))
						(raise-type-error 'restart "path string" new-file))
					      (set! file new-file)
					      (die)]
					     [() (die)]))))
               (when settings:show-banner?
                 (printf "Welcome to DrScheme Jr version ~a, Copyright (c) 1995-99 PLT~n"
                         (version))
                 (printf "Language: ~a~n"
                         (cadr (assoc (basis:setting-vocabulary-symbol (basis:current-setting))
                                      (map (lambda (s)
                                             (list (basis:setting-vocabulary-symbol s)
                                                   (basis:setting-name s)))
                                           basis:settings)))))
	       
	       (let ([repl-thread
		      (settings:run-in-new-user-thread
		       (lambda ()
			 (when (string? file)
			   (load/prompt file))
			 (when settings:repl?
			   (repl))
			 (settings:load-and-repl-done)))])
		 (mzlib:thread:dynamic-enable-break
		  (lambda ()
		    (let loop ()
		      (with-handlers ([exn:misc:user-break? (lambda (x)
							      (break-thread repl-thread)
							      (loop))])
			(thread-wait repl-thread))))))))
	    (when continue?
	      (loop))))))
    go))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     DrScheme Jr                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-go settings-unit)
  (invoke-unit/sig
   (compound-unit/sig (import)
     (link [mz : prims^ ((let ([_argv argv]
			       [_program program])
			   (unit/sig prims^
			     (import)
			     (define argv _argv)
			     (define program _program))))]
	   [mzlib : mzlib:core^ ((require-library "corer.ss"))]
	   [print-convert : mzlib:print-convert^ ((require-library "pconverr.ss")
						  (mzlib string)
						  (mzlib function))]
	   [interface : drscheme:interface^
		      ((require-library-unit/sig "interface.ss" "userspce") aries drzodiac)]
	   [drzodiac : zodiac:system^
		     ((require-library-unit/sig "link2.ss" "zodiac")
		      (interface : zodiac:interface^)
		      (mzlib pretty-print)
		      (mzlib file))]
	   [aries : plt:aries^ ((require-library-unit/sig "link-jr.ss" "stepper")
                                mzlib
				(drzodiac : zodiac:system^)
                                (interface : zodiac:interface^))]
	   [basis-import : plt:basis-import^ ((unit/sig plt:basis-import^
						(import)
						(define (invalid-teachpack s)
						  (printf "Invalid teachpack: ~a~n" s))
						(define in-mzscheme? #t)))]
	   [params : plt:userspace:params^ ((require-library-unit/sig "paramr.ss" "userspce"))]
	   [basis : plt:basis^

		  ((require-library-unit/sig "basis.ss" "userspce")
		   basis-import
		   params
		   drzodiac
		   interface
		   aries
		   print-convert
		   (mzlib pretty-print)
		   (mzlib function))]
           
           [settings : drscheme-jr:settings^
                     (settings-unit mz
                                    basis
                                    mzlib)]
	   [dr-jr : () (dr-jrU
			(drzodiac : zodiac:system^)
			print-convert
			basis
			(mzlib pretty-print)
			(mzlib function)
			(mzlib thread)
			settings)])
     (export))))
