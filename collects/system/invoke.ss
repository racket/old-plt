
(define mred:non-unit-startup? #f)
(define mred:load-user-setup? #t)

(define mred:make-mred-invokable-unit
  (lambda ()
    (let* ([application (mred:make-application@)]
	   [U
	    (compound-unit/sig (import)
	      (link [core : mzlib:core^ (mzlib:core@)]
		    [trigger : mzlib:trigger^ (mzlib:trigger@)]
		    [mred : mred^ (mred@ core trigger application)]
		    [application : mred:application^ (application mred core)])
	      (export (open mred)
		      (open application)))])
      (compound-unit/sig (import)
	 (link [mred : ((open mred^) (open mred:application^)) (U)])
	 (export (unit mred))))))

(define mred:unit-make-application@
  (lambda ()
    (unit/sig mred:application^
      (import [mred@ : mred^]
	      [core@ : mzlib:core^])
      (define app-name "MrEd")
      (define console (make-object mred@:console-frame%))
      (define eval-string (lambda (s)
			    (let ([ce (send console get-edit)])
			      (send ce eval-and-display s)
			      (send ce insert-prompt)
			      #t))))))

(define mred:non-unit-make-application@
  (lambda ()
    (unit/sig mred:application^
      (import [mred : mred^]
	      [core : mzlib:core^])
      (define app-name "MrEd")
      (define console (make-object wx:frame% '() "hidden"))
      (define eval-string (lambda (string) (void))))))

;; one of these two definitions will be redefined by the application
(define mred:make-invokable-unit mred:make-mred-invokable-unit)
(define mred:make-application@ mred:unit-make-application@)

(define mred:non-unit-startup
  (lambda ()
    (set! mred:make-application@ mred:non-unit-make-application@)
    (set! mred:non-unit-startup? #t)
    (mred:invoke)))

(define mred:invoke
  (let ([invoked? #f])
    (lambda ()
      (unless invoked?
	(set! invoked? #t)
	(mred:change-splash-message "Invoking...")
	(unless (and (procedure? mred:make-invokable-unit)
		     (equal? 0 (arity mred:make-invokable-unit)))
	  (error 'mred:invoke "mred:make-invokable-unit is not a procedure of arity 0, it's: ~a~n"
		 mred:make-invokable-unit))
	(let ([unit/sig (mred:make-invokable-unit)])
	  (unless (unit/sig? unit/sig)
	    (error 'mred:invoke "mred:make-invokable-unit didn't return a unit/sig, returned: ~a~n"
		   unit/sig))
	  (invoke-open-unit/sig unit/sig))
	(mred:user-setup)))))

(define mred:startup
  (lambda ()
    (make-object mred:console-frame%)))
	     
(define mred:user-setup
  (lambda ()
    (when mred:load-user-setup?
      (set! mred:load-user-setup? #f)
      (let* ([init-file (build-path (wx:find-directory 'init)
				    (if (eq? wx:platform 'unix)
					".mredrc"
					"mredrc.ss"))])
	(when (file-exists? init-file)
	  (let ([orig-escape (error-escape-handler)])
	    (catch-errors (lambda (s) (wx:message-box s "Error"))
			  (lambda () (orig-escape))
			  (eval-string 
			   (string-append
			    (expr->string `(load/cd ,init-file)))))))))))
