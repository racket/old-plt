
(define mred:non-unit-startup? #f)
(define mred:load-user-setup? #t)

(define mred:make-mred-invokable-unit
  (lambda ()
    (let ([application (mred:make-application@)])
      (unit/sig->unit
       (compound-unit/sig (import)
	 (link [core : mzlib:core^ (mzlib:core@)]
	       [trigger : mzlib:trigger^ (mzlib:trigger@)]
	       [mred : mred^ (mred@ core trigger application)]
	       [application : mred:application^ (application mred core)])
	 (export (unit mred)
		 (unit application mred)))))))

;; one of these two definitions will be redefined by the application
(define mred:make-invokable-unit mred:make-mred-invokable-unit)
(define mred:make-application@
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

(define mred:non-unit-startup
  (lambda ()
    (set! mred:non-unit-startup? #t)
    (set! mred:make-application@ mred:non-unit-make-application@)
    (invoke-open-unit (mred:make-invokable-unit) #f)
    (when mred:load-user-setup?
      (mred:user-setup))))

(define mred:startup
  (lambda ()
    (make-object mred:console-frame%)))
	     

(define mred:user-setup
  (lambda ()
    (let* ([init-file (build-path (wx:find-directory 'init)
				  (if (eq? wx:platform 'unix)
				      ".mredrc"
				      "mredrc.ss"))])
      (when (file-exists? init-file)
	(let ([orig-escape (error-escape-handler)])
	  (catch-errors (lambda (s) (wx:message-box s "Error"))
			(lambda () (orig-escape))
			(mred:eval-string 
			 (string-append
			  (expr->string `(load/cd ,init-file))))))))))
