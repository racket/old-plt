(unit/sig during^
  (import before^
	  mzlib:date^ 
	  mzlib:function^
	  mzlib:string^
	  mzlib:pretty-print^
	  mred^)
  (define remember
    (opt-lambda (name second [minute #f] [hour #f] [day #f] [month #f] [year #f])
      (send edit new-counter name second minute hour day month year)))
  
  (define remember-around
    (opt-lambda (name first second)
      (let ([open (apply (ivar edit new-counter)
			 (if (string? name)
			     (string-append name " start")
			     name)
			 first)]
	    [closed (apply (ivar edit new-counter)
			   (if (string? name)
			       (string-append name " end")
			       name)
			   second)])
	(send open set-open)
	(send closed set-close))))
  
  (define user-config-file (case (system-type)
			     [(unix) "~/.countdownrc"]
			     [(macos) (build-path (collection-path "mzlib") 'up 'up
						  "Countdownrc.ss")]))

  (define show-error
    (let* ([error-frame #f]
	   [error-edit #f]
	   [frame%
	    (class-asi frame%
	      (rename [super-on-close on-close])
	      (override
		[on-close
		 (lambda () (and (super-on-close)
				 (set! error-frame #f)))]))])
      (lambda (err-string)
	(unless error-frame
	  (set! error-frame (make-object frame% ".countdownrc errors"))
	  (set! error-edit (make-object text%))
	  (send error-frame min-width 400)
	  (send error-frame min-height 200)
	  (send (make-object
		 editor-canvas%
		 (make-object
		  horizontal-panel% error-frame))
		set-editor
		error-edit) 
	  (send error-frame show #t))
	(let ([last (send error-edit last-position)])
	  (send* error-edit
	    (begin-edit-sequence)
	    (insert (string #\newline #\newline) last last #f)
	    (insert err-string last last #f)
	    (end-edit-sequence))))))

  (define (load-user-config)
    (set! files-loaded null)
    (let ([cu (compound-unit/sig (import)
		(link [C : mzlib:core^ ((require-library "corer.ss"))]
		      [mred : mred^ (mred@)])
		(export (open mred)
			(open C)))])
      (send edit clear-events)
      (let ([custodian (make-custodian)]
	    [raised-exception? #f]
	    [raised-exception #f]
	    [wakeup (make-semaphore 0)])
	(parameterize ([current-custodian custodian])
	  (parameterize ([current-eventspace (make-eventspace)])
	    (queue-callback
	     (lambda ()
	       (let/ec escape
		 (parameterize ([current-load
				 (let ([ol (current-load)])
				   (lambda (f)
				     (set! files-loaded (cons f files-loaded))
				     (ol f)))]
				[error-print-width 500]
				[current-namespace (make-namespace)]
				[current-exception-handler
				 (lambda (exn)
				   (set! raised-exception? #t)
				   (set! raised-exception exn)
				   (escape (void)))])
		   (global-define-values/invoke-unit/sig ((open mzlib:core^) (open mred^)) cu)
		   (global-defined-value 'remember remember)
		   (global-defined-value 'remember-around remember-around)
		   (load/cd user-config-file)))
	       (semaphore-post wakeup)))))
	(semaphore-wait wakeup)
	(custodian-shutdown-all custodian)
	(when raised-exception?
	  (show-error (if (exn? raised-exception)
			  (exn-message raised-exception)
			  (format "uncaught exception: ~e" raised-exception)))))
      (send edit sync)))

  (define files-loaded null)
  (load-user-config)
  (define last-loaded (current-seconds))

  (thread (rec check-user-config
	       (lambda ()
		 (when (ormap (lambda (file)
				(let ([now (current-seconds)]
				      [last-mod (file-or-directory-modify-seconds file)])
				  (if (<= last-loaded last-mod)
				      (begin (set! last-loaded (current-seconds))
					     #t)
				      #f)))
			      files-loaded)
		   (load-user-config))
		 (sleep 5)
		 (check-user-config)))))
