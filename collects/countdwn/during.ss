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

  (define orig-param (current-parameterization))

  (define (load-user-config)
    (let ([orig-escape #f]
	  [escape-k #f])
      (set! files-loaded null)
      (dynamic-wind
       (lambda ()
	 (set! orig-escape (error-escape-handler))
	 (error-escape-handler (lambda () (escape-k #f)))
	 (send edit begin-edit-sequence))
       (lambda ()
	 (let ([cu (compound-unit/sig (import)
		    (link [C : mzlib:core^ ((require-library "corer.ss"))]
			  [mred : mred^ (mred@)])
		    (export (open mred)
			    (open C)))])
	   (let/ec k
	     (send edit clear-events)
	     (set! escape-k k)
	     (let ([param (make-parameterization)])
	       (map (lambda (x v) ((in-parameterization param x) v))
		    (list current-load
			  error-print-width 
			  current-namespace
			  current-eventspace
			  current-custodian
			  error-display-handler)
		    (list (let ([ol ((in-parameterization param current-load))])
			    (lambda (f)
			      (set! files-loaded (cons f files-loaded))
			      (ol f)))
			  500
			  (make-namespace)
			  (make-eventspace)
			  (make-custodian)
			  (lambda (string)
			    (with-parameterization orig-param
			      (lambda ()
				(show-error string))))))
	       (with-parameterization param
		 (lambda ()
		   (invoke-open-unit/sig cu)
		   (global-defined-value 'remember remember)
		   (global-defined-value 'remember-around remember-around)
		   (load/cd user-config-file)))))
	   (send edit sync)))
       (lambda ()
	 (error-escape-handler orig-escape)
	 (send edit end-edit-sequence)))))

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
		 (sleep 4)
		 (check-user-config)))))
		       