(unit/sig during^
  (import [wx : wx^]
	  before^
	  mzlib:date^ 
	  mzlib:function^
	  mzlib:string^
	  mzlib:pretty-print^
	  [mred : mred^])
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
  
  (define user-config-file "~/.countdownrc")

  (define show-error
    (let* ([error-frame #f]
	   [error-edit #f]
	   [frame%
	    (class-asi mred:frame%
	      (rename [super-on-close on-close])
	      (public
		[on-close
		 (lambda () (and (super-on-close)
				 (set! error-frame #f)))]))])
      (lambda (err-string)
	(unless error-frame
	  (set! error-frame (make-object frame% null ".countdownrc errors" 0 0 300 100))
	  (set! error-edit (make-object mred:media-edit%))
	  (send (make-object
		 mred:media-canvas%
		 (make-object
		  mred:horizontal-panel% error-frame))
		set-media
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
      (dynamic-wind
       (lambda ()
	 (set! orig-escape (error-escape-handler))
	 (error-escape-handler (lambda () (escape-k #f)))
	 (send edit begin-edit-sequence))
       (lambda ()
	 (let ([mzlib (reference-library "corer.ss")])
	   (let/ec k
	     (send edit clear-events)
	     (set! escape-k k)
	     (let ([param (make-parameterization)])
	       (map (lambda (x v) ((in-parameterization param x) v))
		    (list current-namespace 
			  wx:current-eventspace
			  current-custodian
			  error-display-handler)
		    (list (make-namespace 'wx)
			  (wx:make-eventspace)
			  (make-custodian)
			  (lambda (string)
			    (with-parameterization orig-param
			      (lambda ()
				(show-error string))))))
	       (with-parameterization param
		 (lambda ()
		   (invoke-open-unit/sig wx@ wx)
		   (global-defined-value 'remember remember)
		   (global-defined-value 'remember-around remember-around)
		   (invoke-open-unit/sig mzlib)
		   (load/cd user-config-file)))))
	   (send edit sync)))
       (lambda ()
	 (error-escape-handler orig-escape)
	 (send edit end-edit-sequence)))))
  
  (load-user-config)
  (define last-loaded (current-seconds))

  (thread (rec check-user-config
	       (lambda ()
		 (let ([now (current-seconds)]
		       [last-mod (file-modify-seconds user-config-file)])
		   (when (<= last-loaded last-mod)
		     (set! last-loaded (current-seconds))
		     (load-user-config))
		   (sleep 4)
		   (check-user-config))))))
		       