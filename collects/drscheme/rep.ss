(unit/sig drscheme:rep^
  (import [wx : wx^]
	  [mred : mred^]
	  [mzlib : mzlib:core^]
	  [print-convert : mzlib:print-convert^]
	  [aries : plt:aries^]
	  [zodiac : drscheme:zodiac^]
	  [zodiac:interface : drscheme:interface^]
	  [drscheme:init : drscheme:init^]
	  [drscheme:snip : drscheme:snip^]
	  [drscheme:language : drscheme:language^]
	  [drscheme:app : drscheme:app^]
	  [drscheme:basis : drscheme:basis^]
	  [drscheme:edit : drscheme:edit^])
  
  (mred:debug:printf 'invoke "drscheme:rep@")
  
  (define WELCOME-DELTA (make-object wx:style-delta%
				     wx:const-change-family
				     wx:const-decorative))
  (define CLICK-DELTA (make-object wx:style-delta%))
  (define RED-DELTA (make-object wx:style-delta%))
  (send* CLICK-DELTA
    (copy WELCOME-DELTA)
    (set-delta-foreground "BLUE")
    (set-delta wx:const-change-underline 1))
  (send* RED-DELTA
    (copy WELCOME-DELTA)
    (set-delta-foreground "RED"))
  (define WARNING-STYLE-DELTA (make-object wx:style-delta% wx:const-change-bold 0))
  (send* WARNING-STYLE-DELTA
    (set-delta-foreground "BLACK")
    (set-delta-background "YELLOW"))

  (define syntax-checking-primitive-eval
    (lambda (expr)
      (drscheme:init:primitive-eval
       (with-handlers ([(lambda (x) #t)
			(lambda (x) (error 'internal-syntax-error
					   (exn-message x)))])
	 (expand-defmacro expr)))))


  (define exception-reporting-rep
    (let ([edit #f])
      (case-lambda 
       [() edit]
       [(x)
	(set! edit x)])))

  (define user-error-display-handler
    (rec drscheme-error-display-handler
	 (lambda (msg)
	   (with-parameterization drscheme:init:system-parameterization
	     (lambda ()
	       (let ([rep (exception-reporting-rep)])
		 (if rep
		     (send rep report-unlocated-error msg)
		     (mred:message-box msg "Uncaught Error"))))))))

  (define user-exception-handler
    (rec drscheme-exception-handler
	 (lambda (exn)
	   (with-parameterization drscheme:init:system-parameterization
	     (lambda ()
	       (let ([rep (exception-reporting-rep)])
		 (if rep
		     (begin
		       (send rep report-exception-error exn)
		       (send rep escape))
		     (begin
		       (mred:message-box (if (exn? exn)
					     (exn-message exn)
					     (format "~e" exn))
					 "Uncaught Exception"))))))
	   ((error-escape-handler)))))

  (define build-parameterization
    (lambda (user-custodian)
      (let* ([p (make-parameterization drscheme:init:eval-thread-parameterization)]
	     [userspace-branch-handler
	      (lambda ()
		(let ([p (make-parameterization-with-sharing 
			  p p (list current-exception-handler)
			  (lambda (x) #f))])
		  ((in-parameterization p current-exception-handler) user-exception-handler)
		  p))]
	     [bottom-eventspace 
	      (parameterize ([current-custodian user-custodian])
		(wx:make-eventspace (userspace-branch-handler)))]
	     [n (if (drscheme:language:use-zodiac)
		    (make-namespace 'no-constants 'wx 'hash-percent-syntax)
		    (make-namespace 'wx))])
	(with-parameterization p
	  (lambda ()
	    (parameterization-branch-handler userspace-branch-handler)
	    (compile-allow-set!-undefined #f)
	    (compile-allow-cond-fallthrough #f)
	    (current-custodian user-custodian)
	    (require-library-use-compiled #f)
	    (error-value->string-handler
	     (let ([drs-error-value->string-handler
		    (lambda (x n)
		      (with-parameterization drscheme:init:system-parameterization
			(lambda ()
			  (let* ([port (open-output-string)]
				 [error-value
				  (if (drscheme:language:r4rs-style-printing)
				      x
				      (print-convert:print-convert x))]
				 [long-string
				  (begin (mzlib:pretty-print@:pretty-print error-value port 'infinity)
					 (get-output-string port))])
			    (if (<= (string-length long-string) n)
				long-string
				(let ([short-string (substring long-string 0 n)]
				      [trim 3])
				  (unless (<= n trim)
				    (let loop ([i trim])
				      (unless (<= i 0)
					(string-set! short-string (- n i) #\.)
					(loop (sub1 i)))))
				  short-string))))))])
	       drs-error-value->string-handler))
	    (debug-info-handler (let ([drs-debug-info
				       (lambda () (unbox aries:error-box))])
				  drs-debug-info))
	    (current-exception-handler user-exception-handler)
	    (error-display-handler user-error-display-handler)
	    (current-namespace n)
	    (break-enabled #t)
	    (wx:current-eventspace bottom-eventspace)
	    ;(wx:eventspace-parameterization bottom-eventspace p)
	    (current-will-executor (make-will-executor))
	    (read-curly-brace-as-paren #t)
	    (read-square-bracket-as-paren #t)
	    (print-struct #t)
	    (error-print-width 250)))
	(drscheme:language:install-language p)
	(drscheme:basis:add-basis n bottom-eventspace user-custodian)
	p)))

  (define-struct process-finish (error?))

  (mred:set-preference-default 'drscheme:repl-always-active #f boolean?)

  (define make-edit%
    (lambda (super%)
      (class super% args
	(inherit insert change-style
		 cleanup-transparent-io 
		 transparent-snip set-caret-owner
		 clear-previous-expr-positions
		 get-end-position
		 set-clickback
		 set-last-header-position
		 this-err-write this-err 
		 this-out
		 this-in this-result this-result-write
		 output-delta set-output-delta
		 do-post-eval
		 insert-prompt
		 erase prompt-mode?
		 get-canvas
		 ready-non-prompt autoprompting?
		 set-prompt-mode
		 delete lock locked?
		 get-text
		 reset-console-start-position
		 last-position
		 set-resetting
		 position-line
		 set-position
		 get-frame
		 begin-edit-sequence
		 end-edit-sequence
		 reset-pretty-print-width
		 scroll-to-position)
	(rename
	  [super-init-transparent-io init-transparent-io]
	  [super-initialize-console initialize-console]
	  [super-reset-console reset-console]
	  [super-init-transparent-io-do-work init-transparent-io-do-work])
	(private
	  return-value
	  return-error
	  to-be-evaluated
	  
	  waiting-for-loaded
	  [load-success? #f])
	
	(public
	  [init-transparent-io
	   (lambda (grab-focus?)
	     (begin-edit-sequence)
	     (super-init-transparent-io grab-focus?)
	     (when (eq? (current-thread) evaluation-thread)
	       (set-caret-owner transparent-snip wx:const-focus-display))
	     (end-edit-sequence))]
	  [init-transparent-io-do-work
	   (lambda (grab-focus?)
	     (with-parameterization drscheme:init:system-parameterization
	       (lambda ()
		 (let ([c-locked? locked?])
		   (begin-edit-sequence)
		   (lock #f)
		   (super-init-transparent-io-do-work grab-focus?)
		   (lock c-locked?)
		   (end-edit-sequence)))))])
	(private
	  [escape-fn #f])
	(public
	  [report-exception-error
	   (lambda (exn)
	     (if (exn? exn)
		 (let ([di (exn-debug-info exn)])
		   (if (and (zodiac:zodiac? di)
			    (drscheme:language:use-zodiac))
		       (let* ([start (zodiac:zodiac-start di)]
			      [finish (zodiac:zodiac-finish di)])
			 (report-error start finish 'dynamic (exn-message exn)))
		       (report-unlocated-error (exn-message exn))))
		 (report-unlocated-error (format "uncaught exception: ~e" exn))))]
	  [report-unlocated-error
	   (lambda (message)
	     (let* ([frame (get-frame)]
		    [interactions-edit (ivar frame interactions-edit)])
	       (send frame ensure-interactions-shown)
	       (let ([locked? (ivar interactions-edit locked?)])
		 (send* interactions-edit
		   (begin-edit-sequence)
		   (lock #f)
		   (this-err-write (string-append message (string #\newline)))
		   (lock locked?)
		   (end-edit-sequence)))))]
	  [report-error
	   (lambda (start-location end-location type input-string)
	     (let* ([start (zodiac:location-offset start-location)]
		    [finish (add1 (zodiac:location-offset end-location))]
		    [file (zodiac:location-file start-location)]
		    [message
		     (if (is-a? file wx:media-edit%)
			 input-string
			 (format "~a: ~a.~a-~a.~a: ~a" file
				 (zodiac:location-line start-location)
				 (zodiac:location-column start-location)
				 (zodiac:location-line end-location)
				 (zodiac:location-column end-location)
				 input-string))])
	       (report-unlocated-error message)
	       (when (is-a? file wx:media-edit%)
		 (send file begin-edit-sequence)
		 (send file set-position start finish)
		 (if (is-a? file edit%)
		     (send file scroll-to-position start
			   #f (sub1 (send file last-position)) -1)
		     (send file scroll-to-position start #f finish))
		 (send file end-edit-sequence)
		 (send (send file get-canvas) set-focus))))])
	(public
	  [on-set-media void]
	  [get-prompt (lambda () "> ")]
	  [user-param #f]
	  [vocab #f]
	  [user-custodian (make-custodian)]
	  
	  [userspace-eval
	   (lambda (sexp)
	     (with-parameterization drscheme:init:system-parameterization
	       (lambda ()
		 (let* ([z (or (unbox aries:error-box)
			       (let ([loc (zodiac:make-location 0 0 0 'eval)])
				 (zodiac:make-zodiac 'mzrice-eval loc loc)))]
			[answer (list (void))]
			[f
			 (lambda (annotated recur)
			   (if (process-finish? annotated)
			       (if (process-finish-error? annotated)
				   (escape)
				   answer)
			       (begin (set! answer
					    (call-with-values
					     (lambda ()
					       (with-parameterization user-param
						 (lambda ()
						   (syntax-checking-primitive-eval annotated))))
					     (lambda x x)))
				      (recur))))])
		   (apply values (process-sexp sexp z f #t))))))]
	  [display-result
	   (lambda (v)
	     (unless (void? v)
	       (let ([v (if (drscheme:language:r4rs-style-printing)
			    v
			    (print-convert:print-convert v))])
		 (parameterize ([mzlib:pretty-print@:pretty-print-size-hook
				 (lambda (x _ port) (and (is-a? x wx:snip%) 1))]
				[mzlib:pretty-print@:pretty-print-print-hook
				 (lambda (x _ port) (this-result-write x))])
		   (mzlib:pretty-print@:pretty-print v this-result)))))])
	(private
	  [get-case-sensitivity
	   (lambda ()
	     (if user-param
		 ((in-parameterization user-param read-case-sensitive))
		 (drscheme:language:setting-case-sensitive?
		  (mred:get-preference 'drscheme:settings))))])
	(public
	  [process-edit/zodiac
	   (lambda (edit f start end annotate?)
	     (process/zodiac
	      (parameterize ([read-case-sensitive (get-case-sensitivity)])
		(zodiac:read (mred:read-snips/chars-from-buffer edit start end)
			     (zodiac:make-location 0 0 start edit)
			     #t 1))
	      f
	      annotate?))]
	  [process-file/zodiac
	   (lambda (filename f annotate?)
	     (let ([port (with-parameterization user-param
			   (lambda ()
			     (open-input-file filename)))])
	       (process/zodiac
		(parameterize ([read-case-sensitive (get-case-sensitivity)])
		  (zodiac:read port
			       (zodiac:make-location 0 0 0 filename)
			       #t 1 user-param))
		(lambda (x r)
		  (when (process-finish? x)
		    (close-input-port port))
		  (f x r))
		annotate?)))]
	  [process-sexp/zodiac
	   (lambda (sexp z f annotate?)
	     (let ([reader
		    (let ([gone #f])
		      (lambda ()
			(or gone
			    (begin (set! gone (zodiac:make-eof z))
				   (zodiac:structurize-syntax sexp z)))))])
	     (process/zodiac reader f annotate?)))]
	  [process/zodiac
	   (lambda (reader f annotate?)
	     (let ([cleanup
		    (lambda (error?)
		      (f (make-process-finish error?) void))])
	       (let loop ()
		 (let ([next-iteration
			(with-handlers ([zodiac:interface:zodiac-exn?
					 (lambda (exn)
					   (report-error (zodiac:interface:zodiac-exn-start-location exn)
							 (zodiac:interface:zodiac-exn-end-location exn)
							 (zodiac:interface:zodiac-exn-type exn)
							 (zodiac:interface:zodiac-exn-message exn))
					   (lambda () (cleanup #t)))])
			  (let/ec k
			    (let ([zodiac-read
				   (parameterize ([read-case-sensitive (get-case-sensitivity)])
				     (reader))])
			      (if (zodiac:eof? zodiac-read)
				  (lambda () (cleanup #f))
				  (let* ([wait-on-scheme
					  (lambda (expr)
					    (let-values ([(answers error?) (send-scheme expr)])
					      (if error?
						  (k (lambda () (cleanup #t)))
						  (apply values answers))))]
					 [evaluator
					  (lambda (exp _ macro)
					    (wait-on-scheme (aries:annotate exp)))]
					 [user-macro-body-evaluator
					  (lambda (x . args)
					    (wait-on-scheme `(,x ,@(map (lambda (x) `(#%quote ,x)) args))))]
					 [exp (call/nal zodiac:scheme-expand/nal
							zodiac:scheme-expand
							[expression: zodiac-read]
							[vocabulary: vocab]
							[user-macro-body-evaluator: user-macro-body-evaluator]
							[elaboration-evaluator: evaluator])]
					 [heading-out (if annotate? 
							  (aries:annotate exp)
							  exp)])
				    (mred:debug:when 'drscheme:sexp
						     (mzlib:pretty-print@:pretty-print heading-out))
				    (lambda () (f heading-out loop)))))))])
		   (next-iteration)))))])
	(private
	  [no-zodiac-vocab (make-namespace)])
	(public
	  [process-edit/no-zodiac
	   (lambda (edit f start end)
	     (let* ([buffer-thunk (mred:read-snips/chars-from-buffer edit start end)]
		    [snip-string (string->list " 'image ")]
		    [port-thunk (let ([from-snip null])
				  (rec port-thunk
				       (lambda ()
					 (if (null? from-snip)
					     (let ([next (buffer-thunk)])
					       (if (or (char? next) (eof-object? next))
						   next
						   (begin (set! from-snip snip-string)
							  (port-thunk))))
					     (begin0 (car from-snip)
						     (set! from-snip (cdr from-snip)))))))]
		    [port (make-input-port port-thunk (lambda () #t) void)])
	       (process/no-zodiac (lambda () (read port)) f)))]
	  [process-file/no-zodiac
	   (lambda (filename f)
	     (call-with-input-file filename
	       (lambda (port)
		 (process/no-zodiac (lambda () (read port)) f))))]
	  [process-sexp/no-zodiac
	   (lambda (sexp f)
	     (process/no-zodiac (let ([first? #t]) 
				  (lambda ()
				    (if first?
					(begin (set! first? #f)
					       sexp)
					eof)))
				f))]
	  [process/no-zodiac
	   (lambda (reader f)
	     (let loop ()
	       (let-values ([(expr error?) (with-handlers ([(lambda (x) #t)
							    (lambda (exn) (values exn #t))])
					     (values (reader) #f))])
		 (if error?
		     (begin
		       (report-unlocated-error
			(if (exn? expr)
			    (exn-message expr)
			    (format "uncaught exception: ~e" expr)))
		       (f (make-process-finish #t) void))
		     (if (eof-object? expr)
			 (f (make-process-finish #f) void)
			 (f expr loop))))))])
	(public
	  [process-edit
	   (lambda (edit fn start end annotate?)
	     (if (drscheme:language:use-zodiac)
		 (process-edit/zodiac edit fn start end annotate?)
		 (process-edit/no-zodiac edit fn start end)))]
	  [process-file
	   (lambda (filename fn annotate?)
	     (if (drscheme:language:use-zodiac)
		 (process-file/zodiac filename fn annotate?)
		 (process-file/no-zodiac filename fn)))]
	  [process-sexp
	   (lambda (sexp z fn annotate?)
	     (if (drscheme:language:use-zodiac)
		 (process-sexp/zodiac sexp z fn annotate?)
		 (process-sexp/no-zodiac sexp fn)))])


	(private
	  [in-evaluation? #f]
	  [in-evaluation-semaphore (make-semaphore 1)]
	  [in-break? #f]
	  [should-collect-garbage? #f]
	  [ask-about-kill? #f])
	(public
	  [insert-warning
	   (lambda ()
	     (begin-edit-sequence)
	     (insert #\newline (last-position) (last-position))
	     (let ([start (last-position)])
	       (insert "WARNING: Interactions window is out of sync with the definitions window. Click Execute."
		       start start)
	       (let ([end (last-position)])
		 (change-style WARNING-STYLE-DELTA start end)))
	     (end-edit-sequence))]
	  [do-eval
	   (let ([count 0])
	     (lambda (start end)
	       (set! count (add1 count))
	       '(when (<= 5 count)
		 (collect-garbage)
		 (set! count 0))
	       (let* ([frame (get-frame)]
		      [definitions-edit (ivar frame definitions-edit)]
		      [already-warned? (ivar definitions-edit already-warned?)]
		      [needs-execution? (ivar definitions-edit needs-execution?)])
		 (when (if (mred:get-preference 'drscheme:execute-warning-once)
			   (and (not already-warned?)
				needs-execution?)
			   needs-execution?)
		   (send definitions-edit already-warned)
		   (insert-warning)))
	       (do-many-buffer-evals this start end)))]
	  [cleanup-evaluation
	   (opt-lambda ()
	     (mred:debug:printf 'console-threading "cleanup-evaluation: waiting in-evaluation")
	     (semaphore-wait in-evaluation-semaphore)
	     (mred:debug:printf 'console-threading "cleanup-evaluation: passed in-evaluation")
	     (set! in-evaluation? #f)
	     (mred:debug:printf 'console-threading "cleanup-evaluation: posting in-evaluation")
	     (semaphore-post in-evaluation-semaphore)
	     
	     (mred:debug:printf 'console-threading "cleanup-evaluation: turning off busy cursor")
	     (wx:end-busy-cursor)
	     (cleanup-transparent-io)
	     (send (get-frame) enable-evaluation)
	     (reset-break-state)
	     (begin-edit-sequence)	     
	     (set-caret-owner null wx:const-focus-display)
	     (if (thread-running? evaluation-thread)
		 (let ([c-locked? locked?])
		   (lock #f)
		   (insert-prompt)
		   (lock c-locked?)
		   (end-edit-sequence))
		 (begin (lock #t)
			(end-edit-sequence)
			(unless shutting-down?
			  (mred:message-box
			   (format "The evaluation thread is no longer running, ~
                                    so no evaluation can take place until ~
                                    the next execution.")
			   "Warning")))))]
	  [display-results
	   (lambda (anss)
	     (let ([c-locked? locked?])
	       (unless (andmap void? anss)
		 (begin-edit-sequence)
		 (lock #f)
		 (for-each display-result anss)
		 (lock c-locked?)
		 (end-edit-sequence))))]
	  [do-many-buffer-evals
	   (lambda (edit start end)
	     (exception-reporting-rep this)
	     (mred:debug:printf 'console-threading "do-many-buffer-evals: waiting in-evaluation")
	     (semaphore-wait in-evaluation-semaphore)
	     (mred:debug:printf 'console-threading "do-many-buffer-evals: passed in-evaluation")
	     (if in-evaluation?
		 (begin
		   (mred:debug:printf 'console-threading "do-many-buffer-evals: posting in-evaluation.1")
		   (semaphore-post in-evaluation-semaphore))
		 (begin
		   (set! in-evaluation? #t)
		   (mred:debug:printf 'console-threading "do-many-buffer-evals: posting in-evaluation.2")
		   (semaphore-post in-evaluation-semaphore)
		   (send (get-frame) disable-evaluation)
		   (cleanup-transparent-io)
		   (reset-pretty-print-width)
		   (ready-non-prompt)
		   (mred:debug:printf 'console-threading "do-many-buffer-evals: turning on busy cursor")
		   (wx:begin-busy-cursor)
		   (when should-collect-garbage?
		     (set! should-collect-garbage? #f)
		     (collect-garbage))
		   (reset-break-state)
		   (let ([evaluation-sucessful 'not-yet-evaluation-sucessful]
			 [cleanup-semaphore 'not-yet-cleanup-semaphore]
			 [thread-grace 'not-yet-thread-grace]
			 [thread-kill 'not-yet-thread-kill])
		     (run-in-evaluation-thread
		      (lambda ()
			(dynamic-wind
			 (lambda ()
			   (set! evaluation-sucessful  (make-semaphore 0))
			   (set! cleanup-semaphore (make-semaphore 1))
			   (set! thread-grace
				 (thread
				  (lambda ()
				    (semaphore-wait evaluation-sucessful)
				    (mred:debug:printf
				     'console-threading
				     "thread-grace passed.1")
				    (when (semaphore-try-wait? cleanup-semaphore)
				      (mred:debug:printf
				       'console-threading
				       "thread-grace passed.2")
				      (cleanup-evaluation)
				      (parameterize ([current-custodian drscheme:init:system-custodian])
					(kill-thread thread-kill)))
				    (mred:debug:printf
				     'console-threading
				     "thread-grace terminating (thread-kill running? ~s)"
				     (thread-running? thread-kill)))))
			   (set! thread-kill
				 (thread 
				  (lambda ()
				    (thread-wait evaluation-thread)
				    (mred:debug:printf
				     'console-threading
				     "thread-kill passed.1")
				    (when (semaphore-try-wait? cleanup-semaphore)
				      (mred:debug:printf
				       'console-threading
				       "thread-kill passed.2")
				      (cleanup-evaluation)
				      (parameterize ([current-custodian drscheme:init:system-custodian])
					(kill-thread thread-grace)))
				    (mred:debug:printf
				     'console-threading
				     "thread-kill terminating (thread-grace running? ~s)"
				     (thread-running? thread-grace))))))
			 (lambda ()
			   (process-edit edit
					 (lambda (expr recur)
					   (cond
					     [(process-finish? expr)
					      (semaphore-post evaluation-sucessful)]
					     [else
					      (let-values ([(answers error?) (send-scheme expr)])
						(display-results answers)
						(if error?
						    (semaphore-post
						     evaluation-sucessful)
						    (recur)))]))
					 start
					 end
					 #t))
			 (lambda () (void)))))))))])
	(public
	  [reset-break-state (lambda () (set! ask-about-kill? #f))]
	  [break-semaphore (make-semaphore 1)]
	  [break (lambda ()
		   (mred:debug:printf 'console-threading "break: waiting break.1")
		   (semaphore-wait break-semaphore)
		   (mred:debug:printf 'console-threading "break: passed break.1")
		   (cond
		     [(or (not in-evaluation?)
			  in-break?
			  (not evaluation-thread))
		      (mred:debug:printf 'console-threading "break: posting break.1.1")
		      (semaphore-post break-semaphore)
		      (void)]
		     [ask-about-kill? 
		      (set! in-break? #t)
		      (mred:debug:printf 'console-threading "break: posting break.1.2")
		      (semaphore-post break-semaphore)
		      (if (mred:get-choice
			   "Do you want to kill the evaluation?"
			   "Just Break"
			   "Kill"
			   "Kill?")
			  (break-thread evaluation-thread)
			  (kill-thread evaluation-thread))
		      (mred:debug:printf 'console-threading "break: waiting break.2")
		      (semaphore-wait break-semaphore)
		      (mred:debug:printf 'console-threading "break: passed break.2")
		      (set! in-break? #f)
		      (mred:debug:printf 'console-threading "break: posting break.2")
		      (semaphore-post break-semaphore)]
		     [else 
		      (break-thread evaluation-thread)
		      (set! ask-about-kill? #t)
		      (mred:debug:printf 'console-threading "break: posting break.1.3")
		      (semaphore-post break-semaphore)]))])
	(public
	  [current-thread-directory (current-directory)]
	  [escape
	   (lambda ()
	     (with-parameterization user-param
	       (lambda ()
		 ((error-escape-handler))))
	     (mred:message-box "error-escape-handler didn't escape"
			       "Error Escape")
	     (escape-handler))]
	  [exception-handler void]
	  [error-escape-k void]
	  [escape-handler
	   (rec drscheme-error-escape-handler
		(lambda ()
		  (error-escape-k (list (void)) #t)))]
	  [send-scheme 
	   (lambda (expr)
	     (let/ec k
	       (fluid-let ([error-escape-k k])
			  (dynamic-wind
			   (lambda () 
			     (current-directory current-thread-directory)
			     '(set! exception-handler
				    (lambda (exn)
				      (with-parameterization drscheme:init:system-parameterization
					(lambda ()
					  (report-exception-error exn)))
				      ((error-escape-handler))
				      (with-parameterization drscheme:init:system-parameterization
					(lambda ()
					  (mred:message-box "error-escape-handler didn't escape"
							    "Error Escape")))
				      (k (list (void)) #t))))
			   (lambda ()
			     (call-with-values
			      (lambda ()
				(with-parameterization user-param
				  (lambda ()
				    (syntax-checking-primitive-eval expr))))
			      (lambda anss
				(values anss #f))))
			   (lambda () 
			     (set! current-thread-directory (current-directory)))))))])
	(public
	  [evaluation-thread #f]
	  [run-in-evaluation-thread void]
	  [init-evaluation-thread
	   (lambda ()
	     (let ([run-function (lambda (f) (f))]
		   [init-eval-thread
		    (lambda ()
		      (current-parameterization drscheme:init:eval-thread-parameterization)
		      (error-escape-handler escape-handler))])
	       (let-values ([(evaluation-thread2 run-in-evaluation-thread2)
			     (parameterize ([current-custodian user-custodian])
			       (mzlib:thread@:consumer-thread
				run-function init-eval-thread))])
		 (set! run-in-evaluation-thread run-in-evaluation-thread2)
		 (set! evaluation-thread evaluation-thread2))))])
	(public
	  [userspace-load
	   (lambda (filename)
	     (with-parameterization drscheme:init:system-parameterization
	       (lambda ()
		 (let* ([p (with-parameterization user-param
			     (lambda ()
			       (open-input-file filename)))]
			[loc (zodiac:make-location 0 0 0 filename)]
			[chars (begin0
				 (list (read-char p) (read-char p) (read-char p) (read-char p))
				 (close-input-port p))]
			[process-sexps
			 (let ([last (list (void))])
			   (lambda (sexp recur)
			     (cond
			       [(process-finish? sexp)
				(if (process-finish-error? sexp)
				    (escape)
				    last)]
			       [else
				(set! last
				      (call-with-values
				       (lambda ()
					 (with-handlers ([(lambda (x) #t)
							  (lambda (exn) 
							    (report-exception-error exn))])
					   (with-parameterization user-param
					     (lambda ()
					       (syntax-checking-primitive-eval sexp)))))
				       (lambda x x)))
				(recur)])))])
		   (apply values 
			  (if (equal? chars (list #\W #\X #\M #\E))
			      (let ([edit (make-object drscheme:edit:edit%)])
				(send edit load-file filename)
				(process-edit edit process-sexps
					      0 
					      (send edit last-position)
					      #t))
			      (process-file filename process-sexps #t)))))))])
	(private 
	  [insert-delta
	   (lambda (s delta)
	     (let ([before (get-end-position)])
	       (insert s)
	       (let ([after (get-end-position)])
		 (change-style delta before after)
		 (values before after))))])
	(public
	  [takeover void]
	  [shutting-down? #f]
	  [shutdown 
	   (lambda ()
	     (set! shutting-down? #t)
	     (custodian-shutdown-all user-custodian)
	     (when evaluation-thread
	       (kill-thread evaluation-thread)))]
	  [repl-initially-active? #f] 
	  [reset-console
	   (let ([first-dir (current-directory)])
	     (lambda ()
	       (clear-previous-expr-positions)
	       (custodian-shutdown-all user-custodian)
	       (cleanup-transparent-io)
	       (set! should-collect-garbage? #t)
	       (lock #f) ;; locked if the thread was killed
	       (init-evaluation-thread)
	       (drscheme:language:set-use-zodiac)
	       (let ([p (build-parameterization user-custodian)])
		 (with-parameterization p
		   (lambda ()
		     (exception-reporting-rep this)
		     (current-custodian user-custodian)
		     (current-output-port this-out)
		     (current-error-port this-err)
		     (current-input-port this-in)
		     (current-load userspace-load)
		     (current-eval userspace-eval)
		     (exit-handler (lambda (arg)
				     (with-parameterization drscheme:init:system-parameterization
				       (lambda ()
					 (kill-thread evaluation-thread)))))
		     (set! current-thread-directory
			   (let/ec k
			     (unless (get-frame)
			       (k first-dir))
			     (let*-values ([(filename) (send (ivar (get-frame) definitions-edit)
							     get-filename)]
					   [(normalized) (if (string? filename)
							     (mzlib:file@:normalize-path filename)
							     (k first-dir))]
					   [(base _1 _2) (split-path normalized)])
			       base)))))
		 (set! user-param p))
	       (set! vocab (zodiac:create-vocabulary
			    'scheme-w/user-defined-macros/drscheme
			    zodiac:scheme-vocabulary))
	       (unless (mred:get-preference 'drscheme:keep-interactions-history)
		 (set-resetting #t)
		 (delete reset-console-start-position (last-position))
		 (set-prompt-mode #f)
		 (set-resetting #f))
	       (set-position (last-position) (last-position))
	       (insert-delta "Language: " WELCOME-DELTA)
	       (insert-delta 
		(symbol->string
		 (drscheme:language:setting-name 
		  (mred:get-preference
		   'drscheme:settings)))
		    RED-DELTA)
	       (insert-delta (format ".~n") WELCOME-DELTA)
	       (super-reset-console)
	       (set! repl-initially-active? #t)))]
	  [initialize-console
	   (lambda ()
	     (super-initialize-console)
	     
	     (let ([dd output-delta])
	       (insert-delta "Welcome to " WELCOME-DELTA)
	       (let-values ([(before after)
			     (insert-delta "DrScheme" CLICK-DELTA)])
		 (insert-delta (format ", version ~a.~n" (mred:version))
			       WELCOME-DELTA)
		 (set-clickback before after 
				(lambda args (drscheme:app:about-drscheme))
				CLICK-DELTA)))
	     (set-last-header-position (get-end-position))

	     (if (mred:get-preference 'drscheme:repl-always-active)
		 (reset-console)
		 (begin 
		   (insert-delta "Execute has not been clicked." WARNING-STYLE-DELTA)
		   (lock #t))))])
	
	(sequence
	  (mred:debug:printf 'super-init "before drscheme:rep:edit")
	  (apply super-init args)
	  (mred:debug:printf 'super-init "after drscheme:rep:edit")))))
  
  (define edit%
    (make-edit% mred:console-edit%)))
