
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
	  [basis : userspace:basis^]
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

  (define library-unit #f)

  (mred:set-preference-default 'drscheme:library-file
			       #f
			       (lambda (x) (or (string? x) (not x))))
  (mred:add-preference-callback 
   'drscheme:library-file
   (lambda (p v)
     (with-handlers
	 ([(lambda (x) #t)
	   (lambda (x)
	     (mred:message-box (exn-message x) "Invalid Library")
	     #f)])
       (if v
	   (let ([new-unit (load/cd v)])
	     (if (unit/sig? new-unit)
		 (set! library-unit new-unit)
		 (begin
		   (mred:message-box 
		    "Library file does not contain a unit"
		    "Invalid Library")
		   #f)))
	   (set! library-unit #f)))))
    
  (define exception-reporting-rep (make-parameter #f))

  (define (process-edit/zodiac edit f start end annotate?)
    (let ([setting (basis:current-setting)])
      (basis:process/zodiac
       (parameterize ([read-case-sensitive (basis:setting-case-sensitive?
					    setting)])
	 (zodiac:read (mred:read-snips/chars-from-buffer edit start end)
		      (zodiac:make-location 0 0 start edit)
		      #t 1))
       f
       annotate?)))

  (define (process-edit/no-zodiac edit f start end)
    (let* ([buffer-thunk (mred:read-snips/chars-from-buffer edit start end)]
	   [snip-string (string->list " 'non-text-snip ")]
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
      (basis:process/no-zodiac (lambda () (read port)) f)))

  (mred:set-preference-default 'drscheme:repl-always-active #f boolean?)

  (define (make-edit% super%)
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
	       line-start-position
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
      
      (public
	[init-transparent-io
	 (lambda (grab-focus?)
	   (begin-edit-sequence)
	   (super-init-transparent-io grab-focus?)
	   (when  (eq? (current-thread) evaluation-thread)
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
      (public
	[report-exception-error
	 (lambda (exn)
	   (if (exn? exn)
	       (report-located-error (exn-message exn) (exn-debug-info exn))
	       (report-unlocated-error (format "uncaught exception: ~e" exn))))]
	[report-located-error
	 (lambda (message di)
	   (if (and (zodiac:zodiac? di)
		    (basis:setting-use-zodiac? user-setting))
	       (let* ([start (zodiac:zodiac-start di)]
		      [finish (zodiac:zodiac-finish di)])
		 (report-error start finish 'dynamic message))
	       (report-unlocated-error message)))]
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
	[user-setting (mred:get-preference 'drscheme:settings)]
	[user-custodian (make-custodian)])
      (public
	[process-edit
	 (lambda (edit fn start end annotate?)
	   (if (basis:setting-use-zodiac? user-setting)
	       (process-edit/zodiac edit fn start end annotate?)
	       (process-edit/no-zodiac edit fn start end)))]
	[process-file
	 (lambda (filename fn annotate?)
	   (if (basis:setting-use-zodiac? user-setting)
	       (basis:process-file/zodiac filename fn annotate?)
	       (basis:process-file/no-zodiac filename fn)))]
	[process-sexp
	 (lambda (sexp z fn annotate?)
	   (if (basis:setting-use-zodiac? user-setting)
	       (basis:process-sexp/zodiac sexp z fn annotate?)
	       (basis:process-sexp/no-zodiac sexp fn)))])

      (private
	[in-evaluation? #f]
	[in-evaluation-semaphore (make-semaphore 1)]
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
	     (when (<= 5 count)
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

	[display-result
	 (lambda (v)
	   (unless (void? v)
	     (let ([v (if (basis:r4rs-style-printing? user-setting)
			  v
			  (print-convert:print-convert v))])
	       (parameterize ([mzlib:pretty-print@:pretty-print-size-hook
			       (lambda (x _ port) (and (is-a? x wx:snip%) 1))]
			      [mzlib:pretty-print@:pretty-print-print-hook
			       (lambda (x _ port) (this-result-write x))])
		 (mzlib:pretty-print@:pretty-print v this-result)))))]
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
			 (mred:debug:printf 'console-threading "initializing thread-grace and thread-kill")
			 (set! evaluation-sucessful  (make-semaphore 0))
			 (set! cleanup-semaphore (make-semaphore 1))
			 (set! thread-grace
			       (thread
				(lambda ()
				  (semaphore-wait evaluation-sucessful)
				  (mred:debug:printf 'console-threading "thread-grace passed.1")
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
			 (with-parameterization user-param
			   (lambda ()
			     (let/ec k
			       (fluid-let ([error-escape-k 
					    (lambda ()
					      (mred:debug:printf 'console-threading "error-escape-k: posting evaluation-sucessful")
					      (semaphore-post evaluation-sucessful)
					      (k (void)))])
				 (process-edit edit
					       (lambda (expr recur)
						 (mred:debug:printf 'console-threading "process-edit-f: looping~n")
						 (cond
						  [(basis:process-finish? expr)
						   (mred:debug:printf 'console-threading "process-edit-f: posting evaluation-sucessful")
						   (semaphore-post evaluation-sucessful)]
						  [else
						   (let ([answers (call-with-values
								   (lambda ()
								     (dynamic-enable-break
								      (lambda ()
									(if (basis:setting-use-zodiac? (basis:current-setting))
									    (basis:syntax-checking-primitive-eval expr)
									    (basis:primitive-eval expr)))))
								   (lambda x x))])
						     (display-results answers)
						     (recur))]))
					       start
					       end
					       #t))))))
		       (lambda () (void)))))))))])
      (private
	[shutdown-user-custodian
	 (lambda ()
	   (let* ([frame (get-frame)]
		  [interactions-edit (ivar frame interactions-edit)])
	     (send interactions-edit kill-allow-protected
		   (lambda ()
		     (custodian-shutdown-all user-custodian)))))])
      (public
	[reset-break-state (lambda () (set! ask-about-kill? #f))]
	[break (lambda ()
		 (cond
		  [(not evaluation-thread)
		   (void)]
		  [ask-about-kill? 
		   (if (mred:get-choice
			"Do you want to kill the evaluation?"
			"Just Break"
			"Kill"
			"Kill?")
		       (break-thread evaluation-thread)
		       (shutdown-user-custodian))]
		  [else 
		   (break-thread evaluation-thread)
		   (set! ask-about-kill? #t)]))])
      (public
	[error-escape-k void]
	[escape-handler
	 (rec drscheme-error-escape-handler
	      (lambda ()
		(mred:debug:printf 'console-threading "drscheme-error-escape-handler: escaping")
		(error-escape-k)))])

      (private
	[eval-thread-thunks null]
	[eval-thread-state-sema (make-semaphore 1)]
	[eval-thread-queue-sema (make-semaphore 0)])
      (public
	[evaluation-thread #f]
	[impl-thread #f]
	[impl-eventspace #f]
	[run-in-evaluation-thread 
	 (lambda (thunk)
	   (semaphore-wait eval-thread-state-sema)
	   (set! eval-thread-thunks (append eval-thread-thunks (list thunk)))
	   (semaphore-post eval-thread-state-sema)
	   (semaphore-post eval-thread-queue-sema))]
	[init-evaluation-thread
	 (lambda ()
	   (set! impl-thread (current-thread))
	   (set! impl-eventspace (wx:current-eventspace))
	   (let ([dummy-s (make-semaphore 0)])
	     (parameterize ([wx:current-eventspace
			     ((in-parameterization user-param
						   wx:current-eventspace))])
	       (semaphore-callback
		dummy-s
		(lambda ()
		  (dynamic-disable-break
		   (lambda ()
		     (set! evaluation-thread (current-thread))
		     (error-escape-handler escape-handler)
		     (let loop ()
		       (when (semaphore-try-wait? eval-thread-queue-sema)
			 (wx:yield eval-thread-queue-sema))
		       (semaphore-wait eval-thread-state-sema)
		       (let ([thunk (car eval-thread-thunks)])
			 (set! eval-thread-thunks (cdr eval-thread-thunks))
			 (semaphore-post eval-thread-state-sema)
			 (with-parameterization drscheme:init:system-parameterization
			   (lambda ()
			     (thunk))))
		       (loop))))))
	       (semaphore-post dummy-s))))])

      (public
	[userspace-load
	 (lambda (filename)
	   (with-parameterization drscheme:init:system-parameterization
	     (lambda ()
	       (if (basis:setting-use-zodiac? user-setting)
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
				[(basis:process-finish? sexp) last]
				[else
				 (set! last
				       (call-with-values
					(lambda () (basis:syntax-checking-primitive-eval sexp))
					(lambda x x)))
				 (recur)])))])
		     (with-parameterization user-param
		       (lambda ()
			 (apply values 
				(if (equal? chars (list #\W #\X #\M #\E))
				    (let ([edit (make-object drscheme:edit:edit%)])
				      (send edit load-file filename)
				      (process-edit edit process-sexps
						    0 
						    (send edit last-position)
						    #t))
				    (process-file filename process-sexps #t))))))
		   (with-parameterization user-param
		     (lambda ()
		       (drscheme:init:primitive-load filename)))))))])
      (private 
	[insert-delta
	 (lambda (s delta)
	   (let ([before (last-position)])
	     (insert s before before)
	     (let ([after (last-position)])
	       (change-style delta before after)
	       (values before after))))])
      (public
	[takeover void]
	[shutting-down? #f]
	[shutdown 
	 (lambda ()
	   (set! shutting-down? #t)
	   (shutdown-user-custodian))]
	[repl-initially-active? #f] 
	[reset-console
	 (let ([first-dir (current-directory)])
	   (lambda ()
	     (clear-previous-expr-positions)
	     (shutdown-user-custodian)
	     (cleanup-transparent-io)
	     (set! should-collect-garbage? #t)
	     (lock #f) ;; locked if the thread was killed
	     ;(drscheme:language:set-use-zodiac)

	     (begin-edit-sequence)
	     (if (mred:get-preference 'drscheme:keep-interactions-history)
		 (insert #\newline (last-position) (last-position))
		 (begin (set-resetting #t)
			(delete (line-start-position 1) (last-position))
			(set-prompt-mode #f)
			(set-resetting #f)))
	     (set-position (last-position) (last-position))
	     (insert-delta "Language: " WELCOME-DELTA)
	     (insert-delta 
	      (symbol->string
	       (basis:find-setting-name 
		(mred:get-preference
		 'drscheme:settings)))
	      RED-DELTA)
	     (insert-delta (format ".~n") WELCOME-DELTA)
	     (unless (mred:get-preference 'drscheme:keep-interactions-history)
	       (set-last-header-position (last-position)))
	     (set! repl-initially-active? #t)
	     (end-edit-sequence)


	     (set! user-setting (mred:get-preference 'drscheme:settings))
	     (let ([p (basis:build-parameterization
		       (mred:get-preference 'drscheme:settings)
		       (let ([l@
			      (unit/sig ()
				(import plt:userspace^)
				(when library-unit
				  (with-handlers ([(lambda (x) #t)
						   (lambda (x)
						     ((error-display-handler)
						      (format
						       "Invalid Library:~n~a"
						       (if (exn? x) (exn-message x) x))
						      "Invalid Library"))])
				    (invoke-open-unit/sig library-unit #f plt:userspace^))))])
			 (compound-unit/sig (import [params : plt:userspace:params^])
			   (link [userspace : plt:userspace^ 
					    ((require-library-unit/sig "gusrspcr.ss" "gusrspce")
					     params)]
				 [library : () (l@ userspace)])
			   (export (open userspace)))))])
	       (set! user-custodian ((in-parameterization p current-custodian)))
	       (with-parameterization p
		 (lambda ()
		   (exception-reporting-rep this)
		   (current-output-port this-out)
		   (current-error-port this-err)
		   (current-input-port this-in)
		   ;(current-load userspace-load)

		   (wx:current-eventspace (wx:make-eventspace))

		   (basis:error-display/debug-handler report-located-error)
		   
		   (error-display-handler
		    (rec drscheme-error-display-handler
			 (lambda (msg)
			   (let ([rep (exception-reporting-rep)])
			     (with-parameterization drscheme:init:system-parameterization
			       (lambda ()
				 (if rep
				     (send rep report-unlocated-error msg)
				     (mred:message-box msg "Uncaught Error"))))))))

		   (let ([directory
			  (let/ec k
			    (unless (get-frame)
			      (k first-dir))
			    (let*-values ([(filename) (send (ivar (get-frame) definitions-edit)
							    get-filename)]
					  [(normalized) (if (string? filename)
							    (mzlib:file@:normalize-path filename)
							    (k first-dir))]
					  [(base _1 _2) (split-path normalized)])
			      (or base 
				  first-dir)))])
		     (current-directory directory))

		   (let ([dispatch-handler (wx:event-dispatch-handler)]
			 [frame (get-frame)]
			 [semaphore (make-semaphore 1)]
			 [set-running-flag? #t]
			 [running-flag-on? #f]
			 [event-semaphore (make-semaphore 0)])
		     (thread (rec f
				  (lambda ()
				    (semaphore-wait event-semaphore)
				    (sleep 1/10)
				    (semaphore-wait semaphore)
				    (when (and set-running-flag?
					       (not running-flag-on?))
				      (set! running-flag-on? #t)
				      (send frame running))
				    (semaphore-post semaphore)
				    (f))))
		     (wx:event-dispatch-handler
		      (rec drscheme:event-dispatch-handler
			   (lambda (eventspace)
			     (semaphore-wait semaphore)
			     (set! set-running-flag? #t)
			     (semaphore-post semaphore)
			     (semaphore-post event-semaphore)
			     (fluid-let ([evaluation-thread (current-thread)])
			       (dispatch-handler eventspace))
			     (semaphore-wait semaphore)
			     (set! set-running-flag? #f)
			     (when running-flag-on?
			       (set! running-flag-on? #f)
			       (send frame not-running))
			     (semaphore-post semaphore)))))
		   
		   (exit-handler (lambda (arg)
				   (with-parameterization drscheme:init:system-parameterization
				     (lambda ()
				       (shutdown-user-custodian)))))))
	       (set! user-param p))
	     
	     (init-evaluation-thread)
	     (super-reset-console)))]
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
	   (set-last-header-position (last-position))

	   (if (mred:get-preference 'drscheme:repl-always-active)
	       (reset-console)
	       (begin 
		 (insert-delta "Execute has not been clicked." WARNING-STYLE-DELTA)
		 (lock #t))))])
      
      (sequence
	(mred:debug:printf 'super-init "before drscheme:rep:edit")
	(apply super-init args)
	(mred:debug:printf 'super-init "after drscheme:rep:edit"))))
  
  (define edit% (make-edit% mred:console-edit%)))
