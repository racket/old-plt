
(unit/sig drscheme:rep^
  (import [wx : wx^]
	  [mred : mred^]
	  [mzlib : mzlib:core^]
	  [print-convert : mzlib:print-convert^]
	  [zodiac : drscheme:zodiac^]
	  [zodiac:interface : drscheme:interface^]
	  [drscheme:init : drscheme:init^]
	  [drscheme:snip : drscheme:snip^]
	  [drscheme:language : drscheme:language^]
	  [drscheme:app : drscheme:app^]
	  [basis : userspace:basis^]
	  [drscheme:edit : drscheme:edit^])
  
  (define (printf . args) (apply fprintf mred:constants:original-output-port args))
  
  (mred:debug:printf 'invoke "drscheme:rep@")

  (define WELCOME-DELTA (make-object wx:style-delta%
				     wx:const-change-family
				     wx:const-decorative))
  (define CLICK-DELTA (make-object wx:style-delta%))
  (define RED-DELTA (make-object wx:style-delta%))
  (send* CLICK-DELTA
    (copy WELCOME-DELTA)
    (set-delta-foreground "BLUE")
    (set-delta wx:const-change-underline #t))
  (send* RED-DELTA
    (copy WELCOME-DELTA)
    (set-delta-foreground "RED"))
  (define WARNING-STYLE-DELTA (make-object wx:style-delta% wx:const-change-bold))
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
	       this-err this-err-write 
	       this-out this-out-write
	       this-in
	       this-result this-result-write
	       output-delta set-output-delta
	       do-post-eval
	       insert-prompt
	       erase prompt-mode?
	       get-canvas
	       ready-non-prompt autoprompting?
	       set-prompt-mode
	       delete lock locked?
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
		       (string-append (basis:format-source-loc start-location end-location)
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
	 (lambda (thread-to-watch)
	   (mred:debug:printf 'console-threading "cleanup-evaluation: turning off busy cursor")
	   (wx:end-busy-cursor)
	   (cleanup-transparent-io)
	   (send (get-frame) enable-evaluation)
	   (begin-edit-sequence)	     
	   (set-caret-owner null wx:const-focus-display)

	   (if (thread-running? thread-to-watch)
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
	   (mred:debug:printf 'console-threading "do-many-buffer-evals: testing in-evaluation: ~a" in-evaluation?)
	   (unless in-evaluation?
	     (mred:debug:printf 'console-threading "do-many-buffer-evals: posting in-evaluation.2")
	     (send (get-frame) disable-evaluation)
	     (reset-break-state)
	     (cleanup-transparent-io)
	     (reset-pretty-print-width)
	     (ready-non-prompt)
	     (mred:debug:printf 'console-threading "do-many-buffer-evals: turning on busy cursor")
	     (wx:begin-busy-cursor)
	     (when should-collect-garbage?
	       (set! should-collect-garbage? #f)
	       (collect-garbage))
	     (run-in-evaluation-thread
	      (lambda ()
		(mred:debug:printf 'console-threading "do-many-buffer-evals: in evaluation thread")
		(protect-user-evaluation
		 cleanup-evaluation
		 (lambda ()
		   (with-parameterization user-param
		     (lambda ()
		       (process-edit edit
				     (lambda (expr recur)
				       (mred:debug:printf 'console-threading "process-edit-f: looping")
				       (cond
					[(basis:process-finish? expr)
					 (void)]
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
				     #t)))))))))])
      (private
	[shutdown-user-custodian
	 (lambda ()
	   (let* ([frame (get-frame)]
		  [interactions-edit (ivar frame interactions-edit)])
	     (set! in-evaluation? #f)
	     (send (get-frame) not-running)

	     ;; this thread is created to run the actual shutdown, in
	     ;; case the custodian is going to shutdown the current
	     ;; thread!  The semaphore is there for when the case when
	     ;; current thread is not shutdown.
	     (let ([sema (make-semaphore 0)])
	       (parameterize ([current-custodian drscheme:init:system-custodian])
		 (thread (lambda ()
			   (send interactions-edit kill-allow-protected
				 (lambda ()
				   (custodian-shutdown-all user-custodian)))
			   (semaphore-post sema))))
	       (semaphore-wait sema))))])
      (public
	[reset-break-state (lambda () (set! ask-about-kill? #f))]
	[breakable-thread #f]
	[break (lambda ()
		 (cond
		  [(or (not in-evaluation?)
		       (not breakable-thread))
		   (wx:bell)]
		  [ask-about-kill? 
		   (if (mred:get-choice
			"Do you want to kill the evaluation?"
			"Just Break"
			"Kill"
			"Kill?")
		       (break-thread breakable-thread)
		       (begin 
			 (shutdown-user-custodian)))]
		  [else 
		   (break-thread breakable-thread)
		   (set! ask-about-kill? #t)]))])
      (public
	[error-escape-k void])

      (private
	[eval-thread-thunks null]
	[eval-thread-state-sema (make-semaphore 1)]
	[eval-thread-queue-sema (make-semaphore 0)]

	[yield-count 0]
	
	[evaluation-sucessful 'not-yet-evaluation-sucessful]
	[cleanup-sucessful 'not-yet-cleanup-sucessful]
	[cleanup-semaphore 'not-yet-cleanup-semaphore]
	[thread-grace 'not-yet-thread-grace]
	[thread-kill 'not-yet-thread-kill]

	[protect-user-evaluation
	 (lambda (cleanup thunk)
	   (mred:debug:printf 'console-threading "waiting in-evaluation-semaphore.1")
	   (semaphore-wait in-evaluation-semaphore)
	   (set! in-evaluation? #t)
	   (semaphore-post in-evaluation-semaphore)
	   (mred:debug:printf 'console-threading "finished with in-evaluation-semaphore.1")

	   ;; this is evaluation-thread, unless that was kill and user event
	   ;; callbacks are still being handled. in that case, it will be
	   ;; whatever thread is handling the callback
	   (let ([thread-to-watch (current-thread)])

	     (mred:debug:printf 'console-threading "protect-user-evaluation: initializing thread-grace and thread-kill")
	     (fluid-let ([breakable-thread thread-to-watch]
			 [evaluation-sucessful  (make-semaphore 0)]
			 [cleanup-semaphore (begin
					      (mred:debug:printf 'console-threading "protect-user-evaluation: setting cleanup semaphore")
					      (make-semaphore 1))]
			 [cleanup-sucessful (make-semaphore 0)]
			 [thread-grace
			  (thread
			   (lambda ()
			     (semaphore-wait evaluation-sucessful)
			     (mred:debug:printf 'console-threading "thread-grace passed.1")

			     (semaphore-wait cleanup-semaphore)
			     (mred:debug:printf 'console-threading "thread-grace passed.2")

			     (parameterize ([current-custodian drscheme:init:system-custodian])
			       (kill-thread thread-kill))
			     (mred:debug:printf
			      'console-threading
			      "after terminating thread-kill (thread-kill running? ~s)"
			      (thread-running? thread-kill))


			     (cleanup thread-to-watch)
			     (mred:debug:printf 'console-threading "posting cleanup-sucessful.1")
			     (semaphore-post cleanup-sucessful)))]
			 [thread-kill
			  (thread 
			   (lambda ()
			     (thread-wait thread-to-watch)
			     (mred:debug:printf 'console-threading "thread-kill passed.1")

			     (semaphore-wait cleanup-semaphore)
			     (mred:debug:printf 'console-threading "thread-kill passed.2")

			     (parameterize ([current-custodian drscheme:init:system-custodian])
			       (kill-thread thread-grace))
			     (mred:debug:printf
			      'console-threading
			      "after terminating thread-grage (thread-grace running? ~s)"
			      (thread-running? thread-grace))

			     (cleanup thread-to-watch)
			     (mred:debug:printf 'console-threading "posting cleanup-sucessful.2")
			     (semaphore-post cleanup-sucessful)))])

	       (let/ec k
		 (fluid-let ([error-escape-k 
			      (lambda ()
				(mred:debug:printf 'console-threading "error-escape-k: posting evaluation-sucessful")
				(semaphore-post evaluation-sucessful)
				(k (void)))])

		   (mred:debug:printf 'console-threading "protect-user-evaluation: running thunk")
		   (thunk)
		   (mred:debug:printf 'console-threading "protect-user-evaluation: finished thunk")))

	       (mred:debug:printf 'console-threading "protect-user-evaluation: posting evaluation-sucessful")
	       (semaphore-post evaluation-sucessful)
	       (mred:debug:printf 'console-threading "waiting cleanup-sucessful")
	       (semaphore-wait cleanup-sucessful)
	       (mred:debug:printf 'console-threading "passed cleanup-sucessful")


	       (mred:debug:printf 'console-threading "waiting in-evaluation-semaphore.2")
	       (semaphore-wait in-evaluation-semaphore)
	       (set! in-evaluation? #f)
	       (semaphore-post in-evaluation-semaphore)
	       (mred:debug:printf 'console-threading "finished with in-evaluation-semaphore.2"))))])
      (public
	[evaluation-thread #f]
	[run-in-evaluation-thread 
	 (lambda (thunk)
	   (semaphore-wait eval-thread-state-sema)
	   (set! eval-thread-thunks (append eval-thread-thunks (list thunk)))
	   (semaphore-post eval-thread-state-sema)
	   (semaphore-post eval-thread-queue-sema))]
	[init-evaluation-thread
	 (lambda (user-param first-box)

	   ;; this semaphore is only used to get time on the user eventspace's
	   ;; event handling thread
	   (let ([dummy-s (make-semaphore 0)])


	     (parameterize ([wx:current-eventspace
			     ((in-parameterization user-param
						   wx:current-eventspace))])
	       (semaphore-callback
		dummy-s
		(lambda ()
		  (mzlib:function@:dynamic-disable-break
		   (lambda ()
		     (set-box! first-box #f)
		     (let ([escape-handler
			    (rec drscheme-error-escape-handler
				 (lambda ()
				   (mred:debug:printf 'console-threading "drscheme-error-escape-handler: escaping")
				   (error-escape-k)))])
		       (error-escape-handler escape-handler)
		       (basis:bottom-escape-handler escape-handler))

		     (set! yield-count 0)
		     (send (get-frame) not-running)
		     (set! evaluation-thread (current-thread))
		     (let loop ()
		       (unless (semaphore-try-wait? eval-thread-queue-sema)
			 (fluid-let ([yield-count (+ yield-count 1)])
			   (wx:yield eval-thread-queue-sema)))
		       (semaphore-wait eval-thread-state-sema)
		       (let ([thunk (car eval-thread-thunks)])
			 (set! eval-thread-thunks (cdr eval-thread-thunks))
			 (semaphore-post eval-thread-state-sema)
			 (dynamic-wind
			  (lambda () (send (get-frame) running))
			  (lambda ()
			    (with-parameterization drscheme:init:system-parameterization
			      (lambda ()
				(thunk))))
			  (lambda () (send (get-frame) not-running))))
		       (loop))))))
	       (semaphore-post dummy-s))))])

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
	     (mred:debug:printf 'console-threading "initializing repl")
	     (clear-previous-expr-positions)
	     (shutdown-user-custodian)
	     (cleanup-transparent-io)
	     (set! should-collect-garbage? #t)

	     ;; in case the last evaluation thread will killed, clean up some state.
	     (lock #f)
	     (set! in-evaluation? #f)

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
		       (list 'wx)
		       (mred:get-preference 'drscheme:settings)

		       (lambda (in-<=-at-least-two-args
				in-allow-improper-lists
				in-eq?-only-compares-symbols
				parameterization)
			 (let ([u
				(compound-unit/sig (import)
				  (link [params : plt:userspace:params^
						((unit/sig plt:userspace:params^
						   (import)
						   (define <=-at-least-two-args in-<=-at-least-two-args)
						   (define allow-improper-lists in-allow-improper-lists)
						   (define eq?-only-compares-symbols in-eq?-only-compares-symbols)))]
					[userspace : plt:userspace^ 
						   ((require-library-unit/sig "gusrspcr.ss" "gusrspce")
						    params)]
					[library : () ((unit/sig ()
							 (import plt:userspace^)
							 (when library-unit
							   (with-handlers ([(lambda (x) #t)
									    (lambda (x)
									      ((error-display-handler)
									       (format
										"Invalid Library:~n~a"
										(if (exn? x) (exn-message x) x))
									       "Invalid Library"))])
							     (invoke-open-unit/sig library-unit #f plt:userspace^))))
						       userspace)])
				  (export (open userspace)))])
			   (with-parameterization parameterization
			     (lambda ()
			       (invoke-open-unit/sig u))))))])

	       (mred:debug:printf 'console-threading "built parameterization")
	       (set! user-custodian ((in-parameterization p current-custodian)))
	       (with-parameterization p
		 (lambda ()

		   (unless (basis:setting-use-zodiac? (basis:current-setting))
		     (require-library "sig.ss" "mred"))

		   (exception-reporting-rep this)
		   (current-output-port this-out)
		   (current-error-port this-err)
		   (current-input-port this-in)
		   
		   (global-port-print-handler
		    (let ([old (global-port-print-handler)])
		      (lambda (value port)
			(if (or (eq? port this-result)
				(eq? port this-out)
				(eq? port this-err))
			    (parameterize ([mzlib:pretty-print@:pretty-print-size-hook
					    (lambda (x _ port) (and (is-a? x wx:snip%) 1))]
					   [mzlib:pretty-print@:pretty-print-print-hook
					    (lambda (x _ port)
					      (evcase port
						[this-result (this-result-write x)]
						[this-out (this-out-write x)]
						[this-err (this-err-write x)]))])
			      (old value port))
			    (old value port)))))

		   (print-convert:current-print-convert-hook
		    (lambda (expr basic-convert sub-convert)
		      (let ([ans (if (is-a? expr wx:snip%)
				     expr
				     (basic-convert expr))])
			ans)))

		   (current-load
		    (let ([userspace-load (current-load)])
		      (rec drscheme-load-handler
			   (lambda (filename)
			     (unless (string? filename)
			       (raise (make-exn:application:arity
				       (format "drscheme-load-handler: expects argument of type <string>; given: ~e" filename)
				       ((debug-info-handler))
				       filename
				       'string)))
			     (if (and (basis:setting-use-zodiac? user-setting)
				      (let* ([p (open-input-file filename)]
					     [loc (zodiac:make-location basis:INITIAL-LINE
									basis:INITIAL-COLUMN
									basis:INITIAL-OFFSET
									filename)]
					     [chars (begin0
						     (list (read-char p) (read-char p) (read-char p) (read-char p))
						     (close-input-port p))])
					(equal? chars (string->list "WXME"))))
				 (let ([process-sexps
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
				   (apply values 
					  (let ([edit (with-parameterization drscheme:init:system-parameterization
							(lambda ()
							  (make-object drscheme:edit:edit%)))])
					    (with-parameterization drscheme:init:system-parameterization
					      (lambda ()
						(send edit load-file filename)))
					    (process-edit edit process-sexps
							  0 
							  (send edit last-position)
							  #t))))
				 (userspace-load filename))))))


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

		   (exit-handler (lambda (arg)
				   (with-parameterization drscheme:init:system-parameterization
				     (lambda ()
				       (shutdown-user-custodian)))))
		   
		   ;; set all parameter before constructing eventspace
		   ;; so that the parameters are set in the eventspace's
		   ;; parameterization
		   (let* ([user-eventspace #f]
			  [primitive-dispatch-handler (wx:event-dispatch-handler)]
			  [frame (get-frame)]
			  [semaphore (make-semaphore 1)]
			  [set-running-flag? #t]
			  [running-flag-on? #f]
			  [event-semaphore (make-semaphore 0)]

			  [first-box (box #t)]

			  [dispatch-handler-procedure primitive-dispatch-handler])

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
		      (rec drscheme-event-dispatch-handler
			   (lambda (eventspace)
			     (when (and (eq? eventspace user-eventspace)
					(not (unbox first-box)))
			       (mred:debug:printf 'console-threading "drscheme-event-dispatch-handler")

			       (semaphore-wait semaphore)
			       (set! set-running-flag? #t)
			       (semaphore-post semaphore)
			       (semaphore-post event-semaphore)

			       (reset-break-state)

			       (protect-user-evaluation
				void
				(lambda ()
				  (dynamic-enable-break
				   (lambda ()
				     (primitive-dispatch-handler eventspace)))))

			       (semaphore-wait semaphore)
			       (set! set-running-flag? #f)
			       (when running-flag-on?
				 (set! running-flag-on? #f)
				 (send frame not-running))
			       (semaphore-post semaphore)))))

		     (set! user-eventspace (wx:make-eventspace))
		     (wx:current-eventspace user-eventspace)
		     (init-evaluation-thread p first-box)

		     ;; this subterfuge with the extra variable indirection is necessary
		     ;; so that the correct event-dispatch-handler is installed in
		     ;; eventspace's parameterization, but the real dispatch handler
		     ;; cannot actually be installed until after init-evaluation-thread
		     ;; returns, since initializing the evaluation thread requires dispatching
		     ;; on an event. (could have used a boolean flag "first-event?" but this
		     ;; seems better for all the events after the first one. No more test,
		     ;; only one extra indirection (probably doens't really matter...))
		     '(set! dispatch-handler-procedure real-dispatch-handler-procedure))))
	       (mred:debug:printf 'console-threading "extended parameterization")

	       (set! user-param p))
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
