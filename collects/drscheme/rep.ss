(unit/sig drscheme:rep^
  (import [mred : mred^]
	  [mzlib : mzlib:core^]
	  [print-convert : mzlib:print-convert^]
	  [aries : plt:aries^]
	  [zodiac : zodiac:system^]
	  [zodiac:interface : drscheme:interface^]
	  [drscheme:snip : drscheme:snip^]
	  [drscheme:language : drscheme:language^]
	  [drscheme:app : drscheme:app^]
	  [drscheme:basis : drscheme:basis^]
	  [drscheme:edit : drscheme:edit^])
  
  (mred:debug:printf 'invoke "drscheme:rep@")
  
  (define top-parameterization (current-parameterization))
  (define system-parameterization (make-parameterization top-parameterization))
  (define eval-thread-parameterization (make-parameterization top-parameterization))
  (define system-custodian (current-custodian))
  (current-parameterization system-parameterization)
  (parameterization-branch-handler
   (lambda ()
     (make-parameterization system-parameterization)))
	  
  (print-struct #t)
  (break-enabled #f)
  ((in-parameterization eval-thread-parameterization break-enabled) #f)
  ((in-parameterization eval-thread-parameterization print-struct) #t)
  (define primitive-eval (current-eval))
  (define primitive-load (current-load))

  (error-display-handler
   (lambda (msg)
     (with-parameterization system-parameterization
       (lambda ()
	 (display msg)
	 (newline)
	 (mred:message-box (format "Internal Error: ~a" msg)
			   "Internal Error")))))
  
  (define report-exception-error
    (lambda (exn last-resort-edit)
      (if (exn? exn)
	  (let ([di (exn-debug-info exn)])
	    (if (zodiac:zodiac? di)
		(let* ([start (zodiac:zodiac-start di)]
		       [finish (zodiac:zodiac-finish di)]
		       [file (zodiac:location-file start)]
		       [edit (if (is-a? file wx:media-edit%)
				 file
				 last-resort-edit)]
		       [frame (send edit get-frame)]
		       [interactions (ivar frame interactions-edit)])
		  (send interactions report-error start finish
			'dynamic (exn-message exn)))
		(mred:message-box
		 (format "~a" (exn-message exn))
		 "Uncaught Exception")))
	  (mred:message-box (format "~s" exn)
			    "Uncaught Exception"))))

  (define build-parameterization
    (let ([orig-eventspace (wx:current-eventspace)])
      (lambda (user-custodian last-resort-edit)
	(let* ([p (make-parameterization eval-thread-parameterization)]
	       [bottom-eventspace (parameterize ([current-custodian user-custodian])
				    (wx:make-eventspace p))]
	       [n (make-namespace 'no-constants 'wx 'hash-percent-syntax)]
	       [exception-handler
		(lambda (exn)
		  (with-parameterization system-parameterization
		    (lambda ()
		      (report-exception-error exn last-resort-edit)))
		  ((error-escape-handler)))])
	  (with-parameterization p
	    (lambda ()
	      (parameterization-branch-handler
	       (lambda ()
		 (let* ([new (make-parameterization p)])
		   ((in-parameterization new current-exception-handler)
		    exception-handler)
		   new)))
	      (require-library-use-compiled #f)
	      (current-custodian user-custodian)
	      (error-value->string-handler
	       (lambda (x n)
		 (let* ([port (open-output-string)]
			[error-value
			 (if (eq? (drscheme:language:setting-printing
				   (mred:get-preference 'drscheme:settings)) 
				  'r4rs-style)
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
			 short-string)))))
	      (debug-info-handler (lambda () (unbox aries:error-box)))
	      (current-namespace n)
	      (eval `(#%define plt:home-directory ,mred:constants:plt-home-directory))
	      (break-enabled #t)
	      (wx:current-eventspace bottom-eventspace)
	      ;(wx:eventspace-parameterization bottom-eventspace p)
	      (current-will-executor (make-will-executor))
	      (read-curly-brace-as-paren #t)
	      (read-square-bracket-as-paren #t)
	      (print-struct #t)
	      (error-print-width 250)))
	  (drscheme:basis:add-basis n bottom-eventspace user-custodian)
	  (with-parameterization p
	    (lambda ()
	      (eval
	       `(allow-improper-lists
		 ,(drscheme:language:setting-allow-improper-lists?
		   (mred:get-preference 'drscheme:settings))))
	      (eval
	       `(eq?-only-compares-symbols?
		 ,(drscheme:language:setting-eq?-only-compares-symbols?
		   (mred:get-preference 'drscheme:settings))))))
	  p))))

  (define-struct process/zodiac-finish (error?))

  (define make-edit%
    (lambda (super%)
      (class super% args
	(inherit insert change-style
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
		 scroll-to-position)
	(rename
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
	  [init-transparent-io-do-work
	   (lambda (grab-focus?)
	     (with-parameterization system-parameterization
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
	  [report-error
	   (lambda (start-location end-location type input-string)
	     (let* ([start (zodiac:location-offset start-location)]
		    [finish (add1 (zodiac:location-offset end-location))]
		    [file (zodiac:location-file start-location)]
		    [frame (get-frame)]
		    [interactions-edit (ivar frame interactions-edit)]
		    [message
		     (if (is-a? file wx:media-edit%)
			 input-string
			 (format "~a: ~a.~a-~a.~a: ~a" file
				 (zodiac:location-line start-location)
				 (zodiac:location-column start-location)
				 (zodiac:location-line end-location)
				 (zodiac:location-column end-location)
				 input-string))])
	       (send frame ensure-interactions-shown)
	       (let ([locked? (ivar interactions-edit locked?)])
		 (send* interactions-edit
		   (begin-edit-sequence)
		   (lock #f)
		   (this-err-write (string-append message (string #\newline)))
		   (lock locked?)
		   (end-edit-sequence)))
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
	  [param #f]
	  [vocab #f]
	  [user-custodian (make-custodian)]
	  
	  [userspace-eval
	   (lambda (sexp)
	     (with-parameterization system-parameterization
	       (lambda ()
		 (let* ([z (or (unbox aries:error-box)
			       (let ([loc (zodiac:make-location 0 0 0 'eval)])
				 (zodiac:make-zodiac 'mzrice-eval loc loc)))]
			[reader
			 (let ([gone #f])
			   (lambda ()
			     (or gone
				 (begin (set! gone (zodiac:make-eof z))
					(zodiac:structurize-syntax sexp z)))))]
			[answer (list (void))]
			[f
			 (lambda (annotated recur)
			   (if (process/zodiac-finish? annotated)
			       (if (process/zodiac-finish-error? annotated)
				   (escape)
				   answer)
			       (begin (set! answer
					    (call-with-values
					     (lambda ()
					       (with-parameterization param
						 (lambda ()
						   (primitive-eval annotated))))
					     (lambda x x)))
				      (recur))))])
		   (apply values (process/zodiac reader f #t))))))]
	  [display-result
	   (lambda (v)
	     (unless (void? v)
	       (let ([v (if (eq? (drscheme:language:setting-printing
				  (mred:get-preference 'drscheme:settings))
				 'r4rs-style)
			    v
			    (print-convert:print-convert v))])
		 (parameterize ([mzlib:pretty-print@:pretty-print-size-hook
				 (lambda (x _ port) (and (is-a? x wx:snip%) 1))]
				[mzlib:pretty-print@:pretty-print-print-hook
				 (lambda (x _ port) (this-result-write x))])
		   (mzlib:pretty-print@:pretty-print v this-result)))))]
	  [process-edit/zodiac
	   (lambda (edit f start end annotate?)
	     (process/zodiac
	      (zodiac:read (mred:read-snips/chars-from-buffer edit start end)
			   (zodiac:make-location 0 0 start edit))
	      f
	      annotate?))]
	  [process-file/zodiac
	   (lambda (filename f annotate?)
	     (let ([port (open-input-file filename)])
	       (process/zodiac
		(zodiac:read port (zodiac:make-location 0 0 0 filename))
		(lambda (x r)
		  (when (process/zodiac-finish? x)
		    (close-input-port port))
		  (f x r))
		annotate?)))]
	  [process/zodiac
	   (lambda (reader f annotate?)
	     (let* ([cleanup
		     (lambda (error?)
		       (f (make-process/zodiac-finish error?) void))])
	       (let loop ()
		 (let ([next-iteration
			(with-handlers ([zodiac:interface:zodiac-exn?
					 (lambda (exn)
					   (report-error (zodiac:interface:zodiac-exn-start-location exn)
							 (zodiac:interface:zodiac-exn-end-location exn)
							 (zodiac:interface:zodiac-exn-type exn)
							 (zodiac:interface:zodiac-exn-message exn))
					   (lambda () (cleanup #t)))])
			  (let ([zodiac-read (reader)])
			    (if (zodiac:eof? zodiac-read)
				(lambda () (cleanup #f))
				(let* ([exp (call/nal
					     zodiac:scheme-expand/nal
					     zodiac:scheme-expand
					     (expression: zodiac-read)
					     (vocabulary: vocab)
					     (parameterization: param))]
				       [heading-out (if annotate? 
							(aries:annotate exp)
							exp)])
				  (mred:debug:when 'drscheme:sexp
						   (mzlib:pretty-print@:pretty-print heading-out))
				  (lambda () (f heading-out loop))))))])
		   (next-iteration)))))])
	(private
	  [in-evaluation? #f]
	  [in-evaluation-semaphore (make-semaphore 1)]
	  [in-break? #f]
	  [should-collect-garbage? #f]
	  [ask-about-kill? #f])
	(public
	  [insert-warning
	   (let ([warning-style-delta
		  (make-object wx:style-delta% wx:const-change-bold 0)])
	     (send* warning-style-delta
	       (set-delta-foreground "BLACK")
	       (set-delta-background "YELLOW"))
	     (lambda ()
	       (begin-edit-sequence)
	       (insert #\newline (last-position) (last-position))
	       (let ([start (last-position)])
		 (insert "WARNING: Program has changed. Click Execute."
			 start start)
		 (let ([end (last-position)])
		   (change-style warning-style-delta start end)))
	       (end-edit-sequence)))]
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
	     (send (get-frame) enable-evaluation)
	     (reset-break-state)
	     (if (thread-running? evaluation-thread)
		 (begin 
		   (let ([c-locked? locked?])
		     (begin-edit-sequence)
		     (lock #f)
		     (insert-prompt)
		     (lock c-locked?)
		     (end-edit-sequence)))
		 (begin (lock #t)
			(unless shutting-down?
			  (mred:message-box
			   (format "The evaluation thread is no longer running, ~
                                    so no evaluation can take place until ~
                                    the next execution.")
			   "Warning")))))]
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
		   (ready-non-prompt)
		   (mred:debug:printf 'console-threading "do-many-buffer-evals: turning on busy cursor")
		   (wx:begin-busy-cursor)
		   (when should-collect-garbage?
		     (set! should-collect-garbage? #f)
		     (collect-garbage))
		   (reset-break-state)
		   (let ([evaluation-sucessful (make-semaphore 0)]
			 [cleanup-semaphore (make-semaphore 1)])
		     (letrec ([thread-grace 
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
				    (parameterize ([current-custodian system-custodian])
				      (kill-thread thread-kill)))
				  (mred:debug:printf
				   'console-threading
				   "thread-grace terminating (thread-kill running? ~s)"
				   (thread-running? thread-kill))))]
			      [thread-kill 
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
				    (parameterize ([current-custodian system-custodian])
				      (kill-thread thread-grace)))
				  (mred:debug:printf
				   'console-threading
				   "thread-kill terminating (thread-grace running? ~s)"
				   (thread-running? thread-grace))))])
		       (void))
		     (process-edit/zodiac edit
					  (lambda (expr recur)
					    (cond
					      [(process/zodiac-finish? expr)
					       (semaphore-post evaluation-sucessful)]
					      [else
					       (send-scheme expr 
							    (lambda (error?)
							      (if error?
								  (semaphore-post
								   evaluation-sucessful)
								  (recur))))]))
					  start
					  end
					  #t)))))])
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
	  [send-scheme (opt-lambda (x [after void]) (void))]
	  [evaluation-thread #f]
	  [current-thread-directory (current-directory)]
	  [escape
	   (lambda ()
	     (with-parameterization param
	       (lambda ()
		 ((error-escape-handler))))
	     (mred:message-box "error-escape-handler didn't escape"
			       "Error Escape")
	     (error-escape-handler))]
	  [init-evaluation-thread
	   (lambda ()
	     (parameterize ([current-custodian user-custodian])
	       (let-values 
		   ([(evaluation-thread2 send-scheme2)
		     (let* ([error-escape-k void]
			    [escape-handler
			     (lambda ()
			       (error-escape-k #t))])
		       (mzlib:thread@:consumer-thread
			(opt-lambda (expr [after void])
			  (let/ec k
			    (set! error-escape-k k)
			    (let* ([user-code-error? #t])
			      (dynamic-wind
			       (lambda ()
				 (current-directory current-thread-directory))
			       (lambda ()
				 (let/ec k
				   (call-with-values
				    (lambda ()
				      (parameterize ([current-exception-handler
						      (lambda (exn)
							(with-parameterization system-parameterization
							  (lambda ()
							    (report-exception-error exn this)))
							((error-escape-handler))
							(with-parameterization system-parameterization
							  (lambda ()
							    (mred:message-box "error-escape-handler didn't escape"
									      "Error Escape")
							    (set! user-code-error? #t)))
							(k (void)))])
					(with-parameterization param
					  (lambda ()
					    (primitive-eval expr)))))
				    (lambda anss
				      (let ([c-locked? locked?])
					(unless (andmap void? anss)
					  (begin-edit-sequence)
					  (lock #f)
					  (for-each display-result anss)
					  (lock c-locked?)
					  (end-edit-sequence)))))
				   (set! user-code-error? #f)))
			       (lambda () 
				 (set! current-thread-directory (current-directory))
				 (after user-code-error?))))))
			(lambda ()
			  (current-parameterization eval-thread-parameterization)
			  (error-escape-handler escape-handler))))])
		 (set! send-scheme send-scheme2)
		 (set! evaluation-thread evaluation-thread2))))])
	(public
	  [userspace-load
	   (lambda (filename)
	     (with-parameterization system-parameterization
	       (lambda ()
		 (unless (and (file-exists? filename)
			      (member 'read (file-or-directory-permissions filename)))
		   (with-parameterization param
		     (lambda () (primitive-load filename))))
		 (let* ([p (open-input-file filename)]
			[loc (zodiac:make-location 0 0 0 filename)]
			[chars (begin0
				 (list (read-char p) (read-char p) (read-char p) (read-char p))
				 (close-input-port p))]
			[user-load-relative-directory (in-parameterization param current-load-relative-directory)]
			[old-load-relative-directory (user-load-relative-directory)])
		   (dynamic-wind
		    (lambda ()
		      (let-values ([(base name must-be-dir?)
				    (split-path
				     (mzlib:file@:normalize-path filename))])
			(user-load-relative-directory base)))
		    (lambda ()
		      (let ([process-sexps
			     (let ([last (list (void))])
			       (lambda (sexp recur)
				 (cond
				   [(process/zodiac-finish? sexp)
				    (if (process/zodiac-finish-error? sexp)
					(escape)
					last)]
				   [else
				    (set! last
					  (call-with-values
					   (lambda ()
					     (with-handlers ([(lambda (x) #t)
							      (lambda (exn)
								(report-exception-error exn this))])
					       (with-parameterization param
						 (lambda ()
						   (primitive-eval sexp)))))
					   (lambda x x)))
				    (recur)])))])
			(apply values 
			      (if (equal? chars (list #\W #\X #\M #\E))
				  (let ([edit (make-object drscheme:edit:edit%)])
				    (send edit load-file filename)
				    (process-edit/zodiac edit process-sexps
							 0 (send edit last-position)
							 #t))
				  (process-file/zodiac filename process-sexps #t)))))
		    (lambda ()
		      (user-load-relative-directory old-load-relative-directory)))))))])
	(public
	  [takeover void]
	  [shutting-down? #f]
	  [shutdown 
	   (lambda ()
	     (set! shutting-down? #t)
	     (custodian-shutdown-all user-custodian)
	     (kill-thread evaluation-thread))]
	  [reset-console
	   (let ([first-dir (current-directory)])
	     (lambda ()
	       (clear-previous-expr-positions)
	       (custodian-shutdown-all user-custodian)
	       (set! should-collect-garbage? #t)
	       (lock #f) ;; locked if the thread was killed
	       (init-evaluation-thread)
	       (set! vocab (zodiac:create-vocabulary
			    'scheme-w/user-defined-macros/drscheme
			    zodiac:scheme-vocabulary))
	       (let ([p (build-parameterization user-custodian this)])
		 (with-parameterization p
		   (lambda ()
		     (current-custodian user-custodian)
		     (current-output-port this-out)
		     (current-error-port this-err)
		     (current-input-port this-in)
		     (current-load userspace-load)
		     (current-eval userspace-eval)
		     (exit-handler (lambda (arg)
				     (with-parameterization system-parameterization
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
		 (set! param p))
	       (unless (mred:get-preference 'drscheme:keep-interactions-history)
		 (set-resetting #t)
		 (delete reset-console-start-position (last-position))
		 (set-prompt-mode #f)				   
		 (set-resetting #f))
	       (super-reset-console)))]
	  [initialize-console
	   (lambda ()
	     (super-initialize-console)
	     (let* ([delta (make-object wx:style-delta%
					wx:const-change-family
					wx:const-decorative)]
		    [click-delta (make-object wx:style-delta%)]
		    [red-delta (make-object wx:style-delta%)])
	       (send click-delta copy delta)
	       (send click-delta set-delta-foreground "BLUE")
	       (send click-delta set-delta wx:const-change-underline 1)
	       (send red-delta copy delta)
	       (send red-delta set-delta-foreground "RED")
	       
	       (let ([dd output-delta]
		     [insert-delta
		      (lambda (s delta)
			(let ([before (get-end-position)])
			  (insert s)
			  (let ([after (get-end-position)])
			    (change-style delta before after)
			    (values before after))))])
		 (insert-delta "Welcome to " delta)
		 (let-values ([(before after)
			       (insert-delta "DrScheme" click-delta)])
		   (insert-delta (format ", version ~a.~nVocabulary: " (mred:version))
				 delta)
		   (insert-delta 
		    (format "~a"
			    (list-ref
			     drscheme:basis:level-strings
			     (drscheme:basis:level->number
			      (drscheme:language:setting-vocabulary-symbol
			       (mred:get-preference
				'drscheme:settings)))))
		    red-delta)
		   (insert-delta (format ".~n") delta)
		   (set-clickback before after 
				  (lambda args (drscheme:app:about-drscheme))
				  click-delta)))
	       (set-last-header-position (get-end-position))
	       (reset-console)))])
	
	(sequence
	  (mred:debug:printf 'super-init "before drscheme:rep:edit")
	  (apply super-init args)
	  (mred:debug:printf 'super-init "after drscheme:rep:edit")))))
  
  (define edit%
    (make-edit% mred:console-edit%)))
