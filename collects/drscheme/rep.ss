
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

    (print-struct #t)

    (error-display-handler
     (let ([p (current-parameterization)])
       (lambda (msg)
	 (with-parameterization p
	   (lambda ()
	     (display msg)
	     (newline)
	     (mred:message-box (format "Internal Error: ~a" msg)
			       "Internal Error"))))))

    (define build-parameterization
      (let ([orig-eventspace (wx:current-eventspace)])
	(lambda (base-parameterization)
	  (let* ([system-parameterization (current-parameterization)]
		 [p (make-parameterization base-parameterization)]
		 [bottom-eventspace (wx:make-eventspace p)]
		 [n (make-namespace 'no-constants
				    'wx
				    'hash-percent-syntax)])
	    (with-parameterization p
	      (lambda ()
		(error-value->string-handler
		 (lambda (x n)
		   (let ([long-string 
			  (format "~s" 
				  (if (eq? (drscheme:language:setting-printing
					    (mred:get-preference 'drscheme:settings)) 
					   'r4rs-style)
				      x
				      (print-convert:print-convert x)))])
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
		(let ([p (global-defined-value 'plt:home-directory)])
		  (current-namespace n)
		  (eval `(#%define plt:home-directory ,p)))
		(break-enabled #t)
		(user-break-poll-handler 
		 (lambda ()
		   (let ([one (parameterize 
			       ([wx:current-eventspace bottom-eventspace])
			       (wx:check-for-break))]
			 [two (parameterize 
			       ([wx:current-eventspace orig-eventspace])
			       (wx:check-for-break))])
		     (or one two))))
		(wx:current-eventspace bottom-eventspace)
		;(wx:eventspace-parameterization bottom-eventspace p)
		(exit-handler (lambda (arg)
				(with-parameterization system-parameterization
				  (lambda ()
				    (mred:exit)))))
		(read-curly-brace-as-paren #t)
		(read-square-bracket-as-paren #t)
		(print-struct #t)
		(error-print-width 250)))
	    (drscheme:basis:add-basis n)
	    p))))


    (define make-edit%
      (lambda (super%)
	(class super% args
	  (inherit insert change-style
		   get-end-position
		   set-clickback
		   set-last-header-position
		   this-err-write this-err 
		   this-out this-out-write
		   this-in this-result this-result-write
		   output-delta set-output-delta
		   do-pre-eval user-parameterization
		   do-post-eval
		   insert-prompt
		   erase prompt-mode?
		   get-canvas
		   ready-non-prompt autoprompting?
		   last-position
		   position-line
		   set-position
		   get-frame
		   begin-edit-sequence
		   end-edit-sequence
		   scroll-to-position)
	  (rename
	    [super-initialize-console initialize-console]
	    [super-reset-console reset-console])
	  (private
	    return-value
	    return-error
	    to-be-evaluated
	    
	    waiting-for-loaded
	    [load-success? #f])
	  
	  (private
	    [escape-fn #f])
	  (public
	    [report-error
	     (lambda (start-location end-location type string)
	       (let* ([start (zodiac:location-offset start-location)]
		      [finish (add1 (zodiac:location-offset end-location))]
		      [file (zodiac:location-file start-location)])
		 (cond
		   [(is-a? file wx:media-edit%)
		    (let* ([frame (send file get-frame)]
			   [console-edit (ivar frame interactions-edit)]
			   [console-end-position (send console-edit get-end-position)])
		      (send frame ensure-interactions-shown)
		      (send console-edit this-err-write string)
		      (send (send file get-canvas) set-focus)
		      (send file set-position start finish)
		      (send file scroll-to-position start #f (sub1 (send file last-position))))]
		   [else
		    (mred:message-box
		     (format "~a: ~a.~a-~a.~a: ~a" file
			     (zodiac:location-line start-location)
			     (zodiac:location-column start-location)
			     (zodiac:location-line end-location)
			     (zodiac:location-column end-location)
			     string)
		     "Error")])))])
	  (public
	    [on-set-media void]
	    [get-prompt (lambda () "> ")]
	    

	    [system-parameterization (current-parameterization)]
	    [primitive-eval  (current-eval)]
	    [param #f]

	    [current-thread-desc #f]
	    [current-thread-directory (current-directory)]

	    [protect-threads-queue (make-semaphore 1)]
	    [threads-queue null]

	    [break (lambda ()
		     (when current-thread-desc
		       (break-thread current-thread-desc))
		     (semaphore-wait protect-threads-queue)
		     (for-each (lambda (t)
				 (unless (eq? t current-thread-desc)
				   (break-thread t)))
			       threads-queue)
		     (set! threads-queue null)
		     (semaphore-post protect-threads-queue)
		     (if current-thread-desc
			 (set! current-thread-desc #f)
			 (insert-prompt)))]
	    [userspace-eval
	     (lambda (sexp)
	       (with-parameterization system-parameterization
		 (lambda ()
		   (let* ([z (or (unbox aries:error-box)
				 (let ([loc (zodiac:make-location 0 0 0 'eval)])
				   (zodiac:make-zodiac 'mzrice-eval loc loc)))]
			  [_ (mred:debug:printf 'zodiac
						"zodiac eval; sexp: ~a~n" sexp)]
			  [structurized (zodiac:structurize-syntax sexp z)]
			  [_ (mred:debug:printf 'zodiac
						"zodiac eval; structurized: ~a~n" structurized)]
			  [_ (mred:debug:printf 'zodiac
						"zodiac eval; unsexp: ~a~n" (zodiac:sexp->raw structurized))]
			  [expanded (zodiac:scheme-expand structurized param)]
			  [_ (mred:debug:printf 'zodiac
						"zodiac eval; expanded: ~a~n" expanded)]
			  [_ (mred:debug:printf 'zodiac
						"zodiac eval; unparsed: ~a~n" (zodiac:parsed->raw expanded))]
			  [annotated (aries:annotate expanded)]
			  [_ (mred:debug:printf 'zodiac
						"zodiac eval; annotated: ~a~n" annotated)])
		     (user-eval annotated)))))]
	    [user-eval
	     (lambda (expr)
	       (with-parameterization param
		 (lambda ()
		   (primitive-eval expr))))]
	    [display-result
	     (lambda (v)
	       (unless (void? v)
		 (with-parameterization param
		   (lambda ()
		     (parameterize
			 ([mzlib:pretty-print@:pretty-print-size-hook
			   (lambda (x _ port) (and (is-a? x wx:snip%) 1))]
			  [mzlib:pretty-print@:pretty-print-print-hook
			   (lambda (x _ port) (this-result-write x))])
		       (mzlib:pretty-print@:pretty-print v this-result))))))]
	    [send-scheme
	     (let ([s (make-semaphore 1)])
	       (opt-lambda (expr [before void] [after void])
		 (let* ([print-style (drscheme:language:setting-printing
				      (mred:get-preference 'drscheme:settings))])
		   (semaphore-wait protect-threads-queue)
		   (set! threads-queue
			 (cons
			  (thread
			   (lambda ()
			     (let ([user-code-error? #t])
			       (mzlib:function@:dynamic-wind/protect-break
				(lambda () 
				  (semaphore-wait s)
				  (current-directory current-thread-directory)
				  (set! current-thread-desc (current-thread))
				  (before))
				(lambda ()
				  (set! user-code-error? 
					(let/ec k
					  (call-with-values
					   (lambda ()
					     (with-parameterization param
					       (lambda ()
						 (with-handlers ([void
								  (lambda (exn)
								    (with-parameterization system-parameterization
								      (lambda ()
									(if (exn? exn)
									    (let ([di (exn-debug-info exn)])
									      (report-error (zodiac:zodiac-start di)
											    (zodiac:zodiac-finish di)
											    'dynamic
											    (exn-message exn)))
									    (mred:message-box (format "~s" exn)
											      "Uncaught Exception"))
									(k #t))))])
						   (primitive-eval expr)))))
					   (lambda anss
					     (let ([anss (let loop ([v anss])
							   (cond
							     [(null? v) null]
							     [(void? (car v)) (loop (cdr v))]
							     [else (cons (car v) (loop (cdr v)))]))])
					       (if (null? anss)
						   (void)
						   (let ([f (lambda (ans)
							      (let ([res 
								     (if (eq? print-style 'r4rs-style)
									 ans
									 (print-convert:print-convert ans))])
								(with-parameterization param
								  (lambda ()
								    (display-result res)))))])
						     (for-each f anss))))))
					  #f)))
				(lambda () 
				  (set! current-thread-desc #f)
				  (semaphore-post s)
				  (set! current-thread-directory (current-directory))
				  (semaphore-wait protect-threads-queue)
				  (set! threads-queue (mzlib:function@:remq (current-thread) threads-queue))
				  (semaphore-post protect-threads-queue)
				  (after user-code-error?))))))
			  threads-queue))
		   (semaphore-post protect-threads-queue))))]
	    [do-eval
	     (lambda (start end)
	       (do-many-buffer-evals this start end
				     (lambda () 
				       (wx:begin-busy-cursor)
				       (do-pre-eval))
				     (lambda () 
				       (wx:end-busy-cursor)
				       (do-post-eval))))]
	    [do-many-buffer-evals
	     (lambda (edit start end pre post)
	       (let* ([loc (zodiac:make-location 0 0 start edit)]
		      [reader (zodiac:read 
			       (mred:read-snips/chars-from-buffer edit start end)
			       loc)]
		      [cleanup
		       (lambda ()
			 (do-post-eval)
			 (wx:end-busy-cursor))])
		 (wx:begin-busy-cursor)
		 (do-pre-eval)
		 (let loop ()
		   (let/ec k
		     (with-handlers ([zodiac:interface:zodiac-exn?
				      (lambda (exn)
					(report-error (zodiac:interface:zodiac-exn-start-location exn)
						      (zodiac:interface:zodiac-exn-end-location exn)
						      (zodiac:interface:zodiac-exn-type exn)
						      (exn-message exn))
					(cleanup)
					(k #f))])
		       (let ([zodiac-read (reader)])
			 (if (zodiac:eof? zodiac-read)
			     (cleanup)
			     (send-scheme (aries:annotate (zodiac:scheme-expand zodiac-read))
					  void
					  (lambda (error?)
					    (if error?
						(cleanup)
						(loop)))))))))))])
	  (public
	    [do-load
	     (lambda (filename)
	       (let* ([p (open-input-file filename)]
		      [chars (list (read-char p) (read-char p) (read-char p) (read-char p))])
		 (close-input-port p)
		 (let ([loc (zodiac:make-location 0 0 0 filename)]
		       [re-p (void)])
		   (dynamic-wind
		    (lambda ()
		      (set! re-p
			    (if (equal? chars (list #\W #\X #\M #\E))
				(let ([edit (make-object drscheme:edit:edit%)])
				  (send edit load-file filename)
				  (mred:read-snips/chars-from-buffer edit))
				(open-input-file filename))))
		    (lambda ()
		      (let ([reader (zodiac:read re-p loc)])
			(let loop ([last (void)]
				   [z-sexp (reader)])
			  (if (zodiac:eof? z-sexp)
			      last
			      (loop (user-eval (aries:annotate (zodiac:scheme-expand z-sexp)))
				    (reader))))))
		    (lambda ()
		      (when (input-port? re-p)
			(close-input-port re-p)))))))])
	  (public
	    [reset-console
	     (lambda ()
	       (when param
		 (wx:kill-eventspace (with-parameterization param
				       wx:current-eventspace)))
	       (set! param (let ([p (build-parameterization user-parameterization)])
			     (with-parameterization p
			       (lambda ()
				 (current-output-port this-out)
				 (current-error-port this-err)
				 (current-input-port this-in)
				 (current-load do-load)
				 (current-eval userspace-eval)
				 (set! current-thread-directory
				       (current-directory))))
			     p))
	       (super-reset-console))]
	    [initialize-console
	     (lambda ()
	       (super-initialize-console)
	       (let* ([delta (make-object wx:style-delta%
					  wx:const-change-family
					  wx:const-decorative)]
		      [click-delta (make-object wx:style-delta%)]
		      [red-delta (make-object wx:style-delta%)])
		 (send delta set-delta wx:const-change-size 10)
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
