
(unit/sig drscheme:rep^
  (import [mred : mred-interfaces^]
	  [mzlib : mzlib:core^]
	  [fw : framework^]
	  [print-convert : mzlib:print-convert^]
	  [zodiac : drscheme:zodiac^]
	  [zodiac:interface : drscheme:interface^]
	  [drscheme:init : drscheme:init^]
	  [drscheme:snip : drscheme:snip^]
	  [drscheme:language : drscheme:language^]
	  [drscheme:app : drscheme:app^]
	  [basis : userspace:basis^]
	  [drscheme:edit : drscheme:edit^])
  
  (define (printf . args) (apply fprintf drscheme:init:original-output-port args))

  ;; keymap stuff that now must be here
;    (define setup-global-scheme-interaction-mode-keymap
;      (lambda (keymap)
;	(fw:keymap:set-keymap-error-handler keymap)
;	(fw:keymap:set-keymap-implied-shifts keymap)
	
;	(send keymap chain-to-keymap (fw:keymap:get-global) #f)
	
;	(send keymap add-key-function "put-previous-sexp"
;	      (lambda (edit event) 
;		(send edit copy-prev-previous-expr)))
;	(send keymap add-key-function "put-next-sexp"
;	      (lambda (edit event) 
;		(send edit copy-next-previous-expr)))
	
;	(fw:keymap:send-map-function-meta keymap "p" "put-previous-sexp")
;	(fw:keymap:send-map-function-meta keymap "n" "put-next-sexp")))

;    (define global-scheme-interaction-mode-keymap (make-object mred:keymap%))
;    (setup-global-scheme-interaction-mode-keymap global-scheme-interaction-mode-keymap)

  (define WELCOME-DELTA (make-object mred:style-delta% 'change-family 'decorative))
  (define CLICK-DELTA (make-object mred:style-delta%))
  (define RED-DELTA (make-object mred:style-delta%))
  (send* CLICK-DELTA
    (copy WELCOME-DELTA)
    (set-delta-foreground "BLUE")
    (set-delta 'change-underline #t))
  (send* RED-DELTA
    (copy WELCOME-DELTA)
    (set-delta-foreground "RED"))
  (define WARNING-STYLE-DELTA (make-object mred:style-delta% 'change-bold))
  (send* WARNING-STYLE-DELTA
    (set-delta-foreground "BLACK")
    (set-delta-background "YELLOW"))

  (define library-unit #f)

  (fw:preferences:set-default 'drscheme:library-file
			       #f
			       (lambda (x) (or (string? x) (not x))))
  (fw:preferences:add-callback 
   'drscheme:library-file
   (lambda (p v)
     (with-handlers
	 ([(lambda (x) #t)
	   (lambda (x)
	     (mred:message-box "Invalid Library" (exn-message x))
	     #f)])
       (if v
	   (let ([new-unit (load/cd v)])
	     (if (unit/sig? new-unit)
		 (set! library-unit new-unit)
		 (begin
		   (mred:message-box 
		    "Invalid Library"
		    "Library file does not contain a unit")
		   #f)))
	   (set! library-unit #f)))))
    
  (define exception-reporting-rep (make-parameter #f))

  (define (process-edit/zodiac edit f start end annotate?)
    (let ([setting (basis:current-setting)])
      (basis:process/zodiac
       (parameterize ([read-case-sensitive (basis:setting-case-sensitive?
					    setting)])
	 (zodiac:read (fw:gui-utils:read-snips/chars-from-buffer edit start end)
		      (zodiac:make-location 0 0 start edit)
		      #t 1))
       f
       annotate?)))

  (define (process-edit/no-zodiac edit f start end)
    (let* ([buffer-thunk (fw:gui-utils:read-snips/chars-from-buffer edit start end)]
	   [snip-string (string->list " 'non-string-snip ")]
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

  (fw:preferences:set-default 'drscheme:repl-always-active #f boolean?)

  (define (show-interactions-history) (mred:message-box "Interactions History" "Not yet implemented"))

  (define-struct sexp (left right prompt))
  
  (define newline-string (string #\newline))
  
  (define separator-snipclass
    (make-object
     (class-asi mred:snip-class%
       (override
	 [read (lambda (s) 
		 (let ([size-box (box 0)])
		   (send s get size-box)
		   (make-object separator-snip%)))]))))
  (send* separator-snipclass
    (set-version 1)
    (set-classname "mred:sepatator-snip%"))
  (send (mred:get-the-snip-class-list) add separator-snipclass)
  
  ;; the two numbers 1 and 2 which appear here are to line up this snip
  ;; with the embedded snips around it in the drscheme rep.
  ;; I have no idea where the extra pixels are going.
  (define separator-snip%
    (class mred:snip% ()
      (inherit get-style set-snipclass set-flags get-flags get-admin)
      (private [width 500]
	       [height 1]
	       [white-around 2])
      (override
	[write (lambda (s) 
		 (send s put (char->integer #\r)))]
	[copy (lambda () 
		(let ([s (make-object (object-class this))])
		  (send s set-style (get-style))
		  s))]
	[get-extent
	 (lambda (dc x y w-box h-box descent-box space-box lspace-box rspace-box)
	   (for-each (lambda (box) (when box (set-box! box 0)))
		     (list descent-box space-box lspace-box rspace-box))
	   (let* ([admin (get-admin)]
		  [reporting-media (send admin get-text)]
		  [reporting-admin (send reporting-media get-admin)]
		  [widthb (box 0)]
		  [space 2])
	     (send reporting-admin get-view #f #f widthb #f)
	     (set! width (- (unbox widthb)
			    space
			    2)))
	   (set! height 1)
	   (when w-box
	     (set-box! w-box width))
	   (when h-box
	     (set-box! h-box (+ (* 2 white-around) height))))]
	[draw
	 (let* ([body-pen (send mred:the-pen-list find-or-create-pen
				"BLUE" 0 'solid)]
		[body-brush (send mred:the-brush-list find-or-create-brush
				  "BLUE" 'solid)])
	   (lambda (dc x y left top right bottom dx dy drawCaret)
	     (let ([orig-pen (send dc get-pen)]
		   [orig-brush (send dc get-brush)])
	       (send dc set-pen body-pen)
	       (send dc set-brush body-brush)
	       
	       (send dc draw-rectangle (+ x 1)
		     (+ white-around y) width height)
	       
	       (send dc set-pen orig-pen)
	       (send dc set-brush orig-brush))))]
	[get-text
	 (opt-lambda (offset num [flattened? #f])
	   "separator-snip")])
      (sequence
	(super-init)
	(set-flags (cons 'hard-newline (get-flags)))
	(set-snipclass separator-snipclass))))
  
  (define console-max-save-previous-exprs 30)
  (let* ([list-of? (lambda (p?)
		     (lambda (l)
		       (and (list? l)
			    (andmap p? l))))]
	 [snip/string? (lambda (s) (or (is-a? s mred:snip%) (string? s)))]
	 [list-of-snip/strings? (list-of? snip/string?)]
	 [list-of-lists-of-snip/strings? (list-of? list-of-snip/strings?)])
    (fw:preferences:set-default
     'mred:console-previous-exprs
     null
     list-of-lists-of-snip/strings?))
  (let ([marshall 
	 (lambda (lls)
	   (map (lambda (ls)
		  (map (lambda (s)
			 (cond
			   [(is-a? s mred:original:string-snip%)
			    (send s get-text 0 (send s get-count))]
			   [(string? s) s]
			   [else "'non-string-snip"]))
		       ls))
		lls))]
	[unmarshall (lambda (x) x)])
    (fw:preferences:set-un/marshall
     'mred:console-previous-exprs
     marshall unmarshall))
  
  (define (make-edit% super%)
    (class super% args
      (inherit insert change-style
	       clear-undos set-caret-owner
	       clear-previous-expr-positions
	       get-end-position
	       set-clickback
	       do-post-eval
	       insert-prompt
 	       erase prompt-mode?
	       get-canvas
	       ready-non-prompt
	       set-prompt-mode
	       delete lock locked?
	       paragraph-start-position
	       last-position
	       set-resetting
	       position-line
	       set-position
	       begin-edit-sequence
	       end-edit-sequence
	       reset-pretty-print-width
	       scroll-to-position
	       get-top-level-window)
      (rename
       [super-initialize-console initialize-console]
       [super-reset-console reset-console])

      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                            ;;;
      ;;;             Zodiac Interface               ;;;
      ;;;                                            ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (public
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
	   (let* ([frame (get-top-level-window)]
		  [interactions-edit (ivar frame interactions-edit)])
	     (send frame ensure-interactions-shown)
	     (let ([locked? (send interactions-edit locked?)])
	       (send interactions-edit begin-edit-sequence)
	       (send interactions-edit lock #f)
	       (send interactions-edit this-err-write (string-append message (string #\newline)))
	       (send interactions-edit lock locked?)
	       (send interactions-edit end-edit-sequence))))]
	[report-error
	 (lambda (start-location end-location type input-string)
	   (let* ([start (zodiac:location-offset start-location)]
		  [finish (add1 (zodiac:location-offset end-location))]
		  [file (zodiac:location-file start-location)]
		  [message
		   (if (is-a? file mred:text%)
		       input-string
		       (string-append (basis:format-source-loc start-location end-location)
				      input-string))])
	     (report-unlocated-error message)
	     (when (is-a? file mred:text%)
	       (send file begin-edit-sequence)
	       (send file set-position start finish)
	       (if (is-a? file edit%)
		   (send file scroll-to-position start
			 #f (sub1 (send file last-position)) 'end)
		   (send file scroll-to-position start #f finish))
	       (send file end-edit-sequence)
	       (send (send file get-canvas) focus))))]
	[on-set-media void])

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

      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                            ;;;
      ;;;                Evaluation                  ;;;
      ;;;                                            ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (override
	[get-prompt (lambda () "> ")])
      (public
	[user-param #f]
	[user-setting (fw:preferences:get 'drscheme:settings)]
	[user-custodian (make-custodian)])
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
	   (end-edit-sequence))])
      (override
	[do-eval
	 (let ([count 0])
	   (lambda (start end)
	     (set! count (add1 count))
	     (when (<= 5 count)
	       (collect-garbage)
	       (set! count 0))
	     (let* ([frame (get-top-level-window)]
		    [definitions-edit (ivar frame definitions-edit)]
		    [already-warned? (ivar definitions-edit already-warned?)]
		    [needs-execution? (ivar definitions-edit needs-execution?)])
	       (when (if (fw:preferences:get 'drscheme:execute-warning-once)
			 (and (not already-warned?)
			      needs-execution?)
			 needs-execution?)
		 (send definitions-edit already-warned)
		 (insert-warning)))
	     (do-many-buffer-evals this start end)))])
      (public
	[cleanup-evaluation
	 (lambda (thread-to-watch)
	   (mred:end-busy-cursor)
	   (cleanup-transparent-io)
	   (send (get-top-level-window) enable-evaluation)
	   (begin-edit-sequence)	     
	   (set-caret-owner #f 'display)

	   (if (thread-running? thread-to-watch)
	       (let ([c-locked? (locked?)])
		 (lock #f)
		 (insert-prompt)
		 (lock c-locked?)
		 (end-edit-sequence))
	       (begin (lock #t)
		      (end-edit-sequence)
		      (unless shutting-down?
			(mred:message-box
			 "Warning"
			 (format "The evaluation thread is no longer running, ~
			 so no evaluation can take place until ~
			 the next execution."))))))])
      (public
	[do-many-buffer-evals
	 (lambda (edit start end)
	   (unless in-evaluation?
	     (send (get-top-level-window) disable-evaluation)
	     (reset-break-state)
	     (cleanup-transparent-io)
	     (reset-pretty-print-width)
	     (ready-non-prompt)
	     (mred:begin-busy-cursor)
	     (when should-collect-garbage?
	       (set! should-collect-garbage? #f)
	       (collect-garbage))
	     (run-in-evaluation-thread
	      (lambda ()
		(protect-user-evaluation
		 cleanup-evaluation
		 (lambda ()
		   (with-parameterization user-param
		     (lambda ()
		       (process-edit edit
				     (lambda (expr recur)
				       (cond
					[(basis:process-finish? expr)
					 (void)]
					[else
					 (let ([answers (call-with-values
							 (lambda ()
							   (mzlib:thread:dynamic-enable-break
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
	   (let* ([frame (get-top-level-window)]
		  [interactions-edit (ivar frame interactions-edit)])
	     (set! in-evaluation? #f)
	     (send (get-top-level-window) not-running)

	     ;; this thread is created to run the actual shutdown, in
	     ;; case the custodian is going to shutdown the current
	     ;; thread!  The semaphore is there for when the case when
	     ;; current thread is not shutdown.
	     (let ([sema (make-semaphore 0)])
	       (parameterize ([current-custodian drscheme:init:system-custodian])
		 (thread (lambda ()
			   (custodian-shutdown-all user-custodian)
			   (semaphore-post sema)))
		 (semaphore-wait sema)))))])
      (public
	[reset-break-state (lambda () (set! ask-about-kill? #f))]
	[breakable-thread #f]
	[break (lambda ()
		 (cond
		  [(or ;(not in-evaluation?)
		       (not breakable-thread))
		   (mred:bell)]
		  [ask-about-kill? 
		   (if (fw:gui-utils:get-choice
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
	   (semaphore-wait in-evaluation-semaphore)
	   (set! in-evaluation? #t)
	   (semaphore-post in-evaluation-semaphore)

	   ;; this is evaluation-thread, unless that was killed and user event
	   ;; callbacks are still being handled. in that case, it will be
	   ;; whatever thread is handling the callback
	   (let ([thread-to-watch (current-thread)])

	     (fluid-let ([breakable-thread thread-to-watch]
			 [evaluation-sucessful  (make-semaphore 0)]
			 [cleanup-semaphore (make-semaphore 1)]
			 [cleanup-sucessful (make-semaphore 0)]
			 [thread-grace
			  (thread
			   (lambda ()
			     (semaphore-wait evaluation-sucessful)

			     (semaphore-wait cleanup-semaphore)

			     (parameterize ([current-custodian drscheme:init:system-custodian])
			       (kill-thread thread-kill))

			     (cleanup thread-to-watch)
			     (semaphore-post cleanup-sucessful)))]
			 [thread-kill
			  (thread 
			   (lambda ()
			     (thread-wait thread-to-watch)

			     (semaphore-wait cleanup-semaphore)

			     (parameterize ([current-custodian drscheme:init:system-custodian])
			       (kill-thread thread-grace))

			     (cleanup thread-to-watch)
			     (semaphore-post cleanup-sucessful)))])

	       (let/ec k
		 (fluid-let ([error-escape-k 
			      (lambda ()
				(semaphore-post evaluation-sucessful)
				(k (void)))])
		   (thunk)))

	       (semaphore-post evaluation-sucessful)
	       (semaphore-wait cleanup-sucessful)


	       (semaphore-wait in-evaluation-semaphore)
	       (set! in-evaluation? #f)
	       (semaphore-post in-evaluation-semaphore))))])
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
	   (parameterize ([mred:current-eventspace
			   ((in-parameterization user-param
						 mred:current-eventspace))])
	     (mred:queue-callback
	      (lambda ()
		(mzlib:thread:dynamic-disable-break
		 (lambda ()
		   (set-box! first-box #f)
		   (let ([escape-handler
			  (rec drscheme-error-escape-handler
			       (lambda ()
				 (error-escape-k)))])
		     (error-escape-handler escape-handler)
		     (basis:bottom-escape-handler escape-handler))

		   (set! yield-count 0)
		   (send (get-top-level-window) not-running)
		   (set! evaluation-thread (current-thread))
		   (let loop ()
		     (unless (semaphore-try-wait? eval-thread-queue-sema)
		       (fluid-let ([yield-count (+ yield-count 1)])
			 (mred:yield eval-thread-queue-sema)))
		     (semaphore-wait eval-thread-state-sema)
		     (let ([thunk (car eval-thread-thunks)])
		       (set! eval-thread-thunks (cdr eval-thread-thunks))
		       (semaphore-post eval-thread-state-sema)
		       (dynamic-wind
			(lambda ()
			  (semaphore-wait running-semaphore)
			  (set! evaluation-running #t)
			  (semaphore-post running-semaphore)
			  (update-running))
			(lambda ()
			  (with-parameterization drscheme:init:system-parameterization
			    (lambda ()
			      (thunk))))
			(lambda ()
			  (semaphore-wait running-semaphore)
			  (set! evaluation-running #f)
			  (semaphore-post running-semaphore)
			  (update-running))))
		     (loop))))))))])
      (public
	[shutting-down? #f]
	[shutdown 
	 (lambda ()
	   (set! shutting-down? #t)
	   (shutdown-user-custodian))])
      
	(private
	  [running-semaphore (make-semaphore 1)]
	  [running-on? #f]
	  [running-events 0]
	  [evaluation-running #f]
	  [update-running
	   (lambda ()
	     (semaphore-wait running-semaphore)
	     (if (or (> running-events 0)
		     evaluation-running)
		 (unless running-on?
		   (set! running-on? #t)
		   (send (get-top-level-window) running))
		 (when running-on?
		   (set! running-on? #f)
		   (send (get-top-level-window) not-running)))
	     (semaphore-post running-semaphore))])


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;					     ;;;
	;;;                  I/O                     ;;;
	;;;					     ;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(inherit get-admin set-prompt-position get-canvases find-snip)
	(public
	  [transparent-edit #f]
	  [transparent-snip #f]
	  [this-in-char-ready? (lambda () #t)]
	  [cleanup-transparent-io
	   (lambda ()
	     (when transparent-edit
	       (set! saved-newline? #f) 
	       (send transparent-edit shutdown)
	       (set-position (last-position))
	       (set-caret-owner #f)
	       (let ([a (get-admin)])
		 (when a
		   (send a grab-caret)))
	       (send transparent-edit lock #t)
	       (set! transparent-edit #f)))]

	[init-transparent-io
	 (lambda (grab-focus?)
	   (begin-edit-sequence)
	   (if transparent-edit
	       (when grab-focus?
		 (let ([a (send transparent-edit get-admin)])
		   (when a
		     (send a grab-caret))))
	       (init-transparent-io-do-work grab-focus?))
	   (when  (eq? (current-thread) evaluation-thread)
	     (set-caret-owner transparent-snip 'display))
	   (end-edit-sequence))]


	[init-transparent-io-do-work
	 (lambda (grab-focus?)
	   (with-parameterization drscheme:init:system-parameterization
	     (lambda ()
	       (let ([c-locked? (locked?)])
		 (begin-edit-sequence)
		 (lock #f)
		 (let ([starting-at-prompt-mode? prompt-mode?])
		   (set! transparent-edit (make-object transparent-io-edit%))
		   
		   ;; ensure that there is a newline before the snip is inserted
		   (unless (member 'hard-newline
				   (send (find-snip (last-position) 'before) get-flags))
		     (insert (string #\newline) (last-position) (last-position) #f))
		   
		   (when starting-at-prompt-mode?
		     (set-prompt-mode #f))
		   
		   (unless transparent-edit
		     (printf "transparent-edit is ~a!" transparent-edit))
		   (send transparent-edit auto-wrap #t)
		   (let ([snip (make-object mred:editor-snip% transparent-edit)])
		     (set! transparent-snip snip)
		     (insert snip (last-position) (last-position) #f)
		     (insert (string #\newline) (last-position) (last-position) #f)
		     (for-each (lambda (c) (send c add-wide-snip snip))
			       (get-canvases)))
		   (when grab-focus?
		     (let ([a (send transparent-edit get-admin)])
		       (when a
			 (send a grab-caret))))
		   (when starting-at-prompt-mode?
		     (insert-prompt)))
		 (set-prompt-position (last-position))
		 (lock c-locked?)
		 (end-edit-sequence)))))]

	  [this-in-read
	   (lambda ()
	     (parameterize ([mred:current-eventspace drscheme:init:system-eventspace])
	       (mred:queue-callback
		(lambda ()
		  (init-transparent-io #t)
		  (send transparent-edit fetch-char))
		#f)))])


	(public
	  [output-delta (make-object mred:style-delta%
			  'change-weight
			  'bold)]
	  [result-delta (make-object mred:style-delta%
			  'change-weight
			  'bold)]
	  [error-delta (make-object mred:style-delta%
			 'change-style
			 'slant)])

	(sequence
	  (send error-delta set-delta-foreground "RED")
	  (send result-delta set-delta-foreground (make-object mred:color% 0 0 175))
	  (send output-delta set-delta-foreground (make-object mred:color% 150 0 150)))

	(public
	  [MAX-CACHE-TIME 4000]
	  [MIN-CACHE-TIME 100]
	  [CACHE-TIME MIN-CACHE-TIME]
	  [TIME-FACTOR 10]
	  [generic-write
	   (let ([time-of-last-call (current-milliseconds)])
	     (lambda (edit s style-func)
	       (when prompt-mode?
		 (insert (string #\newline) (last-position) (last-position) #f)
		 (set-prompt-mode #f))

	       (let* ([start (send edit last-position)]
		      [c-locked? (send edit locked?)])
		 (send edit begin-edit-sequence)
		 (send edit lock #f)
		 (send edit insert
		       (if (is-a? s mred:snip%)
			   (send s copy)
			   s)
		       start
		       start
		       #f)
		 (let ([end (send edit last-position)])
		   ;(send edit change-style null start end) ; wx
		   (send edit set-prompt-position end)
		   (style-func start end))
		 (send edit lock c-locked?)
		 (send edit end-edit-sequence))))]
	  [generic-close (lambda () '())]
	  
	  
	  [saved-newline? #f]

	  [this-result-write 
	   (lambda (s)
	     (parameterize ([mred:current-eventspace drscheme:init:system-eventspace])
	       (mred:queue-callback
		(lambda ()
		  (generic-write this
				 s 
				 (lambda (start end)
				   (change-style result-delta
						 start end))))
		#f)))]
	  [this-out-write
	   (lambda (s)
	     (parameterize ([mred:current-eventspace drscheme:init:system-eventspace])
	       (mred:queue-callback
		(lambda ()
		  (init-transparent-io #f)
		  (let* ([old-saved-newline? saved-newline?]
			 [len (and (string? s)
				   (string-length s))]
			 [s1 (if (and len
				      (> len 0)
				      (char=? (string-ref s (- len 1)) #\newline))
				 (begin 
				   (set! saved-newline? #t)
				   (substring s 0 (- len 1)))
				 (begin
				   (set! saved-newline? #f)
				   s))]
			 [gw
			  (lambda (s)
			    (generic-write
			     transparent-edit
			     s
			     (lambda (start end)
			       (when transparent-edit
				 (send transparent-edit
				       change-style output-delta start end)))))])
		    (when old-saved-newline?
		      (gw (string #\newline)))
		    (gw s1)))
		#f)))]
	  [this-err-write
	   (lambda (s)
	     (parameterize ([mred:current-eventspace drscheme:init:system-eventspace])
	       (mred:queue-callback
		(lambda ()
		  (cleanup-transparent-io)
		  (generic-write this
				 s
				 (lambda (start end)
				   (change-style error-delta 
						 start end))))
		#f)))]
	  
	  [this-err (make-output-port this-err-write generic-close)]
	  [this-out (make-output-port this-out-write generic-close)]
	  [this-in (make-input-port this-in-read this-in-char-ready? generic-close)]
	  [this-result (make-output-port this-result-write generic-close)]
	  [set-display/write-handlers
	   (lambda ()
	     (for-each
	      (lambda (port port-out-write)
		(let ([original-write-handler (port-write-handler port)]
		      [original-display-handler (port-display-handler port)]
		      [handler-maker
		       (lambda (port-handler pretty original)
			 (port-handler
			  port
			  (rec console-pp-handler
			       (lambda (v p)
				 (if (or (string? v) 
					 (char? v)
					 (number? v)
					 (symbol? v))
				     (original v p)
				     (parameterize ([mzlib:pretty-print:pretty-print-size-hook
						     (lambda (x _ port) (and (is-a? x mred:snip%) 1))]
						    [mzlib:pretty-print:pretty-print-print-hook
						     (lambda (x _ port) (port-out-write x))])
				       (pretty v p 'infinity)))))))])
		  (handler-maker port-display-handler 
				 mzlib:pretty-print:pretty-display 
				 original-display-handler)
		  (handler-maker port-write-handler
				 mzlib:pretty-print:pretty-print
				 original-write-handler)))
	      (list this-out this-err this-result)
	      (list this-out-write this-err-write this-result-write)))])

      (public
	[display-results
	 (lambda (anss)
	   (for-each 
	    (lambda (v)
	      (unless (void? v)
		(let ([v (if (basis:r4rs-style-printing? user-setting)
			     v
			     (print-convert:print-convert v))])
		  (parameterize ([mzlib:pretty-print:pretty-print-size-hook
				  (lambda (x _ port) (and (is-a? x mred:original:snip%) 1))]
				 [mzlib:pretty-print:pretty-print-print-hook
				  (lambda (x _ port) (this-result-write x))])
		    (mzlib:pretty-print:pretty-print v this-result)))))
	    anss))])

      
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;					     ;;;
	;;;                Execution                 ;;;
	;;;					     ;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (private 
	[insert-delta
	 (lambda (s delta)
	   (let ([before (last-position)])
	     (insert s before before #f)
	     (let ([after (last-position)])
	       (change-style delta before after)
	       (values before after))))])
      (public
	[repl-initially-active? #f])
      (override
	[reset-console
	 (let ([first-dir (current-directory)])
	   (lambda ()
	     (clear-previous-expr-positions)
	     (shutdown-user-custodian)
	     (cleanup-transparent-io)
	     (set! should-collect-garbage? #t)

	     (set! running-on? #f)
	     (set! running-events 0)

	     ;; in case the last evaluation thread was killed, clean up some state.
	     (lock #f)
	     (set! in-evaluation? #f)

	     (begin-edit-sequence)
	     (set-resetting #t)
	     (delete (paragraph-start-position 1) (last-position))
	     (set-prompt-mode #f)
	     (set-resetting #f)
	     (set-position (last-position) (last-position))
	     (insert-delta "Language: " WELCOME-DELTA)
	     (insert-delta 
	      (symbol->string
	       (basis:find-setting-name 
		(fw:preferences:get
		 'drscheme:settings)))
	      RED-DELTA)
	     (insert-delta (format ".~n") WELCOME-DELTA)
	     (set! repl-initially-active? #t)
	     (end-edit-sequence)

	     (set! user-setting (fw:preferences:get 'drscheme:settings))

	     (let ([p (basis:build-parameterization
		       (list 'mred)
		       (fw:preferences:get 'drscheme:settings)

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
						   ((require-library "gusrspcr.ss" "gusrspce")
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

	       (set! user-custodian ((in-parameterization p current-custodian)))
	       (with-parameterization p
		 (lambda ()

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
			    (parameterize ([mzlib:pretty-print:pretty-print-size-hook
					    (lambda (x _ port) (and (is-a? x mred:snip%) 1))]
					   [mzlib:pretty-print:pretty-print-print-hook
					    (lambda (x _ port)
					      (evcase port
						[this-result (this-result-write x)]
						[this-out (this-out-write x)]
						[this-err (this-err-write x)]))])
			      (old value port))
			    (old value port)))))

		   (print-convert:current-print-convert-hook
		    (lambda (expr basic-convert sub-convert)
		      (let ([ans (if (is-a? expr mred:snip%)
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
				 (mred:message-box "Debugging Error" msg)
				 (if rep
				     (send rep report-unlocated-error msg)
				     (mred:message-box "Uncaught Error" msg))))))))
		   
		   (let ([directory
			  (let/ec k
			    (unless (get-top-level-window)
			      (k first-dir))
			    (let*-values ([(filename) (send (ivar (get-top-level-window) definitions-edit)
							    get-filename)]
					  [(normalized) (if (string? filename)
							    (mzlib:file:normalize-path filename)
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
			  [primitive-dispatch-handler (mred:event-dispatch-handler)]
			  [frame (get-top-level-window)]
			  [running-flag-on? #f]
			  [event-semaphore (make-semaphore 0)]

			  [ht (make-hash-table-weak)] ;; maps eventspaces to depth of nested mred:yields (ints)

			  [first-box (box #t)]

			  [dispatch-handler-procedure primitive-dispatch-handler])

		     (thread (rec f
				  (lambda ()
				    (semaphore-wait event-semaphore)
				    ;(sleep 1/10)
				    (update-running)
				    (f))))

		     (mred:event-dispatch-handler
		      (rec drscheme-event-dispatch-handler
			   (lambda (eventspace)
			     (mzlib:thread:dynamic-disable-break
			      (lambda ()
				(when (and (eq? eventspace user-eventspace)
					   (not (unbox first-box)))

				  (semaphore-wait running-semaphore)
				  (hash-table-put! ht eventspace (+ 1 (hash-table-get ht eventspace (lambda () 0))))
				  (when (= 1 (hash-table-get ht eventspace))
				    (set! running-events (+ 1 running-events))
				    (reset-break-state)
				    (semaphore-post event-semaphore))
				  (semaphore-post running-semaphore)


				  (protect-user-evaluation
				   void
				   (lambda ()
				     (mzlib:thread:dynamic-enable-break
				      (lambda ()
					(primitive-dispatch-handler eventspace)))))

				  (semaphore-wait running-semaphore)
				  (hash-table-put! ht eventspace (max 0 (- (hash-table-get ht eventspace (lambda () 0)) 1)))
				  (when (= 0 (hash-table-get ht eventspace))
				    (set! running-events (- running-events 1)))
				  (semaphore-post running-semaphore)
				  (update-running)))))))

		     (set! user-eventspace (mred:make-eventspace))
		     (mred:current-eventspace user-eventspace)
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

	       (set! user-param p))

	     (super-reset-console)))]
	[initialize-console
	 (lambda ()
	   (super-initialize-console)
	   
	   (insert-delta "Welcome to " WELCOME-DELTA)
	   (let-values ([(before after)
			 (insert-delta "DrScheme" CLICK-DELTA)])
	     (insert-delta (format ", version ~a.~n" (fw:version:version))
			   WELCOME-DELTA)
	     (set-clickback before after 
			    (lambda args (drscheme:app:about-drscheme))
			    CLICK-DELTA))
	 
	   (if (or (fw:preferences:get 'drscheme:repl-always-active)
		   (not (send (ivar (get-top-level-window) interactions-edit) get-filename)))
	       (reset-console)
	       (begin 
		 (insert-delta "Execute has not been clicked." WARNING-STYLE-DELTA)
		 (lock #t))))])
      (sequence
	(apply super-init args))))

  (define make-console-edit%
    (lambda (super%)
      (class super% args
	(inherit position-line position-location
		 line-location get-admin
		 set-position set-caret-owner
		 clear-undos insert delete
		 begin-edit-sequence
		 end-edit-sequence
		 run-after-edit-sequence
		 ;styles-fixed?
		 set-styles-fixed
		 change-style split-snip
		 scroll-to-position locked? lock
		 last-position get-start-position get-end-position
		 get-text get-snip-position
		 get-character find-snip find-string
		 erase
		 invalidate-bitmap-cache
		 get-extent get-style-list)
	(rename [super-on-local-char on-local-char]
		[super-on-paint on-paint]
		[super-after-set-size-constraint after-set-size-constraint])
	(private
	  [edit-sequence-count 0])
	(public
	  [styles-fixed? #f]
	  [orig-stdout (current-output-port)]
	  [orig-stderr (current-error-port)])
	(public
	  [normal-delta #f])
	
	(rename [super-on-insert on-insert]
		[super-after-insert after-insert]
		
		[super-on-delete on-delete]
		[super-after-delete after-delete]
		
		[super-on-change-style on-change-style]
		[super-after-change-style after-change-style]
		
		[super-on-edit-sequence on-edit-sequence]
		[super-after-edit-sequence after-edit-sequence]
		
		[super-after-set-position after-set-position])
	(private
	  [find-which-previous-sexp
	   (lambda ()
	     (let*-values ([(x y) (values (get-start-position) (get-end-position))])
	       (let loop ([sexps previous-expr-positions])
		 (cond
		   [(null? sexps) (values #f #f)]
		   [(pair? sexps) (let* ([hd (car sexps)]
					 [left (car hd)]
					 [right (cdr hd)]
					 [tl (cdr sexps)])
				    (if (and (<= left x right)
					     (<= left y right))
					(values left right)
					(loop tl)))]))))]
	  [moving-down? #f]
	  [needs-to-move #f]
	  [needs-to-move-left #f]
	  [needs-to-move-right #f]
	  [needs-to-move-original null]
	  [updating-highlighted-prompt #f]
	  [on-something
	   (opt-lambda (super start len [attend-to-styles-fixed? #f])
	     (cond
	       [(or resetting?
		    moving-down?
		    updating-highlighted-prompt
		    (not (number? prompt-position))
		    (>= start prompt-position)
		    (and attend-to-styles-fixed? styles-fixed?))
		(super start len)]
	       [else 
		(let-values ([(left right) (find-which-previous-sexp)])
		  (and #f
		       left
		       (super start len)
		       '(begin
			  (when needs-to-move
			    (mred:message-box "Internal Error" "needs to move already #t!!"))
			  (newline)
			  (newline)
			  (set! needs-to-move #t)
			  (set! needs-to-move-left left)
			  (set! needs-to-move-right right)
			  (split-snip left)
			  (split-snip right)
			  '(fprintf mred:constants:original-output-port 
				    "left ~a right ~a~n" left right)
			  (set! needs-to-move-original
				(let loop ([snip (find-snip left mred:const-snip-after)])
				  (cond
				    [(not snip) null]
				    [(< (get-snip-position snip) right)
				     '(fprintf mred:constants:original-output-port 
					       "orig: ~s~n" (send snip get-text 0 10000))
				     (cons (send snip copy) (loop (send snip next)))]
				    [else null])))
			  (begin-edit-sequence)
			  #t)))]))]
	  [after-something
	   (lambda (combine start len)
	     (when (and (not needs-to-move)
			(not moving-down?)
			(or resetting?
			    (and prompt-mode? (< start prompt-position))))
	       (set! prompt-position (combine prompt-position len)))
	     (when needs-to-move
	       (set! needs-to-move #f)
	       (set! needs-to-move-right (combine needs-to-move-right len))
	       (split-snip needs-to-move-left)
	       (split-snip needs-to-move-right)
	       (let ([start-selection (get-start-position)]
		     [end-selection (get-end-position)]
		     [delta (- prompt-position needs-to-move-left)])
		 (set! moving-down? #t)
		 (let loop ([snip (find-snip needs-to-move-left 'after)])
		   (cond
		     [(not snip) (void)]
		     [(< (get-snip-position snip) needs-to-move-right)
		      (insert (send snip copy) (last-position) (last-position) #t)
		      (loop (send snip next))]
		     [else (void)]))
		 (delete needs-to-move-left needs-to-move-right #f)
		 (for-each (lambda (s) 
			     '(fprintf mred:constants:original-output-port 
				       "copy: ~s~n" (send s get-text 0 10000))
			     (insert (send s copy) needs-to-move-left needs-to-move-left #f))
			   needs-to-move-original)
		 (set-position (+ start-selection delta)
			       (+ end-selection delta)))
	       (set! moving-down? #f)
	       (end-edit-sequence)))])
	(public
	  [resetting? #f]
	  [set-resetting (lambda (v) (set! resetting? v))])
	(override
	  [on-insert
	   (lambda (start len)
	     (on-something super-on-insert start len))]
	  [on-delete
	   (lambda (start len)
	     (on-something super-on-delete start len))]
	  [on-change-style
	   (lambda (start len)
	     (on-something super-on-change-style start len #t))]
	  [after-insert
	   (lambda (start len)
	     (after-something + start len)
	     (super-after-insert start len))]
	  [after-delete
	   (lambda (start len)
	     (after-something - start len)
	     (super-after-delete start len))]
	  [after-change-style
	   (lambda (start len)
	     (after-something (lambda (start len) start) start len)
	     (super-after-change-style start len))]
	  [on-edit-sequence
	   (lambda ()
	     (super-on-edit-sequence))]
	  [after-edit-sequence
	   (lambda ()
	     (super-after-edit-sequence))]
	  [after-set-position
	   (lambda ()
	     (super-after-set-position))])
	
	(private
	  [last-str (lambda (l)
		      (if (null? (cdr l))
			  (car l)
			  (last-str (cdr l))))])

	  
	(public
	  [prompt-mode? #f]
	  [set-prompt-mode (lambda (x) (set! prompt-mode? x))]
	  [get-prompt (lambda () "> ")]
	  [prompt-position 0]
	  [set-prompt-position (lambda (v) (set! prompt-position v))]
	  [find-prompt 
	   (lambda (pos) 
	     (if (> pos prompt-position)
		 prompt-position
		 0))]
	  [auto-save? #f])

	(public
	  [previous-expr-pos -1]
	  [previous-expr-positions null]
	  [clear-previous-expr-positions
	   (lambda ()
	     (set! previous-expr-positions null))]
	  [copy-previous-expr
	   (lambda (which)
	     (let ([snip/strings (list-ref (fw:preferences:get
					    'mred:console-previous-exprs) 
					   which)])
	       (begin-edit-sequence)
	       (unless prompt-mode?
		 (insert-prompt))
	       (delete prompt-position (last-position) #f)
	       (for-each (lambda (snip/string)
			   (insert (if (is-a? snip/string mred:snip%)
				       (send snip/string copy)
				       snip/string)
				   prompt-position))
			 snip/strings)
	       (set-position (last-position))
	       (end-edit-sequence)))]
	  [copy-next-previous-expr
	   (lambda ()
	     (let ([previous-exprs (fw:preferences:get 'mred:console-previous-exprs)])
	       (unless (null? previous-exprs)
		 (set! previous-expr-pos
		       (if (< (add1 previous-expr-pos) (length previous-exprs))
			   (add1 previous-expr-pos)
			   0))
		 (copy-previous-expr previous-expr-pos))))]
	  [copy-prev-previous-expr
	   (lambda ()
	     (let ([previous-exprs (fw:preferences:get 'mred:console-previous-exprs)])
	       (unless (null? previous-exprs)
		 (set! previous-expr-pos
		       (if (<= previous-expr-pos 0)
			   (sub1 (length previous-exprs))
			   (sub1 previous-expr-pos)))
		 (copy-previous-expr previous-expr-pos))))]
	  
	  [do-save-and-eval
	   (lambda (start end)
	     (split-snip start)
	     (split-snip end)
	     (let ([snips
		    (let loop ([snip (find-snip start 'after-or-none)]
			       [snips null])
		      (cond
			[(not snip) snips]
			[(<= (get-snip-position snip) end)
			 (loop (send snip next)
			       (cons (send snip copy) snips))]
			[else snips]))])
	       (set! previous-expr-positions (cons (cons start end) previous-expr-positions))
	       (set! previous-expr-pos -1)
	       (let* ([previous-exprs (fw:preferences:get 'mred:console-previous-exprs)]
		      [new-previous-exprs 
		       (let* ([trimmed-previous-exprs
			       (if (>= (length previous-exprs) console-max-save-previous-exprs)
				   (cdr previous-exprs)
				   previous-exprs)])
			 (let loop ([l trimmed-previous-exprs])
			   (if (null? l)
			       (list snips)
			       (cons (car l) (loop (cdr l))))))])
		 (fw:preferences:set 'mred:console-previous-exprs new-previous-exprs))
	       (do-eval start end)))]
	  
	  [reset-pretty-print-width
	   (lambda ()
	     (let* ([standard (send (get-style-list) find-named-style "Standard")])
	       (when standard
		 (let* ([admin (get-admin)]
			[width
			 (let ([bw (box 0)]
			       [b2 (box 0)])
			   (send admin get-view b2 b2 bw b2)
			   (unbox bw))]
			[dc (send admin get-dc)]
			[new-font (send standard get-font)]
			[old-font (send dc get-font)])
		   (send dc set-font new-font)
		   (let* ([char-width (send dc get-char-width)]
			  [min-columns 50]
			  [new-columns (max min-columns 
					    (floor (/ width char-width)))])
		     (send dc set-font old-font)
		     (mzlib:pretty-print:pretty-print-columns new-columns))))))]
	  [do-eval
	   (lambda (start end)
	     (error 'do-eval "abstract method"))]
	  [do-pre-eval
	   (lambda ()
	     (ready-non-prompt))]
	  [do-post-eval
	   (lambda ()
	     (insert-prompt))])
	
	(private
	  [only-spaces-after
	   (lambda (pos)
	     (let ([last (last-position)])
	       (let loop ([pos pos])
		 (if (= pos last)
		     #t
		     (let ([c (get-character pos)])
		       (if (char-whitespace? c)
			   (loop (add1 pos))
			   #f))))))])
	
	(public
	  [eval-busy? (lambda () #f)])
	(override
	  [on-local-char
	   (lambda (key)
	     (let ([cr-code 13]
		   [lf-code 10]
		   [start (get-start-position)]
		   [end (get-end-position)]
		   [last (last-position)]
		   [code (send key get-key-code)]
		   [copy-to-end/set-position
		    (lambda (start end)
		      (split-snip start)
		      (split-snip end)
		      (let loop ([snip (find-snip start 'after)])
			(cond
			  [(not snip) (void)]
			  [(< (get-snip-position snip) end)
			   (insert (send snip copy) (last-position))
			   (loop (send snip next))]
			  [else (void)]))
		      (set-position (last-position)))])
	       (cond
		 [(not (and (char? code) (or (char=? code #\return) (char=? code #\newline))))
		  (super-on-local-char key)]
		 [(and (< start end) (< end prompt-position)
		       (not (eval-busy?)))
		  (begin-edit-sequence)
		  (when (not prompt-mode?)
		    (insert-prompt))
		  (copy-to-end/set-position start end)
		  (end-edit-sequence)]
		 [(and (= start last) 
		       (not prompt-mode?)
		       (not (eval-busy?)))
		  (insert-prompt)]
		 [(and (< prompt-position start)
		       (only-spaces-after start)
		       (not (eval-busy?)))
		  (let ([balanced? (fw:scheme-paren:balanced?
				    this
				    prompt-position
				    last)])
		    (if balanced?
			(begin
			  (delete start last)
			  (do-save-and-eval prompt-position start))
			(super-on-local-char key)))]
		 [(< start prompt-position)
		  (let ([match
			    (fw:scheme-paren:backward-match
			     this start 0)])
		    (if match
			(begin
			  (begin-edit-sequence)
			  (copy-to-end/set-position match start)
			  (end-edit-sequence))
			(super-on-local-char key)))]
		 [else (super-on-local-char key)])))])
	(public
	  [insert-prompt
	   (lambda ()
	     (set! prompt-mode? #t)
	     (begin-edit-sequence)
	     (let* ([last (last-position)]
		    [start-selection (get-start-position)]
		    [end-selection (get-end-position)]
		    [last-str (get-text (- last 1) last)])
	       (unless (string=? last-str newline-string)
		 (insert #\newline last))
	       (let ([last (last-position)])
		 (insert (get-prompt) last)
		 (change-style normal-delta last (last-position)))
	       (set! prompt-position (last-position))
	       ;(clear-undos)
	       (end-edit-sequence)
	       (scroll-to-position start-selection #f (last-position) 'start)))])
	(public
	  [reset-console
	   (lambda ()
	     (void))])
	(public
	  [ready-non-prompt
	   (lambda ()
	     (when prompt-mode?
	       (set! prompt-mode? #f)
	       (let ([c-locked (locked?)])
		 (begin-edit-sequence)
		 (lock #f)
		 (insert #\newline (last-position))
		 (lock c-locked)	   
		 (end-edit-sequence))))]
	  
	  [initialize-console
	   (lambda ()
	     #t)])
	(sequence
	  (apply super-init args)))))
  
  (define make-transparent-io-edit%
    (lambda (super%)
      (class super% args
	(inherit change-style prompt-position set-prompt-position
		 resetting? set-resetting lock get-text
		 set-position last-position get-character
		 clear-undos set-cursor
		 do-pre-eval do-post-eval)
	(rename [super-on-insert on-insert]
		[super-on-local-char on-local-char])
	(private
	  [input-delta (make-object mred:style-delta%)]
	  [data null])
	(sequence
	  (let ([mult (send input-delta get-foreground-mult)]
		[add (send input-delta get-foreground-add)])
	    (send mult set 0 0 0)
	    (send add set 0 150 0)))
	(private [shutdown? #f])

	(public
	  [potential-sexps-protect (make-semaphore 1)]
	  [potential-sexps null]
	  [wait-for-sexp (make-semaphore 0)]
	  
	  [auto-set-wrap #t]
	  [shutdown
	   (lambda ()
	     (set! shutdown? #t)
	     (lock #t))]
	  [consumed-delta 
	   (make-object mred:style-delta% 'change-bold)]
	  [mark-consumed
	   (lambda (start end)
	     (let ([old-resetting resetting?])
	       (set-resetting #t)
	       (change-style consumed-delta start end)
	       (set-resetting old-resetting))
	     (set-prompt-position end))]
	  [fetch-sexp
	   (lambda ()
	     (set-cursor (make-object mred:cursor% 'ibeam))
	     (let loop ()
	       (let ([yield/loop? #f]
		     [answer #f])
		 (dynamic-wind
		  (lambda ()
		    (set! yield/loop? #f)
		    (set! answer #f)
		    (semaphore-wait potential-sexps-protect))
		  (lambda ()
		    (cond
		      [shutdown? 
		       (void)]
		      [(null? potential-sexps)
		       (set! yield/loop? #t)]
		      [else (let* ([sexp (car potential-sexps)]
				   [start (car sexp)]
				   [end (cdr sexp)]
				   [text (get-text start end #t)])
			      (set! potential-sexps (cdr potential-sexps))
			      (mark-consumed start end)
			      (clear-undos)
			      (set! answer (read (open-input-string text))))]))
		  (lambda ()
		    (semaphore-post potential-sexps-protect)))
		 (begin0
		   (cond
		     [yield/loop?
		      ((with-handlers ([exn:misc:user-break?
				       (lambda (x) void)])
		         (mzlib:thread:dynamic-enable-break (lambda () (mred:yield wait-for-sexp)))
			 loop))]
		     [answer answer]
		     [else (void)])
		   (set-cursor #f)))))]
	  [fetch-char
	   (lambda ()
	     (let ([found-char
		    (lambda (pos)
		      (mark-consumed pos (add1 pos))
		      (get-character pos))])
	       (let loop ()
		 (let ([first-case? #f]
		       [second-case? #f]
		       [third-case? #f])
		   (dynamic-wind
		    (lambda ()
		      [set! first-case? #f]
		      [set! second-case? #f]
		      [set! third-case? #f]
		      (semaphore-wait potential-sexps-protect))
		    (lambda ()
		      (cond
			[(not (null? potential-sexps))
			 (let ([first-sexp (car potential-sexps)])
			   (set! potential-sexps null)
			   (set! first-case? (found-char (car first-sexp))))]
			[(< prompt-position (last-position))
			 (set! second-case? #t)]
			[else 
			 (set! third-case? #t)]))
		    (lambda ()
		      (semaphore-post potential-sexps-protect)))
		   (cond
		     [first-case? first-case?]
		     [second-case? 
		      (found-char prompt-position)]
		     [third-case?
		      ((with-handlers ([exn:misc:user-break?
					(lambda (x) void)])
		        (mzlib:thread:dynamic-enable-break (lambda () (mred:yield wait-for-sexp)))
			loop))])))))])
	(override
	  [get-prompt (lambda () "")])
	(override
	  [on-insert
	   (lambda (start len)
	     (let ([old-r resetting?])
	       (set-resetting #t)
	       (change-style input-delta start (+ start len))
	       (set-resetting old-r))
	     (super-on-insert start len))])
	(override
	  [do-eval
	   (lambda (start end)
	     (do-pre-eval)
	     (let ([new-sexps
		    (let loop ([pos start])
		      (cond
			[(< pos end) 
			 (let ([next-sexp
				(fw:scheme-paren:forward-match
				 this pos end)])
			   (cons (cons pos next-sexp)
				 (loop next-sexp)))]
			[else null]))])
	       (semaphore-wait potential-sexps-protect)
	       (set! potential-sexps (append potential-sexps new-sexps))
	       (semaphore-post potential-sexps-protect)
	       (semaphore-post wait-for-sexp)
	       (do-post-eval)))])
	(sequence
	  (apply super-init args)))))
  
  (define console-edit% (make-console-edit% fw:scheme:text%))

  (define transparent-io-edit% 
    (make-transparent-io-edit%
     (make-console-edit%
      fw:text:searching%)))
  
  (define edit% (make-edit% console-edit%)))