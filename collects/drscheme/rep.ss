
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
	  [drscheme:frame : drscheme:frame^]
	  [basis : userspace:basis^]
	  [drscheme:edit : drscheme:edit^])

  ;; Max length of output queue (user's thread blocks if the
  ;; queue is full):
  (define output-limit-size 500)

  ;; note: the parameter basis:current-setting contains the setting
  ;; currently in use in the repl. The preference drscheme:setting,
  ;; however, contains the current settings in the language dialog.

  (define (printf . args) (apply fprintf drscheme:init:original-output-port args))

  (define (system thunk)
    (parameterize ([current-output-port drscheme:init:original-output-port]
		   [current-error-port drscheme:init:original-error-port]
		   [current-custodian drscheme:init:system-custodian]
		   [mred:current-eventspace drscheme:init:system-eventspace]
		   [break-enabled #f])
      (thunk)))
  
  (define setup-scheme-interaction-mode-keymap
    (lambda (keymap)
      (send keymap add-function "put-previous-sexp"
	    (lambda (edit event) 
	      (send edit copy-prev-previous-expr)))
      (send keymap add-function "put-next-sexp"
	    (lambda (edit event) 
	      (send edit copy-next-previous-expr)))

      (fw:keymap:send-map-function-meta keymap "p" "put-previous-sexp")
      (fw:keymap:send-map-function-meta keymap "n" "put-next-sexp")))

  (define scheme-interaction-mode-keymap (make-object mred:keymap%))
  (setup-scheme-interaction-mode-keymap scheme-interaction-mode-keymap)

  (define welcome-delta (make-object mred:style-delta% 'change-family 'decorative))
  (define click-delta (make-object mred:style-delta%))
  (define red-delta (make-object mred:style-delta%))
  (send* click-delta
    (copy welcome-delta)
    (set-delta-foreground "BLUE")
    (set-delta 'change-underline #t))
  (send* red-delta
    (copy welcome-delta)
    (set-delta-foreground "RED"))
  (define warning-style-delta (make-object mred:style-delta% 'change-bold))
  (send* warning-style-delta
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
	   (lambda (dc x y left top right bottom dx dy draw-caret)
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
  
  (define (setting-has-mred? setting)
    (let ([name (basis:setting-name setting)])
      (or (string=? "MrEd" name)
	  (string=? "MrEd Debug" name))))

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
	   (let ([c-locked? (locked?)])
	     (begin-edit-sequence)
	     (lock #f)
	     (let ([starting-at-prompt-mode? prompt-mode?])
	       (set! transparent-edit (make-object transparent-io-edit%))

	       (send transparent-edit auto-wrap #t)

	       ;; ensure that there is a newline before the snip is inserted
	       (unless (member 'hard-newline
			       (send (find-snip (last-position) 'before) get-flags))
		 (insert (string #\newline) (last-position) (last-position) #f))
	       
	       (when starting-at-prompt-mode?
		 (set-prompt-mode #f))

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
	     (lock c-locked?)
	     (end-edit-sequence)))]

	  [this-in-read
	   (lambda ()
	     (let ([s (make-semaphore 0)]
		   [answer #f])
	       (system
		(lambda ()
		  (mred:queue-callback
		   (lambda ()
		     (init-transparent-io #t)
		     (set! answer (send transparent-edit fetch-char))
		     (semaphore-post s))
		   #f)))
	       (semaphore-wait s)
	       answer))])


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

	(private
	  [flushing-event-running? #f]
	  [limiting-sema (make-semaphore)]
	  [io-collected-thunks null]
	  [io-collected-edits null]
	  [run-io-collected-thunks
	   (lambda ()
	     ;; also need to start edit-sequence in any affected
	     ;; transparent io boxes.
	     (begin-edit-sequence)
	     (for-each (lambda (t) (semaphore-post limiting-sema) (t))
		       (reverse io-collected-thunks))
	     (for-each (lambda (e) (send e end-edit-sequence)) io-collected-edits)
	     (end-edit-sequence)

	     (set! io-collected-edits null)
	     (set! io-collected-thunks null))]
	  [wait-for-io-to-complete
	   (lambda ()
	     (let ([semaphore (make-semaphore 0)])
	       (system
		(lambda ()
		  (mred:queue-callback
		   (lambda ()
		     (run-io-collected-thunks)
		     (semaphore-post semaphore))
		   #f)))
	       (semaphore-wait semaphore)))]
	  [queue-io
	   (lambda (thunk)
	     (semaphore-wait limiting-sema)
	     (let ([this-eventspace user-eventspace])
	       (system
		(lambda ()
		  (mred:queue-callback
		   (lambda ()
		     (set! io-collected-thunks
			   (cons
			    (lambda ()
			      (when (eq? this-eventspace user-eventspace)
				(thunk)))
			    io-collected-thunks))
		     (unless flushing-event-running?
		       (set! flushing-event-running? #t)
		       (mred:queue-callback
			(lambda ()
			  (run-io-collected-thunks)
			  (set! flushing-event-running? #f))
			#f)))
		   #f)))))])

	(public
	  [generic-write
	   (lambda (edit s style-func)
	     
	     (unless (or (eq? this edit) (member edit io-collected-edits))
	       (set! io-collected-edits (cons edit io-collected-edits))
	       (send edit begin-edit-sequence))
	     
	     (when prompt-mode?
	       (insert (string #\newline) (last-position) (last-position) #f)
	       (set-prompt-mode #f))

	     (let* ([start (send edit last-position)]
		    [c-locked? (send edit locked?)])
	       (send edit begin-edit-sequence)
	       (send edit lock #f)
	       (send edit insert
		     (if (is-a? s mred:original:snip%)
			 (send s copy)
			 s)
		     start
		     start
		     #t)
	       (let ([end (send edit last-position)])
		 (style-func start end)
		 (send edit set-prompt-position end))
	       (send edit lock c-locked?)
	       (send edit end-edit-sequence)))]
	  [generic-close (lambda () (void))]
	  
	  [saved-newline? #f]

	  [this-result-write 
	   (lambda (s)
	     (queue-io
	      (lambda ()
		(cleanup-transparent-io)
		(generic-write this
			       s
			       (lambda (start end)
				 (change-style result-delta
					       start end))))))]
	  [this-out-write
	   (lambda (s)
	     (queue-io
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
		  (gw s1)))))]

	  [this-err-write/exn
	   (let ([fallthru-regexp (regexp "^()([a-z:-]*): ")]
		 [class-regexp (regexp "^(.*[^a-z:-])([a-z:-]+(<%>|%))")]
		 [ivar-regexp (regexp "^(ivar: instance variable not found: )([a-z:-]*)")])
	     (lambda (s exn)
	       (queue-io
		(lambda ()
		  (cleanup-transparent-io)
		  (generic-write
		   this
		   s
		   (lambda (start end)
		     (change-style error-delta start end)
		     (cond
		      [(exn:variable? exn)
		       (let* ([var (symbol->string (exn:variable-id exn))]
			      [regexp (format "^(.*)(~a)" var)]
			      [match (regexp-match regexp s)])
			 (when match
			   (let* ([var-start (+ start (string-length (cadr match)))]
				  [var-end (+ var-start (string-length (caddr match)))])
			     (change-style click-delta var-start var-end)
			     (set-clickback var-start var-end
					    (lambda x
					      (drscheme:frame:help-desk var))))))]
		      [else
		       (let ([bind-to-help
			      (lambda (regexp)
				(let ([match (regexp-match regexp s)])
				  (when match
				    (let* ([prefix (cadr match)]
					   [var (caddr match)]
					   [var-start (+ start (string-length prefix))]
					   [var-end (+ var-start (string-length var))])
				      (change-style click-delta var-start var-end)
				      (set-clickback
				       var-start var-end
				       (lambda x
					 (drscheme:frame:help-desk var)))))))])
			 (bind-to-help fallthru-regexp)
			 (bind-to-help class-regexp)
			 (bind-to-help ivar-regexp))])))))))]
	  [this-err-write
	   (lambda (s)
	     (this-err-write/exn s #f))]
	  
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
						     (lambda (x _ port)
						       (and (is-a? x mred:original:snip%) 1))]
						    [mzlib:pretty-print:pretty-print-print-hook
						     (lambda (x _ port)
						       (port-out-write x))]
						    [mzlib:pretty-print:pretty-print-columns
						     'infinity])
				       (pretty v p)))))))])
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
		(let ([v (if (basis:r4rs-style-printing? (basis:current-setting))
			     v
			     (print-convert:print-convert v))])
		  (parameterize ([mzlib:pretty-print:pretty-print-size-hook
				  (lambda (x _ port) (and (is-a? x mred:original:snip%) 1))]
				 [mzlib:pretty-print:pretty-print-print-hook
				  (lambda (x _ port) (this-result-write x))])
		    (mzlib:pretty-print:pretty-print v this-result)))))
	    anss))])

      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                            ;;;
      ;;;             Zodiac Interface               ;;;
      ;;;                                            ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (public
	[report-located-error
	 (lambda (message di exn)
	   (if (and (zodiac:zodiac? di)
		    (basis:zodiac-vocabulary? user-setting))
	       (let* ([start (zodiac:zodiac-start di)]
		      [finish (zodiac:zodiac-finish di)])
		 (report-error start finish 'dynamic message exn))
	       (report-unlocated-error message exn)))]
	[report-unlocated-error
	 (lambda (message exn)
	   (let* ([frame (get-top-level-window)]
		  [interactions-edit (ivar frame interactions-edit)])
	     (send frame ensure-interactions-shown)
	     (let ([locked? (send interactions-edit locked?)])
	       (send interactions-edit begin-edit-sequence)
	       (send interactions-edit lock #f)
	       (send interactions-edit this-err-write/exn
		     (string-append message (string #\newline))
		     exn)
	       (send interactions-edit lock locked?)
	       (send interactions-edit end-edit-sequence))))]
	[report-error
	 (lambda (start-location end-location type input-string exn)
	   (let* ([start (zodiac:location-offset start-location)]
		  [finish (add1 (zodiac:location-offset end-location))]
		  [file (zodiac:location-file start-location)]
		  [message
		   (if (is-a? file mred:text%)
		       input-string
		       (string-append (basis:format-source-loc start-location end-location)
				      input-string))])
	     (report-unlocated-error message exn)
	     (when (is-a? file mred:text%)
	       (send file begin-edit-sequence)
	       (wait-for-io-to-complete)
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
	   (if (basis:zodiac-vocabulary? user-setting)
	       (process-edit/zodiac edit fn start end annotate?)
	       (process-edit/no-zodiac edit fn start end)))]
	[process-file
	 (lambda (filename fn annotate?)
	   (if (basis:zodiac-vocabulary? user-setting)
	       (basis:process-file/zodiac filename fn annotate?)
	       (basis:process-file/no-zodiac filename fn)))]
	[process-sexp
	 (lambda (sexp z fn annotate?)
	   (if (basis:zodiac-vocabulary? user-setting)
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
	[user-setting (fw:preferences:get 'drscheme:settings)]
	[user-custodian (make-custodian)]
	[user-eventspace #f]
	[user-thread #f])
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
	     (insert
	      "WARNING: Interactions window is out of sync with the definitions window. Click Execute."
	      start start)
	     (let ([end (last-position)])
	       (change-style warning-style-delta start end)))
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
	[cleanup
	 (lambda ()
	   (unless (thread-running? user-thread)
	     (lock #t)
	     (unless shutting-down?
	       (mred:message-box
		"Warning"
		(format "The evaluation thread is no longer running, ~
			 so no evaluation can take place until ~
			 the next execution.")))))]
	[cleanup-interaction
	 (lambda ()
	   (system
	    (lambda ()
	      (mred:end-busy-cursor)
	      (begin-edit-sequence)
	      (wait-for-io-to-complete)
	      (cleanup-transparent-io)
	      (set-caret-owner #f 'display)
	      (when (thread-running? user-thread)
		(let ([c-locked? (locked?)])
		  (lock #f)
		  (insert-prompt)
		  (lock c-locked?)))
	      (cleanup)
	      (end-edit-sequence)
	      (send (get-top-level-window) enable-evaluation))))]
	[cleanup-evaluation
	 (lambda ()
	   (begin-edit-sequence)
	   (wait-for-io-to-complete)
	   (cleanup)
	   (end-edit-sequence))])
      (public
	[do-many-buffer-evals
	 (lambda (edit start end)
	   (semaphore-wait in-evaluation-semaphore)
	   (cond
	    [in-evaluation?
	     (semaphore-post in-evaluation-semaphore)
	     (mred:bell)]
	    [else
	     (set! in-evaluation? #t)
	     (semaphore-post in-evaluation-semaphore)
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
		(with-running-flag
		 (lambda ()
		   (protect-user-evaluation
		    (lambda ()
		      (cleanup-interaction))
		    (lambda ()
		      (process-edit
		       edit
		       (lambda (expr recur)
			 (cond
			  [(basis:process-finish? expr)
			   (void)]
			  [else
			   (let ([answers
				  (call-with-values
				      (lambda ()
					(mzlib:thread:dynamic-enable-break
					 (lambda ()
					   (if (basis:zodiac-vocabulary? (basis:current-setting))
					       (basis:syntax-checking-primitive-eval expr)
					       (basis:primitive-eval expr)))))
				    (lambda x x))])
			     (display-results answers)
			     (recur))]))
		       start
		       end
		       #t)))))))]))])
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
	[break (lambda ()
		 (cond
		  [(not in-evaluation?)
		   (mred:bell)]
		  [ask-about-kill? 
		   (if (fw:gui-utils:get-choice
			"Do you want to kill the evaluation?"
			"Just Break"
			"Kill"
			"Kill?")
		       (break-thread user-thread)
		       (shutdown-user-custodian))]
		  [else 
		   (break-thread user-thread)
		   (set! ask-about-kill? #t)]))])
      (public
	[error-escape-k void])

      (private
	[eval-thread-thunks null]
	[eval-thread-state-sema (make-semaphore 1)]
	[eval-thread-queue-sema (make-semaphore 0)]
	
	[cleanup-sucessful 'not-yet-cleanup-sucessful]
	[cleanup-semaphore 'not-yet-cleanup-semaphore]
	[thread-grace 'not-yet-thread-grace]
	[thread-kill 'not-yet-thread-kill]
	
	[killed-callback void]
	[thread-killed 'not-yet-thread-killed]
	[initialize-killed-thread
	 (lambda ()
	   (system
	    (lambda ()
	      (when (thread? thread-killed)
		(kill-thread thread-killed))
	      (set! thread-killed
		    (thread
		     (lambda ()
		       (thread-wait user-thread)
		       (killed-callback)))))))]

	[protect-user-evaluation
	 (lambda (cleanup thunk)
	   ;; in-evaluation? flag must be set to #t, before getting here.
	   ;; for repl / execute evaluation, it must be set in the 
	   ;; impl eventspace's thread

	   (let/ec k
	     (let ([saved-killed-callback killed-callback]
		   [saved-error-escape-k error-escape-k])
	       (dynamic-wind
		(lambda ()
		  (set! killed-callback cleanup)
		  (set! error-escape-k (lambda () (k (void)))))
		(lambda () (thunk))
		(lambda () 
		  (set! killed-callback saved-killed-callback)
		  (set! error-escape-k saved-error-escape-k)
		  (cleanup)))))

	   (semaphore-wait in-evaluation-semaphore)
	   (set! in-evaluation? #f)
	   (semaphore-post in-evaluation-semaphore))])
      (public
	[evaluation-thread #f]
	[run-in-evaluation-thread 
	 (lambda (thunk)
	   (semaphore-wait eval-thread-state-sema)
	   (set! eval-thread-thunks (append eval-thread-thunks (list thunk)))
	   (semaphore-post eval-thread-state-sema)
	   (semaphore-post eval-thread-queue-sema))]
	[init-evaluation-thread
	 (lambda ()
	   (set! user-custodian (make-custodian))
	   (set! user-eventspace (parameterize ([current-custodian user-custodian])
				   (mred:make-eventspace)))
	   (set! limiting-sema (make-semaphore output-limit-size))
	   (parameterize ([mred:current-eventspace user-eventspace]
			  [current-custodian user-custodian])
	     (mred:queue-callback
	      (lambda ()
		(mzlib:thread:dynamic-disable-break
		 (lambda ()

		   (set! user-thread (current-thread))

		   (initialize-parameters)
		   (initialize-killed-thread)

		   (let ([drscheme-error-escape-handler
			  (lambda ()
			    (error-escape-k))])
		     (error-escape-handler drscheme-error-escape-handler)
		     (basis:bottom-escape-handler drscheme-error-escape-handler))

		   (send (get-top-level-window) not-running)
		   (set! evaluation-thread (current-thread))
		   (let loop ()
		     (unless (semaphore-try-wait? eval-thread-queue-sema)
		       (mred:yield eval-thread-queue-sema))
		     (semaphore-wait eval-thread-state-sema)
		     (let ([thunk (car eval-thread-thunks)])
		       (set! eval-thread-thunks (cdr eval-thread-thunks))
		       (semaphore-post eval-thread-state-sema)
		       (thunk))
		     (loop))))))))])
      (public
	[shutting-down? #f]
	[shutdown 
	 (lambda ()
	   (set! shutting-down? #t)
	   (shutdown-user-custodian))])
      
	(private
	  [with-running-flag
	   (lambda (thunk)
	     (dynamic-wind
	      (lambda ()
		(update-running #t))
	      (lambda ()
		(thunk))
	      (lambda ()
		(update-running #f))))]

	  [start-callback? #f]

	  [waiting-to-turn-on #f]
	  [skip-turning-on #f]
	  [turn-on-semaphore (make-semaphore 1)]
	  
	  [wait-to-turn-on (make-semaphore 0)]
	  
	  [running-callback-start
	   (lambda ()
	     (semaphore-wait turn-on-semaphore)
	     (set! skip-turning-on #f)
	     (unless waiting-to-turn-on
	       (set! waiting-to-turn-on #t)
	       (semaphore-post wait-to-turn-on))
	     (semaphore-post turn-on-semaphore))]
	  [running-callback-stop
	   (lambda ()
	     (semaphore-wait turn-on-semaphore)
	     (when waiting-to-turn-on
	       (set! skip-turning-on #t))
	     (semaphore-post turn-on-semaphore)
	     (conditionally-turn-running-off))]
	  [running-thread
	   (thread
	    (lambda ()
	      (let loop ()
		(semaphore-wait wait-to-turn-on)
		(sleep)
		(semaphore-wait turn-on-semaphore)
		(unless skip-turning-on
		  (update-running #t))
		(set! waiting-to-turn-on #f)
		(semaphore-post turn-on-semaphore)
		(loop))))]

	  [turned-on #f]
	  [turned-on-semaphore (make-semaphore 1)]
	  
	  [update-running/semaphore
	   (lambda (flag)
	     (when (eq? flag turned-on)
	       (semaphore-post turned-on-semaphore)
	       (error 'update-running "flags are already the same (~a)!" flag))
	     (set! turned-on flag)
	     (system
	      (lambda ()
		(if turned-on
		    (mred:queue-callback (lambda () (send (get-top-level-window) running)))
		    (mred:queue-callback (lambda () (send (get-top-level-window) not-running)))))))]
	  [update-running
	   (lambda (flag)
	     (semaphore-wait turned-on-semaphore)
	     (update-running/semaphore flag)
	     (semaphore-post turned-on-semaphore))]
	  [conditionally-turn-running-off
	   (lambda ()
	     (semaphore-wait turned-on-semaphore)
	     (when turned-on
	       (update-running/semaphore #f))
	     (semaphore-post turned-on-semaphore))])


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
      
      (private
	[initialize-parameters
	 (lambda ()
	   (let ([setting (fw:preferences:get 'drscheme:settings)])

	     (basis:initialize-parameters
	      user-custodian
	      (if (setting-has-mred? setting)
		  (list 'mred)
		  (list))
	      setting)

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
				      (lambda (x _ port) (and (is-a? x mred:original:snip%) 1))]
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
		(let ([ans (if (is-a? expr mred:original:snip%)
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
		       (if (and (basis:zodiac-vocabulary? user-setting)
				(let* ([p (open-input-file filename)]
				       [loc (zodiac:make-location basis:initial-line
								  basis:initial-column
								  basis:initial-offset
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
				    (let ([edit (make-object drscheme:edit:edit%)])
				      (send edit load-file filename)
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
		       (if rep
			   (send rep report-unlocated-error msg #f)
			   (mred:message-box "Uncaught Error" msg))))))
	     
	     (let ([directory
		    (let/ec k
		      (unless (get-top-level-window)
			(k drscheme:init:first-dir))
		      (let*-values ([(filename) (send (ivar (get-top-level-window) definitions-edit)
						      get-filename)]
				    [(normalized) (if (string? filename)
						      (mzlib:file:normalize-path filename)
						      (k drscheme:init:first-dir))]
				    [(base _1 _2) (split-path normalized)])
			(or base 
			    drscheme:init:first-dir)))])
	       (current-directory directory))
	     
	     (exit-handler (lambda (arg) (shutdown-user-custodian)))
	     
	     ;; set all parameters before constructing eventspace
	     ;; so that the parameters are set in the eventspace's
	     ;; parameterization
	     (let* ([primitive-dispatch-handler (mred:event-dispatch-handler)]
		    [depth 0])
	       
	       (mred:event-dispatch-handler
		(rec drscheme-event-dispatch-handler
		     (lambda (eventspace)
		       (mzlib:thread:dynamic-disable-break
			(lambda ()
			  (cond
			   [(eq? eventspace user-eventspace)

			    (set! depth (+ depth 1))

			    (cond
			     [(and (= depth 1)
				   (not in-evaluation?))

			      (system
			       (lambda ()
				 (reset-break-state)
				 (running-callback-start)))

			      (protect-user-evaluation
			       (lambda ()
				 (system
				  (lambda ()
				    (running-callback-stop)
				    (cleanup-evaluation)
				    (set! in-evaluation? #f)
				    (set! depth (- depth 1)))))
			       (lambda ()
				 (set! in-evaluation? #t)
				 (mzlib:thread:dynamic-enable-break
				  (lambda ()
				    (primitive-dispatch-handler eventspace)))))]
			     [else
			      (set! depth (- depth 1))])]
			   [else (primitive-dispatch-handler eventspace)])))))))))])
	
      (override
	[reset-console
	 (lambda ()
	   (clear-previous-expr-positions)
	   (shutdown-user-custodian)
	   (cleanup-transparent-io)
	   (set! should-collect-garbage? #t)

	   ;; in case the last evaluation thread was killed, clean up some state.
	   (lock #f)
	   (set! in-evaluation? #f)
	   (conditionally-turn-running-off)

	   (set! user-setting (fw:preferences:get 'drscheme:settings))

	   (begin-edit-sequence)
	   (set-resetting #t)
	   (delete (paragraph-start-position 1) (last-position))
	   (set-prompt-mode #f)
	   (set-resetting #f)
	   (set-position (last-position) (last-position))
	   (insert-delta "Language: " welcome-delta)
	   (insert-delta (basis:setting-name user-setting) red-delta)
	   (unless (equal? (basis:find-setting-named (basis:setting-name user-setting))
			   user-setting)
	     (insert-delta " Custom" red-delta))
	   (insert-delta (format ".~n") welcome-delta)
	   (set! repl-initially-active? #t)
	   (end-edit-sequence)

	   (init-evaluation-thread)
	   
	   (super-reset-console))]
	[initialize-console
	 (lambda ()
	   (super-initialize-console)
	   
	   (insert-delta "Welcome to " welcome-delta)
	   (let-values ([(before after)
			 (insert-delta "DrScheme" click-delta)])
	     (insert-delta (format ", version ~a.~n" (fw:version:version))
			   welcome-delta)
	     (set-clickback before after 
			    (lambda args (drscheme:app:about-drscheme))
			    click-delta))
	 
	   (if (or (fw:preferences:get 'drscheme:repl-always-active)
		   (not (send (ivar (get-top-level-window) interactions-edit) get-filename)))
	       (reset-console)
	       (begin 
		 (insert-delta "Execute has not been clicked." warning-style-delta)
		 (lock #t))))])
      (sequence
	(set-display/write-handlers)
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

	(override 
	 [autosave? (lambda () #f)])

	(private
	  [edit-sequence-count 0])
	(public
	  [orig-stdout (current-output-port)]
	  [orig-stderr (current-error-port)])
	(public
	  [normal-delta #f])
	
	(rename [super-get-keymaps get-keymaps])
	(override
	 [get-keymaps
	  (lambda ()
	    (cons scheme-interaction-mode-keymap (super-get-keymaps)))])

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
	  ;; used to highlight the prompt that the caret is "in the range of".
	  ;; not currently used at all.
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
	  [can-something
	   (opt-lambda (super start len)
	     (cond
	       [(or resetting?
		    (not (number? prompt-position))
		    (>= start prompt-position))
		(super start len)]
	       [else #f]))]
	  [after-something
	   (lambda (combine start len)
	     (when (or resetting?
		       (and prompt-mode? (< start prompt-position)))
	       (set! prompt-position (combine prompt-position len))))])
	(public
	  [resetting? #f]
	  [set-resetting (lambda (v) (set! resetting? v))])
	(override
	  [can-insert?
	   (lambda (start len)
	     (can-something super-on-insert start len))]
	  [can-delete?
	   (lambda (start len)
	     (can-something super-on-delete start len))]
	  [can-change-style?
	   (lambda (start len)
	     (can-something super-on-change-style start len))]
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
	   (lambda ()
	     (let ([snip/strings (list-ref (fw:preferences:get
					    'mred:console-previous-exprs) 
					   previous-expr-pos)])
	       (begin-edit-sequence)
	       (unless prompt-mode?
		 (insert-prompt))
	       (delete prompt-position (last-position) #f)
	       (for-each (lambda (snip/string)
			   (insert (if (is-a? snip/string mred:original:snip%)
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
		 (copy-previous-expr))))]
	  [copy-prev-previous-expr
	   (lambda ()
	     (let ([previous-exprs (fw:preferences:get 'mred:console-previous-exprs)])
	       (unless (null? previous-exprs)
		 (set! previous-expr-pos
		       (if (<= previous-expr-pos 0)
			   (sub1 (length previous-exprs))
			   (sub1 previous-expr-pos)))
		 (copy-previous-expr))))]
	  
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
		    [last-str (if (= last 0)
				  ""
				  (get-text (- last 1) last))])
	       (unless (or (string=? last-str newline-string)
			   (= last 0))
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
	  [ibeam-cursor (make-object mred:cursor% 'ibeam)]
	  [fetch-sexp
	   (lambda ()
	     (set-cursor ibeam-cursor)
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
	(inherit insert-prompt)
	(sequence
	  (apply super-init args)
	  (insert-prompt)))))

  (define transparent-io-edit% 
    (make-transparent-io-edit%
     (make-console-edit%
      fw:text:searching%)))
  
  (define edit% (make-edit% (make-console-edit% fw:scheme:text%))))