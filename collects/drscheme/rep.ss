; =Kernel= means in DrScheme's thread and parameterization
; 
; =User= means the user's thread and parameterization
; 
; =Handler= means in the handler thread of some eventspace; it must
;  be combined with either =Kernel= or =User=


(unit/sig drscheme:rep^
  (import [mred : mred^]
	  [mzlib : mzlib:core^]
	  [fw : framework^]
	  [print-convert : mzlib:print-convert^]
	  [zodiac : zodiac:system^]
	  [zodiac:interface : drscheme:interface^]
	  [drscheme:init : drscheme:init^]
	  [drscheme:snip : drscheme:snip^]
	  [drscheme:language : drscheme:language^]
	  [drscheme:app : drscheme:app^]
	  [drscheme:frame : drscheme:frame^]
	  [basis : userspace:basis^]
	  [drscheme:text : drscheme:text^]
          [help : help:drscheme-interface^])

  ;; Max length of output queue (user's thread blocks if the
  ;; queue is full):
  (define output-limit-size 2000)
  
  ;; note: the parameter basis:current-setting contains the setting
  ;; currently in use in the repl. The preference drscheme:setting,
  ;; however, contains the current settings in the language dialog.

  (define (printf . args) (apply fprintf drscheme:init:original-output-port args))

  (define setup-scheme-interaction-mode-keymap
    (lambda (keymap)
      (send keymap add-function "put-previous-sexp"
	    (lambda (text event) 
	      (send text copy-prev-previous-expr)))
      (send keymap add-function "put-next-sexp"
	    (lambda (text event) 
	      (send text copy-next-previous-expr)))

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

  (define invoke-teachpack void)
  (define core-flat@ (require-library-unit/sig "coreflatr.ss"))
  
  (fw:preferences:set-default 'drscheme:teachpack-file
			       #f
			       (lambda (x) (or (string? x)
					       (not x)
					       (and (list? x)
						    (andmap string? x)))))

  (define (build-teachpack-thunk v)
    (with-handlers
	([(lambda (x) #t)
	  (lambda (x)
	    (mred:message-box "Invalid Teachpack" (exn-message x))
	    #f)])
      (let ([new-unit (parameterize ([read-case-sensitive #t])
			(load/cd v))])
	(if (unit/sig? new-unit)
					; Put the unit into a procedure that invokes it into
					;  the current namespace
	    (let* ([signature 
					; exploded -> flattened
		    (let ([sig (unit-with-signature-exports new-unit)])
		      (let loop ([l (vector->list sig)][r null])
			(cond
			 [(null? l) r]
			 [(symbol? (car l)) (loop (cdr l) (cons (car l) r))]
			 [else (let ([sub (loop (vector->list (cadr l)) null)]
				     [prefix (string-append (symbol->string (car l)) ":")])
				 (loop (cdr l)
				       (append
					(map (lambda (s)
					       (string->symbol
						(string-append
						 prefix
						 (symbol->string s))))
					     sub))))])))])
	      (eval
	       `(lambda ()
		  (with-handlers ([(lambda (x) #t)
				   (lambda (x)
				     ((error-display-handler)
				      (format
				       "Invalid Teachpack:~n~a"
				       (if (exn? x) (exn-message x) x))))])
		    (global-define-values/invoke-unit/sig
		     ,signature
		     (compound-unit/sig
			 (import)
		       (link [userspace : plt:userspace^ 
					((compound-unit/sig 
					     (import)
					   (link [core : mzlib:core-flat^ (,core-flat@)]
						 [mred : mred^ (,mred:mred@)])
					   (export (open core)
						   (open mred))))]
			     [teachpack : ,signature (,new-unit userspace)])
		       (export (open teachpack))))))))
	    (begin
	      (mred:message-box 
	       "Invalid Teachpack"
	       "loading Teachpack file does not result in a unit/sig")
	      #f)))))

  (fw:preferences:add-callback 
   'drscheme:teachpack-file
   (lambda (p v)
     (if v
	 (let loop ([teachpacks (if (string? v)
				    (list v)
				    v)]
		    [thunk void])
	   (cond
	    [(null? teachpacks)
	     (set! invoke-teachpack thunk)]
	    [else
	     (let ([this-thunk (build-teachpack-thunk (car teachpacks))])
	       (if this-thunk
		   (loop (cdr teachpacks)
			 (lambda ()
			   (this-thunk)
			   (thunk)))
		   #f))]))
	 (set! invoke-teachpack void))))
    
  (define exception-reporting-rep (make-parameter #f))

  (define (process-text/zodiac text f start end annotate? text-is-file?)
    (let ([setting (basis:current-setting)]
	  [file (if text-is-file?
		    text
		    (or (send text get-filename)
			"Unknown"))])
      (basis:process/zodiac
       (parameterize ([read-case-sensitive (basis:setting-case-sensitive? setting)])
	 (zodiac:read (fw:gui-utils:read-snips/chars-from-text text start end)
		      (zodiac:make-location 0 0 start file)
		      #t 1))
       f
       annotate?)))

  (define (process-text/no-zodiac text f start end)
    (let* ([buffer-thunk (fw:gui-utils:read-snips/chars-from-text text start end)]
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

  (define-struct sexp (left right prompt))
  
  (define newline-string (string #\newline))
    
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
			   [(is-a? s mred:string-snip%)
			    (send s get-text 0 (send s get-count))]
			   [(string? s) s]
			   [else "'non-string-snip"]))
		       ls))
		lls))]
	[unmarshall (lambda (x) x)])
    (fw:preferences:set-un/marshall
     'mred:console-previous-exprs
     marshall unmarshall))
  (define (show-interactions-history)
    (let* ([f (make-object (drscheme:frame:basics-mixin fw:frame:standard-menus%)
                           "Interactions History"
                           #f
                           300
                           400)]
           [panel (send f get-panel)]
           [text (make-object mred:text%)]
           [canvas (make-object mred:editor-canvas% panel text)])
      (send f show #t)))

  (define error-color (make-object mred:color% "PINK"))
  (define color? (< 8 (mred:get-display-depth)))
  
  (define (quote-regexp-specials s)
    (list->string
     (let loop ([chars (string->list s)])
       (cond
	[(null? chars) null]
	[else
	 (case (car chars)
	   [(#\( #\) #\* #\+ #\? #\[ #\] #\. #\^ #\$ #\\)
	    (cons #\\ (cons (car chars) (loop (cdr chars))))]
	   [else (cons (car chars) (loop (cdr chars)))])]))))

  (define (in-canvas? text)
    (let ([editor-admin (send text get-admin)])
      (cond
       [(is-a? editor-admin mred:editor-snip-editor-admin<%>)
	(let* ([snip (send editor-admin get-snip)]
	       [snip-admin (send snip get-admin)])
	  (and snip-admin
	       (in-canvas? (send snip-admin get-editor))))]
       [(is-a? editor-admin mred:editor-admin%)
	(send text get-canvas)]
       [else #f])))

  (define context<%>
    (interface ()
      ensure-rep-shown
      needs-execution? 
      enable-evaluation
      disable-evaluation
      running
      not-running
      get-directory))

  (define (make-text% super%)
    (rec rep-text%
      (class/d super% (context)
        ((inherit insert change-style
                  set-styles-sticky
                  clear-undos set-caret-owner
                  clear-previous-expr-positions
                  get-end-position
                  set-clickback
                  do-post-eval
                  insert-prompt
                  erase prompt-mode?
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
                  get-admin
                  set-prompt-position
                  get-canvases find-snip
                  inserting-prompt)
         (rename [super-on-insert on-insert]
                 [super-on-delete on-delete]
                 [super-initialize-console initialize-console]
                 [super-reset-console reset-console])
         
         (override on-insert on-delete)
         
         (override get-prompt eval-busy? do-eval
                   initialize-console
                   reset-console)
         
         (public
           transparent-text
           transparent-snip
           cleanup-transparent-io
           init-transparent-io
           init-transparent-input
           init-transparent-io-do-work
           
           this-in-char-ready?
           this-in-read-char
           this-in-peek-char
           generic-write
           generic-close
           this-result-write
           this-out-write
           this-err-write/exn
           this-err-write
           this-err
           this-out
           this-in
           this-result
           set-display/write-handlers
           display-results
           
           report-located-error
           report-unlocated-error
           get-error-range
           reset-highlighting
           format-source-loc
           highlight-error
           report-error
           
           process-text
           process-file
           process-sexp
           
           user-setting
           user-custodian
           user-eventspace
           user-namespace
           user-thread
           
           insert-warning
           
           cleanup
           need-interaction-cleanup?
           cleanup-interaction
           
           do-many-text-evals
           
           reset-break-state
           break
           
           run-in-evaluation-thread
           
           shutdown))
        
        (unless (is-a? context context<%>)
          (error 'drscheme:rep:text% "expected an object that implements drscheme:rep:context<%> as initialization argument, got: ~e"
                 context))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;					     ;;;
	;;;            User -> Kernel                ;;;
	;;;					     ;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        [define protect
          (lambda (proc)
            (let ([ut user-thread])
              (call-in-nested-thread
               (lambda ()
                 (break-enabled #f)
                 (begin0
                   (proc ut)
                   (break-enabled #t))) ; in case a break was queued
               drscheme:init:system-custodian)))]
        
        [define queue-system-callback
          (case-lambda
           [(ut thunk) (queue-system-callback ut thunk #f)]
           [(ut thunk always?)
            (parameterize ([mred:current-eventspace drscheme:init:system-eventspace])
              (mred:queue-callback 
               (lambda ()
                 (when (or always? (eq? ut user-thread))
                   (thunk)))
               #f))])]
        
        [define queue-system-callback/sync
          (lambda (ut thunk)
            (let ([s (make-semaphore)])
              (queue-system-callback ut (lambda () (thunk) (semaphore-post s)))
              (semaphore-wait s)))]
        
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;					     ;;;
	;;;                  I/O                     ;;;
	;;;					     ;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        [define transparent-text #f]
        [define transparent-snip #f]
        [define cleanup-transparent-io ; =Kernel=, =Handler=
          (lambda ()
            (when transparent-text
              (set! saved-newline? #f) 
              (send transparent-text shutdown)
              (set-position (last-position))
              (set-caret-owner #f)
              (let ([a (get-admin)])
                (when a
                  (send a grab-caret)))
              (send transparent-text lock #t)
              (set! transparent-text #f)
              (drop-fetcher)))]
        
        [define init-transparent-io ; =Kernel=, =Handler=
          (lambda (grab-focus?)
            (begin-edit-sequence)
            (if transparent-text
                (when grab-focus?
                  (let ([a (send transparent-text get-admin)])
                    (when a
                      (send a grab-caret))))
                (init-transparent-io-do-work grab-focus?))
            (when  (eq? (current-thread) user-thread)
              (set-caret-owner transparent-snip 'display))
            (end-edit-sequence)
            transparent-text)]
        
        [define init-transparent-input ; =Kernel=, =Handler=
          (lambda ()
            (let ([text (init-transparent-io #t)])
              (mred:yield) ; to flush output and set `saved-newline?'
              (when saved-newline?
                (this-out-write "")
                (mred:yield)) ; flush output again
              text))]
        
        [define init-transparent-io-do-work  ; =Kernel=, =Handler=
          (lambda (grab-focus?)
            (let ([c-locked? (locked?)])
              (begin-edit-sequence)
              (lock #f)
              (let ([starting-at-prompt-mode? prompt-mode?])
                (set! transparent-text (make-object transparent-io-text%))
                
                (send transparent-text auto-wrap #t)
                (send transparent-text balance-required #f)
                
                ;; ensure that there is a newline before the snip is inserted
                (unless (member 'hard-newline
                                (send (find-snip (last-position) 'before) get-flags))
                  (insert (string #\newline) (last-position) (last-position) #f))
                
                (when starting-at-prompt-mode?
                  (set-prompt-mode #f))
                
                (let ([snip (make-object mred:editor-snip% transparent-text)])
                  (set! transparent-snip snip)
                  (insert snip (last-position) (last-position) #f)
                  (insert (string #\newline) (last-position) (last-position) #f)
                  (for-each (lambda (c) (send c add-wide-snip snip))
                            (get-canvases)))
                (when grab-focus?
                  (let ([a (send transparent-text get-admin)])
                    (when a
                      (send a grab-caret)))))
              (lock c-locked?)
              (end-edit-sequence)))]
        
        [define make-fetcher
          (lambda ()
            (make-object
                (class object% ()
                  (public
                    [fetch-char-sema (make-semaphore 1)]
                    [fetcher-spawned? #f]
                    [char-fetched-sema (make-semaphore)]
                    [char-fetched #f]
                    [fetch ; =Protected-User=
                     (lambda (ut peek?)
                       ; Only one reader at a time:
                       (semaphore-wait/enable-break fetch-char-sema)
                       ; Now we're the active reader...
                       (unless fetcher-spawned?
                         (set! fetcher-spawned? #t)
                         ; Spawn a fetcher:
                         (queue-system-callback
                          ut
                          (lambda () ; =Kernel=, =Handler=
                            (let ([text (init-transparent-input)])
                              (set! char-fetched (send text fetch-char)))
                            (semaphore-post char-fetched-sema))))
                       ; Wait for a char, allow breaks:
                       (with-handlers ([void (lambda (x)
                                               ; Let someone else try to read...
                                               (semaphore-post fetch-char-sema)
                                               (raise x))])
                         (semaphore-wait/enable-break char-fetched-sema))
                       ; Got the char (no breaks)
                       (if peek?
                           ; preserve the fecthed cahr
                           (semaphore-post char-fetched-sema)
                           ; Next reader'll have to spawn a fetcher
                           (set! fetcher-spawned? #f))
                       (begin0
                         char-fetched
                         ; Got our char; let another reader go
                         (semaphore-post fetch-char-sema)))])
                  (sequence (super-init)))))]
        [define fetcher #f]
        [define fetcher-semaphore (make-semaphore 1)]
        [define drop-fetcher ; =Kernel=, =Handler=
          (lambda ()
            (semaphore-wait fetcher-semaphore)
            (set! fetcher #f)
            (semaphore-post fetcher-semaphore))]
        [define this-in-fetch-char ; =User=
          (lambda (peek?)
            (protect
             (lambda (ut) ; =Protected-User=
               (semaphore-wait fetcher-semaphore)
               (unless fetcher (set! fetcher (make-fetcher)))
               (semaphore-post fetcher-semaphore)
               (send fetcher fetch ut peek?))))]
        
        [define this-in-char-ready?
          (lambda () ; =User=
            (protect
             (lambda (ut)  ; =Protected-User=
               (let ([answer #f]
                     [s (make-semaphore 0)])
                 (queue-system-callback
                  ut
                  (lambda () ; =Kernel=, =Handler=
                    (let ([text (init-transparent-input)])
                      (set! answer (send text check-char-ready?)))
                    (semaphore-post s)))
                 ; enable-break in case the thread dies and the callback never 
                 ;  happens:
                 (semaphore-wait/enable-break s)
                 answer))))]
        
        [define this-in-read-char ; =User=
          (lambda ()
            (this-in-fetch-char #f))]
        
        [define this-in-peek-char ; =User=
          (lambda ()
            (this-in-fetch-char #t))]
        
        [define output-delta (make-object mred:style-delta%
                               'change-weight
                               'bold)]
        [define result-delta (make-object mred:style-delta%
                               'change-weight
                               'bold)]
        [define error-delta (make-object mred:style-delta%
                              'change-style
                              'slant)]
        (send error-delta set-delta-foreground "RED")
        (send result-delta set-delta-foreground (make-object mred:color% 0 0 175))
        (send output-delta set-delta-foreground (make-object mred:color% 150 0 150))
        
        [define flushing-event-running (make-semaphore 1)]
        [define limiting-sema (make-semaphore output-limit-size)] ; waited once foreach in io-collected-thunks
        
        [define io-semaphore (make-semaphore 1)]
        [define io-collected-thunks null] ; protected by semaphore
        [define io-collected-texts null] ; always set in the kernel's handler thread
        [define run-io-collected-thunks ; =Kernel=, =Handler=
          (lambda ()
            ;; also need to start edit-sequence in any affected
            ;; transparent io boxes.
            (semaphore-wait io-semaphore)
            (let ([io-thunks io-collected-thunks])
              (set! io-collected-thunks null)
              (semaphore-post io-semaphore)
              
              (begin-edit-sequence)
              (for-each (lambda (t) (semaphore-post limiting-sema) (t))
                        (reverse io-thunks))
              (for-each (lambda (e) (send e end-edit-sequence)) io-collected-texts)
              (unless (null? io-thunks)
                (scroll-to-position (last-position)))
              (end-edit-sequence)
              
              (set! io-collected-texts null)))]
        
        [define wait-for-io-to-complete ; =Kernel=, =Handler=
          (lambda ()
            (unless (null? io-collected-thunks)
              (let ([semaphore (make-semaphore 0)])
                (mred:queue-callback
                 (lambda () ; =Kernel=, =Handler=
                   (run-io-collected-thunks)
                   (semaphore-post semaphore))
                 #f)
                (mred:yield semaphore))))]
        [define queue-output ; =User=
          (lambda (thunk)
            (protect
             (lambda (ut) ; =Protected-User=
               ; limiting-sema prevents queueing too much output from the user
               (semaphore-wait/enable-break limiting-sema)
               ; Queue the output:
               (let ([this-eventspace user-eventspace])
                 (semaphore-wait io-semaphore)
                 (if (eq? ut user-thread)
                     ; Queue output:
                     (set! io-collected-thunks
                           (cons thunk io-collected-thunks))
                     ; Release limit allocation, instead:
                     (semaphore-post limiting-sema))
                 (semaphore-post io-semaphore))
               ; If there's not one, queue an event that will flush the output queue
               (when (semaphore-try-wait? flushing-event-running)
                 ; Unlike most callbacks, this one has to run always, even if
                 ;   the user thread changes.
                 (queue-system-callback
                  ut
                  (lambda () ; =Kernel=, =Handler=
                    (semaphore-post flushing-event-running)
                    (run-io-collected-thunks))
                  #t)))))]
        
        [define generic-write ; =Kernel=, =Handler=
          (lambda (text s style-func)
            
            (let ([add-text
                   (lambda (text)
                     (unless (or (eq? this text)
                                 (member text io-collected-texts))
                       (set! io-collected-texts (cons text io-collected-texts))
                       (send text begin-edit-sequence)))])
              (add-text text))
            
            (when prompt-mode?
              (insert (string #\newline) (last-position) (last-position) #f)
              (set-prompt-mode #f))
            
            (let* ([start (send text last-position)]
                   [c-locked? (send text locked?)])
              (send text begin-edit-sequence)
              (send text lock #f)
              (send text insert
                    (if (is-a? s mred:snip%)
                        (send s copy)
                        s)
                    start
                    start
                    #t)
              (let ([end (send text last-position)])
                (style-func start end)
                (send text set-prompt-position end))
              (send text lock c-locked?)
              (send text end-edit-sequence)))]
        
        [define generic-close void]
        [define saved-newline? #f]
        [define this-result-write 
          (lambda (s) ; =User=
            (queue-output
             (lambda () ; =Kernel=, =Handler=
               (cleanup-transparent-io)
               (generic-write this
                              s
                              (lambda (start end)
                                (change-style result-delta
                                              start end))))))]
        [define this-out-write
          (lambda (s) ; = User=
            (queue-output
             (lambda () ; =Kernel=, =Handler=
               (let* ([text (init-transparent-io #f)]
                      [old-saved-newline? saved-newline?]
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
                          text
                          s
                          (lambda (start end)
                            (send text change-style output-delta start end))))])
                 (when old-saved-newline?
                   (gw (string #\newline)))
                 (gw s1)))))]
        
        [define this-err-write/exn ; =User=
          (let* ([raw-symbol-chars "a-z/!>:%\\+\\*\\?-"]
                 [symbol-chars (format "[~a]" raw-symbol-chars)]
                 [not-symbol-chars (format "[^~a]" raw-symbol-chars)]
                 [fallthru-regexp-str (format "^()(~a*): " symbol-chars)]
                 ;[_ (printf "fallthru-regex-str: ~s~n" fallthru-regexp-str)]
                 [fallthru-regexp (regexp fallthru-regexp-str)]
                 [class-regexp-str (format "^(.*~a)(~a+(<%>|%)).*$" not-symbol-chars symbol-chars)]
                 ;[_ (printf "class-regex-str: ~s~n" class-regexp-str)]
                 [class-regexp (regexp class-regexp-str)]
                 [ivar-regexp-str (format
                                   "^(ivar: instance variable not found: )(~a*)"
                                   symbol-chars)]
                 ;[_ (printf "ivar-regex-str: ~s~n" ivar-regexp-str)]
                 [ivar-regexp (regexp ivar-regexp-str)])
            (lambda (s exn) ; =User=
              (queue-output
               (lambda () ; =Kernel=, =Handler=
                 (cleanup-transparent-io)
                 (generic-write
                  this
                  s
                  (lambda (start end)
                    (change-style error-delta start end)
                    (cond
                      [(exn:variable? exn)
                       (let* ([var (symbol->string (exn:variable-id exn))]
                              [regexp (format "^(.*)(~a)" (quote-regexp-specials var))]
                              [match (regexp-match regexp s)])
                         (when match
                           (let* ([var-start (+ start (string-length (cadr match)))]
                                  [var-end (+ var-start (string-length (caddr match)))])
                             (change-style click-delta var-start var-end)
                             (set-clickback var-start var-end
                                            (lambda x
                                              (help:help-desk var))))))]
                      [else
                       (let ([bind-to-help
                              (lambda (regexp s)
                                (let ([match (regexp-match regexp s)])
                                  (if match
                                      (let* ([prefix (cadr match)]
                                             [var (caddr match)]
                                             [var-start (+ start (string-length prefix))]
                                             [var-end (+ var-start (string-length var))])
                                        (change-style click-delta var-start var-end)
                                        (set-clickback
                                         var-start var-end
                                         (lambda x
                                           (help:help-desk var)))
                                        prefix)
                                      #f)))])
                         (let loop ([s s])
                           (when s
                             (loop (bind-to-help class-regexp s))))
                         (bind-to-help ivar-regexp s)
                         (bind-to-help fallthru-regexp s))])))))))]
        [define this-err-write ; =User=
          (lambda (s)
            (this-err-write/exn s #f))]
        
        [define this-err (make-output-port this-err-write generic-close)]
        [define this-out (make-output-port this-out-write generic-close)]
        [define this-in (make-input-port this-in-read-char this-in-char-ready? generic-close
                                         this-in-peek-char)]
        [define this-result (make-output-port this-result-write generic-close)]
        [define set-display/write-handlers
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
                                                   (and (is-a? x mred:snip%) 1))]
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
             (list this-out-write this-err-write this-result-write)))]
        
        [define display-results ; =User=, =Handler=, =Breaks=
          (lambda (anss)
            (for-each 
             (lambda (v)
               (unless (void? v)
                 (let ([v (if (basis:r4rs-style-printing? (basis:current-setting))
                              v
                              (print-convert:print-convert v))])
                   (parameterize ([mzlib:pretty-print:pretty-print-size-hook
                                   (lambda (x _ port) (and (is-a? x mred:snip%) 1))]
                                  [mzlib:pretty-print:pretty-print-print-hook
                                   (lambda (x _ port) (this-result-write x))])
                     (mzlib:pretty-print:pretty-print v this-result)))))
             anss))]
        
        
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                            ;;;
      ;;;             Zodiac Interface               ;;;
      ;;;                                            ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        [define recent-error-text #f]
        [define error-range #f]
        
        [define report-located-error ; =Kernel=, =Handler=
          (lambda (message di exn)
            (if (and di
                     (zodiac:zodiac? di)
                     (basis:zodiac-vocabulary? user-setting))
                (let* ([start (zodiac:zodiac-start di)]
                       [finish (zodiac:zodiac-finish di)])
                  (report-error start finish 'dynamic message exn))
                (report-unlocated-error message exn)))]
        [define report-unlocated-error ; =Kernel=
          (lambda (message exn)
            (send context ensure-rep-shown)
            (let ([old-locked? locked?])
              (begin-edit-sequence)
              (lock #f)
              (this-err-write/exn (string-append message (string #\newline))
                                  exn)
              (lock old-locked?)
              (end-edit-sequence)))]
        
        [define get-error-range
          (lambda ()
            (if color?
                error-range
                (if recent-error-text
                    (cons (send recent-error-text get-start-position)
                          (send recent-error-text get-end-position)))))]
        
        [define reset-highlighting void]
        
        [define format-source-loc ;; =Kernel=, =Handler=
          (lambda (start end)
            (let ([translate-loc
                   (lambda (loc)
                     (let ([loc-name (zodiac:location-file loc)])
                       (zodiac:make-location (zodiac:location-line loc)
                                             (zodiac:location-column loc)
                                             (zodiac:location-offset loc)
                                             (if (is-a? loc-name mred:editor<%>)
                                                 (or (send loc-name get-filename)
                                                     loc-name)
                                                 loc-name))))])
              (basis:format-source-loc 
               (translate-loc start)
               (translate-loc end)
               (fw:preferences:get 'framework:line-offsets)
               (fw:preferences:get 'framework:display-line-numbers))))]
        
        [define highlight-error
          (lambda (file start finish)
            (when (is-a? file fw:text:basic%)
              (send file begin-edit-sequence)
              (set! recent-error-text file)
              (wait-for-io-to-complete)
              (reset-highlighting)
              (set! error-range (cons start finish))
              (if color?
                  (let ([reset (send file highlight-range start finish error-color #f #f 'high)])
                    (set! reset-highlighting
                          (lambda ()
                            (unless inserting-prompt
                              (set! error-range #f)
                              (reset)
                              (set! reset-highlighting void)))))
                  (send file set-position start finish))
              (send file scroll-to-position start #f finish)
              (send file end-edit-sequence)
              (send file set-caret-owner #f 'global)))]
        
        [define report-error ; =Kernel=, =Handler=
          (lambda (start-location end-location type input-string exn)
            (let* ([start (zodiac:location-offset start-location)]
                   [finish (add1 (zodiac:location-offset end-location))]
                   [file (zodiac:location-file start-location)]
                   [message
                    (if (is-a? file mred:text%)
                        input-string
                        (string-append (format-source-loc start-location end-location)
                                       input-string))])
              (report-unlocated-error message exn)
              (set! recent-error-text #f)
              (highlight-error file start finish)))]
        [define on-set-media void]
        
        [define on-insert
          (lambda (x y)
            (reset-highlighting)
            (super-on-insert x y))]
        [define on-delete
          (lambda (x y)
            (reset-highlighting)
            (super-on-delete x y))]
        
        [define process-text ; =User=, =Handler=, =No-Breaks=
          (lambda (text fn start end annotate? text-is-file?)
            (if (basis:zodiac-vocabulary? user-setting)
                (process-text/zodiac text fn start end annotate? text-is-file?)
                (process-text/no-zodiac text fn start end)))]
        [define process-file
          (lambda (filename fn annotate?)
            (if (basis:zodiac-vocabulary? user-setting)
                (basis:process-file/zodiac filename fn annotate?)
                (basis:process-file/no-zodiac filename fn)))]
        [define process-sexp
          (lambda (sexp z fn annotate?)
            (if (basis:zodiac-vocabulary? user-setting)
                (basis:process-sexp/zodiac sexp z fn annotate?)
                (basis:process-sexp/no-zodiac sexp fn)))]
        
        
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                            ;;;
      ;;;                Evaluation                  ;;;
      ;;;                                            ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        [define get-prompt (lambda () "> ")]
        [define eval-busy? (lambda () (not (and user-thread
                                                (thread-running? user-thread))))]
        
        [define user-setting (fw:preferences:get 'drscheme:settings)]
        [define user-custodian (make-custodian)]
        [define user-eventspace #f]
        [define user-namespace #f]
        [define user-thread #f]
        
        [define in-evaluation? #f] ; a heursitic for making the Break button send a break
        [define should-collect-garbage? #f]
        [define ask-about-kill? #f]
        
        [define insert-warning
          (lambda ()
            (begin-edit-sequence)
            (insert #\newline (last-position) (last-position))
            (let ([start (last-position)])
              (insert
               "WARNING: Interactions window is out of sync with the definitions window. Click Execute."
               start start)
              (let ([end (last-position)])
                (change-style warning-style-delta start end)))
            (end-edit-sequence))]
        
        (define already-warned? #f)

        [define do-eval
          (let ([count 0])
            (lambda (start end)
              (set! count (add1 count))
              (when (<= 5 count)
                (collect-garbage)
                (set! count 0))
              (let* ([needs-execution? (send context needs-execution?)])
                (when (if (fw:preferences:get 'drscheme:execute-warning-once)
                          (and (not already-warned?)
                               needs-execution?)
                          needs-execution?)
                  (set! already-warned? #t)
                  (insert-warning)))
              (do-many-text-evals this start end)))]
        
        [define cleanup
          (lambda ()
            (update-running #f)
            (unless (and user-thread (thread-running? user-thread))
              (lock #t)
              (unless shutting-down?
                (mred:message-box
                 "Warning"
                 (format "The evaluation thread is no longer running, ~
                 so no evaluation can take place until ~
                 the next execution.")))))]
        [define need-interaction-cleanup? #f]
        [define cleanup-interaction ; =Kernel=, =Handler=
          (lambda ()
            (set! need-interaction-cleanup? #f)
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
            (send context enable-evaluation))]

	[define do-many-text-evals
	  (lambda (text start end)
	    (do-many-evals
	     (lambda (single-loop-eval)
	       (process-text
                ; BUG: is it possible that a macro turns on breaking?
		text
		(lambda (expr recur) ; =User=, =Handler=, =No-Breaks=
		  (cond
		   [(basis:process-finish? expr)
		    (void)]
		   [else
		    (single-loop-eval
		     (lambda ()
		       (let ([answers
			      (call-with-values
			       (lambda ()
				 (if (basis:zodiac-vocabulary? (basis:current-setting))
				     (basis:syntax-checking-primitive-eval expr)
				     (basis:primitive-eval expr)))
			       list)])
			 (display-results answers))))
		    (recur)]))
                   start
                   end
                   #t
                   #t))))]
		    

	;; do-many-evals : (((-> void) -> void) -> void)
	[define do-many-evals ; =Kernel=, =Handler=

	  ;; run-loop has the loop. It expects one argument, a procedure that
	  ;; can be called with a thunk. The argument to run-loop maintains the right
	  ;; breaking state and calls the thunk it was called with.
          (lambda (run-loop) 
            (send context disable-evaluation)
            (cleanup-transparent-io)
            (reset-pretty-print-width)
            (ready-non-prompt)
            (mred:begin-busy-cursor)
            (when should-collect-garbage?
              (set! should-collect-garbage? #f)
              (collect-garbage))
            (set! need-interaction-cleanup? #t)
            
            (run-in-evaluation-thread
             (lambda () ; =User=, =Handler=, =No-Breaks=
               (reset-break-state)
               
               (protect-user-evaluation
                ; Evaluate the expression(s)
                (lambda () ; =User=, =Handler=, =No-Breaks=
                  ; This procedure must also ensure that breaks are off before
                  ;  returning or escaping.
                  (run-loop
		   (lambda (thunk)
                        ; Evaluate the user's expression. We're careful to turn on
                        ;   breaks as we go in and turn them off as we go out.
                        ;   (Actually, we adjust breaks however the user wanted it.)
                        ; A continuation hop might take us out of this instance of
                        ;   evaluation and into another one, which is fine.
                        (dynamic-wind
                         (lambda () 
                           (break-enabled user-break-enabled)
                           (set! user-break-enabled 'user))
                         (lambda ()
			   (thunk))
                         (lambda () 
                           (set! user-break-enabled (break-enabled))
                           (break-enabled #f))))))
                
                ; Cleanup after evaluation:
                (lambda () ; =User=, =Handler=, =No-Breaks=
                  (queue-system-callback/sync user-thread cleanup-interaction))))))]
        
        [define shutdown-user-custodian ; =Kernel=, =Handler=
          ; Use this procedure to shutdown when in the middle of other cleanup
          ;  operations, such as when the user clicks "Execute".
          ; Don't use it to kill a thread where other, external cleanup
          ;  actions must occur (e.g., the exit handler for the user's
          ;  thread). In that case, shut down user-custodian directly.
          (lambda ()
            (custodian-shutdown-all user-custodian)
            (set! user-thread #f)
            (semaphore-wait io-semaphore)
            (for-each (lambda (i) (semaphore-post limiting-sema)) io-collected-thunks)
            (set! io-collected-thunks null)
            (semaphore-post io-semaphore))]
        
        [define reset-break-state (lambda () (set! ask-about-kill? #f))]
        [define break
          (lambda () ; =Kernel=, =Handler=
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
                   (custodian-shutdown-all user-custodian))]
              [else
               (break-thread user-thread)
               (set! ask-about-kill? #t)]))]
        
        [define error-escape-k void]
        [define user-break-enabled #t]
        
        [define eval-thread-thunks null]
        [define eval-thread-state-sema 'not-yet-state-sema]
        [define eval-thread-queue-sema 'not-yet-thread-sema]
        
        [define cleanup-sucessful 'not-yet-cleanup-sucessful]
        [define cleanup-semaphore 'not-yet-cleanup-semaphore]
        [define thread-grace 'not-yet-thread-grace]
        [define thread-kill 'not-yet-thread-kill]
        
        [define thread-killed 'not-yet-thread-killed]
        [define initialize-killed-thread ; =Kernel=
          (lambda ()
            (when (thread? thread-killed)
              (kill-thread thread-killed))
            (set! thread-killed
                  (thread
                   (lambda () ; =Other=
                     (let ([ut user-thread])
                       (thread-wait ut)
                       (mred:queue-callback
                        (lambda ()
                          (when (eq? user-thread ut)
                            (if need-interaction-cleanup?
                                (cleanup-interaction)
                                (cleanup))))))))))]
        
        [define protect-user-evaluation ; =User=, =Handler=, =No-Breaks=
          (lambda (thunk cleanup)
            ;; We only run cleanup if thunk finishes normally or tries to
            ;; error-escape. Otherwise, it must be a continuation jump
            ;; into a different call to protect-user-evaluation.
            
            ;; `thunk' is responsible for ensureing that breaks are off when
            ;; it returns or jumps out.
            
            (update-running #t)
            
            (let/ec k
              (let ([saved-error-escape-k error-escape-k]
                    [cleanup? #f])
                (dynamic-wind
                 (lambda ()
                   (set! cleanup? #f)
                   (set! error-escape-k (lambda () 
                                          (set! cleanup? #t)
                                          (k (void)))))
                 (lambda () (thunk) 
                   ; Breaks must be off!
                   (set! cleanup? #t))
                 (lambda () 
                   (set! error-escape-k saved-error-escape-k)
                   (when cleanup?
                     (update-running #f)
                     (cleanup)))))))]
        
        [define run-in-evaluation-thread ; =Kernel=
          (lambda (thunk)
            (semaphore-wait eval-thread-state-sema)
            (set! eval-thread-thunks (append eval-thread-thunks (list thunk)))
            (semaphore-post eval-thread-state-sema)
            (semaphore-post eval-thread-queue-sema))]
        [define init-evaluation-thread ; =Kernel=
          (lambda ()
            (set! user-custodian (make-custodian))
            (set! user-eventspace (parameterize ([current-custodian user-custodian])
                                    (mred:make-eventspace)))
            (set! user-break-enabled #t)
            (set! eval-thread-thunks null)
            (set! eval-thread-state-sema (make-semaphore 1))
            (set! eval-thread-queue-sema (make-semaphore 0))
            
            (let ([init-thread-complete (make-semaphore 0)]
                  [goahead (make-semaphore)]
                  [o (current-output-port)])
              (parameterize ([mred:current-eventspace user-eventspace])
                (mred:queue-callback
                 (lambda () ; =User=, =No-Breaks=
                   ; No user code has been evaluated yet, so we're in the clear...
                   (break-enabled #f)
                   (set! user-thread (current-thread))
                   
                   (initialize-parameters)
                   
                   (set! user-namespace (current-namespace))
                   
                   (let ([drscheme-error-escape-handler
                          (lambda ()
                            (error-escape-k))])
                     (error-escape-handler drscheme-error-escape-handler)
                     (basis:bottom-escape-handler drscheme-error-escape-handler))
                   
                   (update-running #f)
                   
                   ;; let init-thread procedure return,
                   ;; now that parameters (and user-namespace) are set
                   (semaphore-post init-thread-complete)
                   
                   ; We're about to start running user code.
                   
                   ; Pause to let killed-thread get initialized
                   (semaphore-wait goahead)
                   
                   (let loop () ; =User=, =Handler=, =No-Breaks=
                     ; Wait for something to do
                     (unless (semaphore-try-wait? eval-thread-queue-sema)
                       ; User event callbacks run here; we turn on
                       ;  breaks in the dispatch handler.
                       (mred:yield eval-thread-queue-sema))
                     ; About to eval something
                     (semaphore-wait eval-thread-state-sema)
                     (let ([thunk (car eval-thread-thunks)])
                       (set! eval-thread-thunks (cdr eval-thread-thunks))
                       (semaphore-post eval-thread-state-sema)
                       ; This thunk evals the user's expressions with appropriate
                       ;   protections.
                       (thunk))
                     (loop)))))
              (semaphore-wait init-thread-complete)
              ; Start killed-thread
              (initialize-killed-thread)
              ; Let user expressions go...
              (semaphore-post goahead)))]
        
        [define shutting-down? #f]
        [define shutdown ; =Kernel=, =Handler=
          (lambda ()
            (set! shutting-down? #t)
            (shutdown-user-custodian))]
        
        [define update-running ; =User=, =Handler=, =No-Breaks=
          (lambda (on?)
            (set! in-evaluation? on?)
            (queue-system-callback
             user-thread
             (lambda ()
               (if in-evaluation?
                   (send context running)
                   (send context not-running)))))]
        
        
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;					     ;;;
	;;;                Execution                 ;;;
	;;;					     ;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        [define insert-delta
          (lambda (s delta)
            (let ([before (last-position)])
              (insert s before before #f)
              (let ([after (last-position)])
                (change-style delta before after)
                (values before after))))]
        
        [define repl-initially-active? #f]
        
        [define initialize-parameters ; =User=
          (lambda ()
            (let ([setting (fw:preferences:get 'drscheme:settings)])
              
              (basis:initialize-parameters
               user-custodian
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
                       (raise (raise-type-error
                               'drscheme-load-handler
                               "string"
                               filename)))
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
                                  (let ([text (make-object drscheme:text:text%)])
                                    (parameterize ([mred:current-eventspace
                                                    drscheme:init:system-eventspace]) ;; to get the right snipclasses
                                      (send text load-file filename))
                                    (begin0
                                      (process-text text process-sexps
                                                    0 
                                                    (send text last-position)
                                                    #t
                                                    #f)
                                      (send text on-close)))))
                         (userspace-load filename))))))
              
              (basis:error-display/debug-handler
               (lambda (msg marks exn)
                 (queue-system-callback/sync
                  user-thread
                  (lambda () 
                    (report-located-error msg marks exn)))))
              
              (error-display-handler
               (rec drscheme-error-display-handler
                 (lambda (msg)
                   (let ([rep (exception-reporting-rep)])
                     (if rep
                         (send rep report-unlocated-error msg #f)
                         (mred:message-box "Uncaught Error" msg))))))
              
              (current-directory (or (send context get-directory) 
                                     drscheme:init:first-dir))
              
              (exit-handler (lambda (arg) ; =User=
                              (custodian-shutdown-all user-custodian)))
              
              (invoke-teachpack)
              
              ;; set all parameters before constructing eventspace
              ;; so that the parameters are set in the eventspace's
              ;; parameterization
              (let* ([primitive-dispatch-handler (mred:event-dispatch-handler)])
                
                (mred:event-dispatch-handler
                 (rec drscheme-event-dispatch-handler ; <= a name for #<...> printout
                   (lambda (eventspace) ; =User=, =Handler=
                     ; Breaking is enabled if the user turned on breaks and
                     ;  is in a `yield'. If we get a break, that's ok, because
                     ;  the kernel never queues an event in the user's eventspace.
                     (cond
                       [(eq? eventspace user-eventspace)
                        ; =User=, =Handler=, =No-Breaks=
                        
                        (let* ([ub? (eq? user-break-enabled 'user)]
                               [break-ok? (if ub?
                                              (break-enabled)
                                              user-break-enabled)])
                          (break-enabled #f)
                          
                          ; We must distinguish between "top-level" events and
                          ;  those within `yield' in the user's program.
                          
                          (cond
                            [(not in-evaluation?)
                             
                             (reset-break-state) ; Is this a good idea?
                             
                             (protect-user-evaluation
                              ; Run the dispatch:
                              (lambda () ; =User=, =Handler=, =No-Breaks=
                                ; This procedure is responsible for adjusting breaks to
                                ;  match the user's expectations:
                                (dynamic-wind
                                 (lambda () 
                                   (break-enabled break-ok?)
                                   (unless ub?
                                     (set! user-break-enabled 'user)))
                                 (lambda ()
                                   (primitive-dispatch-handler eventspace))
                                 (lambda ()
                                   (unless ub?
                                     (set! user-break-enabled (break-enabled)))
                                   (break-enabled #f))))
                              ; Cleanup after dispatch
                              void)
                             
                             ; Restore break:
                             (when ub?
                               (break-enabled break-ok?))]
                            [else
                             ; Nested dispatch; don't adjust interface, and restore break:
                             (break-enabled break-ok?)
                             (primitive-dispatch-handler eventspace)]))]
                       [else 
                        ; =User=, =Non-Handler=, =No-Breaks=
                        (primitive-dispatch-handler eventspace)])))))))]
        
        [define  reset-console
          (lambda ()
            (when (thread? thread-killed)
              (kill-thread thread-killed))
            (shutdown-user-custodian)
            (cleanup-transparent-io)
            (clear-previous-expr-positions)
            (set! should-collect-garbage? #t)
            
            ;; in case the last evaluation thread was killed, clean up some state.
            (lock #f)
            (update-running #f)
            
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

        [define initialize-console
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
            
            (reset-console))]
        
        (set-display/write-handlers)
        (super-init)
        (set-styles-sticky #f))))

  (define make-console-text%
    (lambda (super%)
      (rec console-text%
      (class/d super% args
	((inherit position-line position-location
		  line-location get-admin
		  set-position set-caret-owner
		  clear-undos insert delete
		  begin-edit-sequence
		  end-edit-sequence
		  run-after-edit-sequence
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
		 [super-after-set-size-constraint after-set-size-constraint]
		 [super-get-keymaps get-keymaps])
	 (rename [super-can-insert? can-insert?]
		 [super-after-insert after-insert]
		 
		 [super-can-delete? can-delete?]
		 [super-after-delete after-delete]
		 
		 [super-can-change-style? can-change-style?]
		 [super-after-change-style after-change-style]
		 
		 [super-on-edit-sequence on-edit-sequence]
		 [super-after-edit-sequence after-edit-sequence]
		 
		 [super-after-set-position after-set-position])
	 (override autosave?
		   get-keymaps
		   can-insert?
		   can-delete?
		   can-change-style?
		   after-insert
		   after-delete
		   after-change-style
		   on-edit-sequence
		   after-edit-sequence
		   after-set-position
		   on-local-char)

	 (public set-resetting
		 resetting?

		 copy-prev-previous-expr
		 copy-next-previous-expr
		 copy-previous-expr
		 clear-previous-expr-positions

		 balance-required

		 initialize-console

		 clear-previous-expr-positions
		 copy-previous-expr
		 previous-expr-pos
		 previous-expr-positions
		 prompt-mode?
		 set-prompt-mode
		 ready-non-prompt
		 inserting-prompt

		 reset-pretty-print-width

		 get-prompt
		 insert-prompt
		 set-prompt-position
		 prompt-position
		 reset-console

		 do-pre-eval
		 do-eval
		 do-post-eval
		 eval-busy?)

	 )

	[define autosave? (lambda () #f)]

	[define edit-sequence-count 0]

	[define orig-stdout (current-output-port)]
	[define orig-stderr (current-error-port)]
	[define normal-delta #f]
	
	[define get-keymaps
	  (lambda ()
	    (cons scheme-interaction-mode-keymap (super-get-keymaps)))]

	;; used to highlight the prompt that the caret is "in the range of".
	;; not currently used at all.
	[define find-which-previous-sexp
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
	[define can-something
	  (opt-lambda (super start len)
	    (cond
	     [(or resetting?
		  (not (number? prompt-position))
		  (>= start prompt-position))
	      (super start len)]
	     [else #f]))]
	[define after-something
	  (lambda (combine start len)
	    (when (or resetting?
		      (and prompt-mode? (< start prompt-position)))
	      (set! prompt-position (combine prompt-position len))))]

	[define resetting? #f]
	[define set-resetting (lambda (v) (set! resetting? v))]

	[define can-insert?
	  (lambda (start len)
	    (can-something super-can-insert? start len))]
	[define can-delete?
	  (lambda (start len)
	    (can-something super-can-delete? start len))]
	[define can-change-style?
	  (lambda (start len)
	    (can-something super-can-change-style? start len))]
	[define after-insert
	  (lambda (start len)
	    (after-something + start len)
	    (super-after-insert start len))]
	[define after-delete
	  (lambda (start len)
	    (after-something - start len)
	    (super-after-delete start len))]
	[define after-change-style
	  (lambda (start len)
	    (after-something (lambda (start len) start) start len)
	    (super-after-change-style start len))]
	[define on-edit-sequence
	  (lambda ()
	    (super-on-edit-sequence))]
	[define after-edit-sequence
	  (lambda ()
	    (super-after-edit-sequence))]
	[define after-set-position
	  (lambda ()
	    (super-after-set-position))]
	
	[define last-str (lambda (l)
			   (if (null? (cdr l))
			       (car l)
			       (last-str (cdr l))))]

	[define prompt-mode? #f]
	[define set-prompt-mode (lambda (x) (set! prompt-mode? x))]
	[define get-prompt (lambda () "> ")]
	[define prompt-position 0]
	[define set-prompt-position (lambda (v) (set! prompt-position v))]
	[define find-prompt 
	  (lambda (pos) 
	    (if (> pos prompt-position)
		prompt-position
		0))]
	[define auto-save? #f]
	[define balance-required
	  (let ([v #t])
	    (case-lambda
	     [() v]
	     [(x) (set! v x)]))]

	[define previous-expr-pos -1]
	[define previous-expr-positions null]
	[define clear-previous-expr-positions
	  (lambda ()
	    (set! previous-expr-positions null))]
	[define copy-previous-expr
	  (lambda ()
	    (let ([snip/strings (list-ref (fw:preferences:get
					   'mred:console-previous-exprs) 
					  previous-expr-pos)])
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
	[define copy-next-previous-expr
	  (lambda ()
	    (let ([previous-exprs (fw:preferences:get 'mred:console-previous-exprs)])
	      (unless (null? previous-exprs)
		(set! previous-expr-pos
		      (if (< (add1 previous-expr-pos) (length previous-exprs))
			  (add1 previous-expr-pos)
			  0))
		(copy-previous-expr))))]
	[define copy-prev-previous-expr
	  (lambda ()
	    (let ([previous-exprs (fw:preferences:get 'mred:console-previous-exprs)])
	      (unless (null? previous-exprs)
		(set! previous-expr-pos
		      (if (<= previous-expr-pos 0)
			  (sub1 (length previous-exprs))
			  (sub1 previous-expr-pos)))
		(copy-previous-expr))))]
	
	[define do-save-and-eval
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
	
	[define reset-pretty-print-width
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
	[define do-eval
	  (lambda (start end)
	    (error 'do-eval "abstract method"))]
	[define do-pre-eval
	  (lambda ()
	    (ready-non-prompt))]
	[define do-post-eval
	  (lambda ()
	    (insert-prompt))]
	
	[define only-spaces-after
	  (lambda (pos)
	    (let ([last (last-position)])
	      (let loop ([pos pos])
		(if (= pos last)
		    #t
		    (let ([c (get-character pos)])
		      (if (char-whitespace? c)
			  (loop (add1 pos))
			  #f))))))]
	
	[define eval-busy? (lambda () #f)]

	[define on-local-char
	  (lambda (key)
	    (let ([start (get-start-position)]
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
	       [(not (and (char? code) 
			  (or (char=? code #\return) (char=? code #\newline))))
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
		(if (balance-required)
		    (let ([balanced? (fw:scheme-paren:balanced?
				      this
				      prompt-position
				      last)])
		      (if balanced?
			  (begin
			    (delete start last)
			    (do-save-and-eval prompt-position start))
			  (super-on-local-char key)))
		    (begin
		      (delete start last)
		      (do-save-and-eval prompt-position start)))]
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
	       [else (super-on-local-char key)])))]

	[define inserting-prompt #f]
	[define insert-prompt
	  (lambda ()
	    (set! prompt-mode? #t)
	    (fluid-let ([inserting-prompt #t])
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
		(scroll-to-position start-selection #f (last-position) 'start))))]
	[define reset-console
	  (lambda ()
	    (void))]
	[define ready-non-prompt
	  (lambda ()
	    (when prompt-mode?
	      (set! prompt-mode? #f)
	      (let ([c-locked (locked?)])
		(begin-edit-sequence)
		(lock #f)
		(insert #\newline (last-position))
		(lock c-locked)	   
		(end-edit-sequence))))]	  
	[define initialize-console
	  (lambda ()
	    #t)]
	(apply super-init args)))))
  
  (define make-transparent-io-text%
    (lambda (super%)
      (rec transparent-io-text%
      (class super% args
	(inherit change-style prompt-position set-prompt-position
		 resetting? set-resetting lock get-text
		 set-position last-position get-character
		 clear-undos set-cursor
		 do-pre-eval do-post-eval balance-required)
	(rename [super-after-insert after-insert]
		[super-on-local-char on-local-char])
	(private
	  [input-delta (make-object mred:style-delta%)]
	  [data null]
	  [stream-start 0]
	  [stream-end 0])
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
	     (semaphore-post wait-for-sexp)
	     (lock #t))]
	  [consumed-delta 
	   (make-object mred:style-delta% 'change-bold)]
	  [mark-consumed
	   (lambda (start end)
	     (let ([old-resetting resetting?])
	       (set-resetting #t)
	       (change-style consumed-delta start end)
	       (set-resetting old-resetting)))]
	  [ibeam-cursor (make-object mred:cursor% 'ibeam)]
	  [check-char-ready? ; =Reentrant=
	   (lambda ()
	     (semaphore-wait potential-sexps-protect)
	     (begin0
	      (cond
	       [(not (null? potential-sexps)) #t]
	       [(< stream-start stream-end) #t]
	       [else #f])
	      (semaphore-post potential-sexps-protect)))]
	  [fetch-char ; =Kernel=, =Handler=, =Non-Reentrant= (queue requests externally)
	   (lambda ()
	     (let ([found-char
		    (lambda ()
		      (let ([s stream-start])
			(mark-consumed s (add1 s))
			(set! stream-start (add1 s))
			(get-character s)))])
	       (let loop ()
		 (let ([ready-char #f])
		   (dynamic-wind
		    (lambda ()
		      (set! ready-char #f)
		      (semaphore-wait potential-sexps-protect))
		    (lambda ()
		      (cond
			[(not (null? potential-sexps))
			 (let ([first-sexp (car potential-sexps)])
			   (set! potential-sexps null)
			   (set! stream-start (car first-sexp))
			   (set! stream-end (cdr first-sexp))
			   (set-prompt-position (max prompt-position stream-end))
			   (set! ready-char (found-char)))]
			[(< stream-start stream-end)
			 (set! ready-char (found-char))]
			[else (void)]))
		    (lambda ()
		      (semaphore-post potential-sexps-protect)))
		   (or ready-char
		       (begin
			 (mred:yield wait-for-sexp)
			 (if shutdown? 
			     eof
			     (loop))))))))])

	(override
	  [get-prompt (lambda () "")])
	(override
	  [after-insert
	   (lambda (start len)
	     ;; We assume that the insert comes from the user. If it's printer output,
	     ;;  the style will be changed again (so we wasted effort).
	     ;; Since output is more common than input, there's certainly room for
	     ;;  a significant optimization here.
	     (super-after-insert start len)
	     (let ([old-r resetting?])
	       (set-resetting #t)
	       (change-style input-delta start (+ start len))
	       (set-resetting old-r)))])
	(override
	  [do-eval
	   (lambda (start end)
	     (do-pre-eval)
	     (unless (balance-required)
	       (set! end (add1 end)))
	     (let ([new-sexps
		    (let loop ([pos start])
		      (cond
			[(< pos end) 
			 (let ([next-sexp
				(if (balance-required)
				    (fw:scheme-paren:forward-match this pos end)
				    end)])
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
	  (insert-prompt))))))

  (define transparent-io-text% 
    (make-transparent-io-text%
     (make-console-text%
      (fw:scheme:text-mixin
       fw:text:searching%))))
  
  (define text% (make-text% (make-console-text% fw:scheme:text%))))
