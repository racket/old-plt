(unit/sig mred:console^
  (import [wx : wx^]
	  [mred:constants : mred:constants^]
	  [mred:container : mred:container^]
	  [mred:preferences : mred:preferences^]
	  [mred:edit : mred:edit^]
	  [mred:frame : mred:frame^]
	  [mred:group : mred:group^]
	  [mred:canvas : mred:canvas^]
	  [mred:find-string : mred:find-string^]
	  [mred:exit : mred:exit^]
	  [mred:finder : mred:finder^]
	  [mred:handler : mred:handler^]
	  [mred:gui-utils : mred:gui-utils^]
	  [mred:scheme-mode : mred:scheme-mode^]
	  [mred:scheme-paren : mred:scheme-paren^]
	  [mred:icon : mred:icon^]
	  [mred:hyper-frame : mred:hyper-frame^]
	  [mred:version : mred:version^]
	  [mred:application : mred:application^]
	  [mzlib:function : mzlib:function^]
	  [mzlib:string : mzlib:string^]
	  [mzlib:pretty-print : mzlib:pretty-print^])
  
  (define (printf string . args)
    (apply fprintf mred:constants:original-output-port string args))
  
  (mred:debug:printf 'invoke "mred:console@")

  (define-struct sexp (left right prompt))
  
  (define newline-string (string #\newline))
  
  (define (copyright-string)
    (string-append
     "version "
     (mred:version:version)
     "."
     newline-string
     "Copyright (c) 1995-1998 PLT, Rice University (Matthew Flatt and Robert Bruce Findler)"))
  
  (define credits-proc
    (lambda (indent-string)
      (let ([newline-indent-string (string-append (string #\newline)
						  indent-string)])
	(string-append
	 indent-string
	 "wxWindows (c) 1994 Artificial Intelligence"
	 " Applications Institute, The University of Edinburgh."
	 (if (eq? wx:window-system 'xt)
	     (string-append
	      newline-indent-string
	      "wxWindows/Xt (c) 1995, GNU (Markus Holzem).")
	     "")
	 newline-indent-string
	 "MzScheme (c) 1995-97 Matthew Flatt."
	 newline-indent-string
	 "libscheme (c) 1994 Brent Benson."
	 newline-indent-string
	 "conservative GC (c) 1988-89 Hans-J. Boehm, Alan J. Demers,"
	 " (c) 1991-96 Xerox Corp,"
	 " (c) 1996 Silicon Graphics."
	 newline-indent-string
	 "C++ GC extension by Jesse Hull and John Ellis"
	 " (c) 1994 Xerox Corp."
	 newline-string))))
  
  (define (welcome-message)
    (string-append
     "Welcome to " (copyright-string)
     newline-string
     "Based on the following: "
     newline-string
     (credits-proc "  ")))
  
  (define message-delta 
    (make-object wx:style-delta%
		 wx:const-change-family
		 wx:const-decorative))
  (define url-delta
    (make-object wx:style-delta%
		 wx:const-change-family
		 wx:const-decorative))
  (send* url-delta
    (set-delta-foreground "BLUE")
    (set-underlined-on #t))
  
  (define url "http://www.cs.rice.edu/CS/PLT/packages/mred/")
  
  (define credits
    (lambda ()
      (let* ([f (make-object mred:container:frame% null "MrEd Credits"
			     -1 -1 600 350)]
	     [p (make-object mred:container:horizontal-panel% f)]
	     [c (make-object mred:canvas:wide-snip-canvas% p)]
	     [is (make-object wx:image-snip% 
			      (build-path (collection-path "icons") "mred.gif")
			      wx:const-bitmap-type-gif)]
	     [oe (make-object mred:edit:media-edit%)]
	     [ie (make-object mred:edit:media-edit%)]
	     [es (make-object mred:edit:media-snip% ie #f)]
	     [top (make-object wx:style-delta% wx:const-change-alignment wx:const-align-top)])
	(send* c 
	  (set-media oe)
	  (widen-snips #t)
	  (add-wide-snip es))
	(send* oe 
	  (set-auto-set-wrap #f)
	  (insert is)
	  (insert es))
	(send* ie 
	  (insert (welcome-message))
	  (insert #\newline)
	  (insert "See the license agreement or "))
	(let ([start (send ie get-start-position)])
	  (send ie insert url)
	  (let ([end (send ie get-start-position)])
	    (send ie insert " for more info.")
	    (send* ie 
	      (change-style message-delta 0 (send ie last-position))
	      (change-style url-delta start end)
	      (set-clickback start end
			     (lambda (edit start end)
			       (make-object
				mred:hyper-frame:hyper-view-frame%
				url))))))
	(send* ie 
	  (set-autowrap-bitmap null)
	  (set-position 1))
	(send oe change-style top 0 2)
	(send f show #t))))
  
  (define separator-snipclass
    (make-object
     (class-asi wx:snip-class%
       (public
	 [read (lambda (s) 
		 (let ([size-box (box 0)])
		   (send s get size-box)
		   (make-object separator-snip%)))]))))
  (send* separator-snipclass
    (set-version 1)
    (set-classname "mred:sepatator-snip%"))
  (send (wx:get-the-snip-class-list) add separator-snipclass)
  
  
  ;; the two numbers 1 and 2 which appear here are to line up this snip
  ;; with the embedded snips around it in the drscheme rep.
  ;; I have no idea where the extra pixels are going.
  (define separator-snip%
    (class wx:snip% ()
      (inherit get-style set-snipclass set-flags get-flags get-admin)
      (private [width 500]
	       [height 1]
	       [white-around 2])
      (public
	[write (lambda (s) 
		 (send s put (char->integer #\r)))]
	[copy (lambda () 
		(let ([s (make-object (object-class this))])
		  (send s set-style (get-style))
		  s))]
	[get-extent
	 (lambda (dc x y w-box h-box descent-box space-box lspace-box rspace-box)
	   (for-each (lambda (box) (unless (null? box) (set-box! box 0)))
		     (list descent-box space-box lspace-box rspace-box))
	   (let* ([admin (get-admin)]
		  [reporting-media (send admin get-media)]
		  [reporting-admin (send reporting-media get-admin)]
		  [widthb (box 0)]
		  [space 2])
	     (send reporting-admin get-view null null widthb null)
	     (set! width (- (unbox widthb) 
			    space
			    2)))
	   (set! height 1)
	   (unless (null? w-box)
	     (set-box! w-box width))
	   (unless (null? h-box)
	     (set-box! h-box (+ (* 2 white-around) height))))]
	[draw
	 (let* ([body-pen (send wx:the-pen-list find-or-create-pen
				"BLUE" 0 wx:const-solid)]
		[body-brush (send wx:the-brush-list find-or-create-brush
				  "BLUE" wx:const-solid)])
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
	   "1")])
      (sequence
	(super-init)
	(set-flags (bitwise-ior (get-flags) wx:const-snip-hard-newline))
	(set-snipclass separator-snipclass))))
  
  (define show-interactions-history
    (let* ([cached-frame #f]
	   [% (class mred:frame:simple-menu-frame% args
		(rename [super-do-close do-close])
		(public
		  [do-close
		   (lambda ()
		     (set! cached-frame #f)
		     (super-do-close))])
		(sequence
		  (apply super-init args)
		  (set! cached-frame this)))]
	   [fixed-style (make-object wx:style-delta% wx:const-change-family wx:const-modern)]
	   [update-edit
	    (lambda (edit exprs)
	      (send* edit
		(begin-edit-sequence)
		(lock #f)
		(erase))
	      (for-each
	       (lambda (lines)
		 (let ([pos (send edit get-start-position)])
		   (for-each
		    (lambda (line/snip)
		      (send edit insert
			    (if (string? line/snip)
				line/snip
				(send line/snip copy))
			    pos
			    pos))
		    lines))
		 (send edit insert #\newline)
		 (send edit insert (make-object separator-snip%)))
	       exprs)
	      (send* edit
		(change-style fixed-style 0 (send edit last-position))
		(lock #t)
		(end-edit-sequence)))]
	   [callback
	    (lambda (p v)
	      (when cached-frame
		(update-edit (send cached-frame get-edit) v)))])
      (mred:preferences:add-preference-callback 'mred:console-previous-exprs
						callback)
      (lambda ()
	(if cached-frame
	    (send cached-frame show #t)
	    (let* ([f (make-object % "Interactions History")]
		   [e (send f get-edit)])
	      (update-edit e (mred:preferences:get-preference 'mred:console-previous-exprs))
	      (send f show #t))))))
  
  (define make-scheme-mode-edit%
    (lambda (super%)
      (class super% args
	(inherit set-mode)
	(public
	  [get-insert-edit
	   (lambda () (make-object wx:media-snip%
				   (make-object scheme-mode-edit%)))])
	(sequence
	  (mred:debug:printf 'super-init "before scheme-mode-edit%")
	  (apply super-init args)
	  (mred:debug:printf 'super-init "before scheme-mode-edit%")
	  (set-mode (make-object mred:scheme-mode:scheme-mode%))))))
  
  (define scheme-mode-edit% (make-scheme-mode-edit% mred:edit:backup-autosave-edit%))
  
  (define make-single-threader
    (lambda ()
      (let ([sema (make-semaphore 1)])
	(lambda (thunk)
	  (dynamic-wind
	   (lambda () (semaphore-wait sema))
	   thunk
	   (lambda () (semaphore-post sema)))))))
  
  (define console-max-save-previous-exprs 30)
  (let* ([list-of? (lambda (p?)
		     (lambda (l)
		       (and (list? l)
			    (andmap p? l))))]
	 [snip/string? (lambda (s) (or (is-a? s wx:snip%) (string? s)))]
	 [list-of-snip/strings? (list-of? snip/string?)]
	 [list-of-lists-of-snip/strings? (list-of? list-of-snip/strings?)])
    (mred:preferences:set-preference-default
     'mred:console-previous-exprs
     null
     list-of-lists-of-snip/strings?))
  (let ([only-text-snips?
	 (lambda (ls)
	   (and (list? ls)
		(andmap (lambda (s)
			  (is-a? s wx:text-snip%))
			ls)))]
	[marshall 
	 (lambda (lls)
	   (map (lambda (ls)
		  (map (lambda (s)
			 (cond
			   [(is-a? s wx:text-snip%)
			    (send s get-text 0 (send s get-count))]
			   [(string? s) s]
			   [else "'non-text-snip"]))
		       ls))
		lls))]
	[unmarshall (lambda (x) x)])
    (mred:preferences:set-preference-un/marshall
     'mred:console-previous-exprs
     marshall unmarshall))
  
  (define share-except 
    (lambda (cp l)
      (lambda ()
	(make-parameterization-with-sharing
	 cp cp
	 l
	 (lambda (pm) #f)))))

  (define (dynamic-disable-break f)
    (with-parameterization ((share-except
			     (current-parameterization)
			     (list break-enabled
				   current-custodian 
				   current-exception-handler
				   parameterization-branch-handler)))
      (lambda ()
	(mzlib:function:dynamic-disable-break f))))
  
  (define (dynamic-not-killable c needs-break? f)
    ; Assume breaking is currently disabled
    (let* ([cp (current-parameterization)]
	   [result #f]
	   [t-done (make-semaphore)]
	   [t (parameterize ([current-custodian c]
			     [parameterization-branch-handler
			      (share-except
			       cp
			       ; Share break-enabled!
			       (list current-exception-handler))])
		   (thread
		    (lambda ()
		      (if needs-break?
			  (with-handlers ([exn:misc:user-break?
					   void])
			       (set! result (f)))
			  (set! result (f)))
		      (semaphore-post t-done))))]
	   [orig-t (current-thread)]
	   [t2 (if needs-break?
		   (parameterize ([current-custodian c]
				  [parameterization-branch-handler
				   (share-except
				    cp
				    (list break-enabled current-exception-handler))])
		       (thread
			(lambda ()
			  ; If the original thread is killed, break the
			  ;  work thread.
			  (thread-wait orig-t)
			  (break-thread t))))
		   #f)]
	   [kill-t2 (lambda ()
		      (when t2
			(parameterize ([current-custodian c])
			   (kill-thread t2))))])
      (if needs-break?
	  (with-handlers ([exn:misc:user-break?
			   (lambda (x)
			     (break-thread t)
			     (kill-t2)
			     (raise x))])
	     (wx:yield t-done))
	  (wx:yield t-done))
      (kill-t2)
      result))

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
		 erase set-mode get-canvas
		 invalidate-bitmap-cache
		 get-extent get-style-list canvases)
	(rename [super-set-auto-set-wrap set-auto-set-wrap]
		[super-on-local-char on-local-char]
		[super-on-paint on-paint]
		[super-after-set-size-constraint after-set-size-constraint])
	(private
	  [edit-sequence-count 0])
	(public
	  [styles-fixed? #f]
	  [orig-stdout (current-output-port)]
	  [orig-stderr (current-error-port)])
	(public
	  
	  [normal-font wx:const-modern]
	  [normal-delta null]
	  [output-delta (make-object wx:style-delta%
				     wx:const-change-weight
				     wx:const-bold)]
	  [result-delta (make-object wx:style-delta%
				     wx:const-change-weight
				     wx:const-bold)]
	  [error-delta (make-object wx:style-delta%
				    wx:const-change-style
				    wx:const-slant)])
	
	(sequence
	  (let ([mult (send error-delta get-foreground-mult)]
		[add (send error-delta get-foreground-add)])
	    (send mult set 0 0 0)
	    (send add set 255 0 0))
	  (let ([mult (send result-delta get-foreground-mult)]
		[add (send result-delta get-foreground-add)])
	    (send mult set 0 0 0)
	    (send add set 0 0 175))
	  (let ([mult (send output-delta get-foreground-mult)]
		[add (send output-delta get-foreground-add)])
	    (send mult set 0 0 0)
	    (send add set 150 0 150)))
	
	(rename [super-on-insert on-insert]
		[super-after-insert after-insert]
		
		[super-on-delete on-delete]
		[super-after-delete after-delete]
		
		[super-on-change-style on-change-style]
		[super-after-change-style after-change-style]
		
		[super-on-edit-sequence on-edit-sequence]
		[super-after-edit-sequence after-edit-sequence]
		
		[super-on-focus on-focus]
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
			    (wx:message-box "needs to move already #t!!" "Internal Error"))
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
				(let loop ([snip (find-snip left wx:const-snip-after)])
				  (cond
				    [(null? snip) null]
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
	       '(wx:message-box (format "1 prompt-position ~a" prompt-position))
	       (set! needs-to-move #f)
	       (set! needs-to-move-right (combine needs-to-move-right len))
	       (split-snip needs-to-move-left)
	       (split-snip needs-to-move-right)
	       (let ([start-selection (get-start-position)]
		     [end-selection (get-end-position)]
		     [delta (- prompt-position needs-to-move-left)])
		 (set! moving-down? #t)
		 (let loop ([snip (find-snip needs-to-move-left wx:const-snip-after)])
		   '(wx:message-box (format "2 prompt-position ~a" prompt-position))
		   (cond
		     [(null? snip) (void)]
		     [(< (get-snip-position snip) needs-to-move-right)
		      (insert (send snip copy) (last-position) (last-position) #t)
		      (loop (send snip next))]
		     [else (void)]))
		 '(wx:message-box (format "3 prompt-position ~a" prompt-position))
		 (delete needs-to-move-left needs-to-move-right #f)
		 (wx:message-box (format "4 prompt-position ~a" prompt-position))
		 (for-each (lambda (s) 
			     '(fprintf mred:constants:original-output-port 
				       "copy: ~s~n" (send s get-text 0 10000))
			     (insert (send s copy) needs-to-move-left needs-to-move-left #f)
			     '(wx:message-box "inserted one"))
			   needs-to-move-original)
		 (wx:message-box (format "5 prompt-position ~a" prompt-position))
		 (set-position (+ start-selection delta)
			       (+ end-selection delta)))
	       (set! moving-down? #f)
	       (end-edit-sequence)))])
	(public
	  [resetting? #f]
	  [set-resetting (lambda (v) (set! resetting? v))])
	(public
	  [on-insert
	   (lambda (start len)
	     (on-something super-on-insert start len))]
	  [on-delete
	   (lambda (start len)
	     (on-something super-on-delete start len))]
	  [on-change-style
	   (lambda (start len)
	     (on-something super-on-change-style start len #t))]
	  [on-focus
	   (lambda (on?)
	     (super-on-focus on?))]
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

	  
	;; These two instance variables are used to control the delaying of
	;; console io with edit-sequences.
	;; The box `timer-on' is either #f, meaning no delaying is happening, or
	;; a box, containing a list of embedded edits which are in edit-sequences
	;; The generic-write procedure initializes the begin-edit-sequence
	;; and creates a thread to end the edit-sequence. 
	;; It updates the timer-on instance variable with a box containing a list
	;; of the active transparent edits, and that box is captured in the closure 
	;; of the thread's procedure. When the thread awakens it checks the box.
	;; If the box contains #f, the computation has finished and the io has been flushed.
	;; If the box contains a list, the computation has not yet completed 
	;; but the io should be flushed.
	;; Also note that the generic-write procedure does not create more than one thread
	;; to end the edit-sequence at one time.
	(private
	  [timer-on #f]
	  [timer-sema (make-semaphore 1)]

	  [my-custodian (current-custodian)]
	  [kill-lock (make-semaphore 1)]
	  [want-kill? #f])

	(public
	  [MAX-CACHE-TIME 4000]
	  [MIN-CACHE-TIME 100]
	  [CACHE-TIME MIN-CACHE-TIME]
	  [TIME-FACTOR 10]
	  
	  [kill-protect
	   (lambda (who can-break? f)
	     (dynamic-disable-break
	      (lambda ()
		(if (and (not want-kill?)
			 (not can-break?)
			 (semaphore-try-wait? kill-lock))
		    (begin
		      (begin0
		       (f)
		       (semaphore-post kill-lock)))
		    (begin
		      (dynamic-not-killable
		       my-custodian
		       can-break?
		       f))))))]
	  [kill-allow-protected
	   (lambda (f)
	     (set! want-kill? #t)
	     (semaphore-wait kill-lock)
	     (begin0
	      (f)
	      (set! want-kill? #f)
	      (semaphore-post kill-lock)))]

	  [generic-write
	   ; This must be called within a procedure wrapped by
	   ; kill-protect
	   (let ([first-time? #t])
	     (lambda (edit s style-func)
		  (let ([handle-insertion
			 (lambda ()
			   (let ([start (send edit last-position)]
				 [c-locked? (ivar edit locked?)])
			     (send edit lock #f)
			     (send edit insert
				   (if (is-a? s wx:snip%)
					 (send s copy)
					 s))
			     (let ([end (send edit last-position)])
			       (send edit change-style null start end)
			       (send edit set-prompt-position end)
			       (style-func start end))
			     (send edit lock c-locked?)))])
		    (if first-time?
			(begin
			  (set! first-time? #f)
			  (semaphore-wait timer-sema)
			  (begin-edit-sequence #f)
			  (handle-insertion)
			  (end-edit-sequence)
			  (semaphore-post timer-sema))
			(begin
			  (semaphore-wait timer-sema)
			  (unless timer-on
			    (let ([on-box (box (if transparent-edit
						   (list transparent-edit)
						   null))])
			      (begin-edit-sequence #f)
			      (when transparent-edit
				(send transparent-edit begin-edit-sequence))
			      (set! timer-on on-box)
			      (parameterize ([current-custodian my-custodian]
					     [parameterization-branch-handler
					      (share-except
					       (current-parameterization)
					       (list current-exception-handler))])
			        (thread
				 (lambda ()
				   (dynamic-wind
				    void
				    (lambda ()
				      (sleep (/ CACHE-TIME 1000.)))
				    (lambda ()
				      (semaphore-wait timer-sema)
				      (when (unbox on-box)
					(let* ([start (current-milliseconds)]
					       [_ (begin (for-each (lambda (e) 
								     (send e end-edit-sequence))
								   (unbox on-box))
							 (end-edit-sequence))]
					       [end (current-milliseconds)]
					       [new-cache-time (* TIME-FACTOR (- end start))]
					       [between
						(min (max MIN-CACHE-TIME
							  new-cache-time)
						     MAX-CACHE-TIME)])
					  (set! CACHE-TIME between)
					  (set! timer-on #f)))
				      (semaphore-post timer-sema))))))))
			  (begin-edit-sequence #f)
			  (set-position (last-position))
			  (when (and prompt-mode? autoprompting?)
			    (insert #\newline))
			  (handle-insertion)
			  (end-edit-sequence)
			  (semaphore-post timer-sema)
			  (set-prompt-mode #f))))))]
	  [generic-close (lambda () '())]
	  [flush-console-output
	   (lambda ()
	     (semaphore-wait timer-sema)
	     (when timer-on
	       (for-each (lambda (e) (send e end-edit-sequence)) (unbox timer-on))
	       (end-edit-sequence)
	       (set-box! timer-on #f)
	       (set! timer-on #f))
	     (set! CACHE-TIME MIN-CACHE-TIME)
	     (semaphore-post timer-sema))])
	
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
	  [auto-save? #f]
	  [autoprompting? #f]
	  [enable-autoprompt
	   (opt-lambda ([v #t])
	     (set! autoprompting? v))]
	  [saved-newline? #f]
	  [this-out-write
	   (lambda (s)
	     (kill-protect
	      'curout
	      #f
	      (lambda ()
		(parameterize ([current-output-port orig-stdout]
			       [current-error-port orig-stderr])
		  (init-transparent-io #f)
		  (let* ([old-saved-newline? saved-newline?]
			 [len (string-length s)]
			 [s1 (if (and (> len 0)
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
			       (send transparent-edit
				     change-style output-delta start end))))])
		    (when old-saved-newline?
		      (gw (string #\newline)))
		    (gw s1))))))]
	  [this-err-write
	   (lambda (s)
	     (kill-protect
	      'curerr
	      #f
	      (lambda ()
		(parameterize ([current-output-port orig-stdout]
			       [current-error-port orig-stderr])
		  (cleanup-transparent-io)
		  (generic-write this
				 s
				 (lambda (start end)
				   (change-style error-delta 
						 start end)))))))])
	
	(public
	  [io-edit% transparent-io-edit%]
	  [transparent-edit #f]
	  [transparent-snip #f]
	  [this-in-char-ready? (lambda () #t)]
	  [cleanup-transparent-io
	   (lambda ()
	     (when transparent-edit
	       (set! saved-newline? #f) 
	       (send transparent-edit shutdown)
	       (set-position (last-position))
	       (set-caret-owner null)
	       (let ([a (get-admin)])
		 (unless (null? a)
		   (send a grab-caret)))		 
	       (set! transparent-edit #f)))]
	  [set-auto-set-wrap
	   (lambda (x)
	     (super-set-auto-set-wrap x)
	     (for-each (lambda (c) (send c widen-snips x)) canvases))]
	  [init-transparent-io-do-work
	   (lambda (grab-focus?)
	     (let ([starting-at-prompt-mode? prompt-mode?])
	       (set! transparent-edit (make-object io-edit%))
	       (semaphore-wait timer-sema)
	       (when timer-on
		 (set-box! timer-on (cons transparent-edit (unbox timer-on)))
		 (send transparent-edit begin-edit-sequence))
	       (semaphore-post timer-sema)
	       (send transparent-edit enable-autoprompt #f)
	       (dynamic-wind
		(lambda () (begin-edit-sequence #f))
		(lambda ()

		  ;; ensure that there is a newline before the snip is inserted
		  (when (zero? (bitwise-and wx:const-snip-hard-newline
					      (send (find-snip (last-position) wx:const-snip-before)
						    get-flags)))
		    (insert #\newline (last-position) (last-position)))
		      
		  (when starting-at-prompt-mode?
		    (set! prompt-mode? #f))

		  (unless transparent-edit
		    (printf "transparent-edit is ~a!" transparent-edit))
		  (send transparent-edit set-auto-set-wrap #t)
		  (let ([snip (make-object wx:media-snip% transparent-edit)])
		    (set! transparent-snip snip)
		    (insert snip (last-position) (last-position))
		    (insert (string #\newline) (last-position) (last-position))
		    (for-each (lambda (c) (send c add-wide-snip snip))
			      canvases))
		  (when grab-focus?
		    (let ([a (send transparent-edit get-admin)])
		      (unless (null? a)
			(send a grab-caret))))
		  (when starting-at-prompt-mode?
		    (insert-prompt)))
		(lambda ()
		  (end-edit-sequence)))
	       (set! prompt-position (last-position))))]
	  [init-transparent-io
	   (lambda (grab-focus?)
	     (if transparent-edit
		 (when grab-focus?
		   (let ([a (send transparent-edit get-admin)])
		     (unless (null? a)
			(send a grab-caret))))
		 (init-transparent-io-do-work grab-focus?)))]
	  [single-threader (make-single-threader)]
	  [this-in-read
	   (let* ([g (lambda ()
		       (init-transparent-io #t)
		       (send transparent-edit fetch-char))])
	     (lambda ()
	       (kill-protect
		'read-char
		#t
		(lambda ()
		  (single-threader g)))))]
	  [transparent-read
	   (let* ([g (lambda ()
		       (init-transparent-io #t)
		       (send transparent-edit fetch-sexp))])
	     (lambda ()
	       (kill-protect
		'read
		#t
		(lambda ()
		  (single-threader g)))))])
	(public
	  [set-output-delta
	   (lambda (delta)
	     (set! output-delta delta))]
	  
	  [previous-expr-pos -1]
	  [previous-expr-positions null]
	  [clear-previous-expr-positions
	   (lambda ()
	     (set! previous-expr-positions null))]
	  [copy-previous-expr
	   (lambda (which)
	     (let ([snip/strings (list-ref (mred:preferences:get-preference
					    'mred:console-previous-exprs) 
					   which)])
	       (begin-edit-sequence)
	       (when (and autoprompting? (not prompt-mode?))
		 (insert-prompt))
	       (delete prompt-position (last-position) #f)
	       (for-each (lambda (snip/string)
			   (insert (if (is-a? snip/string wx:snip%)
				       (send snip/string copy)
				       snip/string)
				   prompt-position))
			 snip/strings)
	       (set-position (last-position))
	       (end-edit-sequence)))]
	  [copy-next-previous-expr
	   (lambda ()
	     (let ([previous-exprs (mred:preferences:get-preference 'mred:console-previous-exprs)])
	       (unless (null? previous-exprs)
		 (set! previous-expr-pos
		       (if (< (add1 previous-expr-pos) (length previous-exprs))
			   (add1 previous-expr-pos)
			   0))
		 (copy-previous-expr previous-expr-pos))))]
	  [copy-prev-previous-expr
	   (lambda ()
	     (let ([previous-exprs (mred:preferences:get-preference 'mred:console-previous-exprs)])
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
		    (let loop ([snip 
				(find-snip start
					   wx:const-snip-after-or-null)]
			       [snips null])
		      (cond
			[(null? snip) snips]
			[(<= (get-snip-position snip) end)
			 (loop (send snip next)
			       (cons (send snip copy) snips))]
			[else snips]))])
	       (set! previous-expr-positions (cons (cons start end) previous-expr-positions))
	       (set! previous-expr-pos -1)
	       (let* ([previous-exprs (mred:preferences:get-preference 'mred:console-previous-exprs)]
		      [new-previous-exprs 
		       (let* ([trimmed-previous-exprs
			       (if (>= (length previous-exprs) console-max-save-previous-exprs)
				   (cdr previous-exprs)
				   previous-exprs)])
			 (let loop ([l trimmed-previous-exprs])
			   (if (null? l)
			       (list snips)
			       (cons (car l) (loop (cdr l))))))])
		 (mred:preferences:set-preference 'mred:console-previous-exprs new-previous-exprs))
	       (do-eval start end)))]
	  
	  [reset-pretty-print-width
	   (lambda ()
	     (let* ([standard (send (get-style-list) find-named-style "Standard")])
	       (unless (null? standard)
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
	     (reset-pretty-print-width)
	     (do-pre-eval)
	     (mred:gui-utils:local-busy-cursor
	      (get-canvas)
	      (lambda ()
		(eval-and-display (get-text start end #t))))
	     (do-post-eval))]
	  [do-pre-eval
	   (lambda ()
	     (ready-non-prompt))]
	  [do-post-eval
	   (lambda ()
	     (insert-prompt))])
	
	(public
	  [eval-str mzlib:string:eval-string]
	  [this-result-write 
	   (lambda (s)
	     (kill-protect
	      'result
	      #f
	      (lambda ()
		(generic-write this
			       s 
			       (lambda (start end)
				 (change-style result-delta
					       start end))))))]
	  [this-result (make-output-port this-result-write generic-close)]
	  [display-result
	   (lambda (v)
	     (unless (void? v)
	       '(begin-edit-sequence)
	       (parameterize 
		   ([mzlib:pretty-print:pretty-print-size-hook
		     (lambda (x _ port) (and (is-a? x wx:snip%) 1))]
		    [mzlib:pretty-print:pretty-print-print-hook
		     (lambda (x _ port) (this-result-write x))])
		 (mzlib:pretty-print:pretty-print v this-result))
	       '(end-edit-sequence)))]
	  [eval-and-display
	   (lambda (str)
	     (catch-errors
	      #f
	      (lambda () #f)
	      (call-with-values 
	       (lambda ()
		 (dynamic-enable-break
		  (lambda ()
		    (eval-str str))))
	       (lambda v
		 (let ([v
			(let loop ([v v])
			  (cond
			    [(null? v) null]
			    [(void? (car v)) (loop (cdr v))]
			    [else (cons (car v) (loop (cdr v)))]))])
		   (if (null? v)
		       (void)
		       (for-each (lambda (x)
				   (display-result x))
				 v)))))))])
	
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
	  [eval-busy? (lambda () #f)]
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
		      (let loop ([snip (find-snip start wx:const-snip-after)])
			(cond
			  [(null? snip) (void)]
			  [(< (get-snip-position snip) end)
			   (insert (send snip copy) (last-position))
			   (loop (send snip next))]
			  [else (void)]))
		      (set-position (last-position)))])
	       (cond
		 [(not (or (= code cr-code) (= code lf-code)))
		  (super-on-local-char key)]
		 [(and (< start end) (< end prompt-position)
		       (not (eval-busy?)))
		  (begin-edit-sequence)
		  (when (not prompt-mode?)
		    (insert-prompt))
		  (copy-to-end/set-position start end)
		  (end-edit-sequence)]
		 [(and (= start last) (not prompt-mode?)
		       autoprompting? (not (eval-busy?)))
		  (insert-prompt)]
		 [(and (< prompt-position start)
		       (only-spaces-after start)
		       (not (eval-busy?)))
		  (let ([balanced? (mred:scheme-paren:scheme-balanced?
				    this
				    prompt-position
				    last)])
		    (if balanced?
			(begin
			  (delete start last)
			  (cleanup-transparent-io)
			  (do-save-and-eval prompt-position start))
			(super-on-local-char key)))]
		 [(< start prompt-position)
		  (let ([match
			    (mred:scheme-paren:scheme-backward-match
			     this start 0)])
		    (if match
			(begin
			  (begin-edit-sequence)
			  (copy-to-end/set-position match start)
			  (end-edit-sequence))
			(super-on-local-char key)))]
		 [else (super-on-local-char key)])))]
	  
	  [insert-prompt
	   (lambda ()
	     (set! prompt-mode? #t)
	     (begin-edit-sequence)
	     (flush-console-output)
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
	       (scroll-to-position start-selection #f (last-position) 1)))])
	
	(public
	  [reset-console-start-position 0]
	  [reset-console-end-position #f])
	(private
	  [reset-console-end-location #f]
	  [reset-console-start-location #f]
	  [reset-console-locations
	   (lambda ()
	     (when reset-console-end-position
	       (set! reset-console-end-location
		     (line-location (position-line reset-console-end-position) #f)))
	     (when reset-console-start-position
	       (set! reset-console-start-location
		     (line-location (position-line reset-console-start-position) #f)))
	     (when (and reset-console-start-location reset-console-end-location)
	       (invalidate-bitmap-cache 0 reset-console-start-location
					-1 (- reset-console-end-location
					      reset-console-start-location))))])
	(public
	  [set-last-header-position
	   (lambda (p)
	     (set! reset-console-start-position p)
	     (reset-console-locations))]
	  [after-set-size-constraint
	   (lambda ()
	     (super-after-set-size-constraint)
	     (run-after-edit-sequence
	      (lambda ()
		(reset-console-locations))
	      'reset-console-locations))]
	  [reset-console
	   (let* ([delta (make-object wx:style-delta%)]
		  [color-add (send delta get-foreground-add)]
		  [color-mult (send delta get-foreground-mult)])
	     (send color-mult set 0 0 0)
	     (send color-add set 127 127 127)
	     (lambda ()
	       (cleanup-transparent-io)
	       (when (number? prompt-position)
		 (set! reset-console-end-position prompt-position))
	       (set-resetting #t)
	       (if (< (wx:display-depth) 8)
		   (begin (reset-console-locations)
			  (send (get-canvas) force-redraw))
		   (when (and reset-console-end-position
			      reset-console-start-position)
		     (change-style delta reset-console-start-position 
				   reset-console-end-position)))
	       (set-resetting #f)))]
	  [on-paint
	   (lambda (before dc left top right bottom dx dy draw-caret)
	     (super-on-paint before dc left top right bottom
			     dx dy draw-caret)
	     (when (and (not before) 
			reset-console-start-location
			reset-console-end-location)
	       (let ([height (max 0
				  (- reset-console-end-location
				     reset-console-start-location))])
		 (when (> height 0)
		   (let* ([old-pen (send dc get-pen)]
			  [old-brush (send dc get-brush)]
			  [old-logical (send dc get-logical-function)]
			  [pen (send wx:the-pen-list
				     find-or-create-pen
				     "BLACK" 0
				     wx:const-transparent)]
			  [brush (make-object wx:brush% "BLACK" 
					      wx:const-stipple)]
			  [x dx]
			  [y (+ dy reset-console-start-location)]
			  [width (let ([b (box 0)])
				   (get-extent b null)
				   (max 0 (unbox b)))])
		     (send brush set-stipple (mred:icon:get-reset-console-bitmap))
		     (send dc set-pen pen)
		     (send dc set-brush brush)
		     (send dc set-logical-function wx:const-xor)
		     (send dc draw-rectangle x y width height)
		     (send dc set-pen old-pen)
		     (send dc set-brush old-brush)
		     (send dc set-logical-function old-logical))))))]
	  [ready-non-prompt
	   (lambda ()
	     (when prompt-mode?
	       (set! prompt-mode? #f)
	       (let ([c-locked locked?])
		 (begin-edit-sequence)
		 (lock #f)
		 (insert #\newline (last-position))
		 (lock c-locked)	   
		 (end-edit-sequence))))]
	  
	  [initialize-console
	   (lambda ()
	     #t)]
	  [make-this-in
	   (lambda ()
	     (make-input-port this-in-read
			      this-in-char-ready?
			      generic-close))]
	  [make-this-out
	   (lambda ()
	     (make-output-port this-out-write generic-close))]
	  [make-this-err
	   (lambda ()
	     (make-output-port this-err-write generic-close))]
	  [this-err (make-this-err)]
	  [this-out (make-this-out)]
	  [this-in (make-this-in)]
	  [takeover
	   (lambda ()
	     (mred:debug:unless 
	      'no-takeover
	      (error-display-handler
	       (let ([old (error-display-handler)])
		 (rec console-error-display-handler
		      (lambda (x)
			(old x)
			(flush-console-output)))))
	      (current-output-port this-out)
	      (current-input-port this-in)
	      (current-error-port this-err)
	      (port-read-handler this-in (lambda (x) (transparent-read)))
	      (mzlib:pretty-print:pretty-print-display-string-handler 
	       (lambda (string port)
		 (for-each (lambda (x) (write-char x port))
			   (string->list string))))
	      (for-each (lambda (port port-out-write)
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
							       (lambda (x _ port) (and (is-a? x wx:snip%) 1))]
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
			(list this-out-write this-err-write this-result-write))))])
	(sequence
	  (mred:debug:printf 'super-init "before console-edit%")
	  (apply super-init args)
	  (mred:debug:printf 'super-init "after console-edit%"))
	(sequence
	  (set-mode (make-object mred:scheme-mode:scheme-interaction-mode%))
	  (takeover)))))
  
  (define console-edit% (make-console-edit% mred:edit:backup-autosave-edit%))
  
  (define ibeam-cursor (make-object wx:cursor% wx:const-cursor-ibeam))

  (define make-transparent-io-edit%
    (lambda (super%)
      (class super% args
	(inherit change-style prompt-position set-prompt-position
		 resetting? set-resetting lock get-text
		 flush-console-output set-position last-position get-character
		 clear-undos set-auto-set-wrap locked? set-cursor
		 do-pre-eval do-post-eval)
	(rename [super-on-insert on-insert]
		[super-on-local-char on-local-char])
	(private
	  [input-delta (make-object wx:style-delta%)]
	  [data null])
	(sequence
	  (let ([mult (send input-delta get-foreground-mult)]
		[add (send input-delta get-foreground-add)])
	    (send mult set 0 0 0)
	    (send add set 0 150 0)))
	(private [shutdown? #f])

	(public [reset-console-end-position #f]
		[reset-console-start-position #f])

	(public
	  [potential-sexps-protect (make-semaphore 1)]
	  [potential-sexps null]
	  [wait-for-sexp (make-semaphore 0)]
	  
	  [auto-set-wrap #t]
	  [on-local-char-NO-LONGER
	   (lambda (key)
	     (flush-console-output)
	     (super-on-local-char key))]
	  [shutdown
	   (lambda ()
	     (flush-console-output)
	     (set! shutdown? #t)
	     '(lock #t))]
	  [consumed-delta 
	   (make-object wx:style-delta% wx:const-change-bold)]
	  [mark-consumed
	   (lambda (start end)
	     (flush-console-output)
	     (let ([old-resetting resetting?])
	       (set-resetting #t)
	       (change-style consumed-delta start end)
	       (set-resetting old-resetting))
	     (set-prompt-position end))]
	  [fetch-sexp
	   (lambda ()
	     (set-cursor ibeam-cursor)
	     (flush-console-output)
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
		         (dynamic-enable-break (lambda () (wx:yield wait-for-sexp)))
			 loop))]
		     [answer answer]
		     [else (void)])
		   (set-cursor null)))))]
	  [fetch-char
	   (lambda ()
	     (flush-console-output)
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
		        (dynamic-enable-break (lambda () (wx:yield wait-for-sexp)))
			loop))])))))]
	  [takeover void]
	  [get-prompt (lambda () "")]
	  [on-insert
	   (lambda (start len)
	     (let ([old-r resetting?])
	       (set-resetting #t)
	       (change-style input-delta start (+ start len))
	       (set-resetting old-r))
	     (super-on-insert start len))]
	  
	  [do-eval
	   (lambda (start end)
	     (do-pre-eval)
	     (let ([new-sexps
		    (let loop ([pos start])
		      (cond
			[(< pos end) 
			 (let ([next-sexp
				(mred:scheme-paren:scheme-forward-match
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
  
  (define transparent-io-edit% 
    (make-transparent-io-edit%
     (make-console-edit%
      mred:edit:searching-edit%)))
  
  (define make-console-frame%
    (lambda (super%)
      (class super% ([close-item? #f]
		     [insert-welcome? #t]
		     [show? #t])
	(inherit active-edit get-edit get-canvas show make-menu)
	(rename [super-can-close? can-close?]
		[super-on-close on-close]
		[super-file-menu:close file-menu:close])
	(private 
	  edit-offset 
	  other-offset)
	(public
	  [get-canvas% (lambda () mred:canvas:wide-snip-canvas%)]
	  [get-edit% (lambda () console-edit%)])
	(public 
	  [can-close?
	   (lambda ()
	     (and (super-can-close?)
		  (unless (= 1 
			     (length
			      (send mred:group:the-frame-group get-frames)))
		    (mred:exit:exit))))]
	  [on-close
	   (lambda ()
	     (super-on-close))]
	  [next-menu-id (lambda () other-offset)]
	  [load-file
	   (lambda (file)
	     (load/cd file))]
	  [on-quit mred:exit:exit]
	  
	  [file-menu:revert #f]
	  [file-menu:close (and close-item?
				(lambda () 
				  (super-file-menu:close)))]
	  [file-menu:between-open-and-save
	   (lambda (file-menu)
	     (send file-menu append-item "&Load Scheme File..."
		   (lambda ()
		     (let ([file (parameterize ([mred:finder:dialog-parent-parameter
						 this])
				   (mred:finder:get-file))])
		       (when file
			 (load-file file)))))
	     (send file-menu append-item "Show Interactions History" show-interactions-history)
	     (send file-menu append-separator))])
	
	(sequence
	  (mred:debug:printf 'super-init "before console-frame%")
	  (super-init (string-append (mred:application:current-app-name) "Console"))
	  (mred:debug:printf 'super-init "after console-frame%")
	  (let ([edit (get-edit)])
	    (send edit set-file-format wx:const-media-ff-std)
	    
	    
	    ; Welcome message and initial prompt:
	    (mred:gui-utils:local-busy-cursor 
	     (get-canvas)
	     (lambda ()
	       (let ([last (send edit last-position)]
		     [insert-delta
		      (lambda (string delta)
			(let ([before (send edit get-start-position)])
			  (send* edit
			    (insert string)
			    (change-style delta before (send edit get-end-position)))))])
		 (when insert-welcome?
		   (insert-delta "Welcome to " message-delta)
		   (let ([before (send edit get-start-position)])
		     (insert-delta "MrEd" url-delta)
		     (send edit set-clickback 
			   before 
			   (send edit get-start-position)
			   (lambda (edit start end)
			     (credits))))
		   (insert-delta " " message-delta)
		   (insert-delta (copyright-string) message-delta)
		   (insert-delta "." message-delta)
		   (insert-delta #\newline message-delta)
		   (send edit change-style message-delta 0 last))
		 
		 (let ([dd (ivar edit output-delta)])
		   (dynamic-wind
		    (lambda ()
		      (send edit set-output-delta message-delta))
		    (lambda ()
		      (send edit initialize-console))
		    (lambda ()
		      (send edit set-output-delta dd)))))))
	    (send edit set-last-header-position (send edit get-end-position))
	    (send edit enable-autoprompt)
	    (send edit insert-prompt)
	    (send edit clear-undos)
	    (when show?
	      (show #t)))))))
  
  '(define console-frame% 
     (class wx:frame% ()
       (inherit show)
       (sequence 
	 (printf "not creating a console~n")
	 (super-init '() "Not a console" -1 -1 300 300)
	 (show #t)
	 (thread (lambda () 
		   (read-eval-print-loop)
		   (mred:exit:exit))))))
  
  (define console-frame% (make-console-frame% 
			  mred:frame:info-frame%)))
