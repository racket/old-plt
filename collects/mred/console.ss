
  (unit/sig mred:console^
    (import [mred:constants : mred:constants^]
	    [mred:preferences : mred:preferences^]
	    [mred:edit : mred:edit^]
	    [mred:frame : mred:frame^]
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
	    [mzlib:pretty-print : mzlib:pretty-print^]
	    [mzlib:trigger : mzlib:trigger^])
	    
    (mred:debug:printf 'invoke "mred:console@")

    (define newline-string (string #\newline))

    (define copyright-string
      (string-append
       "MrEd version "
       (mred:version:version)
       ", Copyright (c) 1995-1997 PLT, Rice University."))

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
	   "MzScheme (c) 1995 Matthew Flatt."
	   newline-indent-string
	   "libscheme (c) 1994 Brent Benson."
	   newline-indent-string
	   "conservative GC (c) 1988, 1989 Hans-J. Boehm, Alan J. Demers"
	   " (c) 1991-1994 Xerox Corp."
	   newline-indent-string
	   "C++ GC extension by Jesse Hull and John Ellis"
	   " (c) 1994 Xerox Corp."
	   newline-string))))


    (define welcome-message
      (string-append
       "Welcome to " copyright-string
       newline-string
       "Based on the following: "
       newline-string
       (credits-proc "  ")
       "See the license agreement or"
       " http://www.cs.rice.edu/CS/PLT/packages/mred/ for more info."))

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

    (define scheme-mode-edit% (make-scheme-mode-edit% mred:edit:edit%))

    (define console-max-save-previous-exprs 30)
		    
    (define make-single-threader
      (lambda ()
	(let ([sema (make-semaphore 1)])
	  (lambda (thunk)
	    (dynamic-wind
	     (lambda () (semaphore-wait sema))
	     thunk
	     (lambda () (semaphore-post sema)))))))

    (define mzlib:function:dynamic-delay-break
      (let ([p (current-output-port)])
        (lambda (t)
          (let* ([break? #f]
                 [old-handler (user-break-poll-handler)]
                 [new-handler
                  (lambda ()
                    (unless break?
                      '(begin (fprintf p ".") (flush-output p))
                      (when (old-handler)
                        '(fprintf p "~nsetting break to #t~n")
                        (set! break? #t)))
                    #f)])
            (begin0
              (parameterize ([user-break-poll-handler new-handler])
                            (t))
              '(fprintf p "~nwill break: ~a~n" break?)
              (when break?
                (break-thread (current-thread))))))))

    (define make-console-edit%
      (lambda (super%)
	(class super% args
	  (inherit begin-edit-sequence end-edit-sequence
		   position-line position-location
		   line-location get-admin
		   set-position set-caret-owner
		   clear-undos insert delete
		   change-style styles-fixed? split-snip
		   scroll-to-position
		   last-position get-start-position get-end-position
		   get-text get-snip-position
		   get-character find-snip find-string
		   erase set-mode get-canvas
		   invalidate-bitmap-cache
		   get-extent get-style-list canvases)
	  (rename [super-set-auto-set-wrap set-auto-set-wrap]
		  [super-on-local-char on-local-char]
		  [super-on-paint on-paint]
		  [super-after-set-size-constraint after-set-size-constraint]
		  [super-after-insert after-insert]
		  [super-after-delete after-delete])
	  (public
	    [orig-stdout (current-output-port)]
	    [orig-stderr (current-error-port)])
	  (public
	    [CACHE-TIME 1] 
	    [CACHE-WRITE-COUNT 100]
	    
	    [normal-font wx:const-modern]
	    [normal-delta '()]
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

	  (rename
	    [super-on-insert on-insert]
	    [super-on-delete on-delete]
	    [super-on-change-style on-change-style])
	  (private
	    [on-something
	     (opt-lambda (super [attend-to-styles-fixed? #f])
	       (lambda (start len)
		 (let ([guard (or (not (number? prompt-position))
				  (>= start prompt-position)
				  (and attend-to-styles-fixed? styles-fixed?)
				  resetting?)])
		   (and guard
			((super) start len)))))])
	  (public
	    [resetting? #f]
	    [set-resetting (lambda (v) (set! resetting? v))]
	    [on-insert (on-something (lambda () super-on-insert))]
	    [on-delete (on-something (lambda () super-on-delete))]
	    [on-change-style (on-something (lambda () super-on-change-style) #t)])
	       
	  (private
	    [last-str (lambda (l)
			(if (null? (cdr l))
			    (car l)
			    (last-str (cdr l))))]
	    [timer-on #f]
	    [timer-sema (make-semaphore 1)]
	    [timer-writes 0])
	  (public
	    [generic-write
	     (lambda (s style-func)
	       (mzlib:function:dynamic-delay-break
		(lambda ()
		  (semaphore-wait timer-sema)
		  (if timer-on
		      (begin
			(set! timer-writes (add1 timer-writes))
			(when (> timer-writes CACHE-WRITE-COUNT)
			  (end-edit-sequence)
			  (begin-edit-sequence #f)
			  (set! timer-writes 0)))
		      (begin
			(set! timer-writes 0)
			(let ([on-box (box #t)])
			  (begin-edit-sequence #f)
			  (set! timer-on on-box)
			  (thread 
			   (lambda ()
			     (dynamic-wind
			      void
			      (lambda ()
				(sleep CACHE-TIME))
			      (lambda ()
				(semaphore-wait timer-sema)
				(when (unbox on-box)
				  (end-edit-sequence)
				  (set! timer-on #f))
				(semaphore-post timer-sema))))))))
		  (semaphore-post timer-sema)
		  (begin-edit-sequence #f)
		  (set-position (last-position))
		  (when (and prompt-mode? autoprompting?)
		    (insert #\newline))
		  (let ((start (last-position)))
		    (insert (if (is-a? s wx:snip%)
				(send s copy)
				s))
		    (let ((end (last-position)))
		      (change-style () start end)
		      (style-func start end)))
		  (end-edit-sequence)
		  (set-prompt-mode #f))))]
	    [generic-close (lambda () '())]
	    [flush-console-output
	     (lambda ()
	       (semaphore-wait timer-sema)
	       (when timer-on
		 (end-edit-sequence)
		 (set-box! timer-on #f)
		 (set! timer-on #f))
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
	    [this-out-write
	     (lambda (s)
	       (mzlib:function:dynamic-delay-break
		(lambda ()
		  (parameterize ([current-output-port orig-stdout]
				 [current-error-port orig-stderr])
	            (init-transparent-io)
		    (send transparent-edit 
			  generic-write s 
			  (lambda (start end)
			    (send* transparent-edit
				   (begin-edit-sequence #f)
				   (change-style output-delta 
						 start end)
				   (end-edit-sequence))))))))]
	    [this-err-write
	     (lambda (s)
	       (mzlib:function:dynamic-delay-break
		(lambda ()
		  (parameterize ([current-output-port orig-stdout]
				 [current-error-port orig-stderr])
		    (generic-write s
				   (lambda (start end)
				     (change-style error-delta 
						   start end)))))))])
	       
	  (public
	    [transparent-edit #f]
	    [this-in-char-ready? (lambda () #t)]
	    [cleanup-transparent-io
	     (lambda ()
	       (when transparent-edit
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
	    [init-transparent-io
	     (lambda ()
	       (unless transparent-edit
		 (when prompt-mode?
		   (set! prompt-mode? #f)
		   (insert #\newline))
		 (set! transparent-edit (make-object transparent-io-edit%))
		 (send transparent-edit enable-autoprompt #f)
		 (dynamic-wind
		  (lambda () (begin-edit-sequence #f))
		  (lambda ()		  
		    (let ([snip (make-object wx:media-snip% transparent-edit)])
		      (for-each (lambda (c) (send c add-wide-snip snip))
				canvases)
		      (insert snip)
		      (insert #\newline))
		    (let ([a (send transparent-edit get-admin)])
		      (unless (null? a)
			(send a grab-caret))))
		  (lambda () (end-edit-sequence)))
		 (set! prompt-position (last-position))))]
	    [single-threader (make-single-threader)]
	    [this-in-read
	     (let* ([g (lambda ()
			 (init-transparent-io)
			 (send transparent-edit fetch-char))]
		    [f (lambda () (mzlib:function:dynamic-delay-break g))])
	       (lambda ()
		 (single-threader f)))]
	    [transparent-read
	     (let* ([g (lambda ()
			 (init-transparent-io)
			 (send transparent-edit fetch-sexp))]
		    [f (lambda ()
			 (mzlib:function:dynamic-delay-break g))])
	       (lambda () (single-threader f)))])
	  (public
	    [set-output-delta
	     (lambda (delta)
	       (set! output-delta delta))]
	    
	    [previous-exprs ()]
	    [max-save-previous-exprs console-max-save-previous-exprs]
	    [previous-expr-pos -1]
	    [copy-previous-expr
	     (lambda (which)
	       (let ([snips (list-ref previous-exprs which)])
		 (begin-edit-sequence)
		 (when (and autoprompting? (not prompt-mode?))
		   (insert-prompt))
		 (delete prompt-position (last-position) #f)
		 (for-each (lambda (snip)
			     (insert (send snip copy) prompt-position))
			   snips)
		 (set-position (last-position))
		 (end-edit-sequence)))]
	    [copy-next-previous-expr
	     (lambda ()
	       (unless (null? previous-exprs)
		 (set! previous-expr-pos
		       (if (< (add1 previous-expr-pos) (length previous-exprs))
			   (add1 previous-expr-pos)
			   0))
		 (copy-previous-expr previous-expr-pos)))]
	    [copy-prev-previous-expr
	     (lambda ()
	       (unless (null? previous-exprs)
		 (set! previous-expr-pos
		       (if (<= previous-expr-pos 0)
			   (sub1 (length previous-exprs))
			   (sub1 previous-expr-pos)))
		 (copy-previous-expr previous-expr-pos)))]
	    
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
		 (set! previous-expr-pos -1)
		 (if (null? previous-exprs)
		     (set! previous-exprs (list snips))
		     (begin
		       (if (>= (length previous-exprs) max-save-previous-exprs)
			   (set! previous-exprs (cdr previous-exprs)))
		       (let loop ([l previous-exprs])
			 (if (null? (cdr l))
			     (set-cdr! l (list snips))
			     (loop (cdr l))))))
		 (do-eval start end)))]
	    [do-eval
	     (lambda (start end)
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
		 (generic-write s 
				(lambda (start end)
				  (change-style result-delta
						start end))))]
	      [this-result (make-output-port this-result-write generic-close)]
	      [display-result
	       (lambda (v)
		 (unless (void? v)
		   (with-parameterization user-parameterization
		     (lambda ()
		       (parameterize 
			   ([mzlib:pretty-print:pretty-print-size-hook
			     (lambda (x _ port) (and (is-a? x wx:snip%) 1))]
			    [mzlib:pretty-print:pretty-print-print-hook
			     (lambda (x _ port) (this-result-write x))])
			 (mzlib:pretty-print:pretty-print v this-result))))))]
	      [eval-and-display
	       (lambda (str)
		 (catch-errors
		  #f
		  (lambda () #f)
		  (call-with-values 
		   (lambda ()
		     (with-parameterization user-parameterization
		       (lambda ()
			 (dynamic-enable-break
			  (lambda ()
			    (eval-str str))))))
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
			      (do-save-and-eval prompt-position start))
			    (super-on-local-char key)))]
		     [(< start prompt-position)
		      (let ([match
				(mred:scheme-paren:scheme-backward-match
				 this start 0)])
			(if match
			    (copy-to-end/set-position match start)
			    (super-on-local-char key)))]
		     [else (super-on-local-char key)])))]
	      
	      [insert-prompt
	       (lambda ()
		 (cleanup-transparent-io)
		 (set! prompt-mode? #t)
		 (let* ([last (last-position)]
			[start-selection (get-start-position)]
			[end-selection (get-end-position)]
			[last-str (get-text (- last 1) last)])
		   (begin-edit-sequence)
		   (unless (string=? last-str newline-string)
		     (insert #\newline last))
		   (let ([last (last-position)])
		     (insert (get-prompt) last)
		     (change-style normal-delta last (last-position)))
		   (set! prompt-position (last-position))
		   (clear-undos)
		   (unless (= start-selection end-selection)
		     (set-position start-selection end-selection) 
		     (scroll-to-position start-selection (last-position) 1))
		   (end-edit-sequence)
		   (flush-console-output)))]
	      [after-insert
	       (lambda (start len)
		 (if (or resetting?
			 (and prompt-mode? (< start prompt-position)))
		     (set! prompt-position (+ len prompt-position)))
		 (super-after-insert start len))]
	      [after-delete
	       (lambda (start len)
		 (if (or resetting?
			 (and prompt-mode? (< start prompt-position)))
		     (set! prompt-position (- prompt-position len)))
		 (super-after-delete start len))])
	    
	    (private
	      [reset-console-start-position 0]
	      [reset-console-end-position #f]
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
		 (reset-console-locations))]
	      [reset-console
	       (let* ([delta (make-object wx:style-delta%)]
		      [color-add (send delta get-foreground-add)]
		      [color-mult (send delta get-foreground-mult)])
		 (send color-mult set 0 0 0)
		 (send color-add set 127 127 127)
		 (lambda ()
		   (when (number? prompt-position)
		     (set! reset-console-end-position prompt-position))
		   (set-resetting #t)
		   (if (< (wx:display-depth) 8)
		       (begin (reset-console-locations)
			      (send (get-canvas) force-redraw))
		       (when reset-console-end-position
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
		   (let* ([old-pen (send dc get-pen)]
			  [old-brush (send dc get-brush)]
			  [old-logical (send dc get-logical-function)]
			  [pen (make-object wx:pen% "BLACK" 1
					    wx:const-transparent)]
			  [brush (make-object wx:brush% "BLACK" 
					      wx:const-stipple)]
			  [x dx]
			  [y (+ dy reset-console-start-location)]
			  [width (begin (get-extent #1=#&0 null)
					(unbox #1#))]
			  [height  (- reset-console-end-location
				      reset-console-start-location)])
		     (send brush set-stipple mred:icon:reset-console-bitmap)
		     (send dc set-pen pen)
		     (send dc set-brush brush)
		     (send dc set-logical-function wx:const-xor)
		     (send dc draw-rectangle x y width height)
		     (send dc set-pen old-pen)
		     (send dc set-brush old-brush)
		     (send dc set-logical-function old-logical))))]
	      [ready-non-prompt
	       (lambda ()
		 (when prompt-mode?
		   (set! prompt-mode? #f)
		   (insert #\newline (last-position))))]
	      
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
		 '(error-display-handler
		  (let ([old (error-display-handler)])
		    (lambda (x)
		      (print-struct #t)
		      (mred:gui-utils:message-box (format "~a" x) "Uncaught Exception")
		      (old x))))
		 (mred:debug:unless 'no-takeover
		   (let ([doit
			  (lambda ()
			    (current-output-port this-out)
			    (current-input-port this-in)
			    (current-error-port this-err)
			    (mzlib:pretty-print:pretty-print-display-string-handler 
			     (lambda (string port)
			       (for-each (lambda (x) (write-char x port))
					 (string->list string))))
			    (for-each (lambda (port port-out-write)
					(let ([handler-maker
					       (lambda (pretty)
						 (lambda (v p)
						   (parameterize 
						       ([mzlib:pretty-print:pretty-print-size-hook
							 (lambda (x _ port) (and (is-a? x wx:snip%) 1))]
							[mzlib:pretty-print:pretty-print-print-hook
							(lambda (x _ port) (port-out-write x))])
						     (pretty v p 'infinity))))])
					(port-write-handler port (handler-maker mzlib:pretty-print:pretty-print))
					(port-display-handler port (handler-maker mzlib:pretty-print:pretty-display))))
				      (list this-out this-err this-result)
				      (list this-out-write this-err-write this-result-write)))])
		     (with-parameterization user-parameterization doit)
		     (doit))))])
	  (sequence
	    (mred:debug:printf 'super-init "before console-edit%")
	    (apply super-init args)
	    (mred:debug:printf 'super-init "after console-edit%")
	    (set-mode (make-object mred:scheme-mode:scheme-interaction-mode%)))
	  (public [user-parameterization (make-parameterization)])
	  (sequence
	    (takeover)
	    (port-read-handler this-in (lambda (x) (transparent-read)))
	    (with-parameterization user-parameterization
	      (lambda ()
		(parameterization-branch-handler 
		 (lambda () (make-parameterization user-parameterization)))))))))
      
    (define console-edit% (make-console-edit% mred:edit:edit%))

    (define make-transparent-io-edit%
      (lambda (super%)
	(class-asi super%
	  (inherit change-style prompt-position set-prompt-position resetting? set-resetting lock get-text
		   flush-console-output set-position last-position get-character
		   clear-undos
		   do-pre-eval do-post-eval)
	  (rename [super-on-insert on-insert]
		  [super-on-local-char on-local-char]
		  [super-generic-write generic-write])
	  (private
	    [input-delta (make-object wx:style-delta%)]
	    [data null])
	  (sequence
	    (let ([mult (send input-delta get-foreground-mult)]
		  [add (send input-delta get-foreground-add)])
	      (send mult set 0 0 0)
	      (send add set 0 150 0)))
	  (private [shutdown? #f])
	  (public
	    [on-local-char
	     (lambda (key)
	       (flush-console-output)
	       (super-on-local-char key))]
	    [shutdown
	     (lambda ()
	       (flush-console-output)
	       (set! shutdown? #t)
	       (lock #t))]
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
	       (flush-console-output)
	       (let loop ()
		 (cond
		   [shutdown? (void)]
		   [(null? potential-sexps) 
		    (wx:yield)
		    (loop)]
		   [else (let* ([sexp (car potential-sexps)]
				[start (car sexp)]
				[end (cdr sexp)]
				[text (get-text start end #t)])
			   (set! potential-sexps (cdr potential-sexps))
			   (mark-consumed start end)
			   (clear-undos)
			   (read (open-input-string text)))])))]
	    [fetch-char
	     (lambda ()
	       (flush-console-output)
	       (let ([found-char
		      (lambda (pos)
			(mark-consumed pos (add1 pos))
			(get-character pos))])
		 (let loop ()
		   (cond
		     [(not (null? potential-sexps))
		      (let ([first-sexp (car potential-sexps)])
			(set! potential-sexps null)
			(found-char (car first-sexp)))]
		     [(< prompt-position (last-position))
		      (found-char prompt-position)]
		     [else (wx:yield) (loop)]))))]
	    [takeover void]
	    [get-prompt (lambda () "")]
	    [generic-write
	     (lambda (s style-funct)
	       (super-generic-write s (lambda (start end)
					(style-funct start end)
					(set-prompt-position end))))]
	    [on-insert
	     (lambda (start len)
	       (let ([old-r resetting?])
		 (set-resetting #t)
		 (change-style input-delta start (+ start len))
		 (set-resetting old-r))
	       (super-on-insert start len))]
	    [potential-sexps null]
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
	       (set! potential-sexps (append potential-sexps new-sexps))
	       (do-post-eval)))]))))
      
      (define transparent-io-edit% 
	(make-transparent-io-edit% console-edit%))


    (define make-console-frame%
      (lambda (super%)
	(class super% ([close-item? #f]
		       [mssg welcome-message]
		       [show? #t])
	  (inherit active-edit get-edit get-canvas show make-menu)
	  (rename [super-on-close on-close]
		  [super-make-menu-bar make-menu-bar])
	  (private 
	    edit-offset 
	    other-offset)
	  (public
	    [get-canvas% (lambda () mred:canvas:wide-snip-canvas%)]
	    [get-edit% (lambda () console-edit%)])
	  (public 
	    [make-menu-bar
	     (let ([reg (regexp "<TITLE>(.*)</TITLE>")])
	       (lambda ()
		 (let* ([mb (super-make-menu-bar)]
			[help-menu (make-menu)]
			[dir (build-path (global-defined-value 
					  'mred:plt-home-directory)
					 "doc")])
		   (if (directory-exists? dir)
		       (let* ([dirs (directory-list dir)]
			      [find-title
			       (lambda (name)
				 (lambda (port)
				   (let loop ([l (read-line port)])
				     (if (eof-object? l)
					 name
					 (let ([match (regexp-match reg l)])
					   (if match
					       (cadr match)
					       (loop (read-line port))))))))]
			      [build-item
			       (lambda (local-dir output)
				 (let* ([f (build-path dir local-dir "index.htm")])
				   (if (file-exists? f)
				       (let ([title (call-with-input-file f (find-title local-dir))])
					 (cons 
					  (list title
						(lambda ()
						  (let* ([f (make-object mred:hyper-frame:hyper-view-frame%
									 (string-append "file:" f))])
						    (send f set-title-prefix title)
						    f)))
					  output))
				       (begin (mred:debug:printf 'help-menu "couldn't find ~a" f)
					      output))))]
			      [item-pairs 
			       (mzlib:function:quicksort
				(mzlib:function:foldl build-item null dirs)
				(lambda (x y) (string-ci<? (car x) (car y))))])
			 (unless (null? item-pairs)
			   (send mb append help-menu "Help"))
			 (for-each (lambda (x) (apply (ivar help-menu append-item) x))
				   item-pairs))
		       (mred:debug:printf 'help-menu "couldn't find PLTHOME/doc directory"))
		   mb)))]
	    [on-close 
	     (lambda ()
	       (super-on-close)
	       (mred:exit:exit))]
	    [next-menu-id (lambda () other-offset)]
	    [load-file
	     (lambda (file)
	       (load/cd file))]
	    [on-quit mred:exit:exit]

	    [file-menu:revert #f]
	    [file-menu:close (and close-item?
				  (lambda () (when (on-close)
					       (show #f))))]
	    [file-menu:between-open-and-save
	     (lambda (file-menu)
	       (send file-menu append-item "&Load Scheme File..."
		     (lambda ()
		       (let ((file (mred:finder:get-file)))
			 (if file
			     (load-file file)))))
	       (send file-menu append-separator))])
	  
	  (sequence
	    (mred:debug:printf 'super-init "before console-frame%")
	    (super-init (string-append mred:application:app-name "Console"))
	    (mred:debug:printf 'super-init "after console-frame%")
	    (let ([edit (get-edit)])
	      (send edit set-file-format wx:const-media-ff-std)
	      
	      
	      ; Welcome message and initial prompt:
	      (when mssg
		(send edit insert mssg)
		(send edit set-last-header-position (send edit get-end-position))
		
		(mred:gui-utils:local-busy-cursor 
		 (get-canvas)
		 (lambda ()
		   (let ([last (send edit last-position)]
			 [delta (make-object wx:style-delta%
					     wx:const-change-family
					     wx:const-decorative)])
		     (send delta set-delta wx:const-change-size 10)
		     (send edit insert #\newline)
		     (send edit change-style delta 0 last)
		     
		     (let ([dd (ivar edit output-delta)])
		       (dynamic-wind
			(lambda ()
			  (send edit set-output-delta delta))
			(lambda ()
			  (send edit initialize-console))
			(lambda ()
			  (send edit set-output-delta dd)))))))
		
		(send edit enable-autoprompt)
		(send edit insert-prompt)
		(send edit clear-undos)
		(when show?
		  (show #t))))))))

    (define console-frame% (make-console-frame%
			    (mred:find-string:make-searchable-frame%
			     mred:frame:simple-menu-frame%))))
