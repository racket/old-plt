
  (unit/sig mred:console^
    (import mred:wx^
	    [mred:constants : mred:constants^]
	    [mred:container : mred:container^]
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
	    [mzlib:pretty-print : mzlib:pretty-print^])
	    
    (mred:debug:printf 'invoke "mred:console@")

    (define-struct sexp (left right prompt))

    (define newline-string (string #\newline))

    (define (copyright-string)
      (string-append
       "version "
       (mred:version:version)
       "."
       newline-string
       "Copyright (c) 1995-1997 PLT, Rice University (Matthew Flatt and Robert Bruce Findler)"))

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

    (define make-console-edit%
      (lambda (super%)
	(class super% args
	  (inherit begin-edit-sequence end-edit-sequence
		   position-line position-location
		   line-location get-admin
		   set-position set-caret-owner
		   clear-undos insert delete
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
	  (public
	    [styles-fixed? #f]
	    [orig-stdout (current-output-port)]
	    [orig-stderr (current-error-port)])
	  (public
	    [CACHE-TIME 3]
	    [CACHE-WRITE-COUNT 300]
	    
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
	    [edit-sequence-count 0]
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
		   (and left
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
			  (fprintf mred:constants:original-output-port 
				   "left ~a right ~a~n" left right)
			  (set! needs-to-move-original
				(let loop ([snip (find-snip left wx:const-snip-after)])
				  (cond
				    [(null? snip) null]
				    [(< (get-snip-position snip) right)
				     (fprintf mred:constants:original-output-port 
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
			       (fprintf mred:constants:original-output-port 
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
	    [has-focus? #t]
	    [update-highlighted-prompt
	     (let ([last-start #f]
		   [last-end #f]
		   [plain (make-object wx:style-delta% wx:const-change-weight wx:const-normal)]
		   [at-prompt (make-object wx:style-delta% wx:const-change-weight wx:const-bold)])
	       (send at-prompt set-delta-foreground "FIREBRICK")
	       (send plain set-delta-foreground "BLACK")
	       (lambda ()
		 (when (and #f
			    (not updating-highlighted-prompt)
			    (zero? edit-sequence-count))
		   (set! updating-highlighted-prompt #t)
		   (begin-edit-sequence)
		   (let ([local-styles-fixed styles-fixed?]
			 [local-locked? locked?]
			 [clear-out
			  (lambda ()
			    (when last-start
			      (change-style plain last-start last-end)
			      (set! last-start #f)
			      (set! last-end #f)))])
		     (set-styles-fixed #f)
		     (lock #f)
		     (if has-focus?
			 (let*-values ([(left right) (find-which-previous-sexp)]
				       [(start end) (values (get-start-position)
							    (get-end-position))])
			   (when (and prompt-mode?
				      (<= prompt-position start)
				      (<= prompt-position end))
			     (set! left prompt-position))
			   (if left
			       (let ([prompt-start (- left (string-length (get-prompt)))])
				 (when (or (not last-start)
					   (not (= last-start prompt-start)))
				   (clear-out)
				   (change-style at-prompt prompt-start left)
				   (set! last-start prompt-start)
				   (set! last-end left)))
			       (clear-out)))
			 (clear-out))
		     (lock local-locked?)
		     (set-styles-fixed local-styles-fixed))
		   (end-edit-sequence)
		   (set! updating-highlighted-prompt #f))))])
	  (public
	    [on-insert
	     (lambda (start len)
	       '(fprintf mred:constants:original-output-port "on-insert~n")
	       (on-something super-on-insert start len))]
	    [on-delete
	     (lambda (start len)
	       '(fprintf mred:constants:original-output-port "on-delete~n")
	       (on-something super-on-delete start len))]
	    [on-change-style
	     (lambda (start len)
	       '(fprintf mred:constants:original-output-port "on-change-style~n")
	       (on-something super-on-change-style start len #t))]
	    [on-focus
	     (lambda (on?)
	       (set! has-focus? on?)
	       (update-highlighted-prompt)
	       (super-on-focus on?))]
	    [after-insert
	     (lambda (start len)
	       '(fprintf mred:constants:original-output-port "after-insert~n")
	       (update-highlighted-prompt)
	       (after-something + start len)
	       (super-after-insert start len))]
	    [after-delete
	     (lambda (start len)
	       '(fprintf mred:constants:original-output-port "after-delete~n")
	       (update-highlighted-prompt)
	       (after-something - start len)
	       (super-after-delete start len))]
	    [after-change-style
	     (lambda (start len)
	       '(fprintf mred:constants:original-output-port "after-change-style~n")
	       (update-highlighted-prompt)
	       (after-something (lambda (start len) start) start len)
	       (super-after-change-style start len))]
	    [on-edit-sequence
	     (lambda ()
	       (and (super-on-edit-sequence)
		    (begin '(fprintf mred:constants:original-output-port "on-edit-sequence ~a~n" edit-sequence-count)
			   (set! edit-sequence-count (add1 edit-sequence-count))
			   #t)))]
	    [after-edit-sequence
	     (lambda ()
	       (set! edit-sequence-count (sub1 edit-sequence-count))
	       '(fprintf mred:constants:original-output-port "after-edit-sequence ~a~n" edit-sequence-count)
	       (update-highlighted-prompt)
	       (super-after-edit-sequence))]
	    [after-set-position
	     (lambda ()
	       (update-highlighted-prompt)
	       (super-after-set-position))])
	  
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
	     (let ([first-time? #t])
	       (lambda (s style-func)
		 (mzlib:function:dynamic-disable-break
		  (lambda ()
		    (let ([handle-insertion
			   (lambda ()
			     (let ([start (last-position)]
				   [c-locked? locked?])
			       (begin-edit-sequence)
			       (lock #f)
			       (insert (if (is-a? s wx:snip%)
					   (send s copy)
					   s))
			       (let ((end (last-position)))
				 (change-style () start end)
				 (style-func start end)
				 (lock c-locked?)
				 (end-edit-sequence))))])
		      (if first-time?
			  (begin
			    (set! first-time? #f)
			    (handle-insertion))
			  (begin
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
			    (handle-insertion)
			    (end-edit-sequence)
			    (set-prompt-mode #f))))))))]
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
	       (mzlib:function:dynamic-disable-break
		(lambda ()
		  (parameterize ([current-output-port orig-stdout]
				 [current-error-port orig-stderr])
	            (init-transparent-io #f)
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
	       (mzlib:function:dynamic-disable-break
		(lambda ()
		  (parameterize ([current-output-port orig-stdout]
				 [current-error-port orig-stderr])
		    (cleanup-transparent-io)
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
	    [init-transparent-io-do-work
	     (lambda (grab-focus?)
	       (let ([starting-at-prompt-mode? prompt-mode?])
		 (set! transparent-edit (make-object transparent-io-edit%))
		 (send transparent-edit enable-autoprompt #f)
		 (dynamic-wind
		  (lambda () (begin-edit-sequence #f))
		  (lambda ()		  
		    (when starting-at-prompt-mode?
		      (set! prompt-mode? #f)
		      (insert #\newline))
		    (send transparent-edit set-auto-set-wrap #t)
		    (let ([snip (make-object wx:media-snip% transparent-edit)])
		      (insert snip)
		      (insert #\newline)
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
	       (unless transparent-edit
		 (init-transparent-io-do-work grab-focus?)))]
	    [single-threader (make-single-threader)]
	    [this-in-read
	     (let* ([g (lambda ()
			 (init-transparent-io #t)
			 (send transparent-edit fetch-char))]
		    [f (lambda () (mzlib:function:dynamic-disable-break g))])
	       (lambda ()
		 (single-threader f)))]
	    [transparent-read
	     (let* ([g (lambda ()
			 (init-transparent-io #t)
			 (send transparent-edit fetch-sexp))]
		    [f (lambda ()
			 (mzlib:function:dynamic-disable-break g))])
	       (lambda () (single-threader f)))])
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
		 (begin-edit-sequence)
		 (with-parameterization user-parameterization
		   (lambda ()
		     (parameterize 
			 ([mzlib:pretty-print:pretty-print-size-hook
			   (lambda (x _ port) (and (is-a? x wx:snip%) 1))]
			  [mzlib:pretty-print:pretty-print-print-hook
			   (lambda (x _ port) (this-result-write x))])
		       (mzlib:pretty-print:pretty-print v this-result))))
		 (end-edit-sequence)))]
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
		 ;(clear-undos)
		 (flush-console-output)
		 (unless (= start-selection end-selection)
		   (set-position start-selection end-selection) 
		   (scroll-to-position start-selection (last-position) 1))
		 (end-edit-sequence)))])
	  
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
	       (reset-console-locations))]
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
			[width (let ([b (box 0)])
				 (get-extent b null)
				 (max 0 (unbox b)))]
			[height (max 0
				     (- reset-console-end-location
					reset-console-start-location))])
		   (send brush set-stipple (mred:icon:get-reset-console-bitmap))
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
	       (error-display-handler
		 (let ([old (error-display-handler)])
		   (lambda (x)
		     (old x)
		     (flush-console-output))))
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
				    (doit)
				    (with-parameterization user-parameterization doit))))])
	  (public [user-parameterization (make-parameterization)])
	  (sequence
	    (mred:debug:printf 'super-init "before console-edit%")
	    (apply super-init args)
	    (mred:debug:printf 'super-init "after console-edit%")
	    (set-mode (make-object mred:scheme-mode:scheme-interaction-mode%)))
	  (sequence
	    (takeover)
	    (with-parameterization user-parameterization
	      (lambda ()
		(port-read-handler this-in (lambda (x) (transparent-read)))
		(parameterization-branch-handler 
		 (lambda () (make-parameterization user-parameterization)))))))))
      
    (define console-edit% (make-console-edit% mred:edit:backup-autosave-edit%))

    (define make-transparent-io-edit%
      (lambda (super%)
	(class super% args
	  (inherit change-style prompt-position set-prompt-position resetting? set-resetting lock get-text
		   flush-console-output set-position last-position get-character
		   clear-undos set-auto-set-wrap
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
	    [potential-sexps-protect (make-semaphore 1)]
	    [potential-sexps null]
	    [wait-for-sexp (make-semaphore 0)]

	    [auto-set-wrap #t]
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
		 (semaphore-wait potential-sexps-protect)
		 (cond
		   [shutdown? 
		    (semaphore-post potential-sexps-protect)
		    (void)]
		   [(null? potential-sexps)
		    (semaphore-post potential-sexps-protect)
		    (wx:yield wait-for-sexp)
		    (loop)]
		   [else (let* ([sexp (car potential-sexps)]
				[start (car sexp)]
				[end (cdr sexp)]
				[text (get-text start end #t)])
			   (set! potential-sexps (cdr potential-sexps))
			   (mark-consumed start end)
			   (clear-undos)
			   (semaphore-post potential-sexps-protect)
			   (read (open-input-string text)))])))]
	    [fetch-char
	     (lambda ()
	       (flush-console-output)
	       (let ([found-char
		      (lambda (pos)
			(mark-consumed pos (add1 pos))
			(get-character pos))])
		 (let loop ()
		   (semaphore-wait potential-sexps-protect)
		   (cond
		     [(not (null? potential-sexps))
		      (let ([first-sexp (car potential-sexps)])
			(set! potential-sexps null)
			(semaphore-post potential-sexps-protect)
			(found-char (car first-sexp)))]
		     [(< prompt-position (last-position))
		      (semaphore-post potential-sexps-protect)
		      (found-char prompt-position)]
		     [else 
		      (semaphore-post potential-sexps-protect)
		      (wx:yield wait-for-sexp)
		      (loop)]))))]
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
	(make-transparent-io-edit% (make-console-edit%
				    mred:edit:searching-edit%)))


    (define make-console-frame%
      (lambda (super%)
	(class super% ([close-item? #f]
		       [insert-welcome? #t]
		       [show? #t])
	  (inherit active-edit get-edit get-canvas show make-menu)
	  (rename [super-on-close on-close])
	  (private 
	    edit-offset 
	    other-offset)
	  (public
	    [get-canvas% (lambda () mred:canvas:wide-snip-canvas%)]
	    [get-edit% (lambda () console-edit%)])
	  (public 
	    [on-close 
	     (lambda ()
	       (and (super-on-close)
		    (mred:exit:exit)))]
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
