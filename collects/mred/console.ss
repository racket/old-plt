(define mred:console@
  (unit/sig mred:console^
    (import [mred:debug : mred:debug^] 
	    [mred:preferences : mred:preferences^]
	    [mred:edit : mred:edit^]
	    [mred:frame : mred:frame^]
	    [mred:find-string : mred:find-string^]
	    [mred:exit : mred:exit^]
	    [mred:finder : mred:finder^]
	    [mred:handler : mred:handler^]
	    [mred:gui-utils : mred:gui-utils^]
	    [mred:scheme-mode : mred:scheme-mode^]
	    [mred:scheme-paren : mred:scheme-paren^]
	    [mzlib:function : mzlib:function^]
	    [mzlib:string : mzlib:string^]
	    [mzlib:pretty-print : mzlib:pretty-print^]
	    [mzlib:trigger : mzlib:trigger^])
	    
    (mred:debug:printf 'invoke "mred:console@")

    (define newline-string (string #\newline))
    (define newline-indent-string (string-append (string #\newline) "   "))

    (define welcome-message
      (string-append
       "Welcome to MrEd version "
       (version)
       ", Copyright (c) 1995-96 PLT, Rice University."
       newline-string
       "Based on the following: "
       newline-indent-string
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
       newline-string
       "See the license agreement or"
       " http://www.cs.rice.edu/CS/PLT/packages/mred/ for more info."))

    (define make-scheme-mode-edit%
      (lambda (super%)
	(class super% args
	       (inherit set-mode)
	       (sequence
		 (mred:debug:printf 'super-init "before scheme-mode-edit%")
		 (apply super-init args)
		 (mred:debug:printf 'super-init "before scheme-mode-edit%")
		 (set-mode (make-object mred:scheme-mode:scheme-mode%))))))

    (define scheme-mode-edit% (make-scheme-mode-edit% mred:edit:edit%))

    (define console-max-save-previous-exprs 30)

    (define make-console-edit%
      (lambda (super%)
	(class super% args
	       (inherit begin-edit-sequence
			end-edit-sequence
			set-position
			insert
			delete
			change-style
			last-position
			get-start-position
			get-end-position
			get-text
			get-character
			find-string
			erase
			set-mode
			get-canvas
			get-style-list)
	       (rename [super-on-local-char on-local-char]
		       [super-after-insert after-insert]
		       [super-after-delete after-delete])
	       (private old-stdout
			old-stderr
			old-stdin)

	       (public
		[CACHE-TIME 1] 
		[CACHE-WRITE-COUNT 100]
		
		[normal-font wx:const-modern]
		[normal-delta '()]
		[display-delta (make-object wx:style-delta%
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
	    (let ([mult (send display-delta get-foreground-mult)]
		  [add (send display-delta get-foreground-add)])
	      (send mult set 0 0 0)
	      (send add set 0 0 175)))

	       (rename
		[super-on-insert on-insert]
		[super-on-delete on-delete]
		[super-on-change-style on-change-style])
	       (private
		[resetting? #f]
		[on-something
		 (lambda (super)
		   (lambda (start len)
		     (and (or (not (number? prompt-position))
			      (>= start prompt-position)
			      resetting?)
			  ((super) start len))))])
	       (public
		[on-insert (on-something (lambda () super-on-insert))]
		[on-delete (on-something (lambda () super-on-delete))]
		[on-change-style (on-something (lambda () super-on-change-style))])
	       
	       (private
		[last-str (lambda (l)
			    (if (null? (cdr l))
				(car l)
				(last-str (cdr l))))]
		[timer-on #f]
		[timer-sema (make-semaphore 1)]
		[timer-writes 0]
		[generic-write
		 (lambda (s style-func)
		   (semaphore-wait timer-sema)
		   (if timer-on
		       (begin
			 (set! timer-writes (add1 timer-writes))
			 (when (> timer-writes CACHE-WRITE-COUNT)
			   (end-edit-sequence)
			   (begin-edit-sequence)
			   (set! timer-writes 0)))
		       (begin
			 (set! timer-writes 0)
			 (let ([on-box (box #t)])
			   (begin-edit-sequence)
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
		   (mzlib:function:dynamic-disable-break
		    (lambda ()
		      (begin-edit-sequence)
		      (set-position (last-position))
		      (if prompt-mode?
			  (insert #\newline))
		      (let ((start (last-position)))
			(insert s)
			(let ((end (last-position)))
			  (change-style () start end)
			  (style-func start end)))
		      (end-edit-sequence)
		      (set! prompt-mode? #f))))]
		[generic-close (lambda () '())])
	       
	       (public
		[prompt-mode? #f]
		[get-prompt (lambda () "> ")]
		[prompt-position 0]
		[find-prompt 
		 (lambda (pos) 
		   (if (> pos prompt-position)
		       prompt-position
		       0))]
		[auto-set-wrap? #t]
		[auto-save? #f]
		[autoprompting? #f]
		[enable-autoprompt
		 (opt-lambda ([v #t])
		   (set! autoprompting? v))]
		[this-out-write
		 (lambda (s)
		   (generic-write s
				  (lambda (start end)
				    (change-style display-delta 
						  start end))))]
		[this-err-write
		 (lambda (s)
		   (generic-write s
				  (lambda (start end)
				    (change-style error-delta start end))))])
	       
	       (private
		[read-queue ()]
		[read-sema (make-semaphore 1)]
		[read-trigger (mzlib:trigger:make-trigger)]
		[this-in-read
		 (lambda ()
		   (let ([trigger
			  (begin
			    (semaphore-wait read-sema)
			    (begin0
			     (if (null? read-queue)
				 read-trigger
				 #f)
			     (semaphore-post read-sema)))])
		     (if trigger
			 (let ([no-more-events (mzlib:trigger:make-trigger)])
			   (thread
			    (lambda ()
			      (let loop ()
				(unless (mzlib:trigger:trigger-hit? no-more-events)
				  (wx:yield)
				  (loop)))))
			   (set! read-waiting? #t)
			   (set! prompt-mode? #t)
			   (set! prompt-position (last-position))
			   (mzlib:trigger:trigger-block trigger #t)
			   (set! read-waiting? #f)
			   (mzlib:trigger:trigger-hit no-more-events)))
		     (semaphore-wait read-sema)
		     (begin0
		      (car read-queue)
		      (set! read-queue (cdr read-queue))
		      (semaphore-post read-sema))))]
		[this-in-char-ready?
		 (lambda () (not (null? read-queue)))])
	       
	       (public
		[read-waiting? #f]
		[read-avail
		 (lambda (str)
		   (semaphore-wait read-sema)
		   (set! read-queue (append! read-queue
					     (string->list str)
					     (list #\newline)))
		   (mzlib:trigger:trigger-hit read-trigger)
		   (set! read-trigger (mzlib:trigger:make-trigger))
		   (semaphore-post read-sema))]
		
		[set-display-delta
		 (lambda (delta)
		   (set! display-delta delta))]
		
		[previous-exprs ()]
		[max-save-previous-exprs console-max-save-previous-exprs]
		[previous-expr-pos -1]
		[copy-previous-expr
		 (lambda (which)
		   (let ([str (list-ref previous-exprs which)])
		     (begin-edit-sequence)
		     (if (and autoprompting? (not prompt-mode?))
			 (insert-prompt))
		     (insert str prompt-position (last-position))
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
		
		[do-save-and-eval-or-read-avail
		 (lambda (str)
		   (set! previous-expr-pos -1)
		   (if (null? previous-exprs)
		       (set! previous-exprs (cons str ()))
		       (when (not (string=? str (last-str previous-exprs)))
			 (if (>= (length previous-exprs) max-save-previous-exprs)
			     (set! previous-exprs (cdr previous-exprs)))
			 (let loop ([l previous-exprs])
			   (if (null? (cdr l))
			       (set-cdr! l (cons str ()))
			       (loop (cdr l))))))
		   (if read-waiting?
		       (read-avail str)
		       (do-eval str)))]
		[do-eval
		 (lambda (str)
		   (do-pre-eval)
		   (mred:gui-utils:local-busy-cursor
		    (get-canvas)
		    (lambda ()
		      (eval-and-display str)))
		   (do-post-eval))]
		[do-pre-eval
		 (lambda ()
		   (ready-non-prompt))]
		[do-post-eval
		 (lambda ()
		   (insert-prompt))])
	       
	       (public
		[eval-str mzlib:string:eval-string]
		[pretty-print-out
		 (lambda (v)
		   (mzlib:pretty-print:pretty-print v this-out))]
		[display-result
		 (lambda (v)
		   (cond
		    [(void? v) v]
		    [(and (is-a? v wx:snip%)
			  (not (send v is-owned?)))
		     (insert v)]
		    [else (pretty-print-out v)]))]
		[eval-and-display
		 (lambda (str)
		   (catch-errors
		    #f
		    (lambda () #f)
		    (dynamic-enable-break
		     (lambda ()
		       (let-values ([v (eval-str str)])
			 (map display-result v))))))])
	       
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
		   (let ((code (send key get-key-code)))
		     (if (or (= code 13) (= code 10))
			 (let ((start (get-start-position))
			       (end (get-end-position))
			       (last (last-position)))
			   (if (and (< start end) (< end prompt-position)
				    (or read-waiting? (not (eval-busy?))))
			       (let ((str (get-text start end)))
				 (begin-edit-sequence)
				 (if (not prompt-mode?)
				     (insert-prompt))
				 (insert str prompt-position (last-position))
				 (set-position (last-position))
				 (end-edit-sequence))
			       (if (and (= start last) (not prompt-mode?)
					autoprompting? (not (eval-busy?)))
				   (insert-prompt)
				   (if (and (< prompt-position start)
					    (only-spaces-after start)
					    (or read-waiting?
						(not (eval-busy?))))
				       (let ([balanced? (mred:scheme-paren:scheme-balanced?
							 this
							 prompt-position
							 last)])
					 (if balanced?
					     (let ([str (get-text prompt-position 
								  start)])
					       (delete start last)
					       (do-save-and-eval-or-read-avail str))
					     (super-on-local-char key)))
				       (if (< start prompt-position)
					   (let ([match
						  (mred:scheme-paren:scheme-backward-match
						   this start 0)])
					     (if match
						 (begin
						   (insert (get-text match start)
							   prompt-position
							   (last-position))
						   (set-position (last-position)))
						 (super-on-local-char key)))
					   (super-on-local-char key))))))
			 (super-on-local-char key))))]
		
		[takeover-output
		 (lambda ()
		   (if (void? old-stdout)
		       (begin
			 (set! old-stdout (current-output-port))
			 (set! old-stderr (current-error-port))
			 (set! old-stdin (current-input-port))
			 (current-output-port this-out)
			 (current-error-port this-err)
			 (current-input-port this-in))))]
		[release-output
		 (lambda ()
		   (if (not (void? old-stdout))
		       (begin
			 (current-output-port old-stdout)
			 (current-error-port old-stderr)
			 (current-input-port old-stdin)
			 (set! old-stdout (void))
			 (set! old-stderr (void))
			 (set! old-stdin (void)))))]
		
		[insert-prompt
		 (lambda ()
		   (set! prompt-mode? #t)
		   (let* ((last (last-position))
			  (last-str (get-text (- last 1) last)))
		     (begin-edit-sequence)
		     (if (not (string=? last-str newline-string))
			 (insert #\newline last))
		     (let ([last (last-position)])
		       (insert (get-prompt) last)
		       (change-style normal-delta last (last-position)))
		     (set! prompt-position (last-position))
		     (end-edit-sequence)
		     (semaphore-wait timer-sema)
		     (when timer-on
		       (end-edit-sequence)
		       (set-box! timer-on #f)
		       (set! timer-on #f))
		     (semaphore-post timer-sema)))]
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
		   (super-after-delete start len))]
		[reset-console
		 (lambda ()
		   (set! resetting? #t)
		   (erase)
		   (set! resetting? #f))]
		
		[ready-non-prompt
		 (lambda ()
		   (when prompt-mode?
		     (set! prompt-mode? #f)
		     (insert #\newline (last-position))))]
		
		[initialize-console
		 (lambda ()
		   #t)]
		[make-this-out
		 (lambda ()
		   (make-output-port this-out-write generic-close))]
		[make-this-err
		 (lambda ()
		   (make-output-port this-err-write generic-close))])
	       (sequence
		 (mred:debug:printf 'super-init "before console-edit%")
		 (apply super-init args)
		 (mred:debug:printf 'super-init "after console-edit%")
		 (set-mode (make-object mred:scheme-mode:scheme-interaction-mode%)))
	       (public
		[this-in (make-input-port this-in-read
					  this-in-char-ready?
					  generic-close)]
		[this-out (make-this-out)]
		[this-err (make-this-err)]))))

    (define console-edit% (make-console-edit% mred:edit:edit%))

    (define make-console-frame%
      (lambda (super%)
	(class super% ([close-item? #f]
		       [mssg welcome-message]
		       [show? #t])
	  (inherit active-edit edit canvas show make-menu)
	  (rename [super-on-close on-close])
	  (private 
	    edit-offset 
	    other-offset)
	  (public
	    [edit% console-edit%])
	  (public 
	    [on-close 
	     (lambda ()
	       (super-on-close)
	       (mred:exit:exit))]
	    [next-menu-id (lambda () other-offset)]
	    [load-file
	     (lambda (file)
	       (load file))]
	    [on-quit mred:exit:exit]

	    [file-menu:revert #f]
	    [file-menu:close
	     (if close-item?
		 (lambda ()
		   (when (on-close)
		     (send (active-edit) release-output)
		     (show #f)))
		 #f)]
	    [file-menu:between-open-and-save
	     (lambda (file-menu)
	       (send file-menu append-item "&Load Scheme File..."
		     (lambda ()
		       (let ((file (mred:finder:get-file)))
			 (if file
			     (load-file file)))))
	       (send file-menu append-separator))]
	    [file-menu:save (lambda ()
			      (send (active-edit) save-file
				    (send (active-edit) get-filename)))]
	    [file-menu:save-as (lambda () (send (active-edit) save-file ""))]
	    [file-menu:print (lambda () (send (active-edit) print '()))])

	  (private
	    [edit-menu:do  (lambda (const) 
				    (lambda () 
				      (send (active-edit) do-edit const)))])
	  (public
	    [edit-menu:undo (edit-menu:do wx:const-edit-undo)]
	    [edit-menu:redo (edit-menu:do wx:const-edit-redo)]
	    [edit-menu:cut (edit-menu:do wx:const-edit-cut)]
	    [edit-menu:copy (edit-menu:do wx:const-edit-copy)]
	    [edit-menu:paste (edit-menu:do wx:const-edit-paste)]
	    [edit-menu:clear (edit-menu:do wx:const-edit-clear)]
	    [edit-menu:select-all
	     (lambda ()
	       (send (active-edit) set-position
		     0 (send (active-edit) last-position)))]
	    [edit-menu:between-find-and-preferences
	     (let ()
	       (lambda (edit-menu)
		 (send edit-menu append-separator)
		 (send edit-menu append-item "Insert Text Box"
		       (edit-menu:do wx:const-edit-insert-text-box))
		 (send edit-menu append-item "Insert Graphic Box"
		       (edit-menu:do wx:const-edit-insert-graphic-box))
		 (send edit-menu append-item "Insert Image..."
		       (edit-menu:do wx:const-edit-insert-image))
		 (send edit-menu append-separator)))]

	    [frame-title "MrEdConsole"])
	  
	  (sequence
	    (mred:debug:printf 'super-init "before console-frame%")
	    (super-init frame-title)
	    (mred:debug:printf 'super-init "after console-frame%")
	    (send edit set-file-format wx:const-media-ff-std)
	    
	    ; Welcome message and initial prompt:
	    (when mssg
	      (send edit insert mssg)
	      
	      (mred:gui-utils:local-busy-cursor 
	       canvas
	       (lambda ()
		 (let ([last (send edit last-position)]
		       [delta (make-object wx:style-delta%
					   wx:const-change-family
					   wx:const-decorative)])
		   (send delta set-delta wx:const-change-size 10)
		   (send edit insert #\newline)
		   (send edit change-style delta 0 last)
		   
		   (let ([dd (ivar edit display-delta)])
		     (dynamic-wind
		      (lambda ()
			(send edit set-display-delta delta))
		      (lambda ()
			(send edit initialize-console))
		      (lambda ()
			(send edit set-display-delta dd)))))))
	      
	      (send edit enable-autoprompt)
	      (send edit takeover-output)
	      (send edit insert-prompt)
	      (send edit clear-undos)
	      (when show?
		(show #t)))))))

    (define console-frame% (make-console-frame%
			    (mred:find-string:make-searchable-frame%
			     mred:frame:simple-menu-frame%)))))
