
(define drscheme:rep@
  (unit/sig drscheme:rep^
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [print-convert : mzlib:print-convert^]
	    [aries : plt:aries^]
	    [zodiac : zodiac:system^]
	    [zodiac:interface : zodiac:interface^]
	    [drscheme:language : drscheme:language^]
	    [drscheme:app : drscheme:app^]
	    [drscheme:basis : drscheme:basis^])

    (mred:debug:printf 'invoke "drscheme:spawn@")

    (error-display-handler
     (lambda (msg)
       (display msg)
       (newline)
       (mred:message-box (format "Internal Error: ~a" msg)
			 "Internal Error!")))

    (define build-parameterization
      (let ([orig-eventspace (wx:current-eventspace)]
	    [bottom-eventspace (wx:make-eventspace)])
	(lambda ()
	  (let ([system-parameterization (current-parameterization)]
		[p (make-parameterization)]
		[n (make-namespace 'no-constants
				   'wx
				   'hash-percent-syntax
				   'auto-else
				   'set!-undefined)])
	    (with-parameterization p
	      (lambda ()
		(current-exception-handler
		  (lambda (exn)
		    (when (exn? exn)
		      (let ([report (exn-debug-info exn)])
			(when (and (not (void? report))
				   report
				   (unbox report))
			  (zodiac:interface:dynamic-error
			   (unbox report) "~a" (exn-message exn)))))
		    (mred:message-box (format "~s" exn) "Uncaught Exception")
		    ((error-escape-handler))))  
		(error-value->string-handler
		 (lambda (x n)
		   (let ([long-string 
			  (format "~s" (print-convert:print-convert x))])
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
		(debug-info-handler (lambda () aries:error-box))
		(current-namespace n)
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
		   this-in this-result
		   output-delta set-output-delta
		   do-pre-eval
		   do-post-eval
		   display-result
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
	    [escape-fn #f]
	    [print-hook (lambda (x _)
			  (when prompt-mode?
			    (insert #\newline)
			    (set! prompt-mode? #f))
			  (insert (send x copy) (last-position)))]
	    [size-hook (lambda (x _) (and (is-a? x wx:snip%) 1))])
	  (public
	    [get-prompt (lambda () "|- ")]
	    [takeover void]
	    [get-escape (lambda () escape-fn)]
	    [set-escape (lambda (x) (set! escape-fn x))]
	    
	    [param #f]
	    [current-thread-desc #f]
	    [break (lambda () 
		     (when current-thread-desc
		       (break-thread current-thread-desc)))]
	    [userspace-eval
	     (let ([system-parameterization (current-parameterization)])
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
		       (user-eval annotated))))))]
	    [user-eval
	     (let* ([primitive-eval (current-eval)])
	       (lambda (expr)
		 (let ([ans (with-parameterization param
			      (lambda ()
				(primitive-eval expr)))])
		   ans)))]
	    [send-scheme
	     (let ([s (make-semaphore 1)])
	       (opt-lambda (get-expr [before void] [after void])
		 (let* ([print-style (drscheme:language:setting-printing
				      (mred:get-preference 'drscheme:settings))]
			[user-code
			 (lambda ()
			   '(begin (printf "sending scheme:~n")
				   (pretty-print (get-expr)))
			   (call-with-values
			    (lambda () (user-eval (get-expr)))
			    (lambda anss
			      (for-each
			       (lambda (ans)
				 (unless (void? ans)
				   (parameterize ([mzlib:pretty-print@:pretty-print-size-hook size-hook]
						  [mzlib:pretty-print@:pretty-print-print-hook print-hook])
				     (mzlib:pretty-print@:pretty-print
				      (if (eq? print-style 'r4rs-style)
					  ans
					  (print-convert:print-convert ans))
				      this-result))))
			       anss))))])
		   (set! current-thread-desc
			 (thread
			  (lambda ()
			    (let ([user-code-error? #t])
			      (mzlib:function@:dynamic-wind/protect-break
			       (lambda () 
				 (semaphore-wait s)
				 (set! current-thread-desc (current-thread))
				 (before))
			       (lambda ()
				 (set! user-code-error? (let/ec k 
							  (mzlib:function@:dynamic-disable-break
							   (lambda ()
							     (set-escape (lambda () (k #t)))))
							  (user-code)
							  #f)))
			       (lambda () 
				 (set! current-thread-desc #f)
				 (set-escape #f)
				 (semaphore-post s)
				 (after user-code-error?))))))))))]
	    [do-many-buffer-evals
	     (lambda (edit start end pre post)
	       (let ([pre (lambda ()
			    (wx:begin-busy-cursor)
			    (pre))]
		     [post (lambda () 
			     (wx:end-busy-cursor)
			     (post))]
		     [get-zodiac-code
		      (lambda ()
			(let* ([structurized (send edit get-zodiac-sexp)]
			       [expanded (zodiac:scheme-expand structurized param)]
			       [annotated (aries:annotate expanded)])
			  annotated))])
		 (do-many-evals get-zodiac-code pre post)))]
	    [do-many-evals
	     (lambda (get-sexp pre post)
		 (let/ec k
		   (let ([new-pre
			  (lambda ()
			    (set-escape (lambda () (k #f)))
			    (pre))]
			 [new-post 
			  (lambda (sucessful?)
			    (set-escape #f)
			    (post))])
		     (send-scheme get-sexp new-pre new-post))))]
	    [do-load
	     (lambda (filename)
	       (call-with-input-file filename
		 (lambda (p)
		   (call-with-values
		    (lambda () (aries:transform p 0 filename))
		    (lambda e
		      (let loop ([sexps e])
			(cond
			 [(null? sexps) (void)]
			 [(null? (cdr sexps)) (eval (car sexps))]
			 [else (begin (eval (car sexps))
				      (loop (cdr sexps)))])))))))]
	    [do-eval
	     (lambda (start end)
	       (let ([get-sexp
		      (lambda ()
			(let* ([loc (zodiac:make-location 0 0 start this)]
			       [reader (zodiac:read
					(mred:read-snips/chars-from-buffer
					 this start end)
					loc)]
			       [exprs (let loop ()
					(let ([expr (reader)])
					  (if (zodiac:eof? expr)
					      null
					      (cons expr (loop)))))]
			       [built `(begin ,@exprs)]
			       [_ (mred:debug:printf 
				   'zodiac
				   "zodiac; built: ~a~n" built)]
			       [structurized (zodiac:structurize-syntax
					      built
					      (zodiac:make-zodiac 'rep loc loc))]
			       [_ (mred:debug:printf 
				   'zodiac
				   "zodiac; structurized: ~a~n" structurized)]
			       [expanded (zodiac:scheme-expand structurized param)]
			       [_ (mred:debug:printf 
				   'zodiac
				   "zodiac; expanded: ~a~n" expanded)]
			       [_ (mred:debug:printf 
				   'zodiac
				   "zodiac; unparsed: ~a~n" (zodiac:parsed->raw expanded))]
			       [annotated (aries:annotate expanded)]
			       [_ (mred:debug:printf 
				   'zodiac
				   "zodiac; annotated: ~a~n" annotated)])
			  annotated))])
		 (mred:local-busy-cursor
		  (get-canvas)
		  (lambda ()
		    (do-many-evals
		     get-sexp
		     (lambda () (do-pre-eval))
		     (lambda () (do-post-eval)))))))])
	  (public
	    [reset-console
	     (lambda ()
	       (set! param (let ([p (build-parameterization)])
			     (with-parameterization p
			       (lambda ()
				 (current-output-port this-out)
				 (current-error-port this-err)
				 (current-input-port this-in)
				 (current-load do-load)
				 (current-eval userspace-eval)
				 (mzlib:pretty-print@:pretty-print-size-hook size-hook)
				 (mzlib:pretty-print@:pretty-print-print-hook print-hook)))
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
		     (insert-delta "." delta)
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
      (make-edit% mred:console-edit%))))
