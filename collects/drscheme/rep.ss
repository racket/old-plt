
(define drscheme:rep@
  (unit/sig drscheme:rep^
    (import [mred : mred^]
	    [mzlib : mzlib:core^]
	    [print-convert : mzlib:print-convert^]
	    [params : plt:parameters^]
	    [aries : plt:aries^]
	    [zodiac : zodiac:system^]
	    [zodiac:interface : zodiac:interface^]
	    [drscheme:app : drscheme:app^]
	    [drscheme:basis : drscheme:basis^])

    (mred:debug:printf 'invoke "drscheme:spawn@")

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
		    (mred:message-box (format "~a" exn) "Uncaught Exception")
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
		(error-display-handler
		  (lambda (msg)
		    (display msg)
		    (newline)
		    (mred:message-box (format "Internal Error: ~a" msg)
				      "Internal Error!")))
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
	    [get-prompt (lambda () "|-")]
	    [takeover void]
	    [get-escape (lambda () escape-fn)]
	    [set-escape (lambda (x) (set! escape-fn x))]
	    
	    [param #f]
	    [current-thread-desc #f]
	    [break (lambda () 
		     (when current-thread-desc
		       (break-thread current-thread-desc)))]
	    [user-eval
	     (lambda (expr)
	       (with-parameterization param
		 (lambda ()
		   (parameterize ([current-output-port this-out]
				  [current-error-port this-err]
				  [current-input-port this-in]
				  [current-load do-load]
				  [mzlib:pretty-print@:pretty-print-size-hook size-hook]
				  [mzlib:pretty-print@:pretty-print-print-hook print-hook])
				 (eval expr)))))]
	    [send-scheme
	     (let ([s (make-semaphore 1)])
	       (opt-lambda (expr [callback void])
		 (let* ([user-code
			 (lambda ()
			   '(begin (printf "sending scheme:~n")
				   (pretty-print expr))
			   (call-with-values
			    (lambda () (user-eval expr))
			    (lambda anss
			      (for-each
			       (lambda (ans)
				 (unless (void? ans)
				   (parameterize ([mzlib:pretty-print@:pretty-print-size-hook size-hook]
						  [mzlib:pretty-print@:pretty-print-print-hook print-hook])
				     (mzlib:pretty-print@:pretty-print
				      (print-convert:print-convert ans)
				      this-result))))
			       anss))))])
		   (set! current-thread-desc
			 (thread
			  (lambda ()
			    (let ([user-code-error? #t])
			      (mzlib:function@:dynamic-wind/protect-break
			       (lambda () 
				 (semaphore-wait s)
				 (set! current-thread-desc (current-thread)))
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
				 (callback user-code-error?))))))))))]
	    [do-many-buffer-evals
	     (lambda (edit start end pre post)
	       (let ([pre (lambda ()
			    (wx:begin-busy-cursor)
			    (pre))]
		     [post (lambda () 
			     (wx:end-busy-cursor)
			     (post))]
		     [get-sexps
		      (lambda ()
			(call-with-values 
			 (lambda ()
			   (aries:transform 
			    (mred:read-snips/chars-from-buffer edit start end)
			    start edit))
			 (lambda e e)))])
		 (do-many-evals get-sexps pre post)))]
	    [do-many-evals
	     (lambda (get-sexps pre post)
	       (let ([post-done? #f])
		 (let/ec k
		   (dynamic-wind
		    (lambda ()
		      (set-escape (lambda () (k #f)))
		      (pre))
		    (lambda ()
		      (let ([sexps (get-sexps)])
			(set! post-done? #t)
			(set-escape #f)
			(let loop ([exprs sexps])
			  (cond
			   [(null? exprs) (post)]
			   [else (send-scheme (car exprs)
					      (lambda (error?)
						(if error?
						    (post)
						    (loop (cdr exprs)))))]))))
		    (lambda ()
		      (unless post-done?
			(set-escape #f)
			(post)))))))]
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
	       (mred:local-busy-cursor
		(get-canvas)
		(lambda ()
		  (do-many-buffer-evals this start end
		   (lambda () (do-pre-eval))
		   (lambda () (do-post-eval))))))])
	  (public
	    [reset-console
	     (lambda ()
	       (set! param (build-parameterization))
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
		     (insert-delta (format ", version ~a.~nLanguage: " (mred:version))
				   delta)
		     (insert-delta 
		      (format "~a Scheme"
			      (list-ref
			       drscheme:basis:level-strings
			       (drscheme:basis:level->number
				(mred:get-preference
				 'drscheme:scheme-level))))
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
