;; replace zodiac-exn? with standard mzscheme syntax error exceptions
;;       (is that already happening somehow?)

(unit/sig userspace:basis^
  (import [zodiac : drscheme:zodiac^]
	  [zodiac:interface : drscheme:interface^]
	  [aries : plt:aries^]
	  [mzlib:print-convert : mzlib:print-convert^]
	  [mzlib:pretty-print : mzlib:pretty-print^]
	  [mzlib:function : mzlib:function^])

  (mred:debug:printf 'invoke "userspace:basis@")

  (define (report-error . x) (error 'report-error))
  (define (report-unlocated-error . x) (error 'report-unlocated-error))

  (define system-parameterization (current-parameterization))
  ;; DEBUGGING!
  (error-escape-handler 
   (let ([o (error-escape-handler)])
     (rec system-parameterization-error-escape-handler
	  (lambda x (apply o x)))))

  (define eq?-only-compares-symbols (make-parameter #f))
  (define r4rs-style-printing (make-parameter #f))
  
  (define-struct/parse setting (use-zodiac?
				vocabulary-symbol
				case-sensitive?
				allow-set!-on-undefined?
				unmatched-cond/case-is-error?
				allow-improper-lists?
				sharing-printing?
				abbreviate-cons-as-list?
				signal-undefined
				signal-not-boolean
				eq?-only-compares-symbols?
				disallow-untagged-inexact-numbers
				whole/fractional-exact-numbers
				printing))
  
  ;; settings : (list-of (list symbol setting))
  (define settings
    (list (list 'Beginner (make-setting/parse
			   `((use-zodiac? #t)
			     (vocabulary-symbol core)
			     (case-sensitive? #t)
			     (allow-set!-on-undefined? #f)
			     (unmatched-cond/case-is-error? #t)
			     (allow-improper-lists? #f)
			     (sharing-printing? #f)
			     (abbreviate-cons-as-list? #f)
			     (signal-undefined #t)
			     (signal-not-boolean #t)
			     (eq?-only-compares-symbols? #t)
			     (disallow-untagged-inexact-numbers #t)
			     (whole/fractional-exact-numbers #f)
			     (printing constructor-style))))
	  (list 'Intermediate (make-setting/parse
			       `((use-zodiac? #t)
				 (vocabulary-symbol structured)
				 (case-sensitive? #t)
				 (allow-set!-on-undefined? #f)
				 (unmatched-cond/case-is-error? #t)
				 (allow-improper-lists? #f)
				 (sharing-printing? #f)
				 (abbreviate-cons-as-list? #t)
				 (signal-undefined #t)
				 (signal-not-boolean #t)
				 (eq?-only-compares-symbols? #t)
				 (disallow-untagged-inexact-numbers #t)
				 (whole/fractional-exact-numbers #f)
				 (printing constructor-style))))
	  (list 'Advanced (make-setting/parse
			   `((use-zodiac? #t)
			     (vocabulary-symbol side-effecting)
			     (case-sensitive? #t)
			     (allow-set!-on-undefined? #f)
			     (unmatched-cond/case-is-error? #t)
			     (allow-improper-lists? #f)
			     (sharing-printing? #t)
			     (abbreviate-cons-as-list? #t)
			     (signal-undefined #t)
			     (signal-not-boolean #f)
			     (eq?-only-compares-symbols? #f)
			     (disallow-untagged-inexact-numbers #f)
			     (whole/fractional-exact-numbers #f)
			     (printing constructor-style))))
	  (list 'Quasi-R4RS (make-setting/parse
			     `((use-zodiac? #t)
			       (vocabulary-symbol advanced)
			       (case-sensitive? #t)
			       (allow-set!-on-undefined? #f)
			       (unmatched-cond/case-is-error? #t)
			       (allow-improper-lists? #t)
			       (sharing-printing? #f)
			       (abbreviate-cons-as-list? #t)
			       (signal-undefined #t)
			       (signal-not-boolean #t)
			       (eq?-only-compares-symbols? #f)
			       (disallow-untagged-inexact-numbers #f)
			       (whole/fractional-exact-numbers #f)
			       (printing r4rs-style))))
	  (list 'Raw (make-setting/parse
			     `((use-zodiac? #f)
			       (vocabulary-symbol raw)
			       (case-sensitive? #t)
			       (allow-set!-on-undefined? #f)
			       (unmatched-cond/case-is-error? #t)
			       (allow-improper-lists? #t)
			       (sharing-printing? #t)
			       (abbreviate-cons-as-list? #t)
			       (signal-undefined #t)
			       (signal-not-boolean #f)
			       (eq?-only-compares-symbols? #f)
			       (disallow-untagged-inexact-numbers #f)
			       (whole/fractional-exact-numbers #f)
			       (printing r4rs-style))))))

  ;; level->number : symbol -> int
  (define level->number
    (lambda (x)
      (case x
	[(core) 0]
	[(structured) 1]
	[(side-effecting) 2]
	[(advanced) 3]
	[(raw) 4]
	[else (error 'level->number "unexpected level: ~a" x)])))

  ;; level-symbols (list-of sym)
  (define level-symbols (list 'core 'structured 'side-effecting 'advanced 'raw))

  ;; level-strings : (list-of string)
  (define level-strings (list "Beginner" "Intermediate" "Advanced" "Quasi-R4RS" "Raw"))

  ;; find-setting-name : setting -> symbol
  (define (find-setting-name setting)
    (or (ormap (lambda (x)
		 (if (equal? (mzlib:function:second x) setting)
		     (mzlib:function:first x)
		     #f))
	       settings)
	'Custom))

  ;; copy-setting : setting -> setting
  (define copy-setting
    (lambda (x)
      (apply make-setting (cdr (vector->list (struct->vector x))))))
  
  ;; get-default-setting : _ -> setting
  (define (get-default-setting)
    (copy-setting (mzlib:function:second (car (reverse settings)))))

  ;; r4rs-style-printing? : setting -> boolean
  (define (r4rs-style-printing? setting)
    (eq? 'r4rs-style (setting-printing setting)))

  ;; current-setting : (parameter (+ #f setting))
  (define current-setting
    (make-parameter
     #f
     (lambda (x)
       (if (or (not x)
	       (setting? x))
	   x
	   (error 'current-setting
		  "must be a setting or #f")))))
				 
  (define-struct process-finish (error?))

  ;; process-file/zodiac : setting
  ;;                       zodiac:vocabulary
  ;;                       string
  ;;                       (((+ sexp zodiac:parsed) ( -> void) -> void)
  ;;                       boolean
  ;;                    -> void
  ;; note: the boolean controls which variant of the union the 3rd arg is passed.
  (define (process-file/zodiac setting vocab filename f annotate?)
    (let ([port (open-input-file filename)])
      (process/zodiac
       setting
       vocab
       (parameterize ([read-case-sensitive (setting-case-sensitive? setting)])
	 (zodiac:read port
		      (zodiac:make-location 0 0 0 filename)
		      #t 1))
       (lambda (x r)
	 (when (process-finish? x)
	   (close-input-port port))
	 (f x r))
       annotate?)))

  ;; process-file/no-zodiac : setting
  ;;                          string
  ;;                          (sexp ( -> void) -> void)
  ;;                       -> void
  (define (process-file/no-zodiac setting filename f)
    (call-with-input-file filename
      (lambda (port)
	(process/no-zodiac setting (lambda () (read port)) f))))

  ;; process-sexp/no-zodiac : setting
  ;;                          sexp
  ;;                          (sexp ( -> void) -> void)
  ;;                       -> void
  (define (process-sexp/no-zodiac setting sexp f)
    (process/no-zodiac setting
		       (let ([first? #t]) 
			 (lambda ()
			   (if first?
			       (begin (set! first? #f)
				      sexp)
			       eof)))
		       f))

  ;; process-sexp/zodiac : setting
  ;;                       vocabulary
  ;;                       sexp
  ;;                       zodiac:sexp
  ;;                       (sexp ( -> void) -> void)
  ;;                    -> void
  (define (process-sexp/zodiac setting vocab sexp z f annotate?)
    (let ([reader
	   (let ([gone #f])
	     (lambda ()
	       (or gone
		   (begin (set! gone (zodiac:make-eof z))
			  (zodiac:structurize-syntax sexp z)))))])
      (process/zodiac setting vocab reader f annotate?)))

  ;; process/zodiac : setting
  ;;                  zodiac:vocabulary
  ;;                  ( -> zodiac:sexp)
  ;;                  ((TST + process-finish) ( -> void) -> void)
  ;;                  boolean
  ;;               -> void
  (define (process/zodiac setting vocab reader f annotate?)
    (let ([cleanup
	   (lambda (error?)
	     (f (make-process-finish error?) void))])
      (let loop ()
	(let ([next-iteration
	       (let/ec k
		 (let ([zodiac-read
			(parameterize ([read-case-sensitive (setting-case-sensitive? setting)])
			  (dynamic-wind
			   (lambda () (zodiac:interface:set-zodiac-phase 'reader))
			   (lambda () (reader))
			   (lambda () (zodiac:interface:set-zodiac-phase #f))))])
		   (if (zodiac:eof? zodiac-read)
		       (lambda () (cleanup #f))
		       (let* ([send-scheme (lambda x (error 'send-scheme))]
			      [wait-on-scheme
			       (lambda (expr)
				 (let-values ([(answers error?) (send-scheme expr)])
				   (if error?
				       (k (lambda () (cleanup #t)))
				       (apply values answers))))]
			      [evaluator
			       (lambda (exp _ macro)
				 (wait-on-scheme (aries:annotate exp)))]
			      [user-macro-body-evaluator
			       (lambda (x . args)
				 (wait-on-scheme `(,x ,@(map (lambda (x) `(#%quote ,x)) args))))]
			      [exp
			       (dynamic-wind
				(lambda ()
				  (zodiac:interface:set-zodiac-phase 'expander))
				(lambda ()
				  (call/nal zodiac:scheme-expand/nal
					    zodiac:scheme-expand
					    [expression: zodiac-read]
					    [vocabulary: vocab]
					    [user-macro-body-evaluator: user-macro-body-evaluator]
					    [elaboration-evaluator: evaluator]))
				(lambda ()
				  (zodiac:interface:set-zodiac-phase #f)))]
			      [heading-out (if annotate? 
					       (aries:annotate exp)
					       exp)])
			 (lambda () (f heading-out loop))))))])
	  (next-iteration)))))

  ;; process/no-zodiac : setting ( -> sexp) ((TST + process-finish) ( -> void) -> void) -> void
  (define (process/no-zodiac setting reader f)
    (let loop ()
      (let ([expr (reader)])
	(if (eof-object? expr)
	    (f (make-process-finish #f) void)
	    (f expr loop)))))

  ;; (parameter (string debug-info -> void))
  (define error-display/debug-handler
    (make-parameter
     (lambda (msg debug)
       ((error-display-handler) msg))))

  ;; drscheme-exception-handler : exn -> A
  ;; effect: displays the exn-message and escapes
  (define (drscheme-exception-handler exn)
    ((error-display/debug-handler) (exn-message exn)
				   (exn-debug-info exn))
    ((error-escape-handler))
    ((error-display-handler) "Exception handler didn't escape"))

  ;; drscheme-branch-handler :  -> parameterization
  ;; parameters: current-parameterization
  (define (drscheme-branch-handler) (make-parameterization (current-parameterization)))

  ;; drscheme-error-value->string-handler : TST number -> string
  (define (drscheme-error-value->string-handler x n)
    (let ([setting (current-setting)])
      (with-parameterization system-parameterization
	(lambda ()
	  (let* ([port (open-output-string)]
		 [error-value
		  (if (r4rs-style-printing? setting)
		      x
		      (mzlib:print-convert:print-convert x))]
		 [long-string
		  (begin (mzlib:pretty-print:pretty-print error-value
							  port
							  'infinity)
			 (get-output-string port))])
	    (if (<= (string-length long-string) n)
		long-string
		(let ([short-string (substring long-string 0 n)]
		      [trim 3])
		  (unless (<= n trim)
		    (let loop ([i trim])
		      (unless (<= i 0)
			(string-set! short-string (- n i) #\.)
			(loop (sub1 i)))))
		  short-string)))))))

  ;; build-parameterization : setting
  ;;                          (unit (import plt:userspace:params^))
  ;;                       -> parameterization
  (define (build-parameterization setting basis@)
    (let* ([parameterization (make-parameterization)]
	   [custodian (make-custodian)]
	   [n (if (setting-use-zodiac? setting)
		  (make-namespace 'no-constants 'wx 'hash-percent-syntax)
		  (make-namespace 'wx))])
      (with-parameterization parameterization
	(lambda ()
	  (current-setting setting)
	  (parameterization-branch-handler drscheme-branch-handler)
	  (compile-allow-set!-undefined #f)
	  (compile-allow-cond-fallthrough #f)
	  (current-custodian custodian)
	  (require-library-use-compiled #f)
	  (error-value->string-handler drscheme-error-value->string-handler)
	  (debug-info-handler (let ([drs-debug-info
				     (lambda () (unbox aries:error-box))])
				drs-debug-info))
	  (current-exception-handler drscheme-exception-handler)
	  (current-namespace n)
	  (break-enabled #t)
	  (current-will-executor (make-will-executor))
	  (read-curly-brace-as-paren #t)
	  (read-square-bracket-as-paren #t)
	  (print-struct #t)
	  (error-print-width 250)

	  (when (setting-use-zodiac? setting)
	    (zodiac:current-vocabulary (setting-vocabulary-symbol setting)))
	  
	  (read-case-sensitive (setting-case-sensitive? setting))

	  ;; this sets both the zodiac and the cons procedure setting,
	  ;; via a dynamic link in basis.ss
	  (zodiac:allow-improper-lists (or (not (setting-use-zodiac? setting))
					   (setting-allow-improper-lists? setting)))
	  
	  (eq?-only-compares-symbols (setting-eq?-only-compares-symbols? setting))
	  
	  (zodiac:disallow-untagged-inexact-numbers (setting-disallow-untagged-inexact-numbers setting))

	  (aries:signal-undefined (setting-signal-undefined setting))
	  (aries:signal-not-boolean (setting-signal-not-boolean setting))
	  
	  (compile-allow-set!-undefined (setting-allow-set!-on-undefined? setting))
	  (compile-allow-cond-fallthrough (not (setting-unmatched-cond/case-is-error? setting)))
	  
	  ;; need to introduce parameter for constructor-style vs r4 style
	  (case (setting-printing setting)
	    [(constructor-style)
	     (r4rs-style-printing #f)
	     (mzlib:print-convert:constructor-style-printing #t)]
	    [(quasi-style)
	     (r4rs-style-printing #f)
	     (mzlib:print-convert:constructor-style-printing #f)
	     (mzlib:print-convert:quasi-read-style-printing #f)]
	    [(quasi-read-style)
	     (mzlib:print-convert:whole/fractional-exact-numbers #f)
	     (r4rs-style-printing #f)
	     (mzlib:print-convert:constructor-style-printing #f)
	     (mzlib:print-convert:quasi-read-style-printing #t)]
	    [(r4rs-style) (r4rs-style-printing #t)]
	    [else (error 'install-language "found bad setting-printing: ~a~n" 
			 (setting-printing setting))])

	  (mzlib:print-convert:show-sharing (setting-sharing-printing? setting))
	  (mzlib:print-convert:whole/fractional-exact-numbers (setting-whole/fractional-exact-numbers setting))
	  (print-graph (setting-sharing-printing? setting))
	  (mzlib:print-convert:abbreviate-cons-as-list (setting-abbreviate-cons-as-list? setting))))
      (with-parameterization parameterization
	(lambda ()
	  (let ([allow-improper-lists zodiac:allow-improper-lists]
		[eq?-only-compares-symbols eq?-only-compares-symbols])
	    (load (build-path (collection-path "system") "debug.ss"))
	    (unless (setting-use-zodiac? setting)
	      (require-library "corem.ss"))
	    (invoke-open-unit/sig basis@ #f plt:userspace:params^))))
      parameterization)))
