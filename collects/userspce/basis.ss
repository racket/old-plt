;; replace zodiac-exn? with standard mzscheme syntax error exceptions
;;       (is that already happening somehow?)

(unit/sig userspace:basis^
  (import [import : userspace:basis-import^]
	  [zodiac : drscheme:zodiac^]
	  [zodiac:interface : drscheme:interface^]
	  [aries : plt:aries^]
	  [mzlib:print-convert : mzlib:print-convert^]
	  [mzlib:pretty-print : mzlib:pretty-print^]
	  [mzlib:function : mzlib:function^])

  (define INITIAL-LINE 1)
  (define INITIAL-COLUMN 1)
  (define INITIAL-OFFSET 0)

  (define original-output-port (current-output-port))
  (define (printf . args)
    (apply fprintf original-output-port args))

  (define (report-error . x) (error 'report-error))
  (define (report-unlocated-error . x) (error 'report-unlocated-error))

  (define system-parameterization (current-parameterization))
  (define primitive-load (current-load))
  (define primitive-eval (current-eval))

  (define <=-at-least-two-args (make-parameter #f))
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
				<=-at-least-two-args
				disallow-untagged-inexact-numbers
				print-tagged-inexact-numbers
				whole/fractional-exact-numbers
				printing))

  (define-values (raw-name raw-vocab-symbol)
    (if import:in-mzscheme?
	(values 'MzScheme 'mzscheme)
	(values 'MrEd 'mred)))
  
  ;; settings : (list-of (list symbol setting))
  (define settings
    (list (vector 'Beginner (make-setting/parse
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
			       (<=-at-least-two-args #t)
			       (disallow-untagged-inexact-numbers #f)
			       (print-tagged-inexact-numbers #t)
			       (whole/fractional-exact-numbers #f)
			       (printing constructor-style))))
	  (vector 'Intermediate (make-setting/parse
				 `((use-zodiac? #t)
				   (vocabulary-symbol structured)
				   (case-sensitive? #t)
				   (allow-set!-on-undefined? #f)
				   (unmatched-cond/case-is-error? #t)
				   (allow-improper-lists? #f)
				   (sharing-printing? #f)
				   (abbreviate-cons-as-list? #f)
				   (signal-undefined #t)
				   (signal-not-boolean #t)
				   (eq?-only-compares-symbols? #t)
				   (<=-at-least-two-args #t)
				   (disallow-untagged-inexact-numbers #f)
				   (print-tagged-inexact-numbers #t)
				   (whole/fractional-exact-numbers #f)
				   (printing constructor-style))))
	  (vector 'Advanced (make-setting/parse
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
			       (<=-at-least-two-args #t)
			       (disallow-untagged-inexact-numbers #f)
			       (print-tagged-inexact-numbers #f)
			       (whole/fractional-exact-numbers #f)
			       (printing constructor-style))))
	  (vector 'R4RS+ (make-setting/parse
			  `((use-zodiac? #t)
			    (vocabulary-symbol advanced)
			    (case-sensitive? #t)
			    (allow-set!-on-undefined? #f)
			    (unmatched-cond/case-is-error? #t)
			    (allow-improper-lists? #t)
			    (sharing-printing? #f)
			    (abbreviate-cons-as-list? #t)
			    (signal-undefined #t)
			    (signal-not-boolean #f)
			    (eq?-only-compares-symbols? #f)
			    (<=-at-least-two-args #f)
			    (disallow-untagged-inexact-numbers #f)
			    (print-tagged-inexact-numbers #f)
			    (whole/fractional-exact-numbers #f)
			    (printing r4rs-style))))
	  (vector raw-name (make-setting/parse
			    `((use-zodiac? #f)
			      (vocabulary-symbol ,raw-vocab-symbol)
			      (case-sensitive? #t)
			      (allow-set!-on-undefined? #f)
			      (unmatched-cond/case-is-error? #t)
			      (allow-improper-lists? #t)
			      (sharing-printing? #t)
			      (abbreviate-cons-as-list? #t)
			      (signal-undefined #t)
			      (signal-not-boolean #f)
			      (eq?-only-compares-symbols? #f)
			      (<=-at-least-two-args #f)
			      (disallow-untagged-inexact-numbers #f)
			      (print-tagged-inexact-numbers #f)
			      (whole/fractional-exact-numbers #f)
			      (printing r4rs-style))))))

  ;; level->number : symbol -> int
  (define level->number
    (lambda (x)
      (evcase x
	['core 0]
	['structured 1]
	['side-effecting 2]
	['advanced 3]
	[raw-vocab-symbol 4]
	[else (error 'level->number "unexpected level: ~a" x)])))

  ;; level-symbols (list-of sym)
  (define level-symbols (list 'core 'structured 'side-effecting 'advanced raw-vocab-symbol))

  ;; level-strings : (list-of string)
  (define level-strings (list "Beginner" "Intermediate" "Advanced" "R4RS+" (symbol->string raw-name)))

  ;; find-setting-name : setting -> symbol
  (define (find-setting-name setting)
    (or (ormap (lambda (x)
		 (if (equal? (vector-ref x 1) setting)
		     (vector-ref x 0)
		     #f))
	       settings)
	'Custom))

  ;; copy-setting : setting -> setting
  (define copy-setting
    (lambda (x)
      (apply make-setting (cdr (vector->list (struct->vector x))))))
  
  ;; get-default-setting : _ -> setting
  (define (get-default-setting) (copy-setting (vector-ref (mzlib:function:second (reverse settings)) 1)))

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
				 
  ;; current-vocabulary : (parameter (+ #f zodiac:vocabulary))
  (define current-vocabulary (make-parameter #f))

  ;; syntax-checking-primitive-eval : sexp -> value
  ;; effect: raises user-exn if expression ill-formed
  (define (syntax-checking-primitive-eval expr)
    (primitive-eval
     (with-handlers ([(lambda (x) #t)
		      (lambda (x)
			(error 'internal-syntax-error (exn-message x)))])
       (expand-defmacro expr))))

  (define-struct process-finish (error?))

  ;; process-file/zodiac : string
  ;;                       (((+ process-finish sexp zodiac:parsed) ( -> void) -> void)
  ;;                       boolean
  ;;                    -> void
  ;; note: the boolean controls which variant of the union is passed to the 3rd arg.
  ;; expects to be called with user's parameterization active
  (define (process-file/zodiac filename f annotate?)
    (let ([port (open-input-file filename 'text)]
	  [setting (current-setting)])
      (dynamic-wind
       void
       (lambda ()
	 (process/zodiac
	  (parameterize ([read-case-sensitive (setting-case-sensitive? setting)])
	    (zodiac:read port
			 (zodiac:make-location INITIAL-LINE
					       INITIAL-COLUMN
					       INITIAL-OFFSET
					       (path->complete-path filename))
			 #t 1))
	  f
	  annotate?))
       (lambda () (close-input-port port)))))

  ;; process-file/no-zodiac : string
  ;;                          ((+ process-finish sexp zodiac:parsed) ( -> void) -> void)
  ;;                       -> void
  ;; expects to be called with user's parameterization active
  (define (process-file/no-zodiac filename f)
    (call-with-input-file filename
      (lambda (port)
	(process/no-zodiac (lambda () (read port)) f))))

  ;; process-sexp/no-zodiac : sexp
  ;;                          ((+ process-finish sexp zodiac:parsed) ( -> void) -> void)
  ;;                       -> void
  ;; expects to be called with user's parameterization active
  (define (process-sexp/no-zodiac sexp f)
    (process/no-zodiac (let ([first? #t]) 
			 (lambda ()
			   (if first?
			       (begin (set! first? #f)
				      sexp)
			       eof)))
		       f))

  ;; process-sexp/zodiac : sexp
  ;;                       zodiac:sexp
  ;;                       ((+ process-finish sexp zodiac:parsed) ( -> void) -> void)
  ;;                       boolean
  ;;                    -> void
  ;; note: the boolean controls which variant of the union is passed to the 2nd arg.
  ;; expects to be called with user's parameterization active
  (define (process-sexp/zodiac sexp z f annotate?)
    (let* ([reader
	    (let ([gone #f])
	      (lambda ()
		(or gone
		    (begin (set! gone (zodiac:make-eof z))
			   (zodiac:structurize-syntax sexp z)))))])
      (process/zodiac reader f annotate?)))

  ;; process/zodiac : ( -> zodiac:sexp)
  ;;                  ((+ process-finish sexp zodiac:parsed) ( -> void) -> void)
  ;;                  boolean
  ;;               -> void
  ;; expects to be called with user's parameterization active
  (define (process/zodiac reader f annotate?)
    (let ([setting (current-setting)]
	  [vocab (current-vocabulary)]
	  [cleanup
	   (lambda (error?)
	     (f (make-process-finish error?) void))])
      (let loop ()
	(let ([next-iteration
	       (let/ec k
		 (let ([annotate
			(lambda (term)
			  (dynamic-wind
			   (lambda () (zodiac:interface:set-zodiac-phase 'expander))
			   (lambda () (aries:annotate term))
			   (lambda () (zodiac:interface:set-zodiac-phase #f))))]
		       [zodiac-read
			(parameterize ([read-case-sensitive (setting-case-sensitive? setting)])
			  (dynamic-wind
			   (lambda () (zodiac:interface:set-zodiac-phase 'reader))
			   (lambda () (reader))
			   (lambda () (zodiac:interface:set-zodiac-phase #f))))])
		   (if (zodiac:eof? zodiac-read)
		       (lambda () (cleanup #f))
		       (let* ([evaluator
			       (lambda (exp _ macro)
				 (primitive-eval (annotate exp)))]
			      [user-macro-body-evaluator
			       (lambda (x . args)
				 (primitive-eval `(,x ,@(map (lambda (x) `(#%quote ,x)) args))))]
			      [exp
			       (dynamic-wind
				(lambda () (zodiac:interface:set-zodiac-phase 'expander))
				(lambda () (call/nal zodiac:scheme-expand/nal
						     zodiac:scheme-expand
						     [expression: zodiac-read]
						     [vocabulary: vocab]
						     [user-macro-body-evaluator: user-macro-body-evaluator]
						     [elaboration-evaluator: evaluator]))
				(lambda () (zodiac:interface:set-zodiac-phase #f)))]
			      [heading-out (if annotate? 
					       (annotate exp)
					       exp)])
			 (lambda () (f heading-out loop))))))])
	  (next-iteration)))))

  ;; process/no-zodiac : ( -> sexp) ((+ sexp process-finish) ( -> void) -> void) -> void
  (define (process/no-zodiac reader f)
    (let loop ()
      (let ([expr (reader)])
	(if (eof-object? expr)
	    (f (make-process-finish #f) void)
	    (f expr loop)))))

  (define (format-source-loc start-location end-location)
    (let ([file (zodiac:location-file start-location)])
      (format "~a: ~a.~a-~a.~a: "
	      file
	      (zodiac:location-line start-location)
	      (zodiac:location-column start-location)
	      (zodiac:location-line end-location)
	      (+ (zodiac:location-column end-location) 1))))

  ;; (parameter (string debug-info -> void))
  (define error-display/debug-handler
    (make-parameter
     (lambda (msg debug)
       ((error-display-handler) 
	(if (zodiac:zodiac? debug)
	    (string-append (format-source-loc (zodiac:zodiac-start debug)
					      (zodiac:zodiac-finish debug))
			   msg)
	    msg)))))

  ;; bottom-escape-handler : (parameter ( -> A))
  ;; escapes
  (define bottom-escape-handler (make-parameter void))

  ;; drscheme-exception-handler : exn -> A
  ;; effect: displays the exn-message and escapes
  (define (drscheme-exception-handler exn)
    (let ([dh (error-display/debug-handler)])
      (if (exn? exn)
	  (dh (exn-message exn) (exn-debug-info exn))
	  (dh (format "uncaught exception: ~e" exn) #f)))
    ((error-escape-handler))
    ((error-display-handler) "Exception handler didn't escape")
    ((bottom-escape-handler)))

  ;; drscheme-branch-handler :  -> parameterization
  ;; parameters: current-parameterization
  (define (drscheme-branch-handler) (make-parameterization (current-parameterization)))

  ;; drscheme-error-value->string-handler : TST number -> string
  (define (drscheme-error-value->string-handler x n)
    (let ([port (open-output-string)])
      (parameterize ([current-output-port port]
		     [mzlib:pretty-print:pretty-print-columns 'infinity])
	(drscheme-print/void x))
      (let* ([long-string (get-output-string port)])
	(close-output-port port)
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

  ;; intermediate-values-during-load : (parameter (TST *-> void))
  (define intermediate-values-during-load (make-parameter (lambda x (void))))

  ;; drscheme-load-handler : string ->* TST
  ;;   It should call intermediate-values-during-load in all 3 branches.
  ;;   It only calls it in one, currently.
  (define (drscheme-load-handler filename)
    (unless (string? filename)
      (raise (make-exn:application:arity
	      (format "drscheme-load-handler: expects argument of type <string>; given: ~e" filename)
	      ((debug-info-handler))
	      filename
	      'string)))
    (let ([zo-file?
	   (let ([l (string-length filename)])
	     (and (<= 3 l)
		  (string=? ".zo" (substring filename (- l 3) l))))])

	(cond
	 [zo-file?
	  (parameterize ([current-eval primitive-eval])
	    (primitive-load filename))]
	 [(setting-use-zodiac? (current-setting))
	  (let* ([process-sexps
		  (let ([last (list (void))])
		    (lambda (sexp recur)
		      (cond
		       [(process-finish? sexp)
			last]
		       [else
			(set! last
			      (call-with-values
			       (lambda () (syntax-checking-primitive-eval sexp))
			       (lambda x
				 (apply (intermediate-values-during-load) x)
				 x)))
			(recur)])))])
	    (apply values (process-file/zodiac filename process-sexps #t)))]
	 [else
	  (primitive-load filename)])))

    ;; drscheme-eval : sexp ->* TST
    (define (drscheme-eval-handler sexp)
      (if (setting-use-zodiac? (current-setting))
	  (let* ([z (or (unbox aries:error-box)
			(let ([loc (zodiac:make-location INITIAL-LINE INITIAL-COLUMN INITIAL-OFFSET 'eval)])
			  (zodiac:make-zodiac 'mzrice-eval loc loc)))]
		 [answer (list (void))]
		 [f
		  (lambda (annotated recur)
		    (if (process-finish? annotated)
			answer
			(begin (set! answer
				     (call-with-values
				      (lambda () (syntax-checking-primitive-eval annotated))
				      (lambda x x)))
			       (recur))))])
	    (apply values (process-sexp/zodiac sexp z f #t)))
	  (primitive-eval sexp)))


    ;; drscheme-print : TST -> void
    ;; effect: prints the value, on the screen, attending to the values of the current setting
    (define drscheme-print
      (lambda (v)
	(unless (void? v)
	  (drscheme-print/void v))))

    ;; drscheme-print/void : TST -> void
    ;; effect: prints the value, on the screen, attending to the values of the current setting
    (define drscheme-print/void
      (lambda (v)
	(let ([value (if (r4rs-style-printing? (current-setting))
			 v
			 (mzlib:print-convert:print-convert v))])
	  (mzlib:pretty-print:pretty-print-handler value))))

  ;; drscheme-port-print-handler : TST port -> void
  ;; effect: prints the value on the port
  (define (drscheme-port-print-handler value port)
    (parameterize ([mzlib:pretty-print:pretty-print-columns 'infinity]
		   [current-output-port port])
      (drscheme-print/void value)))

    ;; build-parameterization : (list-of symbols)
    ;;                          setting
    ;;                          (X Y Z -> (unit (import) (export ...)))
    ;;                       -> parameterization
    (define (build-parameterization namespace-flags setting handle-invokation)
      (let* ([parameterization (make-parameterization)]
	     [custodian (make-custodian)]
	     [n (apply make-namespace
		       (if (setting-use-zodiac? setting)
			   (append (list 'no-constants 'hash-percent-syntax) namespace-flags)
			   namespace-flags))])
	(with-parameterization parameterization
	  (lambda ()
	    (when (setting-use-zodiac? setting)
	      (require-library-use-compiled #f))
	    (current-setting setting)
	    (parameterization-branch-handler drscheme-branch-handler)
	    (compile-allow-set!-undefined #f)
	    (compile-allow-cond-fallthrough #f)
	    (current-custodian custodian)
	    (require-library-use-compiled #f)
	    (error-value->string-handler drscheme-error-value->string-handler)
	    (debug-info-handler (let ([drscheme-debug-info-handler
				       (lambda () (unbox aries:error-box))])
				  drscheme-debug-info-handler))
	    (current-exception-handler drscheme-exception-handler)
	    (current-namespace n)
	    (break-enabled #t)
	    (current-will-executor (make-will-executor))
	    (read-curly-brace-as-paren #t)
	    (read-square-bracket-as-paren #t)
	    (print-struct #t)

	    (error-print-width 250)
	    (current-print drscheme-print)

	    ;; ordering of these two is important;
	    ;; zodiac:current-vocabulary-symbol bangs on zodiac:scheme-vocabulary
	    (when (setting-use-zodiac? setting)
	      (zodiac:current-vocabulary-symbol (setting-vocabulary-symbol setting))
	      (current-vocabulary (zodiac:create-vocabulary
				   'scheme-w/user-defined-macros/drscheme
				   zodiac:scheme-vocabulary)))
	    
	    (read-case-sensitive (setting-case-sensitive? setting))

	    ;; this sets both the zodiac and the cons procedure setting,
	    ;; via a dynamic link in basis.ss
	    (zodiac:allow-improper-lists (or (not (setting-use-zodiac? setting))
					     (setting-allow-improper-lists? setting)))
	    
	    (eq?-only-compares-symbols (setting-eq?-only-compares-symbols? setting))
	    (<=-at-least-two-args (setting-<=-at-least-two-args setting))
	    
	    (zodiac:disallow-untagged-inexact-numbers (setting-disallow-untagged-inexact-numbers setting))

	    (aries:signal-undefined (setting-signal-undefined setting))
	    (aries:signal-not-boolean (setting-signal-not-boolean setting))
	    
	    (compile-allow-set!-undefined (setting-allow-set!-on-undefined? setting))
	    (compile-allow-cond-fallthrough (not (setting-unmatched-cond/case-is-error? setting)))

	    (current-eval drscheme-eval-handler)
	    (current-load drscheme-load-handler)

	    (mzlib:print-convert:empty-list-name 'empty)

	    (global-port-print-handler drscheme-port-print-handler)

	    (case (setting-printing setting)
	      [(constructor-style)
	       (r4rs-style-printing #f)
	       (mzlib:print-convert:constructor-style-printing #t)]
	      [(quasi-style)
	       (r4rs-style-printing #f)
	       (mzlib:print-convert:constructor-style-printing #f)
	       (mzlib:print-convert:quasi-read-style-printing #f)]
	      [(quasi-read-style)
	       (r4rs-style-printing #f)
	       (mzlib:print-convert:constructor-style-printing #f)
	       (mzlib:print-convert:quasi-read-style-printing #t)]
	      [(r4rs-style) (r4rs-style-printing #t)]
	      [else (error 'install-language "found bad setting-printing: ~a~n" 
			   (setting-printing setting))])

	    (mzlib:pretty-print:pretty-print-show-inexactness (setting-print-tagged-inexact-numbers setting))
	    (mzlib:print-convert:show-sharing (setting-sharing-printing? setting))
	    (mzlib:print-convert:whole/fractional-exact-numbers (setting-whole/fractional-exact-numbers setting))
	    (print-graph (setting-sharing-printing? setting))
	    (mzlib:print-convert:abbreviate-cons-as-list (setting-abbreviate-cons-as-list? setting))))
	(with-parameterization parameterization
	  (lambda ()
	    (unless (setting-use-zodiac? setting)
	      (require-library "corem.ss"))))
	(handle-invokation <=-at-least-two-args
			   zodiac:allow-improper-lists
			   eq?-only-compares-symbols
			   parameterization)
	parameterization)))
