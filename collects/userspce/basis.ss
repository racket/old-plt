;; replace zodiac-exn? with standard mzscheme syntax error exceptions
;;       (is that already happening somehow?)

(unit/sig userspace:basis^
  (import [import : userspace:basis-import^]
	  [params : plt:userspace:params^]
	  [zodiac : drscheme:zodiac^]
	  [zodiac:interface : drscheme:interface^]
	  [aries : plt:aries^]
	  [mzlib:print-convert : mzlib:print-convert^]
	  [mzlib:pretty-print : mzlib:pretty-print^]
	  [mzlib:function : mzlib:function^])

  (define initial-line 1)
  (define initial-column 1)
  (define initial-offset 0)

  (define original-output-port (current-output-port))
  (define (printf . args)
    (apply fprintf original-output-port args))

  (define (report-error . x) (error 'report-error))
  (define (report-unlocated-error . x) (error 'report-unlocated-error))

  (define primitive-load (current-load))
  (define primitive-eval (current-eval))

  (define r4rs-style-printing (make-parameter #f))
  
  (define this-program (with-handlers ([void (lambda (x) "mzscheme")])
			 (global-defined-value 'program)))

  (define-struct/parse setting (name
				vocabulary-symbol
				extra-definitions-unit-name
				macro-libraries
				case-sensitive?
				allow-set!-on-undefined?
				unmatched-cond/case-is-error?
				allow-improper-lists?
				allow-reader-quasiquote?
				sharing-printing?
				abbreviate-cons-as-list?
				signal-undefined
				signal-not-boolean
				eq?-only-compares-symbols?
				<=-at-least-two-args
				disallow-untagged-inexact-numbers
				print-tagged-inexact-numbers
				whole/fractional-exact-numbers
				printing
				define-argv?))

  ;; settings : (list-of setting)
  (define settings
    (list (make-setting/parse
	   `((name "Beginner")
	     (extra-definitions-unit-name "beginner.ss")
	     (macro-libraries ())
	     (vocabulary-symbol beginner)
	     (case-sensitive? #t)
	     (allow-set!-on-undefined? #f)
	     (unmatched-cond/case-is-error? #t)
	     (allow-improper-lists? #f)
	     (allow-reader-quasiquote? #f)
	     (sharing-printing? #f)
	     (abbreviate-cons-as-list? #f)
	     (signal-undefined #t)
	     (signal-not-boolean #t)
	     (eq?-only-compares-symbols? #t)
	     (<=-at-least-two-args #t)
	     (disallow-untagged-inexact-numbers #f)
	     (print-tagged-inexact-numbers #t)
	     (whole/fractional-exact-numbers #f)
	     (printing constructor-style)
	     (define-argv? #f)))
	  (make-setting/parse
	   `((name "Intermediate")
	     (extra-definitions-unit-name "intermediate.ss")
	     (macro-libraries ())
	     (vocabulary-symbol intermediate)
	     (case-sensitive? #t)
	     (allow-set!-on-undefined? #f)
	     (unmatched-cond/case-is-error? #t)
	     (allow-improper-lists? #f)
	     (allow-reader-quasiquote? #f)
	     (sharing-printing? #f)
	     (abbreviate-cons-as-list? #t)
	     (signal-undefined #t)
	     (signal-not-boolean #t)
	     (eq?-only-compares-symbols? #t)
	     (<=-at-least-two-args #t)
	     (disallow-untagged-inexact-numbers #f)
	     (print-tagged-inexact-numbers #t)
	     (whole/fractional-exact-numbers #f)
	     (printing constructor-style)
	     (define-argv? #f)))
	  (make-setting/parse
	   `((name "Advanced")
	     (extra-definitions-unit-name "advanced.ss")
	     (macro-libraries ())
	     (vocabulary-symbol advanced)
	     (case-sensitive? #t)
	     (allow-set!-on-undefined? #f)
	     (unmatched-cond/case-is-error? #t)
	     (allow-improper-lists? #f)
	     (allow-reader-quasiquote? #t)
	     (sharing-printing? #t)
	     (abbreviate-cons-as-list? #t)
	     (signal-undefined #t)
	     (signal-not-boolean #f)
	     (eq?-only-compares-symbols? #f)
	     (<=-at-least-two-args #t)
	     (disallow-untagged-inexact-numbers #f)
	     (print-tagged-inexact-numbers #t)
	     (whole/fractional-exact-numbers #f)
	     (printing constructor-style)
	     (define-argv? #f)))
	  (make-setting/parse
	   `((name "MzScheme")
	     (extra-definitions-unit-name #f)
	     (macro-libraries ())
	     (vocabulary-symbol mzscheme)
	     (case-sensitive? #f)
	     (allow-set!-on-undefined? #f)
	     (unmatched-cond/case-is-error? #f)
	     (allow-improper-lists? #t)
	     (allow-reader-quasiquote? #t)
	     (sharing-printing? #f)
	     (abbreviate-cons-as-list? #t)
	     (signal-undefined #f)
	     (signal-not-boolean #f)
	     (eq?-only-compares-symbols? #f)
	     (<=-at-least-two-args #f)
	     (disallow-untagged-inexact-numbers #f)
	     (print-tagged-inexact-numbers #f)
	     (whole/fractional-exact-numbers #f)
	     (printing r4rs-style)
	     (define-argv? #t)))
	  (make-setting/parse
	   `((name "MzScheme Debug")
	     (vocabulary-symbol mzscheme-debug)
	     (extra-definitions-unit-name #f)
	     (macro-libraries ())
	     (case-sensitive? #f)
	     (allow-set!-on-undefined? #f)
	     (unmatched-cond/case-is-error? #f)
	     (allow-improper-lists? #t)
	     (allow-reader-quasiquote? #t)
	     (sharing-printing? #f)
	     (abbreviate-cons-as-list? #t)
	     (signal-undefined #f)
	     (signal-not-boolean #f)
	     (eq?-only-compares-symbols? #f)
	     (<=-at-least-two-args #f)
	     (disallow-untagged-inexact-numbers #f)
	     (print-tagged-inexact-numbers #f)
	     (whole/fractional-exact-numbers #f)
	     (printing r4rs-style)
	     (define-argv? #t)))))

  (define (snoc x y) (append y (list x)))

  ;; add-setting : (symbol setting -> void)
  (define (add-setting setting)
    (set! settings (snoc setting settings)))

  ;; find-setting-named : string -> setting
  ;; effect: raises an exception if no setting named by the symbol exists
  (define (find-setting-named name)
    (unless (string? name)
      (error 'find-setting-named "expected symbol, got ~e" name))
    (let loop ([settings settings])
      (cond
	[(null? settings) (error 'find-setting-named "no setting named ~e" name)]
	[else (let* ([setting (car settings)])
		(if (string=? name (setting-name setting))
		    setting
		    (loop (cdr settings))))])))
		    

  ;; copy-setting : setting -> setting
  (define copy-setting
    (lambda (x)
      (unless (setting? x)
	(error 'copy-setting "expected a setting, got ~e" x))
      (apply make-setting (cdr (vector->list (struct->vector x))))))
  
  ;; get-default-setting : (-> setting)
  (define (get-default-setting) (copy-setting (car settings)))

  ;; get-default-setting-name : (-> symbol)
  (define (get-default-setting-name) (setting-name (get-default-setting)))

  ;; setting-name->number : string -> int
  (define setting-name->number
    (lambda (name)
      (let loop ([n 0]
		 [settings settings])
	(cond
	 [(null? settings) (error 'level->number "unexpected level: ~a" name)]
	 [else (let ([setting (car settings)])
		 (if (string=? name (setting-name setting))
		     n
		     (loop (+ n 1)
			   (cdr settings))))]))))

  ;; number->setting : (int -> symbol)
  (define number->setting (lambda (n) (list-ref settings n)))

  ;; zodiac-vocabulary? : setting -> boolean
  (define (zodiac-vocabulary? setting)
    (not (eq? (setting-vocabulary-symbol setting) 'mzscheme)))

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

  ;; current-zodiac-namespace : (parameter (+ #f namespace))
  ;; If another namespace is installed, drscheme-eval uses primitive-eval
  (define current-zodiac-namespace (make-parameter #f))

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
  ;; expects to be called with user's parameter settings active
  (define (process-file/zodiac filename f annotate?)
    (let ([port (open-input-file filename 'text)]
	  [setting (current-setting)])
      (dynamic-wind
       void
       (lambda ()
	 (process/zodiac
	  (zodiac:read port
		       (zodiac:make-location initial-line
					     initial-column
					     initial-offset
					     (path->complete-path filename))
		       #t 1)
	  f
	  annotate?))
       (lambda () (close-input-port port)))))

  ;; process-file/no-zodiac : string
  ;;                          ((+ process-finish sexp zodiac:parsed) ( -> void) -> void)
  ;;                       -> void
  ;; expects to be called with user's parameter settings active
  (define (process-file/no-zodiac filename f)
    (call-with-input-file filename
      (lambda (port)
	(process/no-zodiac (lambda () (read port)) f))))

  ;; process-sexp/no-zodiac : sexp
  ;;                          ((+ process-finish sexp zodiac:parsed) ( -> void) -> void)
  ;;                       -> void
  ;; expects to be called with user's parameter settings active
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
  ;; expects to be called with user's parameter settings active
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
  ;; expects to be called with user's parameter settings active
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
		       ; Always read with zodiac
		       [zodiac-read
			(dynamic-wind
			 (lambda () (zodiac:interface:set-zodiac-phase 'reader))
			 (lambda () (reader))
			 (lambda () (zodiac:interface:set-zodiac-phase #f)))]
		       ; Sometimes, we throw away source information and
		       ;  expand with MzScheme
		       [use-z-exp? (and (zodiac-vocabulary? (current-setting))
					(eq? (current-namespace) (current-zodiac-namespace)))])
		   (if (zodiac:eof? zodiac-read)
		       (lambda () (cleanup #f))
		       (let* ([evaluator
			       (lambda (exp _ macro)
				 (primitive-eval (annotate exp)))]
			      [user-macro-body-evaluator
			       (lambda (x . args)
				 (primitive-eval `(,x ,@(map (lambda (x) `(#%quote ,x)) args))))]
			      [exp
			       (if use-z-exp?
				   (dynamic-wind
				    (lambda () (zodiac:interface:set-zodiac-phase 'expander))
				    (lambda () (parameterize ([zodiac:user-macro-body-evaluator user-macro-body-evaluator]
							      [zodiac:elaboration-evaluator evaluator])
						 (zodiac:scheme-expand
						  zodiac-read
						  'previous
						  vocab)))
				    (lambda () (zodiac:interface:set-zodiac-phase #f)))
				   (zodiac:sexp->raw zodiac-read))]
			      [heading-out (if (and annotate? use-z-exp?)
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

  ;; (parameter (string debug-info exn -> void))
  (define error-display/debug-handler
    (make-parameter
     (lambda (msg debug exn)
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
	  (dh (exn-message exn) (exn-debug-info exn) exn)
	  (dh (format "uncaught exception: ~e" exn) #f #f)))
    ((error-escape-handler))
    ((error-display-handler) "Exception handler didn't escape")
    ((bottom-escape-handler)))

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
	 [(zodiac-vocabulary? (current-setting))
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
      (if (and (zodiac-vocabulary? (current-setting))
	       (eq? (current-namespace) (current-zodiac-namespace)))
	  (let* ([z (let ([continuation-stack (current-continuation-marks aries:w-c-m-key)])
		      (if (null? continuation-stack)
			  (let ([loc (zodiac:make-location 
				      initial-line initial-column initial-offset
				      'eval)])
			    (zodiac:make-zodiac 'mzrice-eval loc loc))
			  (car continuation-stack)))]
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
	  (mzlib:pretty-print:pretty-print value))))

  ;; drscheme-port-print-handler : TST port -> void
  ;; effect: prints the value on the port
  (define (drscheme-port-print-handler value port)
    (parameterize ([mzlib:pretty-print:pretty-print-columns 'infinity]
		   [current-output-port port])
      (drscheme-print/void value)))

  (define ricedefs@ (require-library "ricedefr.ss" "userspce"))

    ;; initialize-parameters : custodian
    ;;                         (list-of symbols)
    ;;                         setting
    ;;                       -> void
    ;; effect: sets the parameters for drscheme and drscheme-jr
    (define (initialize-parameters custodian namespace-flags setting)
      (let-values ([(extra-definitions)
		    (let ([name (setting-extra-definitions-unit-name setting)])
		      (and name
			   (require-library/proc name "userspce")))]
		   [(n) (apply make-namespace
			       (if (zodiac-vocabulary? setting)
				   (append (list 'hash-percent-syntax) namespace-flags)
				   namespace-flags))])
	(when (zodiac-vocabulary? setting)
	  (use-compiled-file-kinds 'non-elaboration))
	(current-setting setting)
	(compile-allow-set!-undefined #f)
	(compile-allow-cond-fallthrough #f)
	(current-custodian custodian)
	(error-value->string-handler drscheme-error-value->string-handler)
	(debug-info-handler (let ([drscheme-debug-info-handler
				   (lambda () (let ([x (current-continuation-marks
							aries:w-c-m-key)])
						(if (null? x)
						    #f ; no debugging info available
						    (car x))))])
			      drscheme-debug-info-handler))
	(current-exception-handler drscheme-exception-handler)
	(current-namespace n)
	(current-zodiac-namespace n)
	(break-enabled #t)
	(current-will-executor (make-will-executor))
	(read-curly-brace-as-paren #t)
	(read-square-bracket-as-paren #t)
	(print-struct (not (eq? 'r4rs-style (setting-printing setting))))

	(error-print-width 250)
	(current-print drscheme-print)

	(when (zodiac-vocabulary? setting)
	  (current-vocabulary
	   (zodiac:create-vocabulary
	    'scheme-w/user-defined-macros/drscheme
	    (case (setting-vocabulary-symbol setting)
	      [(beginner) zodiac:beginner-vocabulary]
	      [(intermediate) zodiac:intermediate-vocabulary]
	      [(advanced) zodiac:advanced-vocabulary]
	      [(mzscheme-debug) zodiac:scheme-vocabulary]
	      [(mred-debug) (zodiac:get-mred-vocabulary)]
	      [else (error 'init "bad vocabulary spec: ~a ~e"
			   (setting-vocabulary-symbol setting) setting)]))))
	
	(read-case-sensitive (setting-case-sensitive? setting))

	(aries:signal-undefined (setting-signal-undefined setting))
	(aries:signal-not-boolean (setting-signal-not-boolean setting))

	;; Allow ` , and ,@ ? - FIXME!
	(zodiac:allow-reader-quasiquote (setting-allow-reader-quasiquote? setting))
	(zodiac:disallow-untagged-inexact-numbers (setting-disallow-untagged-inexact-numbers setting))

	;;; SHOULD ONLY DO THIS IN CERTAIN LANGUAGE LEVELS!!!
	;; ricedefs
	(let ([improper-lists?
	       (or (not (zodiac-vocabulary? setting))
		   (setting-allow-improper-lists? setting))])
	  (zodiac:allow-improper-lists improper-lists?)
	  (params:allow-improper-lists improper-lists?))
	(params:eq?-only-compares-symbols (setting-eq?-only-compares-symbols? setting))
	(params:<=-at-least-two-args (setting-<=-at-least-two-args setting))
	(invoke-open-unit/sig ricedefs@ #f (params : plt:userspace:params^))
	;; end ricedefs

	(compile-allow-set!-undefined (setting-allow-set!-on-undefined? setting))
	(compile-allow-cond-fallthrough (not (setting-unmatched-cond/case-is-error? setting)))

	(current-eval drscheme-eval-handler)
	(current-load drscheme-load-handler)

	(when (setting-define-argv? setting)
	  (global-defined-value 'argv #())
	  (global-defined-value 'program this-program))

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
	(mzlib:print-convert:abbreviate-cons-as-list (setting-abbreviate-cons-as-list? setting))

	(when extra-definitions
	  (invoke-open-unit/sig extra-definitions))
	(for-each (lambda (l) (apply require-library/proc l))
		  (setting-macro-libraries setting)))))
