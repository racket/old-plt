(unit/sig plt:init-params^
  (import [import : plt:basis-import^]
	  [init-namespace : plt:init-namespace^]
	  [params : plt:userspace:params^]
	  [zodiac : zodiac:system^]
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
  
  (define-struct/parse setting (key
				name
				vocabulary-symbol
				primitives
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
				error-sym/string-only
				disallow-untagged-inexact-numbers
				print-tagged-inexact-numbers
				whole/fractional-exact-numbers
                                print-booleans-as-true/false
				printing
				print-exact-as-decimal?
				read-decimal-as-exact?
				define-argv?
				use-pretty-printer?))
  
  ;; settings : (list-of setting)
  (define settings
    (list (make-setting/parse
	   `((key beginner)
	     (name "Beginning Student")
	     (macro-libraries ())
	     (vocabulary-symbol beginner)
	     (primitives beginner)
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
	     (error-sym/string-only #t)
	     (disallow-untagged-inexact-numbers #f)
	     (print-tagged-inexact-numbers #t)
	     (whole/fractional-exact-numbers #f)
             (print-booleans-as-true/false #t)
	     (printing constructor-style)
	     (print-exact-as-decimal? #t)
	     (read-decimal-as-exact? #t)
	     (define-argv? #f)
	     (use-pretty-printer? #t)))
	  (make-setting/parse
	   `((key intermediate)
	     (name "Intermediate Student")
	     (macro-libraries ())
	     (vocabulary-symbol intermediate)
	     (primitives intermediate)
	     (case-sensitive? #t)
	     (allow-set!-on-undefined? #f)
	     (unmatched-cond/case-is-error? #t)
	     (allow-improper-lists? #f)
	     (allow-reader-quasiquote? #t)
	     (sharing-printing? #f)
	     (abbreviate-cons-as-list? #t)
	     (signal-undefined #t)
	     (signal-not-boolean #t)
	     (eq?-only-compares-symbols? #t)
	     (<=-at-least-two-args #t)
	     (error-sym/string-only #t)
	     (disallow-untagged-inexact-numbers #f)
	     (print-tagged-inexact-numbers #t)
	     (whole/fractional-exact-numbers #f)
	     (print-booleans-as-true/false #t)
             (printing constructor-style)
	     (print-exact-as-decimal? #t)
	     (read-decimal-as-exact? #t)
	     (define-argv? #f)
	     (use-pretty-printer? #t)))
	  (make-setting/parse
	   `((key advanced)
	     (name "Advanced Student")
	     (macro-libraries ())
	     (vocabulary-symbol advanced)
	     (primitives advanced)
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
	     (error-sym/string-only #t)
	     (disallow-untagged-inexact-numbers #f)
	     (print-tagged-inexact-numbers #t)
	     (whole/fractional-exact-numbers #f)
	     (print-booleans-as-true/false #t)
             (printing constructor-style)
	     (print-exact-as-decimal? #t)
	     (read-decimal-as-exact? #t)
	     (define-argv? #f)
	     (use-pretty-printer? #t)))
	  (make-setting/parse
	   `((key full)
	     (name "Textual Full Scheme (MzScheme)")
	     (vocabulary-symbol mzscheme-debug)
	     (primitives all)
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
	     (error-sym/string-only #f)
	     (disallow-untagged-inexact-numbers #f)
	     (print-tagged-inexact-numbers #f)
	     (whole/fractional-exact-numbers #f)
	     (print-booleans-as-true/false #f)
             (printing r4rs-style)
	     (print-exact-as-decimal? #f)
	     (read-decimal-as-exact? #f)
	     (define-argv? #t)
	     (use-pretty-printer? #t)))
	  (make-setting/parse
	   `((key full)
	     (name "Textual Full Scheme without Debugging (MzScheme)")
	     (macro-libraries ())
	     (vocabulary-symbol mzscheme)
	     (primitives all)
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
	     (error-sym/string-only #f)
	     (disallow-untagged-inexact-numbers #f)
	     (print-tagged-inexact-numbers #f)
	     (whole/fractional-exact-numbers #f)
	     (print-booleans-as-true/false #f)
             (printing r4rs-style)
	     (print-exact-as-decimal? #f)
	     (read-decimal-as-exact? #f)
	     (define-argv? #t)
	     (use-pretty-printer? #t)))))
  
  (define (snoc x y) (append y (list x)))
  
  ;; add-setting : (symbol setting -> void)
  (define add-setting
    (case-lambda
     [(setting) (add-setting setting (length settings))]
     [(setting number)
      (set! settings
	    (let loop ([number number]
		       [settings settings])
	      (cond
	       [(or (zero? number) (null? settings))
		(cons setting settings)]
	       [else
		(cons
		 (car settings)
		 (loop (- number 1)
		       (cdr settings)))])))]))
  
  ;; find-setting-named : string -> setting
  ;; effect: raises an exception if no setting named by the string exists
  (define (find-setting-named name)
    (unless (string? name)
      (error 'find-setting-named "expected string, got ~e" name))
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
    (not (or (eq? (setting-vocabulary-symbol setting) 'mzscheme)
	     (eq? (setting-vocabulary-symbol setting) 'mred))))
  
  ;; X-language : setting -> boolean
  ;; returns true if the input language is the specified language
  (define (beginner-language? setting) (eq? (setting-key setting) 'beginner))
  (define (intermediate-language? setting) (eq? (setting-key setting) 'intermediate))
  (define (advanced-language? setting) (eq? (setting-key setting) 'advanced))
  (define (full-language? setting) (eq? (setting-key setting) 'full))

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
  ;; If an unfriendly namespace is installed, drscheme-eval uses primitive-eval
  (define current-zodiac-namespace (make-parameter #f))
  
  ;; syntax-checking-primitive-eval : sexp -> value
  ;; effect: raises user-exn if expression ill-formed
  (define (syntax-checking-primitive-eval expr)
    (primitive-eval
     (with-handlers ([(lambda (x) #t)
		      (lambda (x)
			(error 'internal-syntax-error
			       (format "~a" (exn-message x))))])
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
			(lambda (term read-expr)
			  (dynamic-wind
			   (lambda () (zodiac:interface:set-zodiac-phase 'expander))
			   (lambda () (aries:annotate term read-expr))
			   (lambda () (zodiac:interface:set-zodiac-phase #f))))]
		       ; Always read with zodiac
		       [zodiac-read
			(dynamic-wind
			 (lambda () (zodiac:interface:set-zodiac-phase 'reader))
			 (lambda () (reader))
			 (lambda () (zodiac:interface:set-zodiac-phase #f)))]
		       ; Sometimes, we throw away source information and
		       ;  expand with MzScheme
		       [use-z-exp? (use-zodiac?)])
		   (if (zodiac:eof? zodiac-read)
		       (lambda () (cleanup #f))
		       (let* ([evaluator
			       (lambda (exp _ macro)
				 (primitive-eval (annotate exp #f)))]
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

				   ;; call expand-defmacro here so errors
				   ;; are raised at the right time.
				   (expand-defmacro (zodiac:sexp->raw zodiac-read)))]
			      [heading-out (if (and annotate? use-z-exp?)
					       (annotate exp zodiac-read)
					       exp)])
			 (lambda () (f heading-out loop))))))])
	  (next-iteration)))))

  (define (use-zodiac?)
    (and (zodiac-vocabulary? (current-setting))
	 (or (eq? (current-namespace) (current-zodiac-namespace))
	     (friendly-current-namespace?))))

  ;; friendly-current-namespace? : -> bool
  ;;  Determines whether the namespace has enough keywords to
  ;;  support elaboration and debugging. Caches the result.
  (define friendly-current-namespace?
    (let ([cache (make-hash-table-weak)])
      (lambda ()
	(let ([ns (current-namespace)])
	  (hash-table-get
	   cache ns
	   (lambda ()
	     (let ([kwds (parameterize ([current-namespace (current-zodiac-namespace)])
			   (let ([l (make-global-value-list)])
			     (mzlib:function:filter
			      (lambda (p) (keyword-name? (car p)))
			      l)))])
	       (let ([friendly?
		      (with-handlers ([void (lambda (x) #f)])
			(andmap
			 (lambda (n)
			   (and (keyword-name? (car n))
				(eq? (global-defined-value (car n)) (cdr n))))
			 kwds))])
		 (hash-table-put! cache ns friendly?)
		 friendly?))))))))
  
  ;; process/no-zodiac : ( -> sexp) ((+ sexp process-finish) ( -> void) -> void) -> void
  (define (process/no-zodiac reader f)
    (let loop ()
      (let ([expr (reader)])
	(if (eof-object? expr)
	    (f (make-process-finish #f) void)
	    (f expr loop)))))
  
  (define format-source-loc 
    (case-lambda
     [(start-location end-location)
      (format-source-loc start-location end-location #t)]
     [(start-location end-location start-at-one?)
      (format-source-loc start-location end-location start-at-one? #t)]
     [(start-location end-location start-at-one? lines-and-columns?)
      (let ([file (zodiac:location-file start-location)])
        (if lines-and-columns?
            (let ([offset (if start-at-one? 0 -1)])
              (format "~a: ~a.~a-~a.~a: "
                      file
                      (+ offset (zodiac:location-line start-location))
                      (+ offset (zodiac:location-column start-location))
                      (+ offset (zodiac:location-line end-location))
                      (+ offset 1 (zodiac:location-column end-location))))
            (let ([offset (if start-at-one? 1 0)])
              (format "~a: ~a-~a: "
                      file
                      (+ offset (zodiac:location-offset start-location))
                      (+ 1 offset (zodiac:location-offset end-location))))))]))
                         
  
  ;; (parameter (string (list zodiac:zodiac) exn -> void))
  (define error-display/debug-handler
    (make-parameter
     (lambda (msg debugs exn)
       ((error-display-handler) 
	(let ([debug (and debugs (pair? debugs) (car debugs))])
	  (if (zodiac:zodiac? debug)
	      (string-append (format-source-loc (zodiac:zodiac-start debug)
						(zodiac:zodiac-finish debug))
			     msg)
	      msg))))))
  
  ;; bottom-escape-handler : (parameter ( -> A))
  ;; escapes
  (define bottom-escape-handler (make-parameter void))
  
  ;; drscheme-exception-handler : exn -> A
  ;; effect: displays the exn-message and escapes
  (define (drscheme-exception-handler exn)
    (let ([dh (error-display/debug-handler)])
      (if (exn? exn)
	  (let* ([marks (exn-continuation-marks exn)]
                 [debugs (if (continuation-mark-set? marks)
			     (aries:extract-zodiac-locations marks)
			     null)])
	    (dh (format "~a" (exn-message exn)) debugs exn))
	  (dh (format "uncaught exception: ~e" exn) null #f)))
    ((error-escape-handler))
    ((error-display-handler) "Exception handler did not escape")
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
  ;; probably obsolete
  (define intermediate-values-during-load (make-parameter (lambda x (void))))
  
  (define re:zo (regexp "[.][zZ][oO]$"))

  ;; drscheme-load-handler : string ->* TST
  ;; - sets the intermeidate-values-during-load parameter before evaluating
  ;;   the expressions in the file to guarantee that the parameter is only
  ;;   set for the "outermost" load expression.
  (define (drscheme-load-handler filename)
    (unless (string? filename)
      (raise (make-exn:application:arity
	      (format "drscheme-load-handler: expects argument of type <string>; given: ~e" filename)
	      (current-continuation-marks)
	      filename
	      'string)))
    (let ([zo-file? (regexp-match re:zo filename)]
	  [zodiac? (use-zodiac?)])
      (cond
        [zo-file?
	 (primitive-load filename)]
        [zodiac?
         (let* ([process-sexps
                 (let ([last (list (void))])
                   (lambda (sexp recur)
                     (cond
		       [(process-finish? sexp)
			last]
		       [else
			(set! last
			      (call-with-values
			       (lambda ()
				 (parameterize ([intermediate-values-during-load void])
				   (syntax-checking-primitive-eval sexp)))
			       (lambda x
				 (apply (intermediate-values-during-load) x)
				 x)))
			(recur)])))])
           (apply values (process-file/zodiac filename process-sexps #t)))]
        [else
	 (call-with-input-file filename
	   (lambda (port)
	     (let loop ([last-vals (list (void))])
	       (let ([r (read port)])
		 (if (eof-object? r)
		     (apply values last-vals)
		     (call-with-values
		      (lambda ()
			(parameterize ([intermediate-values-during-load void])
			  (eval r)))
		      (lambda x
			(apply (intermediate-values-during-load) x)
			(loop x))))))))])))
  
  ;; drscheme-eval : sexp ->* TST
  (define (drscheme-eval-handler sexp)
    (if (and (use-zodiac?)
	     (not (compiled-expression? sexp)))
        (let* ([z (let ([continuation-stack (continuation-mark-set->list
                                             (current-continuation-marks)
                                             aries:w-c-m-key)])
                    (if (null? continuation-stack)
                        (let ([loc (zodiac:make-location 
                                    initial-line
                                    initial-column
                                    initial-offset
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
  (define (drscheme-print/void v)
    (let* ([setting (current-setting)]
	   [value (if (r4rs-style-printing? setting)
		      v
		      (mzlib:print-convert:print-convert v))])
      (if (setting-use-pretty-printer? setting)
	  (mzlib:pretty-print:pretty-print value)
	  (write value))))
  
  ;; drscheme-port-print-handler : TST port -> void
  ;; effect: prints the value on the port
  (define (drscheme-port-print-handler value port)
    (parameterize ([mzlib:pretty-print:pretty-print-columns 'infinity]
		   [current-output-port port])
      (drscheme-print/void value)))
  
  (define ricedefs@ (require-library "ricedefr.ss" "userspce"))
  

  (define (teaching-level? setting)
    (let* ([name (setting-name setting)]
	   [ans (or (equal? name "Beginning Student")
		    (equal? name "Intermediate Student")
		    (equal? name "Advanced Student"))])
      ans))

  (define re:mred (regexp "MrEd"))

  ;; initialize-parameters : custodian
  ;;                         (list-of symbols)
  ;;                         setting
  ;;                       -> void
  ;; effect: sets the parameters for drscheme and drscheme-jr
  (define (initialize-parameters custodian setting)
    (let*-values ([(namespace-flags) (let ([name (setting-name setting)])
				       (if (regexp-match re:mred name)
                                           (list 'mred)
                                           (list)))]
		  ;; 
                  [(namespace)
		   (apply make-namespace
			  ;;(cons
			  ;;'no-keywords
			   (if (zodiac-vocabulary? setting)
			       (cons 'hash-percent-syntax namespace-flags)
			       namespace-flags))
		   ;;)
		  ])
      
      (when (zodiac-vocabulary? setting)
        (use-compiled-file-kinds 'non-elaboration))
      (current-setting setting)
      (compile-allow-set!-undefined #f)
      (compile-allow-cond-fallthrough #f)
      (current-custodian custodian)
      (error-value->string-handler drscheme-error-value->string-handler)
      (current-exception-handler drscheme-exception-handler)
      (initial-exception-handler drscheme-exception-handler)
      (current-namespace namespace)
      (current-zodiac-namespace namespace)
      (break-enabled #t)
      (read-curly-brace-as-paren #t)
      (read-square-bracket-as-paren #t)
      (print-struct (not (eq? 'r4rs-style (setting-printing setting))))
      (read-decimal-as-inexact (not (setting-read-decimal-as-exact? setting)))
      
      (init-namespace:init-namespace)

      (error-print-width 250)
      (current-print drscheme-print)
      
      (current-load-relative-directory #f)
      (current-require-relative-collection #f)
      
      (when (zodiac-vocabulary? setting)
        (current-vocabulary
         (zodiac:create-vocabulary
          'scheme-w/user-defined-macros/drscheme
          (case (setting-vocabulary-symbol setting)
            [(beginner) zodiac:beginner-vocabulary]
            [(intermediate) zodiac:intermediate-vocabulary]
            [(advanced) zodiac:advanced-vocabulary]
            [(mzscheme-debug mred-debug) zodiac:scheme-vocabulary]
            [else (error 'init "bad vocabulary spec: ~a ~e"
                         (setting-vocabulary-symbol setting) setting)])))
        (zodiac:reset-previous-attribute 
         #f
         (eq? (setting-vocabulary-symbol setting)
              'mred-debug))
	(zodiac:prepare-current-namespace-for-vocabulary (current-vocabulary)))
      
      (read-case-sensitive (setting-case-sensitive? setting))
      
      (aries:signal-undefined (setting-signal-undefined setting))
      (aries:signal-not-boolean (setting-signal-not-boolean setting))
      
      ;; Allow ` , and ,@ ? - FIXME!
      (zodiac:allow-reader-quasiquote (setting-allow-reader-quasiquote? setting))
      (zodiac:disallow-untagged-inexact-numbers (setting-disallow-untagged-inexact-numbers setting))
      
      ;; ricedefs
      (let ([improper-lists?
             (or (not (zodiac-vocabulary? setting))
                 (setting-allow-improper-lists? setting))])
        (zodiac:allow-improper-lists improper-lists?)
        (params:allow-improper-lists improper-lists?))
      (params:eq?-only-compares-symbols (setting-eq?-only-compares-symbols? setting))
      (params:<=-at-least-two-args (setting-<=-at-least-two-args setting))
      (params:error-sym/string-only (setting-error-sym/string-only setting))
      (when (teaching-level? setting)
        (global-define-values/invoke-unit/sig ricedefs^ ricedefs@ #f (params : plt:userspace:params^)))
      ;; end ricedefs
      
      (compile-allow-set!-undefined (setting-allow-set!-on-undefined? setting))
      (compile-allow-cond-fallthrough (not (setting-unmatched-cond/case-is-error? setting)))
      
      (current-eval drscheme-eval-handler)
      (current-load drscheme-load-handler)
      
      (when (setting-define-argv? setting)
        (global-defined-value 'argv #())
        (global-defined-value 'program this-program))
      
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
      
      (mzlib:pretty-print:pretty-print-exact-as-decimal
       (setting-print-exact-as-decimal? setting))
      (mzlib:pretty-print:pretty-print-show-inexactness
       (setting-print-tagged-inexact-numbers setting))
      (mzlib:print-convert:show-sharing (setting-sharing-printing? setting))
      (mzlib:print-convert:whole/fractional-exact-numbers
       (setting-whole/fractional-exact-numbers setting))
      (mzlib:print-convert:booleans-as-true/false
       (setting-print-booleans-as-true/false setting))
      (print-graph (and (r4rs-style-printing) (setting-sharing-printing? setting)))
      (mzlib:print-convert:abbreviate-cons-as-list (setting-abbreviate-cons-as-list? setting))
      
      ;; ROBBY : attempt to back out of John's changes
      (global-defined-value '#%break aries:break)
      
      (for-each (lambda (l) (apply require-library/proc l))
                (setting-macro-libraries setting)))))
