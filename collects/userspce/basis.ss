(module basis mzscheme
  (require (lib "pretty.ss")
	   (lib "unitsig.ss")
	   (lib "list.ss")
           "macro.ss"
	   (lib "pconvert.ss")
	   (lib "thread.ss")
	   (lib "sig.ss" "stepper"))

  (provide 
   initial-line
   initial-offset
   initial-column
   
   initialize-parameters
   get-settings
   get-default-setting
   get-default-setting-name
   
   drscheme-load-handler
   
   raw-reader
   
   beginner-language?
   intermediate-language?
   advanced-language?
   full-language?
   
   error-display/debug-handler
   current-setting
   bottom-escape-handler
   
   drscheme-print
   
   format-source-loc
   
   primitive-eval
   primitive-load
   
   process
   process-file
   process-sexp
   
   (struct process-finish (error?))
   
   setting-name->number
   number->setting
   setting/unparse
   (struct setting (key
                    name
                    language-defining-module
                    
                    read-decimal-as-exact?
                    case-sensitive?
                    allow-reader-quasiquote?
                    disallow-untagged-inexact-numbers
                    
                    whole/fractional-exact-numbers
                    
                    printing
                    use-pretty-printer?
                    sharing-printing?
                    abbreviate-cons-as-list?
                    print-tagged-inexact-numbers
                    print-booleans-as-true/false
                    print-exact-as-decimal?
                    print-.-symbols-without-bars
                    print-whole/part-fractions
                    
                    define-argv?))
   make-setting/parse
   
   teaching-level?
   
   find-setting-named
   add-setting
   copy-setting
   
   r4rs-style-printing?)
  
  (define initial-line 1)
  (define initial-column 1)
  (define initial-offset 0)
  
  (define original-output-port (current-output-port))
  
  (define (report-error . x) (error 'report-error))
  (define (report-unlocated-error . x) (error 'report-unlocated-error))
  
  (define primitive-load (current-load))
  (define primitive-eval (current-eval))
  
  (define r4rs-style-printing (make-parameter #f))
  
  (define this-program (with-handlers ([void (lambda (x) "mzscheme")])
			 (namespace-variable-binding 'program)))
  
  (define-struct/parse setting
    (key
     name
     language-defining-module

     read-decimal-as-exact?
     case-sensitive?
     allow-reader-quasiquote?
     disallow-untagged-inexact-numbers

     whole/fractional-exact-numbers

     printing
     use-pretty-printer?
     sharing-printing?
     abbreviate-cons-as-list?
     print-tagged-inexact-numbers
     print-booleans-as-true/false
     print-exact-as-decimal?
     print-.-symbols-without-bars
     print-whole/part-fractions
     
     define-argv?)
    (make-inspector))
  
  ;; settings : (list-of setting)
  (define settings
    (list (make-setting/parse
	   `((key beginner)
	     (name "Beginning Student")
	     (language-defining-module (lib "beginner.ss" "lang"))
	     (case-sensitive? #t)
	     (allow-reader-quasiquote? #f)
	     (sharing-printing? #f)
	     (abbreviate-cons-as-list? #f)
	     (disallow-untagged-inexact-numbers #f)
	     (print-tagged-inexact-numbers #t)
	     (whole/fractional-exact-numbers #t)
             (print-booleans-as-true/false #t)
	     (printing constructor-style)
	     (print-exact-as-decimal? #t)
	     (read-decimal-as-exact? #t)
	     (define-argv? #f)
	     (print-.-symbols-without-bars #t)
             (print-whole/part-fractions #t)
	     (use-pretty-printer? #t)))
	  (make-setting/parse
	   `((key intermediate)
	     (name "Intermediate Student")
	     (language-defining-module (lib "beginner.ss" "lang"))
	     (case-sensitive? #t)
	     (allow-reader-quasiquote? #t)
	     (sharing-printing? #f)
	     (abbreviate-cons-as-list? #t)
	     (disallow-untagged-inexact-numbers #f)
	     (print-tagged-inexact-numbers #t)
	     (whole/fractional-exact-numbers #t)
	     (print-booleans-as-true/false #t)
             (printing constructor-style)
	     (print-exact-as-decimal? #t)
	     (read-decimal-as-exact? #t)
	     (print-.-symbols-without-bars #f)
	     (print-whole/part-fractions #t)
             (define-argv? #f)
	     (use-pretty-printer? #t)))
	  (make-setting/parse
	   `((key advanced)
	     (name "Advanced Student")
	     (language-defining-module (lib "intermediate.ss" "lang"))
	     (case-sensitive? #t)
	     (allow-reader-quasiquote? #t)
	     (sharing-printing? #t)
	     (abbreviate-cons-as-list? #t)
	     (disallow-untagged-inexact-numbers #f)
	     (print-tagged-inexact-numbers #t)
	     (whole/fractional-exact-numbers #t)
	     (print-booleans-as-true/false #t)
             (printing constructor-style)
	     (print-exact-as-decimal? #t)
	     (read-decimal-as-exact? #t)
	     (print-.-symbols-without-bars #f)
	     (print-whole/part-fractions #t)
	     (define-argv? #f)
	     (use-pretty-printer? #t)))
	  (make-setting/parse
	   `((key full)
	     (name "Textual Full Scheme (MzScheme)")
	     (language-defining-module (lib "advanced.ss" "lang"))
	     (case-sensitive? #f)
	     (allow-reader-quasiquote? #t)
	     (sharing-printing? #f)
	     (abbreviate-cons-as-list? #t)
	     (disallow-untagged-inexact-numbers #f)
	     (print-tagged-inexact-numbers #f)
	     (whole/fractional-exact-numbers #t)
	     (print-booleans-as-true/false #f)
             (printing r4rs-style)
	     (print-exact-as-decimal? #f)
	     (read-decimal-as-exact? #f)
	     (print-.-symbols-without-bars #f)
	     (print-whole/part-fractions #t)
	     (define-argv? #t)
	     (use-pretty-printer? #t)))
	  (make-setting/parse
	   `((key full)
	     (name "Textual Full Scheme without Debugging (MzScheme)")
	     (language-defining-module #%mz-kernel)
	     (case-sensitive? #f)
	     (allow-reader-quasiquote? #t)
	     (sharing-printing? #f)
	     (abbreviate-cons-as-list? #t)
	     (disallow-untagged-inexact-numbers #f)
	     (print-tagged-inexact-numbers #f)
	     (whole/fractional-exact-numbers #t)
	     (print-booleans-as-true/false #f)
             (printing r4rs-style)
	     (print-exact-as-decimal? #f)
	     (read-decimal-as-exact? #f)
	     (print-.-symbols-without-bars #f)
	     (print-whole/part-fractions #t)
	     (define-argv? #t)
	     (use-pretty-printer? #t)))))
  
  ;; -> (listof settings)
  (define (get-settings) settings)

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

  ;; raw-reader: (parameter (port -> sexp))
  ;; see help desk for details
  (define raw-reader (make-parameter read (lambda (x)
                                            (unless (procedure? x)
                                              (error 'raw-reader
                                                     "expected a procedure, got: ~e" x))
                                            x)))
  
  (define-struct process-finish (error?))
  
  ;; process-file : string
  ;;                ((+ process-finish sexp) ( -> void) -> void)
  ;;                -> void
  ;; expects to be called with user's parameter settings active
  (define (process-file filename f)
    (call-with-input-file filename
      (lambda (port)
	(process (lambda () ((raw-reader) port)) f))))
  
  ;; process-sexp : sexp
  ;;                ((+ process-finish sexp) ( -> void) -> void)
  ;;                -> void
  ;; expects to be called with user's parameter settings active
  (define (process-sexp sexp f)
    (process (let ([first? #t]) 
	       (lambda ()
		 (if first?
		     (begin (set! first? #f)
			    sexp)
		     eof)))
	     f))
  
  ;; process : ( -> sexp) ((+ sexp process-finish) ( -> void) -> void) -> void
  (define (process reader f)
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
      (error 'format-source-location "not yet implementd")
      '(let ([file (zodiac:location-file start-location)])
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
                         
  
  ;; (parameter (string (union #f syntax) exn -> void))
  (define error-display/debug-handler
    (make-parameter
     (lambda (msg debug exn)
       ((error-display-handler) msg)
       (if (syntax? debug)
	   (string-append (format-source-loc debug debug)
			  msg)
	   msg))))
  
  ;; bottom-escape-handler : (parameter ( -> A))
  ;; must escape
  (define bottom-escape-handler (make-parameter void))
  
  ;; drscheme-exception-handler : exn -> A
  ;; effect: displays the exn-message and escapes
  (define (drscheme-exception-handler exn)
    (let ([dh (error-display/debug-handler)])
      (if (exn? exn)
	  (let* ([marks (exn-continuation-marks exn)])
	    (dh (format "~a" (exn-message exn)) #f exn))
	  (dh (format "uncaught exception: ~e" exn) #f #f)))
    ((error-escape-handler))
    ((error-display-handler) "Exception handler did not escape")
    ((bottom-escape-handler)))
  
  ;; drscheme-error-value->string-handler : TST number -> string
  (define (drscheme-error-value->string-handler x n)
    (let ([port (open-output-string)])
      (parameterize ([current-output-port port]
		     [pretty-print-columns 'infinity])
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
  
  (define re:zo (regexp "[.][zZ][oO]$"))

  ;; drscheme-load-handler : string ->* TST
  (define (drscheme-load-handler filename)
    (unless (string? filename)
      (raise (make-exn:application:arity
	      (format "drscheme-load-handler: expects argument of type <string>; given: ~e" filename)
	      (current-continuation-marks)
	      filename
	      'string)))
    (let ([zo-file? (regexp-match re:zo filename)])
      (cond
        [zo-file?
	 (primitive-load filename)]
        [else
         (let ([has-hash-bang?
                (call-with-input-file filename
                  (lambda (port)
                    (equal? (list (read-char port)
                                  (read-char port))
                            (list #\# #\!))))])
           (call-with-input-file filename
             (lambda (port)
               (when has-hash-bang?
                 (read-line port 'any))
               (let loop ([last-vals (list (void))])
                 (let ([r ((raw-reader) port)])
                   (if (eof-object? r)
                       (apply values last-vals)
                       (call-with-values
                        (lambda ()
			  (eval r))
                        (lambda x
                          (loop x)))))))))])))
  
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
		      (print-convert v))])
      (if (setting-use-pretty-printer? setting)
	  (pretty-print value)
	  (write value))))
  
  ;; drscheme-port-print-handler : TST port -> void
  ;; effect: prints the value on the port
  (define (drscheme-port-print-handler value port)
    (parameterize ([pretty-print-columns 'infinity]
		   [current-output-port port])
      (drscheme-print/void value)))
  
  (define (teaching-level? setting)
    (let* ([name (setting-name setting)]
	   [ans (or (equal? name "Beginning Student")
		    (equal? name "Intermediate Student")
		    (equal? name "Advanced Student"))])
      ans))

  ;; initialize-parameters : custodian
  ;;                         (list-of symbols)
  ;;                         setting
  ;;                       -> void
  ;; effect: sets the parameters for drscheme and drscheme-jr
  (define (initialize-parameters custodian setting)
    (let ([namespace (make-namespace 'empty)])
      
      (current-setting setting)
      (current-custodian custodian)
      (error-value->string-handler drscheme-error-value->string-handler)
      (current-exception-handler drscheme-exception-handler)
      (initial-exception-handler drscheme-exception-handler)

      (break-enabled #t)
      (read-curly-brace-as-paren #t)
      (read-square-bracket-as-paren #t)
      (print-struct (not (eq? 'r4rs-style (setting-printing setting))))
      (read-decimal-as-inexact (not (setting-read-decimal-as-exact? setting)))
      
      (error-print-width 250)
      (current-print drscheme-print)
      
      (current-load-relative-directory #f)

      ;; must call the resolver before setting the namespace
      (let ([lang-module-spec (setting-language-defining-module setting)])
	(dynamic-require lang-module-spec #f)
	(let ([orig-namespace (current-namespace)]
	      [lang-name ((current-module-name-resolver)
			  lang-module-spec #f #f)])
	  (current-namespace namespace)
	  (namespace-attach-module orig-namespace lang-name)
	  (namespace-require lang-name)))

      (read-case-sensitive (setting-case-sensitive? setting))
      
      (current-load drscheme-load-handler)
      
      (when (setting-define-argv? setting)
        (namespace-variable-binding 'argv #())
        (namespace-variable-binding 'program this-program))
      
      (global-port-print-handler drscheme-port-print-handler)
      
      (case (setting-printing setting)
        [(constructor-style)
         (r4rs-style-printing #f)
         (constructor-style-printing #t)]
        [(quasi-style)
         (r4rs-style-printing #f)
         (constructor-style-printing #f)
         (quasi-read-style-printing #f)]
        [(quasi-read-style)
         (r4rs-style-printing #f)
         (constructor-style-printing #f)
         (quasi-read-style-printing #t)]
        [(r4rs-style) (r4rs-style-printing #t)]
        [else (error 'install-language "found bad setting-printing: ~a~n" 
                     (setting-printing setting))])
      
      (pretty-print-exact-as-decimal
       (setting-print-exact-as-decimal? setting))
      (pretty-print-show-inexactness
       (setting-print-tagged-inexact-numbers setting))
      (show-sharing (setting-sharing-printing? setting))
      (pretty-print-.-symbol-without-bars
       (setting-print-.-symbols-without-bars setting))

      ;; use the fractional snips instead.
      (whole/fractional-exact-numbers #f)

      (booleans-as-true/false
       (setting-print-booleans-as-true/false setting))
      (print-graph (and (r4rs-style-printing) (setting-sharing-printing? setting)))
      (abbreviate-cons-as-list (setting-abbreviate-cons-as-list? setting)))))
