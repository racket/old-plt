;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      Debugging                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (getenv "MREDDEBUG")
  (letrec ([old-handler (current-load)]
	   [offset 2]
	   [indent 0]
	   [indent-string ""])
    (current-load (lambda (f)
		    (let ([file (if (relative-path? f)
				    (build-path (current-directory) f)
				    f)])
		      (dynamic-wind
		       (lambda ()
			 (set! indent-string (list->string (vector->list (make-vector indent #\space))))
			 (set! indent (+ indent offset))
			 (printf "~aLoading ~a...~n" indent-string file))
		       (lambda () (old-handler file))
		       (lambda ()
			 (printf "~aLoaded ~a...~n" indent-string file)
			 (set! indent (- indent offset)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    Signatures                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(read-case-sensitive #t)

(printf "DrScheme Jr is loading. Please wait...~n")
(flush-output)

(require-library "refer.ss")
(require-library "cores.ss")
(require-library "cmdlines.ss")
(require-library "macro.ss")
(require-library "cmdline.ss")
(require-library "pconvers.ss")

(require-library "zsigs.ss" "zodiac")
(require-library "sigs.ss" "zodiac")

(require-library "ariess.ss" "cogen")
(require-library "params.ss" "userspce")

(require-library "sig.ss" "userspce")

(define-signature prims^ (program argv))
(define-signature drscheme-jr:settings^ (setting startup-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              Flag and Language Definitions             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define build-settingU
  (unit/sig drscheme-jr:settings^
    (import [mz : prims^]
	    [cmd-line : mzlib:command-line^]
	    [basis : userspace:basis^]
	    [mzlib:pretty-print : mzlib:pretty-print^]
	    [mzlib:function : mzlib:function^])
    
    (define setting (basis:get-default-setting))

    (define omit-languages '("MzScheme"))

    ;; Flag definitions

    (define (make-get/set s s!)
      (case-lambda
       [() (s setting)]
       [(x) (s! setting x)]))

    (define flags
      (list
       (list "--case-sens"
	     (make-get/set basis:setting-case-sensitive?
			   basis:set-setting-case-sensitive?!)
	     "case-sensitive symbols and variables"
	     basis:setting-case-sensitive?)
#|
       (list "--set-undef"
	     (make-get/set basis:setting-allow-set!-on-undefined?
			   basis:set-setting-allow-set!-on-undefined?!)
	     "set! on undefined variables"
	     basis:setting-allow-set!-on-undefined?)
|#
       (list "--auto-else"
	     (make-get/set basis:setting-unmatched-cond/case-is-error?
			   basis:set-setting-unmatched-cond/case-is-error?!)
	     "non-matching cond/case produces (void)"
	     basis:setting-unmatched-cond/case-is-error?)
       (list "--improper-lists"
	     (make-get/set basis:setting-allow-improper-lists?
			   basis:set-setting-allow-improper-lists?!)
	     "improper lists"
	     basis:setting-allow-improper-lists?)
       (list "--print-sharing"
	     (make-get/set basis:setting-sharing-printing?
			   basis:set-setting-sharing-printing?!)
	     "show sharing in values"
	     basis:setting-sharing-printing?)
       (list "--print-list"
	     (make-get/set basis:setting-abbreviate-cons-as-list?
			   basis:set-setting-abbreviate-cons-as-list?!)
	     "use `list' where appropriate in constructor style printing"
	     basis:setting-abbreviate-cons-as-list?)
       (list "--signal-undef"
	     (make-get/set basis:setting-signal-undefined
			   basis:set-setting-signal-undefined!)
	     "error if using #<undefined> variable"
	     basis:setting-signal-undefined)
       (list "--boolean-conds"
	     (make-get/set basis:setting-signal-not-boolean
			   basis:set-setting-signal-not-boolean!)
	     "conditionals must be #t or #f"
	     basis:setting-signal-not-boolean)
       (list "--eq-syms"
	     (make-get/set basis:setting-eq?-only-compares-symbols?
			   basis:set-setting-eq?-only-compares-symbols?!)
	     "eq? only for symbols"
	     basis:setting-eq?-only-compares-symbols?)
#|
       (list "--require-inexacts"
	     (make-get/set basis:setting-disallow-untagged-inexact-numbers
			   basis:set-setting-disallow-untagged-inexact-numbers!)
	     "#i required for inexact numbers"
	     basis:setting-disallow-untagged-inexact-numbers)
|#
       (list "--tag-inexacts"
	     (make-get/set basis:setting-print-tagged-inexact-numbers
			   basis:set-setting-print-tagged-inexact-numbers!)
	     "print inexact numbers with #i"
	     basis:setting-print-tagged-inexact-numbers)
       (list "--whole-frac"
	     (make-get/set basis:setting-whole/fractional-exact-numbers
			   basis:set-setting-whole/fractional-exact-numbers!)
	     "separate whole and fractional parts of exact numbers in printer"
	     basis:setting-whole/fractional-exact-numbers)
       (list "--constructor-printing"
	     (case-lambda
	      [() (eq? (basis:setting-printing setting) 'constructor-style)]
	      [(x) (basis:set-setting-printing!
		    setting
		    (if x
			'constructor-style
			'r4rs-style))])
	     "print values using constructor style input syntax"
	     (lambda (setting) (eq? (basis:setting-printing setting) 'constructor-style)))
       (list "--quasi-printing"
	     (case-lambda
	      [() (eq? (basis:setting-printing setting) 'quasi-style)]
	      [(x) (basis:set-setting-printing!
		    setting
		    (if x
			'quasi-style
			'r4rs-style))])
	     "print values using quasi-quote style input syntax"
	     (lambda (setting) (eq? (basis:setting-printing setting) 'quasi-style)))))

    ;; Mapping from language to flag settings

    (define language-levels 
      (mzlib:function:filter (lambda (x) (not (member (car x) omit-languages)))
			     (map (lambda (x)
				    (list (basis:setting-name x)
					  (basis:setting-vocabulary-symbol x)))
				  basis:settings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 Setting the Flags                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (get-argv-file)
      (let ([settings-path
	     (lambda (p)
	       (if p
		   (let-values ([(base name dir?) (split-path (path->complete-path p))])
		     base)
		   'same))])
	(case (system-type)
	  [(unix beos) "~/.drscheme-jr.settings"]
	  [(macos) (build-path (settings-path 
				(find-executable-path mz:program "DrScheme Jr"))
			       "DrScheme Jr Settings")]
	  [(windows) 
	   (let* ([d (getenv "HOMEDRIVE")]
		  [p (getenv "HOMEPATH")]
		  [home (and d p
			     (with-handlers ([void (lambda (x) #f)])
			       (build-path d p)))])
	     (if (and home (directory-exists? home))
		 (build-path home "DrScheme Jr Settings.txt")
		 (build-path (settings-path 
			      (find-executable-path mz:program "DrScheme Jr.exe"))			    
			     "DrScheme Jr Settings.txt")))])))

    (with-handlers ([(lambda (x) #t)
		     (lambda (exn)
		       (printf "error loading saved language settings: ~a~n"
			       (exn-message exn)))])
      (let ([argv-file (get-argv-file)])
	(when (file-exists? argv-file)
	  (with-input-from-file argv-file
	    (lambda ()
	      (let ([l (read)]
		    [s (read)])
		(when (memq l (map (lambda (l) (cadr l)) language-levels))
		  (basis:set-setting-vocabulary-symbol! setting l))
		(for-each
		 (lambda (entry)
		   (let ([tag (car entry)]
			 [value (cdr entry)])
		     (let ([a (assoc tag flags)])
		       (when a
			 ((cadr a) value)))))
		 s)))))))

    (define (bad-arguments s . args)
      (printf "DrScheme Jr error: ~a~n" (apply format s args))
      (exit -1))

    (define (choose-mode)
      (printf "Choose a language:~n")
      (let loop ([n 1]
		 [l language-levels])
	(if (null? l)
	    (printf "[~a]? " (sub1 n))
	    (begin
	      (printf "  ~a - ~a~n" n (caar l))
	      (loop (add1 n) (cdr l)))))
      (flush-output)
      (let ([r (read-line)]
	    [len (length language-levels)])
	(let ([v (with-handlers ([void (lambda (x) #f)])
		   (let* ([p (open-input-string r)]
			  [x (read p)])
		     (and (eof-object? (read p)) x)))])
	  (if (or (eof-object? v)
		  (and (number? v)
		       (<= 1 v len)))
	      (set-level (car (list-ref language-levels
					(if (eof-object? v)
					    (sub1 len)
					    (sub1 v)))))
	      (begin
		(printf "Please answer 1, 2, 3, or 4~n")
		(choose-mode))))))

    (define (set-level level)
      (let ([p (assoc level (map (lambda (s)
				   (list (basis:setting-name s)
					 s))
				 basis:settings))])
	(if (and p (not (member level omit-languages)))
	    (set! setting (basis:copy-setting (cadr p)))
	    (bad-arguments "bad language name: ~s" level))))

    (define (make-implies-string vocab-symbol)
      (let ([impl-setting (if vocab-symbol
			      (let ([a (assoc vocab-symbol (map (lambda (x) 
								  (list (string->symbol (basis:setting-name x))
									x))
								basis:settings))])
				(unless a
				  (error 'DrScheme\ Jr "unknown level: ~a~n" vocab-symbol))
				(vector-ref (mzlib:function:second a) 1))
			      setting)])
	(fluid-let ([setting impl-setting])
	  (let* ([on/off (lambda (x) (if x "on" "off"))]
		 [s (apply
		     string-append
		     (map
		      (lambda (f)
			(format "~n                     ~a ~a" 
				(car f)
				(on/off ((cadr f)))))
		      flags))])
	    s))))

    (define (on? v)
      (cond
       [(string=? v "on") #t]
       [(string=? v "off") #f]
       [else (bad-arguments "expected \"on\" or \"off\", given: ~a" v)]))

    (define (make-on/off s)
      (list (format "Enable/disable ~a" s)
	    "on/off"))

    (define startup-file
      (cmd-line:parse-command-line
       "DrScheme Jr"
       mz:argv
       `([once-each
	  [("-l" "--language")
	   ,(lambda (_ level)
	      (set-level level))
	   (,(format "Set the language, one of:~n          ~a"
		     (apply string-append
			    (map (lambda (l)
				   (format "   ~a" (car l)))
				 language-levels)))
	    "language")]]
	 [multi
	  ,@(map
	     (lambda (f)
	       `[(,(car f))
		 ,(lambda (_ v) ((cadr f) (on? v)))
		 ,(make-on/off (caddr f))])
	     flags)]
	 [once-each
	  [("--choose")
	   ,(lambda (_)
	      (choose-mode))
	   ("Interactively choose the language level")]
	  [("--save")
	   ,(lambda (_)
	      (with-output-to-file (get-argv-file)
		(lambda ()
		  (write (basis:setting-vocabulary-symbol setting))
		  (newline)
		  (mzlib:pretty-print:pretty-print (map (lambda (s)
							  (cons (car s)
								((cadr s))))
							flags))
		  (newline))
		'truncate/replace)
	      (printf "Settings saved.~n"))
	   (,(format "Save current settings to:~n           ~a" (get-argv-file)))]
	  [("--show")
	   ,(lambda (_) (printf "Current settings: ~a~n" (make-implies-string #f)))
	   ("Show the current settings")]
	  [("--lhelp")
	   ,(lambda (_ level)
	      (printf "~a implies the following flags: ~a~n" 
		      level
		      (make-implies-string (string->symbol level)))
	      (exit))
	   ("Show the flags implied by a particular language" "language")]])
       (case-lambda 
	[(accum) #f]
	[(accum file) file])
       '("Scheme file")
       (lambda (s)
	 (display s)
	 (printf " If ~a exists, it initializes the language settings.~n"
		 (get-argv-file))
	 (exit 0))))))

(define dr-jrU
  (unit/sig ()
    (import [zodiac : zodiac:system^]
	    [print-convert : mzlib:print-convert^]
	    [basis : userspace:basis^]
	    [mzlib:pretty-print : mzlib:pretty-print^]
	    [mzlib:function : mzlib:function^]
	    [mzlib:thread : mzlib:thread^]
	    [settings : drscheme-jr:settings^])
    (define user-custodian (make-custodian))

    (define primitive-eval (current-eval))

    (define (repl)
      (let ([escape-k void]
	    [display-prompt
	     (lambda ()
	       (display "> ")
	       (flush-output))]
	    [input #f])
	(error-escape-handler (lambda () (escape-k #t)))
	(let outer-loop ()
	  (when (let/ec k
		  (display-prompt)
		  (fluid-let ([escape-k k])
		    (unless input
		      (set! input
			    (zodiac:read (current-input-port)
					 (zodiac:make-location 1 1 (file-position (current-output-port)) "stdin"))))
		    (basis:process/zodiac
		     input
		     (lambda (sexp loop)
		       (unless (basis:process-finish? sexp)
			 (mzlib:thread:dynamic-enable-break
			  (lambda ()
			    (call-with-values
			     (lambda () (primitive-eval sexp))
			     (lambda args (for-each (current-print) args)))))
			 (display-prompt)
			 (loop)))
		     #t))
		  #f)
	    (outer-loop)))))

    (define read/zodiac
      (lambda (port)
	(let ([r (zodiac:read
		  port
		  (zodiac:make-location 1 1 0 "port"))])
	  (lambda ()
	    (let ([v (r)])
	      (if (zodiac:eof? v)
		  eof
		  (zodiac:sexp->raw v)))))))

    (define drscheme-jr-print-load
      (lambda (f)
	(parameterize ([basis:intermediate-values-during-load
			(lambda values
			  (for-each basis:drscheme-print values))])
	  (load f))))

    (define (load/prompt f)
      (let/ec jump
	(let* ([eeh #f])
	  (dynamic-wind
	   (lambda () 
	     (set! eeh (error-escape-handler))
	     (error-escape-handler jump))
	   (lambda () (drscheme-jr-print-load f))
	   (lambda () 
	     (error-escape-handler eeh)
	     (set! eeh #f))))))

    (define (go)
      (let ([file settings:startup-file])
	(let loop ()
	  (let ([continue? #f])

	    (basis:initialize-parameters
	     user-custodian
	     null
	     settings:setting)
	
	    (mzlib:thread:dynamic-disable-break
	     (lambda ()
	       (global-defined-value 'read/zodiac read/zodiac)
	       (global-defined-value 'restart
				     (let ([die (lambda ()
						  (set! continue? #t)
						  (custodian-shutdown-all user-custodian))])
				       (rec restart
					    (case-lambda
					     [(new-file)
					      (when (or (not (string? new-file))
							(not (or (relative-path? new-file)
								 (absolute-path? new-file))))
						(raise-type-error 'restart "path string" new-file))
					      (set! file new-file)
					      (die)]
					     [() (die)]))))
	       (printf "Welcome to DrScheme Jr version ~a, Copyright (c) 1995-98 PLT~n"
		       (version))
	       (printf "Language: ~a~n"
		       (cadr (assoc (basis:setting-vocabulary-symbol (basis:current-setting))
				    (map (lambda (s)
					   (list (basis:setting-vocabulary-symbol s)
						 (basis:setting-name s)))
					 basis:settings))))
	       
	       (let ([repl-thread (thread
				   (lambda ()
				     
				     (when (string? file)
				       (load/prompt file))
				     
				     (repl)))])
		 (mzlib:thread:dynamic-enable-break
		  (lambda ()
		    (let loop ()
		      (with-handlers ([exn:misc:user-break? (lambda (x)
							      (break-thread repl-thread)
							      (loop))])
			(thread-wait repl-thread))))))))
	    (when continue?
	      (loop))))))

    go))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     DrScheme Jr                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dont' require mzlib or pretty-print here; they are linked with zodiac

(require-library "pconver.ss")

(define go
  (invoke-unit/sig
   (compound-unit/sig (import)
     (link [mz : prims^ ((let ([_argv argv]
			       [_program program])
			   (unit/sig prims^
			     (import)
			     (define argv _argv)
			     (define program _program))))]
	   [mzlib : mzlib:core^ ((require-library "corer.ss"))]
	   [print-convert : mzlib:print-convert^ ((require-library "pconverr.ss")
						  (mzlib string)
						  (mzlib function))]
	   [cmd-line : mzlib:command-line^ ((require-library "cmdliner.ss"))]
	   [interface : drscheme:interface^
		      ((require-library-unit/sig "interface.ss" "userspce") aries drzodiac)]
	   [drzodiac : zodiac:system^
		     ((require-library-unit/sig "link.ss" "zodiac")
		      (interface : zodiac:interface^)
		      (mzlib pretty-print)
		      (mzlib file))]
	   [aries : plt:aries^ ((require-library-unit/sig "ariesr.ss" "cogen")
				(drzodiac : zodiac:system^)
				(interface : zodiac:interface^))]
	   [basis-import : userspace:basis-import^ ((unit/sig userspace:basis-import^
						      (import)
						      (define in-mzscheme? #t)))]
	   [params : plt:userspace:params^ ((require-library-unit/sig "paramr.ss" "userspce"))]
	   [basis : userspace:basis^
		  ((require-library-unit/sig "basis.ss" "userspce")
		   basis-import
		   params
		   drzodiac
		   interface
		   aries
		   print-convert
		   (mzlib pretty-print)
		   (mzlib function))]
	   [settings : drscheme-jr:settings^
		     (build-settingU mz
				     cmd-line
				     basis
				     (mzlib pretty-print)
				     (mzlib function))]
	   [dr-jr : () (dr-jrU
			(drzodiac : zodiac:system^)
			print-convert
			basis
			(mzlib pretty-print)
			(mzlib function)
			(mzlib thread)
			settings)])
     (export))))

