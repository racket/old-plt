;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      Debugging                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (getenv "MREDDEBUG")
  (letrec* ([old-handler (current-load)]
	    [offset-string "  "]
	    [indent-string ""])
    (current-load (lambda (f)
		    (let ([file (if (relative-path? f)
				    (build-path (current-directory) f)
				    f)])
		      (printf "~aLoading ~a...~n" indent-string file)
		      (dynamic-wind
		       (lambda ()
			 (set! indent-string
			       (string-append offset-string indent-string)))
		       (lambda () (old-handler file))
		       (lambda ()
			 (set! indent-string
			       (substring indent-string
					  0
					  (max (- (string-length indent-string)
						  (string-length offset-string))
					       0))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    Signatures                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(printf ",DrScheme Jr is loading. Please wait...~n")
(flush-output)

(require-library "refer.ss")
(require-library "cores.ss")
(require-library "macro.ss")
(require-library "cmdline.ss")
(require-library "pconvers.ss")

(require-library "zsigs.ss" "zodiac")
(require-library "sigs.ss" "zodiac")

(require-library "sparams.ss" "backward")
(require-library "ariess.ss" "cogen")
(require-library "userspcs.ss" "userspce")

(require-library "sig.ss" "userspce")

(define-signature prims^ (program argv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              Flag and Language Definitions             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define build-settingU
  (unit/sig ()
    (import [mz : prims^]
	    [cmd-line : mzlib:command-line^]
	    [zodiac : zodiac:system^]
	    [basis : userspace:basis^]
	    [print-convert : mzlib:print-convert^]
	    [mzlib:pretty-print : mzlib:pretty-print^])
    
    (define setting (basis:get-default-setting))

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
       (list "--set-undef"
	     (make-get/set basis:setting-allow-set!-on-undefined?
			   basis:set-setting-allow-set!-on-undefined?!)
	     "set! on undefined variables"
	     basis:setting-allow-set!-on-undefined?)
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
	     "`list' in --print-convert output"
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
       (list "--tag-inexacts"
	     (make-get/set basis:setting-disallow-untagged-inexact-numbers
			   basis:set-setting-disallow-untagged-inexact-numbers!)
	     "#i required for inexact numbers"
	     basis:setting-disallow-untagged-inexact-numbers)
       (list "--whole-frac"
	     (make-get/set basis:setting-whole/fractional-exact-numbers
			   basis:set-setting-whole/fractional-exact-numbers!)
	     "separate whole and fractional parts of exact numbers in printer"
	     basis:setting-whole/fractional-exact-numbers)
       (list "--print-convert"
	     (case-lambda
	      [() (eq? (basis:setting-printing setting) 'constructor-style)]
	      [(x) (basis:set-setting-printing!
		    (if x
			'constructor-style
			'quasi-r4rs-style))])
	     "print values using constructor input syntax"
	     (lambda (setting) (eq? (basis:setting-printing setting) 'constructor-style)))
       (list "--print-convert-quasi"
	     (case-lambda
	      [() (eq? (basis:setting-printing setting) 'quasi-style)]
	      [(x) (basis:set-setting-printing!
		    (if x
			'quasi-style
			'quasi-r4rs-style))])
	     "print values using quasi-quote input syntax"
	     (lambda (setting) (eq? (basis:setting-printing setting) 'quasi-style)))))

    ;; Mapping from language to flag settings

    (define language-levels (map list basis:level-strings basis:level-symbols))

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
	  [(unix) "~/.drscheme-jr.settings"]
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
      (with-input-from-file (get-argv-file)
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
	     s)))))

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
	      (set-level (string->symbol
			  (car (list-ref language-levels
					 (if (eof-object? v)
					     (sub1 len)
					     (sub1 v))))))
	      (begin
		(printf "Please answer 1, 2, 3, or 4~n")
		(choose-mode))))))

    (define (set-level level)
      (let ([p (assoc level (map (lambda (p)
				   (list (string->symbol (car p)) (cadr p)))
				 language-levels))])
	(if p
	    (begin
	      (set! syntax-level (cadr p))
	      (install-level))
	    (bad-arguments "bad language name: ~s" level))))

    (define (make-implies-string level)
      (let ([orig syntax-level]
	    [s (lambda (v) (if v "on" "off"))])
	(when level
	  (set! syntax-level level)
	  (install-level))
	(let ([s (apply
		  string-append
		  (map
		   (lambda (f)
		     (format "~n                     ~a ~a" 
			     (car f)
			     (s ((cadr f)))))
		   flags))])
	  (when level
	    (set! syntax-level orig)
	    (install-level))
	  s)))

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
	      (set-level (string->symbol level)))
	   (,(format "Set the language, one of:~n          ~a"
		     (apply string-append
			    (map (lambda (l)
				   (format " ~a" (car l)))
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
		  (write syntax-level)
		  (newline)
		  (write (map (lambda (s)
				(cons (car s)
				      ((cadr s))))
			      flags))
		  (newline))
		'truncate/replace))
	   (,(format "Save current settings to:~n           ~a" (get-argv-file)))]
	  [("--show")
	   ,(lambda (_) (printf "Current settings: ~a~n" (make-implies-string #f)))
	   ("Show the current settings")]
	  [("--lhelp")
	   ,(lambda (_ level)
	      (set-level (string->symbol level))
	      (printf "~a implies the following flags: ~a~n" 
		      level
		      (make-implies-string syntax-level))
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

(printf "hi~n")

(define dr-jrU
  (unit/sig ()
    (import)
    (define system-parameterization (current-parameterization))
    (define user-parameterization (current-parameterization))

    (define prompt-read
      (let ([prompt "> "])
	(lambda ()
	  (display prompt)
	  (flush-output)
	  (let* ([ip (current-input-port)]
		 [pos (file-position (current-output-port))]
		 [v ((zodiac:read
		      ip
		      (zodiac:make-location 1 1 pos "stdin")))])
	    (if (zodiac:eof? v)
		eof
		v)))))

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

    (define drscheme-jr-print
      (lambda (v)
	(unless (void? v)
	  (let ([value (if (basis:r4rs-style-printing? (basis:current-setting))
			   v
			   (print-convert:print-convert v))])
	    (mzlib:pretty-print:pretty-print-handler value)))))

    (define drscheme-jr-print-load
      (lambda (f)
	(parameterize ([basis:intermediate-values-during-load
			(lambda values
			  (for-each drscheme-jr-print values))])
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

    (error-print-width 200)

    (define params@
      (let ([eq?-only-on-syms eq?-only-on-syms]
	    [allow-.-lists allow-.-lists])
	(unit/sig plt:userspace:params^
	  (import)
	  (define allow-improper-lists allow-.-lists)
	  (define eq?-only-compares-symbols eq?-only-on-syms))))

    (define (repl file restart)
      (let ([parameterization (make-parameterization)])
	(set! user-parameterization parameterization)
	(with-parameterization parameterization
	  (let ([u@ (unit/sig->unit
		     (compound-unit/sig (import)
		       (link
			[params : plt:userspace:params^ (params@)]
			[userspace : plt:userspace^
				   ((require-library-unit/sig "userspcr.ss" "userspce")
				    params)])
		       (export (open params)
			       (open userspace))))])
	    (lambda ()
	      (global-defined-value 'read/zodiac read/zodiac)
	      (global-defined-value 'restart restart)
	      (invoke-open-unit u@)
	      
	      (require-library-use-compiled #f)))))

      (printf "Welcome to DrScheme Jr version ~a, Copyright (c) 1995-98 PLT~n"
	      (version))
      (printf "Language: ~a~n"
	      (cadr (assoc (basis:setting-vocabulary-symbol (basis:current-setting))
			   (map (lambda (p) (list (cadr p) (car p))) language-levels))))
      
      (require-library-use-compiled #f)
      (when (string? file)
	(load/prompt file))

      (current-prompt-read prompt-read)
      (current-print drscheme-jr-print)
      
      (read-eval-print-loop))

    (define (go)
      (let ([file startup-file])
	(let loop ()
	  (let ([continue? #f])
	    (thread-wait
	     (with-parameterization user-parm ;; breaking!
	       (lambda ()
		 (thread
		  (lambda ()
		    (repl file
			  (let ([die
				 (lambda ()
				   (set! continue? #t)
				   (custodian-shutdown-all c))])
			    (rec restart
				 (case-lambda
				  [(new-file)
				   (unless (or (relative-path? file)
					       (absolute-path? file))
				     (raise-type-error 'restart "path string" file))
				   (set! file new-file)
				   (die)]
				  [() (die)]))))))))))
	  (when continue?
	    (loop)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     DrScheme Jr                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dont' require mzlib or pretty-print here; they are linked with zodiac

(require-library "pconver.ss")

(define cu@
  (compound-unit/sig (import)
    (link [mzlib : mzlib:core^ ((require-library "corer.ss"))]
	  [aries : plt:aries^ ((require-library-unit/sig "ariesr.ss" "cogen")
			       (drzodiac : zodiac:system^)
			       (interface : zodiac:interface^))]
	  [interface : drscheme:interface^
		     ((require-library-unit/sig "interface.ss" "userspce") drzodiac)]
	  [drzodiac : drscheme:zodiac^
		    ((require-library-unit/sig "zlink.ss" "userspce")
		     basis
		     (interface : zodiac:interface^)
		     (mzlib pretty-print@)
		     (mzlib file@))]
	  [basis : userspace:basis^
		 ((require-library-unit/sig "basis.ss" "userspce")
		  zodiac
		  interface
		  aries
		  print-convert
		  (mzlib pretty-print@)
		  (mzlib function@))])
    (export)))

