;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      Debugging                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (getenv "MREDDEBUG")
  (letrec ([old-handler (current-load)]
	   [offset 2]
	   [indent 0]
	   [indent-string ""])
    (current-load
     (lambda (f)
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

(require-library "cmdlines.ss")
(require-library "macro.ss")
(require-library "cmdline.ss")
(require-library "pconvers.ss")

(require-library "sig.ss" "stepper")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       Core                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-library "core.ss" "drscheme-jr")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              Flag and Language Definitions             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define build-settingU
  (unit/sig drscheme-jr:settings^
    (import [mz : prims^]
	    [cmd-line : mzlib:command-line^]
	    [basis : plt:basis^]
	    [mzlib:pretty-print : mzlib:pretty-print^]
	    [mzlib:function : mzlib:function^])

    (define repl? #t)
    (define show-banner? #t)
    (define initialize-userspace void)
    (define run-in-new-user-thread thread)
    
    (define (load-and-repl-done) (void))

    (define setting (basis:get-default-setting))

    (define omit-languages '("MzScheme"))

    ;; Flag definitions

    (define (make-get/set s s!)
      (case-lambda
       [() (s setting)]
       [(x) (s! setting x)]))

    (define teachpack #f)

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
       (list "--auto-else-error"
	     (make-get/set basis:setting-unmatched-cond/case-is-error?
			   basis:set-setting-unmatched-cond/case-is-error?!)
	     "non-matching cond/case produces an error"
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
		    [s (read)]
		    [tp (read)])
		(when (memq l (map (lambda (l) (cadr l)) language-levels))
		  (basis:set-setting-vocabulary-symbol! setting l))
		(for-each
		 (lambda (entry)
		   (let ([tag (car entry)]
			 [value (cdr entry)])
		     (let ([a (assoc tag flags)])
		       (when a
			 ((cadr a) value)))))
		 s)
		(when (string? tp)
		  (set! teachpack tp)
		  (basis:teachpack-changed (list tp)))))))))

    (define (bad-arguments s . args)
      (printf "DrScheme Jr error: ~a~n" (apply format s args))
      (exit 1))

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
      (let ([p (find-level level)])
	(if (and p (not (member level omit-languages)))
	    (set! setting (basis:copy-setting (cadr p)))
	    (bad-arguments "bad language name: ~s" level))))

    (define (find-level level)
      (ormap (lambda (s)
	       (and (let ([a (map char-downcase (string->list level))]
			  [b (map char-downcase (string->list (car s)))])
		      (let loop ([b b])
			(and (pair? b)
			     (or (let loop ([a a][b b])
				   (or (null? a)
				       (and (pair? b)
					    (char=? (car a) (car b))
					    (loop (cdr a) (cdr b)))))
				 (loop (cdr b))))))
		    s))
	     (map (lambda (s)
		    (list (basis:setting-name s)
			  s))
		  basis:settings)))

    (define (make-implies-string vocab-name)
      (let ([impl-setting (if vocab-name
			      (let ([a (find-level vocab-name)])
				(unless a
				  (error 'DrScheme\ Jr "unknown level: ~a~n" vocab-name))
				(cadr a))
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
	  [("-t" "--teachpack")
	   ,(lambda (_ tp)
	      (if (file-exists? tp)
		  (begin
		    (set! teachpack tp)
		    (basis:teachpack-changed (list tp)))
		  (begin
		    (printf "teachpack: \"~a\" does not exist.~n" tp)
		    (exit -1))))
	   ("Set the teachpack to <file>" "file")]
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
		  (mzlib:pretty-print:pretty-print
		   (map (lambda (s) (cons (car s) ((cadr s))))
			flags))
		  (newline)
		  (write teachpack)
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
		      (make-implies-string level))
	      (exit 0))
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

(define go
  (make-go
   (compound-unit/sig
     (import [mz : prims^]
             [basis : plt:basis^]
             [mzlib : mzlib:core^])
     (link
      [cmd-line : mzlib:command-line^ ((require-library "cmdliner.ss"))]
      [settings : drscheme-jr:settings^
                (build-settingU mz
                                cmd-line
                                basis
                                (mzlib pretty-print)
                                (mzlib function))])
     (export (open settings)))))

