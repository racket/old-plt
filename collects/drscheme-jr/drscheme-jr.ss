(reference-library "macro.ss")
(reference-library "cmdline.ss")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      Debugging                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define annotate? (not (equal? (getenv "DRSCHEMEJRSKIPARIES") "yes")))
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
;;;              Flag and Language Definitions             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define syntax-level 'advanced)
(define use-print-convert (make-parameter #f))
(define signal-undef (make-parameter #f))
(define allow-.-lists (make-parameter #f))
(define eq?-only-on-syms (make-parameter #f))
(define cond-is-boolean (make-parameter #f))
(define inexact-needs-#i (make-parameter #f))
(define print-with-list (make-parameter #f))
(define print-with-sharing (make-parameter #f))

;; Flag definitions

(define flags
  (list
   (list "--case-sens"
	 read-case-sensitive
	 "case-sensitive symbols and variables")
   (list "--set-undef"
	 compile-allow-set!-undefined
	 "set! on undefined variables")
   (list "--auto-else"
	 compile-allow-cond-fallthrough
	 "auto `else' clause in `cond' and `case'")
   (list "--signal-undef"
	 signal-undef
	 "error if using #<undefined> variable")
   (list "--eq-syms"
	 eq?-only-on-syms
	 "eq? only for symbols")
   (list "--improper-lists"
	 allow-.-lists
	 "improper lists")
   (list "--boolean-conds"
	 cond-is-boolean
	 "conditionals must be #t or #f")
   (list "--tag-inexacts"
	 inexact-needs-#i
	 "#i required for inexact numbers")
   (list "--print-convert"
	 use-print-convert
	 "`read'able printing")
   (list "--print-list"
	 print-with-list
	 "`list' in `read'able printing")
   (list "--print-sharing"
	 print-with-sharing
	 "sharing in `read'able printing")))

;; Mapping from language to flag settings

(define language-levels
  '(("Beginner" core)
    ("Intermediate" structured)
    ("Advanced" side-effecting)
    ("R4RS+" advanced)))

(define (install-level)
  (read-case-sensitive #t)
  (use-print-convert #t)
  (signal-undef #t)
  (compile-allow-set!-undefined #f)
  (compile-allow-cond-fallthrough #f)
  (allow-.-lists #f)
  (cond-is-boolean #f)
  (eq?-only-on-syms #t)
  (inexact-needs-#i #f)
  (print-with-list #t)
  (print-with-sharing #t)
  (case syntax-level
    [(core)
     (cond-is-boolean #t)
     (inexact-needs-#i #t)
     (print-with-sharing #f)]
    [(structured)
     (cond-is-boolean #t)
     (inexact-needs-#i #t)
     (print-with-sharing #f)]
    [(side-effecting)
     (void)]
    [(advanced)
     (eq?-only-on-syms #f)
     (allow-.-lists #t)
     (use-print-convert #f)
     (compile-allow-set!-undefined #t)]
    [else #f]))

;; Initialize to R4RS+ settings
(install-level)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                 Setting the Flags                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-argv-file)
  (case (system-type)
    [(unix) "~/.drscheme-jr.settings"]
    [(macos) (build-path (find-executable-path program "DrScheme Jr")
			 "DrScheme Jr Settings")]
    [(windows) (build-path (find-executable-path program "DrScheme Jr.exe")
			   "DrScheme Jr Settings.txt")]))

(with-handlers ([void void])
  (with-input-from-file (get-argv-file)
    (lambda ()
      (let ([l (read)]
	    [s (read)])
	(when (memq l (map (lambda (l) (cadr l)) language-levels))
	   (set! syntax-level l))
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
  (let loop ([n 1][l language-levels])
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
      (if (or (eof-object? v) (and (number? v) (<= 1 v len)))
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
	(bad-arguments "bad level name: ~s" level))))

(define (make-implies-string level)
  (let ([orig syntax-level]
	[s (lambda (v) (if v "on" "off"))])
    (set! syntax-level level)
    (install-level)
    (let ([s (apply
	      string-append
	      (map
	       (lambda (f)
		 (format "~n                     ~a ~a" 
			 (car f)
			 (s ((cadr f)))))
	       flags))])
      (set! syntax-level orig)
      (install-level)
      s)))

(define (on? v)
  (cond
   [(string=? v "on") #t]
   [(string=? v "off") #f]
   [else (bad-arguments "expected `on' or `off', given: ~a" v)]))

(define (make-on/off s)
  (list (format "Enable/disable ~a" s)
	"on/off"))

(define user-argv
  (parse-command-line
   "DrScheme Jr"
   argv
   `([multi
      ,@(map
	 (lambda (f)
	   `[(,(car f))
	     ,(lambda (_ v) ((cadr f) (on? v)))
	     ,(make-on/off (caddr f))])
	 flags)]
     [once-each
      [("-l" "--language")
       ,(lambda (_ level)
	  (set-level (string->symbol level)))
       (,(format "Set the language, one of:~n          ~a"
		 (apply string-append
			(map (lambda (l)
			       (format " ~a" (car l)))
			     language-levels)))
	"language")]
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
       (,(format "Save current settings to ~a" (get-argv-file)))]
      [("--lhelp")
       ,(lambda (_ level)
	  (set-level (string->symbol level))
	  (printf "~a implies the following flags: ~a~n" 
		  level
		  (make-implies-string syntax-level))
	  (exit))
       ("Show the flags implies by a particular language" "language")]])
   (lambda (accum . rest) rest)
   '("arg")
   (lambda (s)
     (display s)
     (printf " If ~a exists, it initializes the language settings.~n"
	     (get-argv-file))
     (exit 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                     DrScheme Jr                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reference-library "mzlib.ss")
(reference-library "pretty.ss")

(reference-library "zsigs.ss" "zodiac")
(reference-library "sigs.ss" "zodiac")

(reference-library "sparams.ss" "backward")
(reference-library "ariess.ss" "cogen")
(reference-library "userspcs.ss" "userspce")
(reference-library "coreu.ss")

(reference-library "pconver.ss")

(define zodiac@ (reference-library-unit/sig "link.ss" "zodiac"))

(define aries@ (reference-library-unit/sig "ariesr.ss" "cogen"))

(define params@ [require-library "paramr.ss" "userspce"])
(define parameters@
  (let ([syntax-level syntax-level])
    (unit/sig plt:parameters^
      (import)
      (define case-sensitive? (read-case-sensitive))
      (define unmatched-cond/case-is-error?
	(compile-allow-cond-fallthrough))
      (define allow-set!-on-undefined?
	(compile-allow-set!-undefined))
      (define check-syntax-level syntax-level)
      (define allow-improper-lists? (eq? 'advanced check-syntax-level)))))

(define z@
  (compound-unit/sig (import)
    (link [params : plt:parameters^ (parameters@)]
	  [mzlib-core : mzlib:core^
		      (mzlib:core@)]
	  [zodiac:interface : zodiac:interface^
			    ((unit/sig zodiac:interface^
			       (import (zodiac : zodiac:system^))
			       (define report-error
				 (lambda (type)
				   (lambda (z s . args)
				     (let* ([msg (apply format s args)]
					    [trans-loc
					     (lambda (loc)
					       (format "~a.~a"
						       (zodiac:location-line loc)
						       (zodiac:location-column loc)))]
					    [build
					     (lambda (left right)
					       (format "~a[~a-~a]:~a-~a~n   ~a"
						       (zodiac:location-file left)
						       (zodiac:location-offset left)
						       (zodiac:location-offset right)
						       (trans-loc left)
						       (trans-loc right)
						       msg))]
					    [escape (error-escape-handler)]
					    [pos-msg
					     (cond
					       [(zodiac:zodiac? z)
						(build (zodiac:zodiac-start z)
						       (zodiac:zodiac-finish z))]
					       [(zodiac:eof? z)
						(build (zodiac:eof-location z)
						       (zodiac:eof-location z))]
					       [(zodiac:period? z)
						(build (zodiac:period-location z)
						       (zodiac:period-location z))]
					       [else (format "~a: ~a" z msg)])])
				       (if type
					   (printf "~a: ~a~n" type pos-msg)
					   (printf "~a~n" pos-msg))
				       (escape)))))
			       
			       (define static-error (report-error #f))
			       (define dynamic-error (report-error #f))
			       (define internal-error (report-error "internal error")))
			     zodiac)]
	  [zodiac : zodiac:system^ (zodiac@
				    zodiac:interface
				    params
				    (mzlib-core pretty-print@)
				    (mzlib-core file@))]
	  [aries : plt:aries^ (aries@
			       zodiac zodiac:interface)])
    (export (unit params)
	    (unit zodiac)
	    (unit zodiac:interface)
	    (unit aries))))

(invoke-open-unit/sig z@ #f)

(define system-parameterization (current-parameterization))

(define prompt-read
  (let ([prompt (if (use-print-convert)
		    "|- "
		    "> ")])
    (lambda ()
      (display prompt)
      (flush-output)
      (let* ([ip (current-input-port)]
	     [pos (file-position (current-output-port))]
	     [v ((zodiac:read
		  ip
		  (zodiac:make-location 1 1 pos "stdin")))])
	'(printf "read: ~a~n" v)
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

(define debug-info (lambda () aries:error-box))

(print-struct #t)
(error-print-width 200)
(aries:signal-undefined (signal-undef))

(define exception-handler
  (lambda (exn)
    (let ([report (exn-debug-info exn)])
      (cond
	[(and (not (void? report)) 
	      report
	      (unbox report))
	 =>
	 (lambda (ub)
	   (zodiac:interface:dynamic-error ub "~a" (exn-message exn)))]
	[else
	 (zodiac:interface:dynamic-error #f
					 "~a"
					 (exn-message exn))]))))

(define drscheme-jr-print (if (use-print-convert)
			      (compose pretty-print-handler print-convert)
			      pretty-print-handler))

(show-sharing #t)
(constructor-style-printing #t)
(quasi-read-style-printing #t)

(define drscheme-jr-user-vocabulary
  (zodiac:create-vocabulary 'scheme-w/-user-defined-macros-vocab
			    zodiac:scheme-vocabulary))

(define drscheme-jr-expand-eval
  (let ([primitive-eval (current-eval)])
    (lambda (x)
      (let* ([annotated
	      (with-parameterization system-parameterization
		(lambda ()
		  (let* ([expanded (call/nal zodiac:scheme-expand/nal
					     zodiac:scheme-expand
					     (expression: x)
					     (vocabulary: drscheme-jr-user-vocabulary))]
			 [_ '(printf "expanded: ~a~n~n" expanded)]
			 [annotated (if annotate?
					(aries:annotate expanded)
					(zodiac:parsed->raw expanded))]
			 [_ '(printf "annotated: ~a~n~n" annotated)])
		    annotated)))])
	(primitive-eval annotated)))))

(define drscheme-jr-eval
  (lambda (x)
    '(printf "eval; x: ~a~n~n" x)
    (let ([read 
	   (with-parameterization system-parameterization
	     (lambda ()
	       (let* ([z (or (unbox aries:error-box)
			     (let ([loc (zodiac:make-location 0 0 0 'eval)])
			       (zodiac:make-zodiac 'drscheme-jr-eval loc loc)))]
		      [read (zodiac:structurize-syntax x z)])
		 '(printf "eval; read: ~a~n~n" read)
		 read)))])
      (drscheme-jr-expand-eval read))))

(define load-dir/path
  (lambda (f)
    (let-values (((base name must-be-dir?)
		  (split-path (path->complete-path f (current-directory)))))
      base)))

(define drscheme-jr-load
  (let ([zo-file?
	 (lambda (f)
	   (let ([l (string-length f)])
	     (and (<= 3 l)
		  (string=? ".zo" (substring f (- l 3) l)))))]
	[old-handler (current-load)])
    (lambda (f)
      (if (zo-file? f)
	  (with-parameterization parameterization
	    (lambda ()
	      (parameterize ((current-eval (with-parameterization 
					       system-parameterization
					     current-eval))
			     (current-load-relative-directory
			      (load-dir/path f)))
		((with-parameterization system-parameterization
		   current-load)
		 f))))
	  (parameterize ((current-load-relative-directory
			  (load-dir/path f)))
	    (call-with-input-file f
	      (lambda (p)
		(let ([read (let* ([fn
				    (normalize-path
				     (if (relative-path? f)
					 (build-path (current-directory) f)
					 f))]
				   [t (with-parameterization system-parameterization
					(lambda ()
					  (zodiac:read
					   p
					   (zodiac:make-location
					    1 1 0
					    fn))))])
			      (lambda ()
				(with-parameterization system-parameterization
				  t)))])
		  (let loop ([this (read)]
			     [next (read)])
		    (cond
		      [(zodiac:eof? this) (void)]
		      [(zodiac:eof? next) (drscheme-jr-expand-eval this)]
		      [else (begin (drscheme-jr-expand-eval this)
				   (loop next (read)))]))))))))))

(define parameterization (make-parameterization))

(define namespace (make-namespace 'no-constants
				  (if annotate?
				      'hash-percent-syntax
				      'all-syntax)))

(with-parameterization parameterization
  (let ([u@ (unit/sig->unit
	     (compound-unit/sig
		 (import)
	       (link
		[params : plt:userspace:params^ (params@)]
		[userspace : plt:userspace^
			   ((reference-library-unit/sig
			     "userspcr.ss" "userspce")
			    params)])
	       (export [open params]
		       (open userspace))))])
    (lambda ()
      (current-namespace namespace)
      (eval `(#%define argv ,(list->vector user-argv)))
      (eval `(#%define read/zodiac ,read/zodiac))
      (invoke-open-unit u@)
      (eval `(allow-improper-lists ,(allow-.-lists)))
      (eval `(eq?-only-compares-symbols ,(eq?-only-on-syms)))
      (current-prompt-read prompt-read)
      (debug-info-handler debug-info)
      (current-print drscheme-jr-print)
      (current-exception-handler exception-handler)
      (current-load drscheme-jr-load) 
      (current-eval drscheme-jr-eval))))

(define (go)
  (printf "Welcome to DrScheme Jr version ~a, Copyright (c) 1995-98 PLT~n"
	  (version))
  (printf "Language: ~a~n"
	  (cadr (assoc params:check-syntax-level
		       (map (lambda (p) (list (cadr p) (car p))) language-levels))))
  
  (current-parameterization parameterization)
  (require-library-use-compiled #f))
  
