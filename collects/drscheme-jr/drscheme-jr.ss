(reference-library "macro.ss")

(define annotate? (not (equal? (getenv "MZRICESKIPARIES") "yes")))

;; get stdin character positions right.

;; be sure to update the mzlib shell script if this changes
;; can I add flags to mzscheme?

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

(define image-dir (vector-ref argv 0))
(define sys-dir (vector-ref argv 1))
(define argv-in (list->vector (cddr (vector->list argv))))

(define syntax-level 'advanced)
(define use-print-convert? (and (defined? 'mzrice:print-convert)
				mzrice:print-convert))
(define signal-undef #t)
(read-case-sensitive #t)
(compile-allow-set!-undefined #t)
(compile-allow-cond-fallthrough #f)

(define no-arguments-given? #t)

(define simple-args
  (list
   (list '--print-convert (lambda () (set! use-print-convert? #t)))
   (list '--case-sens read-case-sensitive #t)
   (list '--case-insens read-case-sensitive #f)
   (list '--set-undef compile-allow-set!-undefined #t)
   (list '--no-set-undef compile-allow-set!-undefined #t)
   (list '--auto-else compile-allow-cond-fallthrough #t)
   (list '--no-auto-else compile-allow-cond-fallthrough #f)
   (list '--signal-undef (lambda () (set! signal-undef #t)))
   (list '--no-signal-undef (lambda () (set! signal-undef #f)))))

(define (bad-arguments s . args)
  (printf "MzRice error: ~a~n" (apply format s args))
  (exit -1))

(define user-argv
  (let loop ([l (map string->symbol (vector->list argv-in))])
    (if (null? l)
	#()
	(case (car l)
	  [(--level) (set! no-arguments-given? #f)
		     (if (null? (cdr l))
			 (bad-arguments "--level flag expects level name")
			 (let ([level (cadr l)])
			   (case level
			     [(core structured  side-effecting advanced)
			      (set! syntax-level level)
			      (loop (cddr l))]
			     [else
			      (bad-arguments "bad level name: ~s" level)])))]
	  [(--help) (printf "MzRice flags:~n  --help~n  --level level, where level is in: ~s~n~a  --~n"
			    '(core structured  side-effecting advanced)
			    (let loop ([l simple-args])
			      (if (null? l)
				  ""
				  (string-append
				   (format "  ~s~n" (caar l))
				   (loop (cdr l))))))
		    (exit 0)]
	  [(--) (list->vector (cdr l))]
	  [else (let ([f (assq (car l) simple-args)])
		  (if f
		      (begin
			(set! no-arguments-given? #f)
			(apply (cadr f) (cddr f))
			(loop (cdr l)))
		      (let ([s (symbol->string (car l))])
			(if (char=? (string-ref s 0) #\-)
			    (bad-arguments "bad flag: ~s" (car l))
			    (list->vector l)))))]))))

(reference-library "mzlib.ss")
(reference-library "pretty.ss")

(begin-elaboration-time
 (define plt-dir (or (getenv "PLTHOME") "/usr/local/lib/plt"))
 (define zodiac-dir (build-path plt-dir "zodiac")))

(reference (begin-elaboration-time (build-path zodiac-dir "zsigs.ss")))
(reference (begin-elaboration-time (build-path zodiac-dir "sigs.ss")))

(current-library-collection-paths
 (cons (begin-elaboration-time (build-path plt-dir "mred" "collects"))
       (current-library-collection-paths)))

(reference-library "sparams.ss" "backward")
(reference-library "ariess.ss" "cogen")
(reference-library "userspcs.ss" "userspce")
(reference-library "coreu.ss")

(reference-library "pconver.ss")

(define zodiac@ (reference-unit/sig
		 (begin-elaboration-time
		  (build-path plt-dir "zodiac" "link.ss"))))

(define aries@ (reference-library-unit/sig "ariesr.ss" "cogen"))

;; this unit needs to be invoked twice
;; once to build zodiac (syntax for the user)
;; and once to build the userspace unit (primitives for the user)
;; this unit should be obsoleted soon...
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
  (let ([prompt (if use-print-convert?
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
(aries:signal-undefined? signal-undef)

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

(define mzrice-print (if use-print-convert?
			 (compose pretty-print-handler print-convert)
			 pretty-print-handler))

(define mzrice-user-vocabulary
  (zodiac:create-vocabulary 'scheme-w/-user-defined-macros-vocab
			    zodiac:scheme-vocabulary))

(define mzrice-expand-eval
  (let ([primitive-eval (current-eval)])
    (lambda (x)
      (let* ([annotated
	      (with-parameterization system-parameterization
		(lambda ()
		  (let* ([expanded (call/nal zodiac:scheme-expand/nal
					     zodiac:scheme-expand
					     (expression: x)
					     (parameterization: parameterization)
					     (vocabulary: mzrice-user-vocabulary))]
			 [_ '(printf "expanded: ~a~n~n" expanded)]
			 [annotated (if annotate?
					(aries:annotate expanded)
					(zodiac:parsed->raw expanded))]
			 [_ '(printf "annotated: ~a~n~n" annotated)])
		    annotated)))])
	(primitive-eval annotated)))))

(define mzrice-eval
  (lambda (x)
    '(printf "eval; x: ~a~n~n" x)
    (let ([read 
	   (with-parameterization system-parameterization
	     (lambda ()
	       (let* ([z (or (unbox aries:error-box)
			     (let ([loc (zodiac:make-location 0 0 0 'eval)])
			       (zodiac:make-zodiac 'mzrice-eval loc loc)))]
		      [read (zodiac:structurize-syntax x z)])
		 '(printf "eval; read: ~a~n~n" read)
		 read)))])
      (mzrice-expand-eval read))))

(define load-dir/path
  (lambda (f)
    (let-values (((base name must-be-dir?)
		  (split-path (path->complete-path f (current-directory)))))
      base)))

(define mzrice-load
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
		      [(zodiac:eof? next) (mzrice-expand-eval this)]
		      [else (begin (mzrice-expand-eval this)
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
		[params : plt:parameters^ (parameters@)]
		[userspace : plt:userspace^
			   ((reference-library-unit/sig
			     "userspcr.ss" "userspce"))])
	       (export (open userspace))))])
    (lambda ()
      (current-namespace namespace)
      (eval `(#%define argv ,user-argv))
      (eval `(#%define read/zodiac ,read/zodiac))
      (invoke-open-unit u@)
      (read-case-sensitive params:case-sensitive?)
      (current-prompt-read prompt-read)
      (debug-info-handler debug-info)
      (current-print mzrice-print)
      (current-exception-handler exception-handler)
      (current-load mzrice-load) 
      (current-eval mzrice-eval))))

(define (go)
  (when no-arguments-given?
    (with-handlers ([void void]) ; If it fails, no matter
      (unless (directory-exists? image-dir)
        (make-directory image-dir))
      (let ([dir (build-path image-dir sys-dir)])
        (unless (directory-exists? dir)
	  (make-directory dir))
	(let ([file (build-path dir "mzrice")])
	  (with-parameterization parameterization
	    (lambda ()
             (eval `(#%define argv 
			      ,(write-image-to-file 
				file 
				(lambda () user-argv))))))))))
  
  (printf "Welcome to MzRice version ~a, Copyright (c) 1995-97 PLT~n"
	  (version))
  (printf "Language: ~a~nImproper lists: ~a~n"
	  params:check-syntax-level
	  params:allow-improper-lists?)
  
  (current-parameterization parameterization)
  (require-library-use-compiled #f))
  