(reference-library "macro.ss")
(reference-library "mzlib.ss")
(reference-library "pretty.ss")

(begin-elaboration-time
  (define plt-dir (or (getenv "PLTHOME") "/usr/local/lib/plt")))

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

(define use-print-convert?
  (or (and (defined? 'mzrice:print-convert)
	   mzrice:print-convert)
      (and (<= 1 (vector-length argv))
	   (or (string=? (vector-ref argv 0) "o")
	       (string=? (vector-ref argv 0) "print-convert")))))

(when use-print-convert?
  (reference-library "pconver.ss"))

(reference-library (begin-elaboration-time
		     (build-path plt-dir "lib" "require.ss")))
(plt:require-library "ariesu.ss")
(plt:require-library "sparamu.ss")
(plt:require-library "userspcu.ss")

(reference
  (begin-elaboration-time
    (build-path plt-dir "zodiac" "zsigs")))
(reference
  (begin-elaboration-time
    (build-path plt-dir "zodiac" "sigs")))

(printf "Got here~n") (flush-output)

(define zodiac:system@
  (reference-unit/sig
    (begin-elaboration-time
      (build-path plt-dir "zodiac" "link"))))

(invoke-open-unit/sig plt:mzscheme-parameters@ params)

(define interface@
  (unit/sig zodiac:interface^
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
		   [(zodiac:zodiac? z) (build (zodiac:zodiac-start z) (zodiac:zodiac-finish z))]
		   [(zodiac:eof? z) (build (zodiac:eof-location z) (zodiac:eof-location z))]
		   [(zodiac:period? z) (build (zodiac:period-location z) (zodiac:period-location z))]
		   [else (format "~a: ~a" z msg)])])
	    (if type
		(printf "~a: ~a~n" type pos-msg)
		(printf "~a~n" pos-msg))
	    (escape)))))
    
    (define static-error (report-error #f))
    (define dynamic-error (report-error #f))
    (define internal-error (report-error "internal error"))))

(define parameters@
  (let ([args (vector->list argv)])
    (unit/sig plt:parameters^
      (import)
      (define case-sensitive? (not (eq? 'a 'A)))
      (define unmatched-cond/case-is-error?
	(with-handlers ((void (lambda (e) #t)))
		       (cond)
		       #f))
      (define allow-set!-on-undefined?
	(with-handlers ((void (lambda (e) #f)))
		       (eval `(set! ,(gensym) 5))
		       #t))
      (define check-syntax-level (if (null? args)
				     'advanced
				     (string->symbol (car args))))
      (define allow-improper-lists? (eq? 'advanced check-syntax-level)))))

(define z@
  (compound-unit/sig
      (import)
    (link [params : plt:parameters^ (parameters@)]
          [pretty : mzlib:pretty-print^ (mzlib:pretty-print@)]
          [zodiac : zodiac:system^ (zodiac:system@ zodiac:interface
				     params pretty)]
	  [zodiac:interface : zodiac:interface^ (interface@ zodiac)]
	  [aries : plt:aries^ (plt:aries@ zodiac zodiac:interface)])
    (export (unit params)
	    (unit zodiac)
	    (unit zodiac:interface)
	    (unit aries))))



(invoke-open-unit/sig z@ #f)

(printf "Language: ~a~nImproper lists: ~a~n"
	params:check-syntax-level params:allow-improper-lists?)

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
  (opt-lambda ([port (current-input-port)])
    (let ([v ((zodiac:read
	       port
	       (zodiac:make-location 1 1 0 "port")))])
      (if (zodiac:eof? v)
	  eof
	  (zodiac:sexp->raw v)))))

(define debug-info (lambda () aries:error-box))

(print-struct #t)
(error-print-width 200)

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

(define mzrice-expand-eval
  (let ([primitive-eval (current-eval)])
    (lambda (x)
      (let* ([annotated
	      (with-parameterization system-parameterization
		(lambda ()
		  (let* ([expanded (zodiac:scheme-expand x parameterization)]
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
	      (parameterize ([current-eval (with-parameterization 
					       system-parameterization
					     current-eval)])
		((with-parameterization system-parameterization
		   current-load)
		 f))))
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
				 (loop next (read)))])))))))))

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
		[userspace : plt:userspace^ (plt:userspace@ params)])
	       (export (open userspace))))])
    (lambda ()
      (current-namespace namespace)
      (eval `(#%define argv ,argv))
      (eval `(#%define read/zodiac ,read/zodiac))
      (invoke-open-unit u@)
      (read-case-sensitive params:case-sensitive?)
      (current-prompt-read prompt-read)
      (debug-info-handler debug-info)
      (current-print mzrice-print)
      (current-exception-handler exception-handler)
      (current-load mzrice-load) 
      (current-eval mzrice-eval))))
(current-parameterization parameterization)
