(define plt-dir (or (getenv "PLTHOME") "/usr/local/lib/plt"))

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
  (require-library "pconver.ss"))

(require-library "mzlib.ss")
(require-library "pretty.ss")

(load-recent (build-path plt-dir "zodiac" "load"))

(require-library (build-path plt-dir "lib" "require.ss"))
(plt:require-library "ariesu.ss")
(plt:require-library "sparamu.ss")
(plt:require-library "ricedefu.ss")

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
		    (format "~a:~a-~a ~a" (zodiac:location-file left)
			    (trans-loc left) (trans-loc right) msg))]
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

(define z@
  (compound-unit/sig
      (import)
    (link [params : plt:parameters^ (plt:mzscheme-parameters@)]
	  [zodiac : zodiac:system^ (zodiac:system@ zodiac:interface params)]
	  [zodiac:interface : zodiac:interface^ (interface@ zodiac)]
	  [aries : plt:aries^ (plt:aries@ zodiac zodiac:interface)])
    (export (unit params)
	    (unit zodiac)
	    (unit zodiac:interface)
	    (unit aries))))


(invoke-open-unit/sig z@ #f)

(define prompt-read
  (let ([prompt (if use-print-convert?
		    "|- "
		    "> ")])
    (lambda ()
      (display prompt)
      (flush-output)
      (let ([v ((zodiac:read
		 (current-input-port)
		 (zodiac:make-location 1 1 0 "stdin")))])
	'(printf "read: ~a~n" v)
	(if (zodiac:eof? v)
	    eof
	    v)))))

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
      (let* ([expanded (zodiac:scheme-expand x)]
	     [_ '(printf "expanded: ~a~n" expanded)]
	     [annotated (aries:annotate expanded)]
	     [_ '(printf "annotated: ~a~n" annotated)])
	(primitive-eval annotated)))))

(define mzrice-eval
  (lambda (x)
    '(printf "eval; x: ~a~n" x)
    (let* ([z (or (unbox aries:error-box)
		  (let ([loc (zodiac:make-location 0 0 0 'eval)])
		    (zodiac:make-zodiac 'mzrice-eval loc loc)))]
	   [read (zodiac:structurize-syntax x z)])
      '(printf "eval; read: ~a~n" read)
      (mzrice-expand-eval read))))

(define mzrice-load
  (lambda (f)
    (call-with-input-file f
      (lambda (p)
	(let ([read (zodiac:read p (zodiac:make-location 1 1 0 f))])
	  (let loop ([v (read)]
		     [last (void)])
	    (if (zodiac:eof? v)
		last
		(loop (read) (mzrice-expand-eval v)))))))))

(define parameterization (make-parameterization))

(define namespace (make-namespace 'no-constants
				  'hash-percent-syntax
				  (if params:unmatched-cond/case-is-error?
				      'no-auto-else
				      'auto-else)
				  (if params:allow-set!-on-undefined?
				      'set!-undefined
				      'no-set!-undefined)))
(with-parameterization parameterization
  (let ([u@ (unit/sig->unit
	     (compound-unit/sig
	       (import)
	       (link
		[params : plt:parameters^ (plt:mzscheme-parameters@)]
		[ricedefs : ricedefs^ (ricedefs@ params)])
	       (export (open ricedefs))))])
    (lambda ()
      (current-namespace namespace)
      (invoke-open-unit u@)
      (read-case-sensitive params:case-sensitive?)
      (current-prompt-read prompt-read)
      (debug-info-handler debug-info)
      (current-print mzrice-print)
      (current-exception-handler exception-handler)
      (current-load mzrice-load) 
      (current-eval mzrice-eval))))

(current-parameterization parameterization)
