
;; Poor man's stack-trace-on-exceptions/profiler.
;; See doc.txt for information.

(module errortrace-lib mzscheme
  (require "stacktrace.ss"
           (lib "list.ss") 
           (lib "unitsig.ss"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Test coverage run-time support
  (define test-coverage-enabled (make-parameter #f))
  
  (define test-coverage-info (make-hash-table))
  
  (define (initialize-test-coverage-point key expr)
    (hash-table-put! test-coverage-info key (list #f expr)))
  
  (define (test-covered key)
    (let ([v (hash-table-get test-coverage-info key)])
      (set-car! v #t)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Profiling run-time support

  (define profile-thread #f)
  (define profile-key (gensym))
  
  (define profiling-enabled (make-parameter #f))
  (define profiling-record-enabled (make-parameter #t))
  (define profile-paths-enabled (make-parameter #f))
  
  (define profile-info (make-hash-table))

  (define (initialize-profile-point key name expr)
    (hash-table-put! profile-info key (list (box #f) 0 0 (and name (syntax-e name)) expr null)))
  
  (define (register-profile-start key)
    (and (profiling-record-enabled)
	 (let ([v (hash-table-get profile-info key)])
	   (let ([b (car v)]
		 [v (cdr v)])
	     (set-car! v (add1 (car v)))
	     (when (profile-paths-enabled)
	       (let ([v (cdddr v)])
		 (set-car! v (cons (current-continuation-marks profile-key) (car v)))))
	     (if (unbox b)
		 #f
		 (begin
		   (set-box! b #t)
		   (current-process-milliseconds)))))))
  
  (define (register-profile-done key start)
    (when start
      (let ([v (hash-table-get profile-info key)])
	(let ([b (car v)]
	      [v (cddr v)])
	  (set-box! b #f)
	  (let ([v (cddr (hash-table-get profile-info key))])
	    (set-car! v (+ (- (current-process-milliseconds) start) (car v))))))))
  
  (define (get-profile-results)
    (hash-table-map profile-info (lambda (key val)
                                   (let ([count (cadr val)]
                                         [time (caddr val)]
                                         [name (cadddr val)]
                                         [expr (cadddr (cdr val))]
                                         [cmss (cadddr (cddr val))])
                                     (list count time name expr
                                           (map
                                            (lambda (cms)
                                              (map (lambda (k)
                                                     (let ([v (cdr (hash-table-get profile-info k))])
                                                       (list (caddr v) (cadddr v))))
                                                   cms))
                                            cmss))))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Stacktrace instrumenter

  ;; with-mark : stx (any -> stx) stx -> stx
  (define (with-mark mark make-st-mark expr)
    (with-syntax ([expr expr]
		  [loc (make-st-mark mark)]
		  [key key])
      (execute-point
       mark
       (syntax
	(with-continuation-mark
	    'key
	    loc
	  expr)))))
  
  (define key (gensym 'key))
  
  (define-values/invoke-unit/sig stacktrace^ stacktrace@ #f stacktrace-imports^)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Execute counts

  (define execute-info (make-hash-table))
  
  (define execute-counts-enabled (make-parameter #f))

  (define (register-executed-once key)
    (let ([i (hash-table-get execute-info key)])
      (set-cdr! i (add1 (cdr i)))))

  (define (execute-point mark expr)
    (if (execute-counts-enabled)
	(let ([key (gensym)])
	  (hash-table-put! execute-info key (cons mark 0))
	  (with-syntax ([key (datum->syntax-object #f key (quote-syntax here))]
			[expr expr]
			[register-executed-once register-executed-once]) ; <--- 3D !
	    (syntax
	     (begin
	       (register-executed-once 'key)
	       expr))))
	expr))

  (define (get-execute-counts)
    (hash-table-map execute-info (lambda (k v) v)))

  (define (annotate-executed-file name)
    (let ([name (path->complete-path name (current-directory))])
      (let ([here (filter (lambda (s)
			    (and (equal? name (syntax-source (car s)))
				 (syntax-position (car s))))
			  (get-execute-counts))])
	(let ([sorted (quicksort here (lambda (a b)
					(let ([ap (syntax-position (car a))]
					      [bp (syntax-position (car b))])
					  (or (< ap bp) ; earlier first
					      (and (= ap bp)
						   (let ([as (syntax-span (car a))]
							 [bs (syntax-span (car b))])
						     (or (> as bs) ; wider first at same pos
							 (and (= as bs)
							      ; less called for same region last
							      (> (cdr a) (cdr b))))))))))]
	      [pic (make-string (file-size name) #\space)])
	  ;; fill out picture:
	  (for-each (lambda (s)
		      (let ([pos (sub1 (syntax-position (car s)))]
			    [span (syntax-span (car s))]
			    [key (let ([c (cdr s)])
				   (cond
				    [(zero? c) #\^]
				    [(= c 1) #\.]
				    [else #\,]))])
			(let loop ([p pos])
			  (unless (= p (+ pos span))
			    (string-set! pic p key)
			    (loop (add1 p))))))
		    sorted)
	  ;; Write annotated file
	  (with-input-from-file name
	    (lambda ()
	      (let loop ()
		(let ([pos (file-position (current-input-port))]
		      [line (read-line (current-input-port) 'any)])
		  (unless (eof-object? line)
		    (printf "~a~n" line)
		    (let ([w (string-length line)])
		      ;; Blank out leading spaces in pic:
		      (let loop ([i 0])
			(cond
			 [(and (< i w)
			       (char-whitespace? (string-ref line i)))
			  (string-set! pic (+ pos i) (string-ref line i))
			  (loop (add1 i))]))
		      (printf "~a~n" (substring pic pos (+ pos w))))
		    (loop))))))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Eval handler, exception handler

  (define instrumenting-enabled (make-parameter #t))
  (define error-context-display-depth (make-parameter 10000 (lambda (x) (and (integer? x) x))))
  
  (define (cleanup v)
    (cond
     [(and (pair? v)
	   (memq (car v) '(#%datum #%app #%top)))
      (cleanup (cdr v))]
     [(pair? v)
      (cons (cleanup (car v)) (cleanup (cdr v)))]
     [else v]))

  ;; port exn -> void
  ;; effect: prints out the context surrounding the exception
  (define (print-error-trace p x)
    (let loop ([n (error-context-display-depth)]
               [l (map st-mark-source
		       (continuation-mark-set->list (exn-continuation-marks x) key))])

      (cond
        [(or (zero? n) (null? l)) (void)]
        [(pair? l)
	 (let* ([stx (car l)]
		[file (cond
		       [(string? (syntax-source stx))
			(string->symbol (syntax-source stx))]
		       [(not (syntax-source stx))
			#f]
		       [else
			(string->symbol (format "~a" (syntax-source stx)))])]
		[line (syntax-line stx)]
		[col (syntax-column stx)]
		[pos (syntax-position stx)])
	   (fprintf p "~a~a: ~e~n" 
		    (or file "[unknown source]")
		    (cond
		     [line (format ":~a:~a" line col)]
		     [pos (format "::~a" pos)]
		     [else ""])
		    (cleanup (syntax-object->datum stx)))
	   (loop (- n 1) (cdr l)))])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Profile printer

  (define (output-profile-results paths? sort-time?)
    (profiling-enabled #f)
    (error-print-width 50)
    (printf "Sorting profile data...~n")
    (let* ([sel (if sort-time? cadr car)]
	   [counts (quicksort (filter (lambda (c) (positive? (car c))) (get-profile-results))
			      (lambda (a b) (< (sel a) (sel b))))]
	   [total 0])
      (for-each
       (lambda (c)
	 (set! total (+ total (sel c)))
	 (printf "====================================================================~n")
	 (printf "time = ~a : no. = ~a : ~e in ~s~n" (cadr c) (car c) (caddr c) (cadddr c))
	 ;; print call paths
	 (when paths?
	   (for-each
	    (lambda (cms)
	      (unless (null? cms)
		(printf "  VIA ~e" (caar cms))
		(for-each
		 (lambda (cm)
		   (printf " <- ~e" (car cm)))
		 (cdr cms))
		(printf "~n")))
	    (cadddr (cdr c)))))
       counts)
      (printf "Total samples: ~a~n" total)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define errortrace-eval-handler    
    (let ([orig (current-eval)]
          [ns (current-namespace)])
      (lambda (e)
        (if (and (eq? ns (current-namespace))
		 (not (compiled-expression? (if (syntax? e)
						(syntax-e e)
						e))))
            ;; Loop to flatten top-level `begin's:
            (let loop ([e (if (syntax? e)
			      e
			      (namespace-syntax-introduce
			       (datum->syntax-object #f e)))])
              (let ([top-e (expand-syntax-to-top-form e)])
                (syntax-case top-e (begin)
                  [(begin expr ...)
                   ;; Found a `begin', so expand/eval each contained
                   ;; expression one at a time
                   (foldl (lambda (e old-val)
                            (loop e))
                          (void)
                          (syntax->list #'(expr ...)))]
                  [_else
                   ;; Not `begin', so proceed with normal expand and eval
                   (let* ([ex (expand-syntax top-e)]
                          [a (if (not (instrumenting-enabled))
                                 ex
                                 (annotate-top ex #f))])
                     (orig a))])))
            (orig e)))))
  
  (define errortrace-error-display-handler
    (let ([orig (error-display-handler)])
      (lambda (msg exn)
        (if (exn? exn)
            (let ([p (open-output-string)])
              (display (exn-message exn) p)
              (newline p)
              (print-error-trace p exn)
              (orig (get-output-string p) exn))
            (orig msg exn)))))
  
  (provide errortrace-eval-handler
           errortrace-error-display-handler
           
           print-error-trace 
	   error-context-display-depth 
	   
	   instrumenting-enabled 

	   profiling-enabled
	   profiling-record-enabled
	   profile-paths-enabled 
	   get-profile-results
	   output-profile-results

	   execute-counts-enabled
	   get-execute-counts
	   annotate-executed-file
           
           annotate-top))
 
