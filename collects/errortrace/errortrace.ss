
;; Poor man's stack-trace-on-exceptions/profiler.
;; See doc.txt for information.

(module errortrace mzscheme
  (require (lib "kerncase.ss" "syntax")
	   (lib "stx.ss" "syntax")
           "stacktrace.ss"
           (lib "list.ss") 
           (lib "unitsig.ss")
           (lib "pretty.ss"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Profiling run-time support

  (define profile-thread #f)
  (define profile-key (gensym))
  
  (define profiling-enabled (make-parameter #f))
  (define profile-paths-enabled (make-parameter #f))
  
  (define profile-info (make-hash-table))
  
  (define (register-profile-start key)
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
	      (current-process-milliseconds))))))
  
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
  ;; Profiling instrumenter

  (define (profile-point body name expr env trans?)
    (if (profiling-enabled)
        (let ([key (gensym)])
          (hash-table-put! profile-info key (list (box #f) 0 0 (and name (syntax-e name)) expr null))
          (with-syntax ([key (datum->syntax-object #f key (quote-syntax here))]
                        [start (datum->syntax-object #f (gensym) (quote-syntax here))]
                        [profile-key (datum->syntax-object #f profile-key (quote-syntax here))]
                        [register-profile-start register-profile-start]
                        [register-profile-done register-profile-done])
            (with-syntax ([rest 
                           (insert-at-tail*
                            (syntax (register-profile-done 'key start))
                            body
                            trans?)])
              (syntax
               ((let ([start (register-profile-start 'key)])
                  (with-continuation-mark 'profile-key 'key
                                          (begin . rest))))))))
        body))
  
  (define (insert-at-tail* e exprs trans?)
    (if (stx-null? (stx-cdr exprs))
        (list (insert-at-tail e (stx-car exprs) trans?))
        (cons (stx-car exprs) (insert-at-tail* e (stx-cdr exprs) trans?))))
  
  (define (insert-at-tail se sexpr trans?)
    (with-syntax ([expr sexpr]
		  [e se])
      (kernel-syntax-case sexpr trans?
	;; negligible time to eval
	[id
	 (identifier? sexpr)
	 (syntax (begin e expr))]
	[(quote _) (syntax (begin e expr))]
	[(quote-syntax _) (syntax (begin e expr))]
	[(#%datum . d) (syntax (begin e expr))]
	[(#%top . d) (syntax (begin e expr))]

	;; No tail effect, and we want to account for the time
	[(lambda . _) (syntax (begin0 expr e))]
	[(case-lambda . _) (syntax (begin0 expr e))]
	[(set! . _) (syntax (begin0 expr e))]

	[(let-values bindings . body)
	 (with-syntax ([rest (insert-at-tail* se (syntax body) trans?)])
	   (syntax (let-values bindings . rest)))]
	[(letrec-values bindings . body)
	 (with-syntax ([rest (insert-at-tail* se (syntax body) trans?)])
	   (syntax (letrec-values bindings . rest)))]

	[(begin . _)
	 (insert-at-tail* se sexpr trans?)]
	[(with-continuation-mark . _)
	 (insert-at-tail* se sexpr trans?)]

	[(begin0 body ...)
	 (syntax (begin0 body ... e))]

	[(if test then)
	 (with-syntax ([then2 (insert-at-tail se (syntax then) trans?)])
	   (syntax (if test then2)))]
	[(if test then else)
	 ;; WARNING: e inserted twice!
	 (with-syntax ([then2 (insert-at-tail se (syntax then) trans?)]
		       [else2 (insert-at-tail se (syntax else) trans?)])
	   (syntax (if test then2 else2)))]

	[(#%app . rest)
	 (if (stx-null? (syntax rest))
	     ;; null constant
	     (syntax (begin e expr))
	     ;; application; exploit guaranteed left-to-right evaluation
	     (insert-at-tail* se sexpr trans?))]
	
	[_else
	 (error 'errortrace
		"unrecognized (non-top-level) expression form: ~e"
		(syntax-object->datum sexpr))])))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Stacktrace instrumenter

  ;; with-mark : stx stx -> stx
  (define (with-mark mark expr)
    (with-syntax ([expr expr]
		  [source (if (string? (syntax-source mark))
			      (string->symbol (syntax-source mark))
			      (string->symbol (format "~a" (syntax-source mark))))]
		  [line (syntax-line mark)]
		  [col (syntax-column mark)]
		  [key key])
      (syntax
       (with-continuation-mark
	'key
        '(source line . col)
	expr))))
  
  (define key (gensym 'key))
  
  (define-values/invoke-unit/sig stacktrace^ stacktrace@ #f stacktrace-imports^)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Eval handler, exception handler

  (define instrumenting-enabled (make-parameter #t))
  (define error-context-display-depth (make-parameter 10000 (lambda (x) (and (integer? x) x))))
  
  (current-eval
   (let* ([orig (current-eval)]
          [errortrace-eval-handler
           (lambda (e)
	     (let ([a (if (or (compiled-expression? (if (syntax? e) 
                                                        (syntax-e e) 
                                                        e))
                              (not (instrumenting-enabled)))
                          e
                          (annotate-top (expand e) null #f))])
	       (orig a)))])
     errortrace-eval-handler))
  
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
               [l (continuation-mark-set->list (exn-continuation-marks x) key)])
      (cond
        [(or (zero? n) (null? l)) (void)]
        [(pair? l)
         (let ([m (car l)])
           (fprintf p "~a~n"
                    (let ([file (car m)]
			  [line (cadr m)]
			  [col (cddr m)])
		      (format "~a, line ~a, char ~a." 
                              (or file "UNKNOWN")
                              (or line "???")
                              (or col "???")))))
         (loop (- n 1)
               (cdr l))]
        [else (void)])))
  
  (let* ([orig (error-display-handler)]
         [errortrace-error-display-handler
          (lambda (msg exn)
            (if (exn? exn)
                (let ([p (open-output-string)])
                  (display (exn-message exn) p)
                  (newline p)
                  (print-error-trace p exn)
                  (orig (get-output-string p) exn))
                (orig msg exn)))])
    (error-display-handler errortrace-error-display-handler))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Porfile printer

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
  
  (provide print-error-trace 
	   error-context-display-depth 
	   instrumenting-enabled 
	   profiling-enabled
	   profile-paths-enabled 
	   get-profile-results
	   output-profile-results))
 
