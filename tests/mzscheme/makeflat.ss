
(unless (defined? 'flat-load)
   (global-defined-value 'flat-load "all.ss"))
(unless (defined? 'lines-per-file)
   (global-defined-value 'lines-per-file +inf.0))

(import (lib "pretty.ss"))


(define line-count 0)
(define file-count 0)

(define flatp (open-output-file "flat.ss" 'replace))
(define old-eval (current-eval))
(define old-namespace (current-namespace))

(pretty-print '(define error-test void) flatp)
(pretty-print '(define building-flat-tests #t) flatp)
(pretty-print '(define section #f) flatp)

(define (flat-pp v)
  (pretty-print (if (syntax? v) (syntax->datum v) v) flatp)
  (set! line-count (add1 line-count))
  (when (>= line-count lines-per-file)
    (set! line-count 0)
    (set! file-count (add1 file-count))
    (close-output-port flatp)
    (set! flatp
	  (open-output-file
	   (format "flat~a.ss" file-count)
	   'replace))))

(define error-test
  (case-lambda
   [(expr) (error-test expr #f)]
   [(expr exn?)
    (unless (eq? exn? exn:syntax?)
      (let ([dexpr (syntax->datum expr)])
	(flat-pp 
	 `(thunk-error-test (lambda () ,dexpr)
			    (quote ,dexpr)
			    ,@(if exn?
				  (list (string->symbol
					 (primitive-name
					  exn?)))
				  null)))))]))

(define building-flat-tests #t)

(dynamic-wind
 (lambda () 
   (current-eval
    (lambda (e)
      (unless (or (and (pair? e)
		       (memq (car e) '(load load-relative error-test)))
		  (not (eq? (current-namespace) old-namespace))
		  ;; Skip test use of `eval' on unprintable value:
		  (and (pair? e) (pair? (cdr e))
		       (eq? void (cadr e))))
	(flat-pp e))
      (old-eval e))))
 (lambda ()
   (load-relative flat-load))
 (lambda ()
   (current-eval old-eval)))
