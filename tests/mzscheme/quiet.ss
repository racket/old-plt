
(namespace-variable-value 
 'quiet-load
 #f
 (lambda ()
   (namespace-set-variable-value! 'quiet-load "all.ss")))

(let ([p (make-custom-output-port #f (lambda (str s e flush?) (- e s)) void void)])
  (parameterize ([current-output-port p])
      (load-relative quiet-load))
  (report-errs))
