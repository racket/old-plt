(module utils mzscheme
  (require "my-macros.ss")

  (provide
   
   the-undefined-value
   
   improper-map
   improper-foreach)
  

  (define (internal-error . x)
    (error 'stepper:utils "~s" x))
	   
  (define the-undefined-value (letrec ((x x)) x))
  
  (define make-improper
    (lambda (combine)
      (let  ([improper ;; this is for the name in error messages
              (lambda (f list)
                (let improper-loop ([list list])
                  (cond
                    ((null? list) list)
                    ((pair? list) (combine (f (car list))
                                           (improper-loop (cdr list))))
                    (else (f list)))))])
        improper)))
  (define improper-map (make-improper cons))
  (define improper-foreach (make-improper (lambda (x y) y))))
