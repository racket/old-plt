(module sexp-diff mzscheme
  (require (lib "mz-testing.ss" "tests" "utils"))

  (define-syntax (test-begin stx)
    (syntax-case stx ()
      [(_ expr ...)
       ;#'(begin expr ...) ; testing version
       #'(void) ; non-testing version
       ]))
  
  (test-begin (section 'sexp-diff))
  
  (provide sexp-diff)
  
  (define (sexp-diff a b)
    (cond [(and (null? a) (null? b)) 
           null]
          [(and (pair? a) (pair? b))
           (cons (sexp-diff (car a) (car b))
                 (sexp-diff (cdr a) (cdr b)))]
          [(equal? a b) a]
          [else 'different!]))
  
  (test-begin
   (test null sexp-diff null null)
   (define a `(1 2 (3 4 5 (6 7) () 8) 9))
   (test a sexp-diff a a)
   (define b `(1 0 (3 0 5 0 0 8) 9))
   (test `(1 different! (3 different! 5 different! different! 8) 9) sexp-diff a b)
   
   (report-errs)))

