(module sexp-diff mzscheme
  (require (lib "mz-testing.ss" "tests" "utils"))

  (define-syntax (test-begin stx)
    (syntax-case stx ()
      [(_ expr ...)
       ;#'(begin expr ...) ; testing version
       #'(void) ; non-testing version
       ]))
  
  (test-begin (section 'sexp-diff))

  ; sexp-diff and sexp-diff/expound show the difference between two specified s-expressions.
  ; in each case, the part of the s-expression that is the same is preserved in the result. When
  ; traversal reveals a difference, the point that is different is replaced with either the symbol
  ; different! (in the sexp-diff function) or a list of the form (different! <thing-from-a> <thing-from-b>)
  ; in the sexp-diff/expound function.
  
  (provide sexp-diff sexp-diff/expound)
  
  (define (sexp-diff/core expound?)
    (letrec ([sexp-diff 
              (lambda (a b)
                (cond [(and (null? a) (null? b)) 
                       null]
                      [(and (pair? a) (pair? b))
                       (cons (sexp-diff (car a) (car b))
                             (sexp-diff (cdr a) (cdr b)))]
                      [(equal? a b) a]
                      [else (if expound? 
                                (list 'different! a b)
                                'different!)]))])
      sexp-diff))
  
  (define sexp-diff (sexp-diff/core #f))
  (define sexp-diff/expound (sexp-diff/core #t))
  
  (test-begin
   (test null sexp-diff null null)
   (define a `(1 2 (3 4 5 (6 7) () 8) 9))
   (test a sexp-diff a a)
   (define b `(1 0 (3 0 5 0 0 8) 9))
   (test `(1 different! (3 different! 5 different! different! 8) 9) sexp-diff a b)
   
   (test null sexp-diff/expound null null)
   (test a sexp-diff/expound a a)
   (test `(1 (different! 2 0) (3 (different! 4 0) 5 (different! (6 7) 0) (different! () 0) 8) 9) sexp-diff/expound a b)
   
   (report-errs)))

