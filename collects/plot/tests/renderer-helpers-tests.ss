(module renderer-helpers-tests mzscheme
  (require 
   "test-helpers.ss"
   (lib "renderer-helpers.ss" "plot")
   (lib "test.ss" "schemeunit"))
  
  (provide renderer-helpers-tests)
  
  (define renderer-helpers-tests
    (make-test-suite
      "Renderer Helpers"
      (make-test-case
        "Sample Size"
        (assert-eq? 1 (sample-size 15 0 14)))
      (make-test-case
        "X-values" ; should really be renamed to something else, like samples
        (assert-equal? 
         '(0 1 2 3 4 5)
         (x-values 6 0 5)))
      (make-test-case
        "Make Column"
        (assert equal? '(#(0 1) #(0 2) #(0 3)) (make-column 0 '(1 2 3))))
      (make-test-case
        "XY-List"
        (assert-equal?
         '(#(0 1) #(0 2) #(0 3)
            #(1 1) #(1 2) #(1 3)
            #(2 1) #(2 2) #(2 3))
         (xy-list 3 0 2 1 3)))
      (make-test-case
        "z-grid"
        (assert-equal?
         '((0 10 20) ; x is 0
           (1 11 21)    ;x is 1
           (2 12 22))
         (zgrid (lambda (x y) (+ x (* 10 y))) '(0 1 2) '(0 1 2) 3)))
      (make-test-case
        "scale-vectors"
        (assert apx-equal? 
                  (list (vector (* 5 9/10) (* 1 9/10)) (vector (* 1 9/10) (* 5 9/10)))
                  (scale-vectors '(#(25 5) #(5 25)) 5 5)))
      )))
        
    