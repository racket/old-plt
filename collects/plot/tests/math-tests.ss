(module math-tests mzscheme
  (require 
   "test-helpers.ss"
   (lib "math.ss" "plot")
   (lib "test.ss" "schemeunit"))
  
  (provide math-tests)
  
  (define math-tests
    (make-test-suite
      "math tests"
      (make-test-case
        "Derivative"
        (make-test-case "derivative" ; x^2 at 2
          (assert apx-equal? 4 ((derivative (lambda (x) (* x x))) 2))))
      (make-test-case
        "Gradient"
        (assert apx-equal? 
                (vector 2 4) 
                ((gradient (lambda (x y) (+ (* x x) (* 2 y y)))) (vector 1 1))))
      (make-test-case
        "Vector Magnitude"
        (assert apx-equal?
                (sqrt 30)
                (vector-magnitude #(1 2 3 4)))) ; technically, vectors can be of any length
      )))
   
    
   