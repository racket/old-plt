; test the low level libraries - does not require mred
(module fit-low-level-tests mzscheme
  
  (require
   "test-helpers.ss"
   (lib "test.ss" "schemeunit")   
   (lib "fit-low-level.ss" "plot"))    
  
  (provide
   fit-low-level-tests)
  
  (define 
    fit-low-level-tests
    (make-test-suite
     "fit module low level tests"
     (make-test-case
       "simple fit"
       (assert apx-equal?
               (fit-internal
                (lambda (x y m b) (+ b (* m x))) ; mx + b
                '(0 1 2 3) ; x independant data
                '(0 1 2 3) ; y ind data - must be provided, even if ignored by lambda
                '(3 5 7 9) ; result data
                '(1 1 1 1) ; error data - 1 means all are wieghed equally
                '(1 1)) ; guesses - all must be provided
                '(2 3)))
      ; need to find a case that actually diverges..
;      (make-test-case
;        "a diverging fit?"
;        (assert eq? null
;                (fit-internal
;                 (lambda (x y m b) (+ b (* m x))) ; mx + b
;                 '(0 1 2 3) ; x independant data
;                 '(0 1 2 3) ; y ind data - must be provided, even if ignored by lambda
;                 '(123 2 -123 412) ; result data
;                 '(1 10 1000 1111) ; error data - 1 means all are wieghed equally
;                 '(1231 12)); guesses - all must be provided
;                ))
      ))
  )
                
                
                
                



                
                
                
       
       
       
       
       