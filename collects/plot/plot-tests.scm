(module plot-tests mzscheme
  (require 
   (lib "etc.ss")
   (lib "class.ss")
   (lib "file.ss")
   (lib "mred.ss" "mred")
   (lib "posn.ss" "lang")
   (lib "list.ss")
   (lib "math.ss") 
   (lib "test.ss"    "schemeunit")
   
   (lib "plot.scm" "plplot"))
  
  (define plot-tests 
    (make-test-suite 
      "plot-tests"
      (make-test-suite "Helpers"
        (make-test-case "sample-size"
          (assert = 1 (sample-size 15 0 14)))
        (make-test-case "x-values"
          (assert equal? '(0 1 2 3 4 5) (x-values 6 0 5)))
        (make-test-case "make-column"
          (assert equal? (list (make-posn 0 1) (make-posn 0 2) (make-posn 0 3)) (make-column 0 '(1 2 3))))               
        (make-test-case "xy-list"
          (assert equal? (list (make-posn 0 1) (make-posn 0 2) (make-posn 0 3) 
                               (make-posn 1 1) (make-posn 1 2) (make-posn 1 3) 
                               (make-posn 2 1) (make-posn 2 2) (make-posn 2 3)) (xy-list 3 0 2 1 3))))
      (make-test-suite "Math Ops"        
        (make-test-case "derivative" ; x^2 at 2
          (assert apx-equal? 4 ((derivative (lambda (x) (* x x))) 2)))
        (make-test-case "vector"
          (assert equal? (make-posn 4 8) ((make-vec (lambda (x y) (* x x)) (lambda (x y) (* 2 y))) (make-posn 2 4))))
        (make-test-case "gradient"
          (assert apx-equal? (make-posn 2 4) ((gradient (lambda (x y) (+ (* x x) (* 2 y y)))) (make-posn 1 1)))
        ))
      
      (make-test-suite "Renderer Helpers"
        (make-test-case "scale-vectors"
          (assert apx-equal? 
                  (list (make-posn (* 5 9/10) (* 1 9/10)) (make-posn (* 1 9/10) (* 5 9/10)))
                  (scale-vectors (list (make-posn 25 5) (make-posn 5 25)) 5 5)))
        (make-test-case "normalize-vectors"
          #t
        ))
      ))  
  
  (provide plot-tests)
    
  (define delta .00001)
  
  (define (~= a b) 
    (< (abs (- b a)) delta))
  
  ; apx-equal? : any any -> boolean
  ; checks equality, using =~ instead of eqv? on numbers
  (define (apx-equal? a b)
    (cond [(and (pair? a) (pair? b)) (and (apx-equal? (car a) (car b)) (apx-equal? (cdr a) (cdr b)))] ; list case
          [(and (posn? a) (posn? b)) (and (~= (posn-x a) (posn-x b)) (~= (posn-y a) (posn-y b)))] ; posn structure
          [(and (number? a) (number? b)) (~= a b)] ; number
          [else (equal? a b)]))
  )

   

