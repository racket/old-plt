(module gui-demo2 (lib "frp.ss" "frtime")
  
  (require (lib "gui.scm" "frtime"))
  
  (define (make-op op)
    (lambda-prim (x y)
                 (if (and (number? x) (number? y))
                     (number->string (op x y))
                     "Err")))
  
  (define plus (make-op +))
  (define minus (make-op -))
  (define times (make-op *))
  (define-prim (divide x y)
    (if (and (number? x) (number? y) (not (zero? y)))
        (number->string (/ x y))
        "Err"))
  
  (define op-names (list '+ '- '* '/))
  (define ops (list plus minus times divide))
  
  (define x
    (string->number (make-text "First number:")))
  
  (define op
    (make-choice "Op:" (map symbol->string op-names)))
  
  (define y
    (string->number (make-text "Second number:")))
  
  (make-message
   (string-append "Result = " ((list-ref ops op) x y))))
