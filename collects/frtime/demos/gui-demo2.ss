(module gui-demo2 (lib "frtime.ss" "frtime")
  
  (require (lib "gui.scm" "frtime")
           (lift (lib "etc.ss") identity))
  
  (define op-names (list "+" "-" "*" "/"))
  (define ops (list + - * /))
  
  (define (str->num s)
    (cond
      [(string->number s) => identity]
      [else 0]))
  
  (define x
    (str->num (make-text "First number:")))
  
  (define op
    (make-choice "Op:" op-names))
  
  (define y
    (str->num (make-text "Second number:")))
  
  (make-message
   (format "Result = ~a" ((list-ref ops op) x y)))
  
  (provide (all-defined-except)))
