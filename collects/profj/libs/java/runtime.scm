;Java runtime utilities
;Kathryn Gray 
;July 2001

;This module provides functions needed at runtime for compiled Java code

#cs
(module runtime mzscheme
  
  (require (lib "class.ss")
           (lib "Object.ss" "profj" "libs" "java" "lang")
           (lib "String.ss" "profj" "libs" "java" "lang"))
  
  (provide convert-to-string shift not-equal bitwise remainder and or quotient)
  
  ;convert-to-string: (U string int real bool char Object) -> string
  (define convert-to-string
    (lambda (data)
      (cond
       ((string? data) (make-java-string data))
       ((number? data) (make-java-string (number->string data)))
       ((boolean? data) 
	(make-java-string (if data
                              "true"
                              "false")))
       ((char? data) (make-java-string (string data)))
       ((is-a? data ObjectI) (send data toString))
       ((is-a? data object%) (make-java-string "SchemeObject"))
       (else (error 'JavaRuntime:Internal_Error:convert-to-string
                    (format "Convert to string given unsupported data: ~s" data))))))
  
  ;Performs arithmetic shifts on the given integers. 
  ;shift: symbol int int -> int
  (define shift
    (lambda (op left right)
      (case op
        ((<<) (arithmetic-shift left right))
        ((>>) (arithmetic-shift left (- right)))
        ((>>>) (+ (arithmetic-shift left (- right)) (arithmetic-shift 2 (bitwise-not right)))))))
 
  ;not-equal: num num -> bool
  (define not-equal
    (lambda (left right)
      (not (= left right))))
  
  ;bitwise: symbol (U (int int) (bool bool)) -> int
  (define bitwise
    (lambda (op left right)
      (if (number? left)
          (case op
            ((&) (bitwise-and left right))
            ((^) (bitwise-xor left right))
            ((or) (bitwise-ior left right)))
          (case op
            ((&) (and left right))
            ((^) (and (not (and left right))
                      (or left right)))
            ((or) (or left right))))))
     
  )     