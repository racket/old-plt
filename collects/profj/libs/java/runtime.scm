;Java runtime utilities
;Kathryn Gray 
;July 2001

;This module provides functions needed at runtime for compiled Java code

#cs
(module runtime mzscheme
  
  (require (lib "class.ss")
           (lib "Object.ss" "profj" "libs" "java" "lang")
           (lib "String.ss" "profj" "libs" "java" "lang")
           (lib "Throwable.ss" "profj" "libs" "java" "lang")
           (lib "ArithmeticException.ss" "profj" "libs" "java" "lang")
           (lib "ClassCastException.ss" "profj" "libs" "java" "lang")
           (lib "NullPointerException.ss" "profj" "libs" "java" "lang"))
  
  (provide convert-to-string shift not-equal bitwise mod divide-int 
           divide-float and or cast-primitive cast-reference nullError)
  
  ;convert-to-string: (U string int real bool char Object) -> string
  (define (convert-to-string data)
    (cond
      ((string? data) (make-java-string data))
      ((number? data) (make-java-string (number->string data)))
      ((boolean? data) 
       (make-java-string (if data "true" "false")))
      ((char? data) (make-java-string (string data)))
      ((is-a? data ObjectI) (send data toString))
      ((is-a? data object%) (make-java-string "SchemeObject"))
      (else (error 'JavaRuntime:Internal_Error:convert-to-string
                   (format "Convert to string given unsupported data: ~s" data)))))
  
  ;Performs arithmetic shifts on the given integers. 
  ;shift: symbol int int -> int
  (define (shift op left right)
    (case op
      ((<<) (arithmetic-shift left right))
      ((>>) (arithmetic-shift left (- right)))
      ((>>>) (+ (arithmetic-shift left (- right)) (arithmetic-shift 2 (bitwise-not right))))))
 
  ;not-equal: num num -> bool
  (define (not-equal left right) (not (= left right)))
  
  ;bitwise: symbol (U (int int) (bool bool)) -> int
  (define (bitwise op left right)
    (if (number? left)
        (case op
          ((&) (bitwise-and left right))
          ((^) (bitwise-xor left right))
          ((or) (bitwise-ior left right)))
        (case op
          ((&) (and left right))
          ((^) (and (not (and left right))
                    (or left right)))
          ((or) (or left right)))))

  ;divide-int: int int -> int
  (define (divide-int left right)
    (when (zero? right)
      (create-java-exception ArithmeticException
                             "Illegal division by zero"
                             (lambda (exn msg)
                               (send exn ArithmeticException-constructor-java.lang.String msg))
                             (current-continuation-marks)))
    (quotient left right))
  
  ;divide-float: float float -> float
  (define (divide-float left right)
    (when (zero? right)
      (raise (create-java-exception ArithmeticException
                                    "Illegal division by zero"
                                    (lambda (exn msg)
                                      (send exn ArithmeticException-constructor-java.lang.String msg))
                                    (current-continuation-marks))))
    (if (and (exact? left) (exact? right))
        (exact->inexact (/ left right))
        (/ left right)))
     
  ;modulo: number number -> number
  (define (mod left right)
    (when (zero? right)
      (raise (create-java-exception ArithmeticException
                                    "Illegal division by zero"
                                    (lambda (exn msg)
                                      (send exn ArithmeticException-constructor-java.lang.String msg))
                                    (current-continuation-marks))))
    (remainder left right))
  
  (define (raise-class-cast msg)
    (raise (create-java-exception ClassCastException
                                  msg
                                  (lambda (exn msg)
                                    (send exn ClassCastException-constructor-java.lang.String msg))
                                  (current-continuation-marks))))
  
  ;cast-primitive: value symbol -> value
  (define (cast-primitive val type)
    (case type
      ((boolean) 
       (unless (boolean? val)
         (raise-class-cast (format "Cast to boolean failed for ~a" 
                                   (send (convert-to-string val) get-mzscheme-string))))
       val)
      ((byte short int long)
       (cond
         ((and (number? val) (inexact? val)) (inexact->exact val))
         ((and (number? val) (exact? val)) val)
         ((char? val) (char->integer val))
         (else (raise-class-cast (format "Cast to ~a failed for ~a"
                                         type
                                         (send (convert-to-string val) get-mzscheme-string))))))
      ((char)
       (cond
         ((char? val) val)
         ((and (number? val) (exact? val)) (integer->char val))
         (else (raise-class-cast (format "Cast to character failed for ~a"
                                         (send (convert-to-string val) get-mzscheme-string))))))
      ((float double)
       (cond
         ((number? val) val)
         ((char? val) (char->integer val))
         (else (raise-class-cast (format "Cast to ~a failed for ~a" type
                                         (send (convert-to-string val) get-mzscheme-string))))))))
  
  ;cast-reference: value class symbol-> value
  (define (cast-reference val type name)
    (if (is-a? val type)
        val
        (raise-class-cast (format "Cast to ~a failed for ~a" name (send val my-name)))))
  
  ;nullError: symbol -> void
  (define (nullError kind)
    (raise
     (create-java-exception NullPointerException
                            (case kind
                              ((method) 
                               "This value cannot access a method to call as it is null and therefore has no methods")
                              ((field) 
                               "This value cannot retrieve a field as it is null and therefore has no fields"))
                            (lambda (exn msg)
                              (send exn NullPointerException-constructor-java.lang.String msg))
                            (current-continuation-marks))))
  
  )