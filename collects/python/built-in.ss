(module built-in mzscheme
  (require "primitives.ss"
           (lib "etc.ss"))
  (provide object
           staticmethod
           classmethod
           int
           float
           StringType
           None
           NoneType
           type
           ;; built-in functions
           range
           len
           (rename sqrt py-sqrt))
  
  (define object py-object%)
  (define staticmethod py-static-method%)
  (define int py-int%)
  (define float py-float%)
  (define StringType py-string%)
  (define None py-none)
  (define NoneType py-none%)
  (define type py-type%)
  (define classmethod py-classmethod%)
  
  (define (range i)
    (list->py-list%
     (build-list (py-number%->number i) number->py-number%)))
  
  (define (len l)
    (number->py-number% (length (py-list%->list l))))
  
  (define (py-sqrt n)
    (number->py-number% (sqrt (py-number%->number))))
  
  )