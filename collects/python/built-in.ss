(module built-in mzscheme
  (require "primitives.ss"
           (lib "etc.ss"))
  (provide object
           staticmethod
           classmethod
           int
           float
           slice
           str
           None
           NoneType
           type
           ;; built-in functions
           range
           len
           (rename py-sqrt sqrt)
           (rename py-map map))
  
  (define object py-object%)
  (define staticmethod py-static-method%)
  (define int py-int%)
  (define float py-float%)
  (define str py-string%)
  (define None py-none)
  (define NoneType py-none%)
  (define type py-type%)
  (define classmethod py-classmethod%)
  (define slice py-slice%)
  
  (define (range i)
    (list->py-list%
     (build-list (py-number%->number i) number->py-number%)))
  
  (define (len l)
    (python-method-call l '__len__))
  
  (define (py-sqrt n)
    (number->py-number% (sqrt (py-number%->number n))))
  
  (define py-map (procedure->py-function%
                  (lambda (fn lst . rest)
    (list->py-list%
     (apply map
            (append (list (py-function%->procedure fn)
                          (py-sequence%->list lst))
                    (map py-sequence%->list rest)))))
                  'map))
                  
          
  
  )