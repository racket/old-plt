#cs(module built-in mzscheme
  (require "primitives.ss"
           "runtime-support.ss"
           (lib "list.ss")
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
           (rename py-map map)
           (rename py-filter filter)
           isinstance)
  
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
     
   (define py-filter (py-lambda 'filter (fn lst)
                                (list->py-list% (filter (lambda (x)
                                                          (py-call fn (list x)))
                                                        (py-sequence%->list lst)))))
     
   (define isinstance (py-lambda 'isinstance (obj type)
                                 (bool->py-number% (py-is-a? obj type))))
                  


  
  )