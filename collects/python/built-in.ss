(module built-in mzscheme
  (require "primitives.ss")
  (provide object
           staticmethod
           classmethod
           int
           float
           StringType
           None
           NoneType
           type)
  
  (define object py-object%)
  (define staticmethod py-static-method%)
  (define int py-int%)
  (define float py-float%)
  (define StringType py-string%)
  (define None py-none)
  (define NoneType py-none%)
  (define type py-type%)
  (define classmethod py-classmethod%))