(module built-in mzscheme
  (require "primitives.ss")
  (provide object
           staticmethod)
  
  (define object py-object%)
  (define staticmethod py-static-method%))