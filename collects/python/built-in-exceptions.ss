#cs(module built-in-exceptions mzscheme
  (require "primitives.ss")
  (provide Exception
           TypeError)
  
  (define Exception py-exception%)
  (define TypeError py-type-error%)
  
  )