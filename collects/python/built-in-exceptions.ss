#cs(module built-in-exceptions mzscheme
  (require "primitives.ss")
  (provide Exception
           TypeError
           AssertError)
  
  (define Exception py-exception%)
  (define TypeError py-type-error%)
  (define AssertError py-assert-error%)
  
  )