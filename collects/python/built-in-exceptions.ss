#cs(module built-in-exceptions mzscheme
  (require "primitives.ss")
  (provide Exception
           TypeError
           AssertError
           IndexError
           SystemError
           FutureWarning
           )
  
  (define Exception py-exception%)
  (define TypeError py-type-error%)
  (define AssertError py-assert-error%)
  
  (define IndexError py-index-error%)
  (define SystemError py-system-error%)
  (define FutureWarning py-future-warning%)
     
  )