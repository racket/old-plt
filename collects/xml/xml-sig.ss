
(module xml-sig mzscheme
  (import (lib "unitsig.ss"))
  
  (import "private/sig.ss")

  (define-signature xml^ ((open xml-structs^) (open reader^) (open writer^) (open xexpr^) (open space^)))

  (export xml^))



