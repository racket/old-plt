; the idea behind breakpoint-token is to provide an eq?-testable token which the annotator
; can recognize and replace with a real breakpoint.

(module breakpoint-token mzscheme
  (provide break)
  
  (define (break) 3))


    
  