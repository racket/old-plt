
(module zo-compile mzscheme
  (require "errortrace-lib.ss")
  
  (provide zo-compile)

  (define zo-do-compile
    (make-errortrace-elaborator compile 
				expand-syntax
				list))

  (define (zo-compile stx)
    (zo-do-compile (namespace-syntax-introduce stx))))


