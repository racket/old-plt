
(require-library "deflateu.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mzlib:deflate^
  mzlib:deflate@)

 
