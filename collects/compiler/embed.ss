
(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig (make-embedding-executable)
  (require-relative-library "embedr.ss"))
