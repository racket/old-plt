
(require-relative-library "compiles.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig dynext:compile^
  (require-relative-library "compiler.ss"))
