
(require-relative-library "links.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig dynext:link^
  (require-relative-library "linkr.ss"))

