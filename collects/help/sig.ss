(begin-elaboration-time
 (require-library "sig.ss" "mred")
 (require-library "frameworks.ss" "framework"))

(define-signature help:start-help-desk^
  (start-help-desk))

(begin-elaboration-time
 (require-relative-library "search-sig.ss"))

(begin-elaboration-time
 (require-library "sig.ss" "browser"))

;; Interface; search reports results through these procedures
(define-signature help:help^
  (new-help-frame))
