
(begin-elaboration-time 
 (require-library "invoke.ss"))

(begin-elaboration-time 
 (define-values/invoke-unit (awk) 
   (require-relative-library "awkr.ss")))

(define-macro awk awk)
