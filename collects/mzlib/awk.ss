
(require-library "refer.ss")

(begin-elaboration-time (invoke-open-unit (require-relative-library "awkr.ss")))

(define-macro awk awk)
