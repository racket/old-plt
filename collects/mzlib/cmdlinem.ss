
(require-library "refer.ss")

(begin-elaboration-time
 (invoke-open-unit
  (require-library "cmdlinemr.ss")))

(define-macro command-line command-line)

