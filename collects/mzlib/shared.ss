
(reference-library "refer.ss")
(reference-library "functios.ss")

(begin-elaboration-time
 (invoke-open-unit
  (reference-library "sharedr.ss")))

(define-macro shared shared)


