(require-library "cores.ss")

(require-library "dates.ss")
(require-library "refer.ss")
(require-library "macro.ss")


(define-signature countdown^
  (main-edit%))

(define-signature before^
  (edit frame quit-semaphore))

(define-signature during^
  (remember remember-around))