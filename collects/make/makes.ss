
(define-signature make:make^
  (make/proc
   make-print-checking
   make-print-dep-no-line
   make-print-reasons
   (struct exn:make (target orig-exn))))
