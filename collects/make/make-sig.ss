
(module make-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide make^)

  (define-signature make^
    (make/proc
     make-print-checking
     make-print-dep-no-line
     make-print-reasons
     (struct exn:make (target orig-exn)))))

