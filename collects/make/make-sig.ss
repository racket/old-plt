
(module make-sig mzscheme
  (import (lib "unitsig.ss"))

  (export make^)

  (define-signature make^
    (make/proc
     make-print-checking
     make-print-dep-no-line
     make-print-reasons
     (struct exn:make (target orig-exn)))))

