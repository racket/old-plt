
;; Glue mzc to the real MrSpidey. In contrast, spnoop.ss provides
;;  dummy version of these functions that don't use the real MrSpidey

(unit/sig compiler:mrspidey^
  (import (mrspidey : mrspidey:sba^))

  (define binding-mutated mrspidey:binding-mutated)

  (define (SDL-type ast)
    (let* ([ftype (mrspidey:parsed-ftype ast)])
      (and ftype (mrspidey:FlowType->SDL ftype))))

  (define analyze-program-sexps mrspidey:analyze-program-sexps))

