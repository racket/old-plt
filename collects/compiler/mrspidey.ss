
;; Glue mzc to the real MrSpidey. In contrast, spnoop.ss provides
;;  dummy version of these functions that don't use the real MrSpidey

(unit/sig compiler:mrspidey^
  (import (mrspidey : mrspidey:sba^))
  
  (define copy-annotations!
    (lambda (new old)
      (mrspidey:set-parsed-ftype! new (mrspidey:parsed-ftype old))
      (mrspidey:set-parsed-check! new (mrspidey:parsed-check old))
      (mrspidey:set-parsed-atprim! new (mrspidey:parsed-atprim old))
      (mrspidey:set-app-tvar-args! new (mrspidey:app-tvar-args old))
      (mrspidey:set-binding-refs! new (mrspidey:binding-refs old))
      (mrspidey:set-binding-mutated! new (mrspidey:binding-mutated old))
      new))

  (define binding-mutated mrspidey:binding-mutated)

  (define (SDL-type ast)
    (let* ([ftype (mrspidey:parsed-ftype ast)])
      (and ftype (mrspidey:FlowType->SDL ftype))))

  (define analyze-program-sexps mrspidey:analyze-program-sexps))

