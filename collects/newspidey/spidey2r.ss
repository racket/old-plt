(require-library "coreflats.ss")
(require-library "spidey2s.ss" "newspidey")
(require-library "sigs.ss" "zodiac")

(define (analyze thnk)
  (compound-unit/sig
    (import [zodiac : zodiac:system^])
    (link
     [cf : mzlib:core-flat^ ((require-library "coreflatr.ss"))]
     [p : spidey2^ ((unit/sig spidey2^
                      (import mzlib:core-flat^
                              (zodiac : zodiac:system^))
                      (include "type-reconstruct.ss")
                      (include "parse-zodiac.ss")
                      )
                    cf zodiac)])
    (export (open p))))
