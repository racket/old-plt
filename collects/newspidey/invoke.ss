(require-library "cores.ss")                      ;; for mzlib:core^
(require-library "coreflats.ss")                  ;; for mzlib:core-flat^
(require-library "sigs.ss" "zodiac")              ;; for zodiac:interface^ and zodiac:system^
(require-library "spidey2s.ss" "newspidey")       ;; for spidey2^

(define-values/invoke-unit/sig
 ((unit newspidey : newspidey:driver^) (unit zodiac : zodiac:system^))
 (compound-unit/sig
  (import)
  (link [mzlib-core : mzlib:core^
                    ((require-library "corer.ss"))]
        [mzlib-coreflat : mzlib:core-flat^
                        ((require-library "coreflatr.ss"))]
        ;; should be provided by any program using the zodiac system unit
        [zodiac-interface : zodiac:interface^
                          ((unit/sig zodiac:interface^
                             (import)
                             (define internal-error
                               (lambda (where fmt-spec . args)
                                 (printf "NewSpidey error at: ~s~n" where)
                                 (apply error 'internal-error fmt-spec args)))
                             (define (static-error link-text link-tag where fmt-spec . args)
                               (printf "NewSpidey error tag: ~s~n" link-tag)
                               (printf "NewSpidey error at: ~s~n" where)
                               (printf "args: ~s~n" args)
                               (apply error 'static-error
                                      (string-append link-text ": " fmt-spec " at location(s): ~s. Tag was ~s")
                                      (append args (list where link-tag))))))]
        [zodiac-system : zodiac:system^
                       ((require-library-unit/sig "link2.ss" "zodiac")
                        zodiac-interface
                        (mzlib-core pretty-print)
                        (mzlib-core file))]
        [newspidey : newspidey:driver^
                   ((require-library "compound-unit.ss" "newspidey")
                    mzlib-coreflat
                    zodiac-system)])
  ;; creates a signature with two sub-signatures, so we can have
  ;; two different prefixes (which is why the define-values/invoke-unit/sig
  ;; doesn't have a separate prefix.
  (export (unit newspidey newspidey) (unit zodiac-system zodiac))))
