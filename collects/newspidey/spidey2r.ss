(lambda (drscheme-thunk)
  (compound-unit/sig
    (import [zodiac : zodiac:system^])
    (link
     [coreflat : mzlib:core-flat^
               ((require-library "coreflatr.ss"))]
     [newspidey : ((open newspidey:constraints-gen-and-prop^)(open spidey2^) (open newspidey:driver^))
                ((require-library "compound-unit.ss" "newspidey")
                 coreflat
                 zodiac)]
     ;; just used to run Spidey's main function
     [newspidey-run : ()
                    ((unit/sig ()
                       (import ((open newspidey:constraints-gen-and-prop^)(open spidey2^) (open newspidey:driver^)))
                       ;; run !
                       (newspidey-driver drscheme-thunk)
                       )
                     newspidey)])
    (export (open (newspidey : spidey2^)))))
