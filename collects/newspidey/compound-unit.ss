(compound-unit/sig
  (import [mzlib : mzlib:core-flat^]
          [zodiac : zodiac:system^])
  (link
   [datadef-setexp : newspidey:datadef-setexp^
                   ((require-library "datadef-setexp.ss" "newspidey"))]
   [datadef-types : newspidey:datadef-types^
                  ((require-library "datadef-types.ss" "newspidey"))]
   [constraints-from-type : newspidey:constraints-from-type^
                          ((require-library "constraints-from-type.ss" "newspidey")
                           datadef-setexp
                           datadef-types
                           constraints-gen-and-prop)]
   [constraints-gen-and-prop : newspidey:constraints-gen-and-prop^
                             ((require-library "constraints-gen-and-prop.ss" "newspidey")
                              zodiac
                              mzlib
                              datadef-setexp
                              constraints-from-type)]
   [debug-arity : newspidey:debug-arity^
                ((require-library "debug-arity.ss" "newspidey")
                 zodiac
                 datadef-setexp
                 constraints-gen-and-prop
                 gui-interface)]
   [driver : newspidey:driver^
           ((require-library "driver.ss" "newspidey")
            zodiac
            constraints-from-type
            constraints-gen-and-prop
            type-reconstruction
            debug-arity)]
   [gui-interface : spidey2^
                  ((require-library "gui-interface.ss" "newspidey")
                   zodiac
                   mzlib
                   datadef-setexp
                   datadef-types
                   constraints-gen-and-prop
                   debug-arity
                   type-reconstruction)]
   [type-reconstruction : newspidey:type-reconstruction^
                        ((require-library "type-reconstruction.ss" "newspidey")
                         mzlib
                         datadef-setexp
                         datadef-types
                         constraints-gen-and-prop)])
  (export (open constraints-gen-and-prop)(open gui-interface) (open driver)))
