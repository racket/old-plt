(compound-unit/sig
  (import [zodiac : zodiac:system^]
          [error : zodiac:interface^])
  (link [utils : cogen-utils^
               ((require-library-unit/sig "cogen-utilsr.ss" "cogen")
                zodiac
                error)]
        [aries-core : plt:aries-core^
                    ((require-library-unit/sig "aries-corer.ss" "cogen")
                     zodiac
                     error
                     utils)])
  (export (open utils)
          (open aries-core)))

                               
          