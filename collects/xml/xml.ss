(require-library "xmls.ss" "xml")
(define-values/invoke-unit/sig
 xml^
 (compound-unit/sig
   (import)
   (link
    (PRETTY : mzlib:pretty-print^ ((require-library "prettyr.ss")))
    (FUN : mzlib:function^ ((require-library "functior.ss")))
    (STR : mzlib:string^ ((require-library "stringr.ss")))
    (FILE : mzlib:file^ ((require-library "filer.ss") STR FUN))
    (X : xml^ ((require-library "xmlr.ss" "xml") FUN)))
   (export (open X))))
