;; copyright by Paul Graunke June 2000 AD
(require-library "sgmls.ss" "xml" "legacy")
(require-library "xmls.ss" "xml")
(require-library "invoke.ss")

(define-values/invoke-unit/sig
 ((open xml^) (unit sgml : sgml-reader^))
 (compound-unit/sig
   (import)
   (link
    (FUN : mzlib:function^ ((require-library "functior.ss")))
    (X : xml^ ((require-library "xmlr.ss" "xml") FUN))
    (S : sgml-reader^ ((require-library "sgml-reader.ss" "xml" "legacy") (X : xml-structs^) FUN)))
   (export (open X) (unit S sgml))))
