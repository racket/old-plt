
; Load all of mzlib

(reference-library "refer.ss")

(reference-library "coreu.ss")

(reference-library "synrule.ss")

(reference-library "spidey.ss")
(reference-library "triggeru.ss")
(reference-library "zmathu.ss")
(reference-library "pconveru.ss")
(reference-library "dateu.ss")
(reference-library "inflateu.ss")

(reference-library "mzlibs.ss")

(define mzlib@ (reference-library-unit/sig "mzlibr.ss"))
