
; Load core mzlib

(reference-library "refer.ss")

(reference-relative-library "corem.ss")

(reference-relative-library "prettyu.ss")
(reference-relative-library "fileu.ss")
(reference-relative-library "functiou.ss")
(reference-relative-library "compatu.ss")
(reference-relative-library "stringu.ss")
(reference-relative-library "compileu.ss")
(reference-relative-library "threadu.ss")

(reference-relative-library "cores.ss")

(define mzlib:core@ (reference-unit/sig "corer.ss"))
