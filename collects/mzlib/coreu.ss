
; Load core mzlib

(reference-library "refer.ss")
(reference-library "spidey.ss")

(reference-library "macro.ss")

(reference-library "prettyu.ss")
(reference-library "match.ss")
(reference-library "defstru.ss")
(reference-library "fileu.ss")
(reference-library "functiou.ss")
(reference-library "compatu.ss")
(reference-library "stringu.ss")
(reference-library "compileu.ss")
(reference-library "threadu.ss")
(reference-library "shared.ss")

(reference-library "cores.ss")

(define mzlib:core@ (reference-library-unit/sig "corer.ss"))

