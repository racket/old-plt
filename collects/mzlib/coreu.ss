
; Load core mzlib

(reference-library "refer.ss")

(reference-relative-library "spidey.ss")

(reference-relative-library "macro.ss")

(reference-relative-library "prettyu.ss")
(reference-relative-library "match.ss")
(reference-relative-library "defstru.ss")
(reference-relative-library "fileu.ss")
(reference-relative-library "functiou.ss")
(reference-relative-library "compatu.ss")
(reference-relative-library "stringu.ss")
(reference-relative-library "compileu.ss")
(reference-relative-library "threadu.ss")
(reference-relative-library "shared.ss")

(reference-relative-library "cores.ss")

(define mzlib:core@ (reference-relative-library-unit/sig "corer.ss"))
