
; Load core mzlib

(reference-library "refer.ss")

(reference "spidey.ss")

(reference "macro.ss")

(reference "prettyu.ss")
(reference "match.ss")
(reference "defstru.ss")
(reference "fileu.ss")
(reference "functiou.ss")
(reference "compatu.ss")
(reference "stringu.ss")
(reference "compileu.ss")
(reference "threadu.ss")
(reference "shared.ss")

(reference "cores.ss")

(define mzlib:core@ (reference-unit/sig "corer.ss"))
