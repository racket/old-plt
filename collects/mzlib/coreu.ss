
; Load core mzlib

(require-library "refer.ss")

(require-relative-library "corem.ss")

(require-relative-library "prettyu.ss")
(require-relative-library "fileu.ss")
(require-relative-library "functiou.ss")
(require-relative-library "compatu.ss")
(require-relative-library "stringu.ss")
(require-relative-library "compileu.ss")
(require-relative-library "mathu.ss")
(require-relative-library "threadu.ss")

(require-relative-library "cores.ss")

(define mzlib:core@ (require-relative-library-unit/sig "corer.ss"))
