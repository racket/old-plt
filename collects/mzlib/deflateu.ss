
(require-library "deflates.ss")

(begin-elaboration-time
 (require-library "refer.ss"))

(define mzlib:deflate@ (require-library-unit/sig "deflater.ss"))
