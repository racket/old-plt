(require-library "cores.ss")
(require-library "classd.ss")

(define-signature loa:grid^
  (grid-pasteboard%
   grid-canvas%
   grid-snip%))

(define-signature loa^
  (loa-pasteboard%
   loa-canvas%
   loa-checker%))

(define-signature loa:utils^
  (vector-for-each))