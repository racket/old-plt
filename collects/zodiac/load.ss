; $Id: load.ss,v 1.17 1997/07/21 15:51:43 shriram Exp $

(reference-library "macro.ss")
(reference-library "cores.ss")

(reference-library "zsigs.ss")
(reference-library "sigs.ss")

; All this stuff needs to be disappeared.

(reference-library "sparams.ss" "backward")

(define zodiac:system@
  (reference-unit/sig "link.ss"))
