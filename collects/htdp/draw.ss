(require-library "error.ss" "htdp")
(require-library "big-draw.ss" "htdp")

(compound-unit/sig
  (import (PLT : plt:userspace^))
  (link
    (DRAW : drawS (bigDrawU ERR PLT))
    (ERR  : errorS (errorU)))
  (export (open DRAW)))
