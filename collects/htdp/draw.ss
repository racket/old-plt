(module draw mzscheme
  (provide teachpack@)

  (require "error.ss"
           "big-draw.ss"
           (lib "unitsig.ss"))

  (define teachpack@
    (compound-unit/sig
      (import (user : draw-from-user^))
      (link
       (DRAW : drawS (bigDrawU user ERR))
       (ERR  : errorS (errorU)))
      (export (open DRAW)))))
