(require-library "errortrace.ss" "errortrace")

(require-library "simple-draws.ss" "userspce")
(require-library "refer.ss")
(require-library "spidey.ss")
(require-library "graphics.ss" "graphics")
(require-library "cores.ss")

(require-library "errors.ss" "userspce")

(invoke-open-unit/sig
 (compound-unit/sig
   (import)
   (link
    (mred : mred^ (mred@))
    (core : mzlib:core^ ((require-library "corer.ss")))
    (graphics : graphics^ ((require-library "graphicr.ss" "graphics") (core file) mred))
    (err  : userspace:error^ ((require-library "errorr.ss" "userspce")))
    (draw : userspace:simple-draw^ ((require-library "simple-drawr.ss" "userspce") err graphics)))
   (export (open draw)
	   (open (graphics : userspace:simple-graphics^)))))
