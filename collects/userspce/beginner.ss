;; (defined? 'mred^) is true in DrScheme, but false in DrScheme Jr

(require-library "cores.ss")
(when (defined? 'mred^)
  (require-library "graphics.ss" "graphics")
  (require-library "simple-draws.ss" "userspce")
  (require-library "errors.ss" "userspce"))

(if (defined? 'mred^)
    (eval
     '(compound-unit/sig (import)
	(link
	 [core : mzlib:core^ ((require-library "corer.ss" "mzlib"))]
	 [mred : mred^ (mred@)]
	 [graphics : graphics^ ((require-library "graphicr.ss" "graphics") (core file) mred)]
	 [error : userspace:error^ ((require-library "errorr.ss" "userspce"))]
	 [simple-draw : userspace:simple-draw^
		      ((require-library "simple-drawr.ss" "userspce")
		       error
		       graphics)])
	(export
	 (open (core function))
	 (open (core pretty-print))
	 (open (core file))
	 (open (core string))
	 (open (core compile))
	 (open (core thread))
	 (open simple-draw))))
    (compound-unit/sig (import)
      (link
       [core : mzlib:core^ ((require-library "corer.ss" "mzlib"))])
      (export
       (open (core function))
       (open (core pretty-print))
       (open (core file))
       (open (core string))
       (open (core compile))
       (open (core thread)))))
