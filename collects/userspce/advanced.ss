(begin-elaboration-time
 (require-library "coreflats.ss")
 (require-library "turtles.ss" "graphics")
 (require-library "invoke.ss"))

(let ([u (require-relative-library "advancedr.ss")])
  (lambda ()
    (global-define-values/invoke-unit/sig ((open mzlib:core-flat^)
					   (open turtle^)
					   (open ((struct posn (x y)))))
					  u)))
