(require-library "invoke.ss")

(require-library "macro.ss")

(require-library "math.ss")
(require-library "function.ss")

(require-library "value-turtles.ss" "graphics")

(define-values/invoke-unit/sig value-turtles^
  (require-library "value-turtler.ss" "graphics")
  #f
  mzlib:function^
  mzlib:math^
  mred^)
