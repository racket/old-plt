(require-library "core.ss")
(plt:require-library "ricedefs.ss")
(plt:require-library "graphics.ss")
(plt:require-library "sparams.ss")
(plt:require-library "turtles.ss")

(define-signature plt:userspace^
  ((open mzlib:core^)
   (open ricedefs^)))
