(require-library "coreflats.ss")
(require-relative-library "ricedefs.ss")
(require-library "sig.ss" "mred")

(define-signature plt:mz-userspace^
  ((open mzlib:core-flat^)))

(define-signature plt:mr-userspace^
  ((open mred^)
   (open mzlib:core-flat^)
   (open turtles^)
   (struct posn (x y))))

(define-signature plt:advanced^
  ((open mzlib:core-flat^)
   (open turtles^)
   (struct posn (x y))))

(define-signature plt:userspace^ plt:mr-userspace^)
