(require-library "coreflats.ss")
(require-library "compats.ss")
(require-relative-library "ricedefs.ss")
;(reference "sparams.ss")
(require-library "sig.ss" "mred")

(define-signature plt:userspace^
  ((open mred^)
   (open mzlib:core-flat^)))

