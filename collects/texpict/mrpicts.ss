
(begin-elaboration-time
 (require-relative-library "commons.ss"))

(define-signature mrpict-extra^
  (dc-for-text-size

   text caps-text
   dc

   draw-pict
   make-pict-drawer))

(define-signature mrpict^
  ((open texpict-common^)
   (open mrpict-extra^)))
