
(begin-elaboration-time
 (require-relative-library "commons.ss"))

(define-signature mrpict-extra^
  (dc-for-text-size

   text
   dc

   draw-pict))

(define-signature mrpict^
  ((open texpict-common^)
   (open mrpict-extra^)))
