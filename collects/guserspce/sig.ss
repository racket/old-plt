(require-library "sig.ss" "userspce")

(require-library "sig.ss" "mred")
(require-library "turtles.ss" "graphics")

;; redefinition of userspace's plt:userspace^ in sig.ss
(define-signature plt:userspace^
  ((open mred^)
   (open mzlib:core-flat^)
   (open turtle^)
   (struct posn (x y))))
(define-signature plt:advanced-extras^
  ((struct posn (x y))
   (open mzlib:core-flat^)
   (open turtle^)))

(when (with-handlers ([void (lambda (x) #f)])
	(collection-path "mred"))
  (require-library "turtles.ss" "graphics")
  (require-library "sig.ss" "mred"))
