(require-library "coreflats.ss")
(require-relative-library "ricedefs.ss")
(require-library "sig.ss" "mred")

(define-signature plt:beginner-extras^
  ((struct posn (x y) -setters)
   (open mzlib:core-flat^)))

(define-signature plt:intermediate-extras^
  plt:beginner-extras^)

(begin-construction-time
 (if (defined? 'mred@)
     `(define-signature plt:userspace^
	((open mred^)
	 (open mzlib:core-flat^)
	 (open turtle^)
	 (struct posn (x y))))
     `(define-signature plt:userspace^
	((open mzlib:core-flat^)
	 (struct posn (x y))))))

(begin-construction-time
 (if (defined? 'mred@)
     `(define-signature plt:advanced-extras^
	((struct posn (x y))
	 (open mzlib:core-flat^)
	 (open turtle^)))
     `(define-signature plt:advanced-extras^
	((struct posn (x y))
	 (open mzlib:core-flat^)))))
