(require-library "coreflats.ss")
(require-relative-library "ricedefs.ss")

; Has to work in DrJr without MrEd:
(when (with-handlers ([void (lambda (x) #f)])
        (collection-path "mred"))
  (require-library "sig.ss" "mred"))

(define-signature plt:userspace^
  ((open mred^)
   (open mzlib:core-flat^)))

