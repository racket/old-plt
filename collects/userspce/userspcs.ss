(require-library "cores.ss")
(require-library "compats.ss")
(require-relative-library "ricedefs.ss")
;(reference "sparams.ss")

(define-signature plt:userspace^
  ((open mzlib:pretty-print^)
   (open mzlib:file^)
   (open mzlib:function^)
   (open mzlib:compat^)
   (open mzlib:string^)
   (open ricedefs^)))

(define-signature plt:userspace:params^
  (<=-at-least-two-args
   allow-improper-lists
   eq?-only-compares-symbols))