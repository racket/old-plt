(reference-library "cores.ss")
(reference "ricedefs.ss")
(reference "sparams.ss")

(define-signature plt:userspace^
  ((open mzlib:pretty-print^)
   (open mzlib:file^)
   (open mzlib:function^)
   (open mzlib:compat^)
   (open mzlib:string^)
   (open ricedefs^)))
