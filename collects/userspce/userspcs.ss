(require-library "core.ss")
(plt:require-library "ricedefs.ss")
(plt:require-library "sparams.ss")

(define-signature plt:userspace^
  ((open mzlib:pretty-print^)
   (open mzlib:file^)
   (open mzlib:function^)
   (open mzlib:compat^)
   (open mzlib:string^)
   (open ricedefs^)))
