(require-library "functios.ss")
(require-library "compats.ss")
(require-library "strings.ss")
(plt:require-library "ricedefs.ss")
(plt:require-library "graphics.ss")
(plt:require-library "sparams.ss")


(define-signature drscheme:userspace^
  ((open mzlib:function^)
   (open mzlib:compat^)
   (open mzlib:string^)
   (open ricedefs^)
   (open graphics^)))

