
(begin-elaboration-time
 (require-library "mzlibs.ss"))

(begin-elaboration-time
 (require-library "coreflats.ss"))

(define-signature mzlib:flat^
  ((open mzlib:core^)
   (open mzlib:compat^)
   (open mzlib:print-convert^)
   (open mzlib:date^)
   (open mzlib:inflate^)
   (open mzlib:command-line^)
   (open mzlib:restart^)
   (open mzlib:transcript^)))
