
(reference-library "cores.ss")
(reference-library "triggers.ss")
(reference-library "zmaths.ss")
(reference-library "pconvers.ss")
(reference-library "dates.ss")
(reference-library "inflates.ss")
(reference-library "threads.ss")

(define-signature mzlib^
  ((open mzlib:core^)
   (unit trigger@ : mzlib:trigger^)
   (unit zmath@ : mzlib:zmath^)
   (unit print-convert@ : mzlib:print-convert^)
   (unit print-convert-hooks@ : mzlib:print-convert-hooks^)
   (unit date@ : mzlib:date^)
   (unit inflate@ : mzlib:inflate^)
   (unit thread@ : mzlib:thread^)))
