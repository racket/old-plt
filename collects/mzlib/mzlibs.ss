
(require-library "cores.ss")
(require-library "triggers.ss")
(require-library "zmaths.ss")
(require-library "pconvers.ss")
(require-library "dates.ss")
(require-library "inflates.ss")
(require-library "cmdlines.ss")
(require-library "restarts.ss")

(define-signature mzlib^
  ((open mzlib:core^)
   (unit trigger@ : mzlib:trigger^)
   (unit zmath@ : mzlib:zmath^)
   (unit print-convert@ : mzlib:print-convert^)
   (unit date@ : mzlib:date^)
   (unit inflate@ : mzlib:inflate^)
   (unit command-line@ : mzlib:command-line^)
   (unit restart@ : mzlib:restart^)))
