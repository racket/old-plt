
(module info (lib "infotab.ss" "setup")
  (define name "readline")
  (define compile-omit-files '("mzrl.ss" "mzmake.ss"))
  (define install-collection "installer.ss")
  (define blurb
    `("The readline collection provides glue for using GNU's readline library"
      " with the MzScheme read-eval-print-loop.")))
