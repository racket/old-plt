(module info (lib "infotab.ss" "setup")
  (define name "Setup PLT")
  (define compile-prefix
    `(begin
       (require-library "refer.ss")
       (require-library "plt-installers.ss" "setup")
       (require-library "setupsig.ss" "setup")))
  (define compile-omit-files
    (list "setup.ss" "setupsig.ss" "plt-installers.ss" "get-infos.ss"))
  (define compile-elaboration-zos
    (list "setupsig.ss" "get-infos.ss"))
  (define mzscheme-launcher-libraries (list "setup.ss"))
  (define mzscheme-launcher-names (list "Setup PLT")))
