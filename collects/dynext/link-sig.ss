
(module link-sig mzscheme
  (import (lib "unitsig.ss"))

  (export dynext:link^)

  (define-signature dynext:link^
    (link-extension
     current-extension-linker
     current-extension-linker-flags
     current-make-link-input-strings
     current-make-link-output-strings
     current-standard-link-libraries
     use-standard-linker)))
