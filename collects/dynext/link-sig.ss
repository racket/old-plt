
(module link-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide dynext:link^)

  (define-signature dynext:link^
    (link-extension
     current-extension-linker
     current-extension-linker-flags
     current-make-link-input-strings
     current-make-link-output-strings
     current-make-standard-link-libraries
     use-standard-linker
     link-variant)))
