
(define-signature mzlib:print-convert^
  (show-sharing
   constructor-style-printing
   quasi-read-style-printing

   print-convert
   print-convert-expr
   build-share
   get-shared
   current-read-eval-convert-print-prompt 
   install-converting-printer))

(define-signature mzlib:print-convert-hooks^
  (before-test?
   before-convert

   print-convert-hook
   build-share-hook
   build-share-name-hook))

