
(define-signature dynext:file^
  (make-directory*

   append-zo-suffix
   append-c-suffix
   append-constant-pool-suffix
   append-object-suffix
   append-extension-suffix

   extract-base-filename/ss
   extract-base-filename/c
   extract-base-filename/kp
   extract-base-filename/o))
