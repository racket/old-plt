
(begin-elaboration-time
 (require-library "launchers.ss" "launcher")
 (require-library "dynexts.ss" "dynext"))

(define-signature compiler:setup-option^
  (verbose
   make-verbose
   compiler-verbose
   clean
   make-zo
   make-so
   make-launchers
   call-install
   specific-collections
   archives))
