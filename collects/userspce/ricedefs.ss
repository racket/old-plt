(define-signature ricedefs^
  (random-char
   call/cc0
   2vector 2make-vector 2vector-set!
   foreach! 2foreach!
   2vector-init 2vector-print
   tabulate
   ; atom? cons? 
   allow-improper-lists
   cons set-cdr! list* append append!))
