
(define-signature mzlib:thread^
  (consumer-thread
   merge-input
   with-semaphore
   semaphore-wait-multiple

   dynamic-disable-break
   dynamic-wind/protect-break
   make-single-threader))
