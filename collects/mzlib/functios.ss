
(begin-elaboration-time
 (require-relative-library "spidey.ss"))

(define-signature mzlib:function^
  (true
   false
   
   set-first!
   first
   second
   third
   fourth
   fifth
   sixth
   seventh
   eighth

   set-rest!
   rest

   cons?
   empty
   empty?

   boolean=?
   symbol=?

   char->string

   identity
   compose
   foldl
   foldr

   last-pair

   remv
   remq
   remove
   remv*
   remq*
   remove*

   assf
   memf

   filter

   build-string
   build-vector
   build-list

   quicksort
   mergesort

   loop-until

   ignore-errors))
