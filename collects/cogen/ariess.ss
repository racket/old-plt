(define-signature plt:aries-core^
  (annotate
   ; transform
   ; error-box
   ; the w-c-m key is the gensym'ed symbol created by
   ; aries to tag the continuation-marks which annotated
   ; code creates.
   extract-zodiac-location
   w-c-m-key
   break))

(define-signature cogen-utils^
  (check-for-keyword
   check-for-keyword/proc
   
   the-undefined-value
   (struct undefined (id))
   signal-undefined
   undefined-error-format
   
   (struct not-boolean (val))
   signal-not-boolean
   not-boolean-error-format
   
   is-unit-bound?
   read->raw
   arglist->ilist
   
   improper-map
   improper-foreach))

(define-signature plt:aries^
  ((open plt:aries-core^)
   
   signal-not-boolean
   signal-undefined))