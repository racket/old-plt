(define-signature plt:aries-core^
  (annotate
   extract-zodiac-location
   w-c-m-key
   break))

(define-signature cogen-utils^
  (get-binding-name
   lookup-new-binding-name
   set-new-binding-name!
   
   check-for-keyword
   check-for-syntax-or-macro-keyword
   
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