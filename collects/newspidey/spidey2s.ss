(require-library "coreflats.ss")
(require-library "sigs.ss" "zodiac")

(define-signature spidey2^
  (get-prims ; -> (listof (list zodiac:location zodiac:location
             ;                  (union 'green 'red)))
   get-loc ; : set-var -> (union #f zodiac:location)
   get-var ; : location/offset -> (union set-var #f)
   get-type; : set-var -> type
   pp-type; : type -> string
   parents ; : set-var -> (listof set-var)
   children ; : set-var -> (listof set-var)
   has-member? ; : type (union 'number 'null 'pair 'procedure ...) -> boolean
   ))
