(require-library "coreflats.ss")
(require-library "sigs.ss" "zodiac")

(define-signature newspidey:datadef-setexp^
  ((struct Const (val))
   (struct Token (name))
   (struct Set-var (name))
   (struct Dom-arity (arity pos set-var))
   (struct Dom-interval (interval pos n set-var))
   (struct Rng-arity (arity set-var))
   (struct Rng-interval (interval n set-var))
   (struct Label (name))
   (struct Car (set-var))
   (struct Cdr (set-var))
   
   (struct Interval (lo hi))
   (struct Arity (req proh))))

(define-signature newspidey:datadef-types^
  ((struct Type-Arrow (doms rng))
   (struct Type-Cons (car cdr))
   (struct Type-Scheme (vars type))
   (struct Type-Var (name))
   (struct Type-Binding (set-var type))
   (struct Type-Rec (bindings type))
   (struct Type-Union (types))
   (struct Type-Empty ())
   
   *empty-type*))

(define-signature newspidey:constraints-from-type^
  (lookup-prim-type
   lookup-prim-label
   add-constraints-from-type
   init-prim
   prim-init-list))

(define-signature newspidey:constraints-gen-and-prop^
  (*pair-token*
   gen-set-var
   associate-label-with-ars
   add-constraint-with-bounds
   lookup-hi-and-filter
   lookup-ars-from-label
   in-interval
   lookup-lambda-from-label
   derive-top-term-constraints
   propagate-constraints
   lookup-term-from-set-var
   *location-list*
   lookup-lo-and-filter
   get-all-set-vars
   lookup-set-vars-from-dom-int))

(define-signature newspidey:debug-arity^
  (*bad-apps*
   debug-arity))

(define-signature newspidey:driver^
  (newspidey-driver))

(define-signature spidey2^ ;; gui-interface.ss
  (get-prims ; -> (listof (list zodiac:location zodiac:location
             ;                  (union 'green 'red)))
   get-loc ; set-var -> (union #f zodiac:location)
   get-var ; location/offset -> (union set-var #f)
   get-type; set-var -> type
   pp-type; type -> string
   parents ; set-var -> (listof set-var)
   children ; set-var -> (listof set-var)
   has-member? ; type (union 'number 'null 'pair 'procedure ...) -> boolean
   ))

(define-signature newspidey:type-reconstruction^
  (init-set-var-to-type
   type-reduce-rec-bindings
   type-reduce
   mk-type))
