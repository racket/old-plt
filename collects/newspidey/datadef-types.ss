(unit/sig newspidey:datadef-types^
  (import)

;; doms = (listof Type), rng = Type
(define-struct Type-Arrow (doms rng))
;; car = Type, cdr = Type
(define-struct Type-Cons (car cdr))
;; vars = (listof symbol), type = Type
(define-struct Type-Scheme (vars type))
;; name = symbol
(define-struct Type-Var (name))
;; set-var = Set-var, type = Type
(define-struct Type-Binding (set-var type))
;; bindings = (listof Type-Binding), type = Type
(define-struct Type-Rec (bindings type))
;; types = (listof Type)
(define-struct Type-Union (types))
;;
(define-struct Type-Empty ())

(define *empty-type* (make-Type-Empty))

  ) ;; unit/sig
