(module basic mzscheme
	(provide (all-defined))

	(define-struct <unit> () (make-inspector))


	(define-struct value-set (name type) (make-inspector))
	(define-struct <tuple> (list) (make-inspector))
	(define-struct arrow (arglist result) (make-inspector))
	(define-struct tvar (tbox) (make-inspector))
	(define-struct tlist (type) (make-inspector))
	(define-struct tarray (type) (make-inspector))
	(define-struct tvariant (name params varnames variantlist) (make-inspector))
	(define-struct tabstract (name params type))
	(define-struct tconstructor (argtype result) (make-inspector))
	(define-struct usertype (name params))
	(define-struct <voidstruct> () (make-inspector))
	(define-struct ref (type) (make-inspector))
	(define-struct mlexn (name types) (make-inspector))
	(define-struct ml-exn () (make-inspector))
	(define-struct option (type) (make-inspector))
	(define-struct <user-type> () (make-inspector))

	(define-struct |Some| (tlist) (make-inspector))
	(define-struct |None| (tlist) (make-inspector))

)
	