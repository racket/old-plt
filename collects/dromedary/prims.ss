(module prims mzscheme
	(require (lib "list.ss")
		 (lib "match.ss")
		 (lib "pretty.ss"))
	(provide library-names user-types built-in-and-user-funcs constructors
		 (struct <tuple> (list))
		 (struct arrow (arglist result))
		 (struct tvar (tbox))
		 (struct tlist (type))
		 (struct tvariant (name varnames variantlist))
		 (struct tconstructor (argtype result))
		 (struct usertype (name))
		 (struct option (type))
		 (struct <voidstruct> (dummy))
		 (struct <unit> (dummy))
		 (struct ref (type))
		 (struct mlexn (name types))
		 != <lt> <gt> <le> <ge> <or> <and>
		 float? any?
		 (all-from (lib "match.ss")))


	(define-struct <tuple> (list) (make-inspector))
	(define-struct arrow (arglist result) (make-inspector))
	(define-struct tvar (tbox) (make-inspector))
	(define-struct tlist (type) (make-inspector))
	(define-struct tvariant (name varnames variantlist) (make-inspector))
	(define-struct tconstructor (argtype result) (make-inspector))
	(define-struct usertype (name))
	(define-struct <voidstruct> (dummy))
	(define-struct <unit> (dummy))
	(define-struct ref (type))
	(define-struct mlexn (name types))
	(define-struct option (type))

	(define library-names (make-hash-table 'equal))

	;; The list functions
	(define list-funcs (make-hash-table 'equal))
	(hash-table-put! list-funcs "hd" (list (make-arrow (list (make-tlist (make-tvar "'a"))) (make-tvar "'a")) car))
	(hash-table-put! list-funcs "tl" (list (make-arrow (list (make-tlist (make-tvar "'a"))) (make-tlist (make-tvar "'a"))) cdr))
	(hash-table-put! list-funcs "rev" (list (make-arrow (list (make-tlist (make-tvar "'a"))) (make-tlist (make-tvar "'a"))) reverse))
	(hash-table-put! list-funcs "map" (list (make-arrow (list (make-arrow (list (make-tvar "'a")) (make-tvar "'b"))) (make-arrow (list (make-tlist (make-tvar "'a"))) (make-tlist (make-tvar "'b"))))  map))
	(hash-table-put! list-funcs "filter" (list (make-arrow (list (make-tlist (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) "bool")) (make-tlist (make-tvar "'a"))) filter))
	(hash-table-put! list-funcs "append" (list (make-arrow (list (make-tlist (make-tvar "'a")) (make-tlist (make-tvar "'a"))) (make-tlist (make-tvar "'a"))) append))

	;; Fill up all the libraries
	(hash-table-put! library-names "List" list-funcs)

	(define (!= a b)
	  (not (equal? a b)))

	(define (<lt> a b)
	  (cond
	   [(number? a)
	    (< a b)]
	   [(boolean? a)
	    (< (boolean-to-number a) (boolean-to-number b))]
	   [(string? a)
	    (string<? a b)]
	   [else
	    (pretty-print "Uncaught exception: Invalid_argument")]))

	(define (<le> a b)
	  (cond
	   [(number? a)
	    (<= a b)]
	   [(boolean? a)
	    (<= (boolean-to-number a) (boolean-to-number b))]
	   [(string? a)
	    (string<=? a b)]
	   [else
	    (pretty-print "Uncaught exception: Invalid_argument")]))

	(define (<gt> a b)
	  (cond
	   [(number? a)
	    (> a b)]
	   [(boolean? a)
	    (> (boolean-to-number a) (boolean-to-number b))]
	   [(string? a)
	    (string>? a b)]
	   [else
	    (pretty-print "Uncaught exception: Invalid_argument")]))

	(define (<ge> a b)
	  (cond
	   [(number? a)
	    (>= a b)]
	   [(boolean? a)
	    (>= (boolean-to-number a) (boolean-to-number b))]
	   [(string? a)
	    (string>=? a b)]
	   [else
	    (pretty-print "Uncaught exception: Invalid_argument")]))

	(define (<set-box!> b v)
	  (begin (set-box! b v)
		 (make-<unit> #f)))

	(define (<or> a b)
	  (or a b))

	(define (<and> a b)
	  (and a b))

	(define (boolean-to-number n)
	  (if n 1 0))

	(define (float? n)
	  (number? n))

	(define (any? n) #t)

	(define user-types (make-hash-table 'equal))
	(hash-table-put! user-types "int" integer?)
	(hash-table-put! user-types "float" float?)
	(hash-table-put! user-types "char" char?)
	(hash-table-put! user-types "string" string?)
	(hash-table-put! user-types "bool" boolean?)
	(hash-table-put! user-types "[]" null?)

	(define constructors (make-hash-table 'equal))
	(hash-table-put! constructors "true" (cons "bool" #t))
	(hash-table-put! constructors "false" (cons "bool" #f))
	(hash-table-put! constructors "[]" (cons (make-tlist (make-tvar "'a")) null))
	(hash-table-put! constructors "()" (cons "unit" (make-<unit> #f)))
;	(hash-table-put! constructors "::" (cons (make-arrow (list (make-<tuple> (list (make-tvar "'a") (make-tlist (make-tvar "'a"))))) (make-tlist (make-tvar "'a"))) cons))
	(hash-table-put! constructors "::" (cons (make-tconstructor (make-<tuple> (list (make-tvar "'a") (make-tlist (make-tvar "'a")))) (make-tlist (make-tvar "'a"))) cons))
	(hash-table-put! constructors "list" (cons (make-tlist (make-tvar "'a")) "some error"))
	(hash-table-put! constructors "float" (cons "float" "some error"))
	(hash-table-put! constructors "int" (cons "int" "some error"))
	(hash-table-put! constructors "bool" (cons "bool" "some error"))
	(hash-table-put! constructors "string" (cons "string" "some error"))
	(hash-table-put! constructors "char" (cons "char" "some error"))
	(hash-table-put! constructors "None" (cons (make-option (make-tvar "'a")) (make-option (make-<voidstruct> #f))))
	(hash-table-put! constructors "Some" (cons (make-tconstructor (make-tvar "'a") (make-option (make-tvar "'a"))) make-option))
	
	
	(define built-in-and-user-funcs (make-hash-table 'equal))
	(hash-table-put! built-in-and-user-funcs "+" (cons (make-arrow (list "int" "int") "int") +))
	(hash-table-put! built-in-and-user-funcs "+." (cons (make-arrow (list "float" "float") "float") +))
	(hash-table-put! built-in-and-user-funcs "-" (cons (make-arrow (list "int" "int") "int") -))
	(hash-table-put! built-in-and-user-funcs "-." (cons (make-arrow (list "float" "float") "float") -))
	(hash-table-put! built-in-and-user-funcs "*" (cons (make-arrow (list "int" "int") "int") *))
	(hash-table-put! built-in-and-user-funcs "*." (cons (make-arrow (list "float" "float") "float") *))
	(hash-table-put! built-in-and-user-funcs "/" (cons (make-arrow (list "int" "int") "int") quotient))
	(hash-table-put! built-in-and-user-funcs "/." (cons (make-arrow (list "float" "float") "float") /))
	(hash-table-put! built-in-and-user-funcs "=" (cons (make-arrow (list (make-tvar "'a") (make-tvar "'b")) "bool") equal?))
	(hash-table-put! built-in-and-user-funcs "==" (cons (make-arrow (list (make-tvar "'a") (make-tvar "'b")) "bool") equal?))
	(hash-table-put! built-in-and-user-funcs "<" (cons (make-arrow (list (make-tvar "'a") (make-tvar "'a")) "bool") <lt>))
	(hash-table-put! built-in-and-user-funcs "<=" (cons (make-arrow (list (make-tvar "'a") (make-tvar "'a")) "bool") <le>))
	(hash-table-put! built-in-and-user-funcs ">" (cons (make-arrow (list (make-tvar "'a") (make-tvar "'a")) "bool") <gt>))
	(hash-table-put! built-in-and-user-funcs ">=" (cons (make-arrow (list (make-tvar "'a") (make-tvar "'a")) "bool") <ge>))
	(hash-table-put! built-in-and-user-funcs "or" (cons (make-arrow (list "bool" "bool") "bool") <or>))
	(hash-table-put! built-in-and-user-funcs "||" (cons (make-arrow (list "bool" "bool") "bool") <or>))
	(hash-table-put! built-in-and-user-funcs "&&" (cons (make-arrow (list "bool" "bool") "bool") <and>))
	(hash-table-put! built-in-and-user-funcs "!=" (cons (make-arrow (list (make-tvar "'a") (make-tvar "'b")) "bool") !=))
	(hash-table-put! built-in-and-user-funcs "not" (cons (make-arrow (list "bool") "bool") not))
	(hash-table-put! built-in-and-user-funcs "~-" (cons (make-arrow (list "int") "int") -))
	(hash-table-put! built-in-and-user-funcs "float" (cons (make-arrow (list "int") "float") (lambda (x) x)))
	(hash-table-put! built-in-and-user-funcs "@" (cons (make-arrow (list (make-tlist (make-tvar "'a")) (make-tlist (make-tvar "'a"))) (make-tlist (make-tvar "'a"))) append))
	(hash-table-put! built-in-and-user-funcs "^" (cons (make-arrow (list "string" "string") "string") string-append))
	(hash-table-put! built-in-and-user-funcs "raise" (cons (make-arrow (list "exception") (make-tvar "'a")) raise))
	(hash-table-put! built-in-and-user-funcs ":=" (cons (make-arrow (list (make-ref (make-tvar "'a")) (make-tvar "'a")) "unit") <set-box!>))
	(hash-table-put! built-in-and-user-funcs "ref" (cons (make-arrow (list (make-tvar "'a")) (make-ref (make-tvar "'a"))) box))
	(hash-table-put! built-in-and-user-funcs "!" (cons (make-arrow (list (make-ref (make-tvar "'a"))) (make-tvar "'a")) unbox))
	

	
)