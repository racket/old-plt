(module prims mzscheme
	(require (lib "list.ss")
		 (lib "match.ss")
		 (lib "pretty.ss"))
	(provide <library-names> user-types built-in-and-user-funcs <constructors>
		 (struct <tuple> (list))
		 (struct arrow (arglist result))
		 (struct tvar (tbox))
		 (struct tlist (type))
		 (struct tarray (type))
		 (struct tvariant (name varnames variantlist))
		 (struct tconstructor (argtype result))
		 (struct usertype (name))
		 (struct option (type))
		 (struct <voidstruct> (dummy))
		 (struct <unit> (dummy))
		 (struct ref (type))
		 (struct mlexn (name types))
		 != <lt> <gt> <le> <ge> <or> <and> <>
		 float? any?
		 array-get
		 (all-from (lib "match.ss")))


	(define-struct <tuple> (list) (make-inspector))
	(define-struct arrow (arglist result) (make-inspector))
	(define-struct tvar (tbox) (make-inspector))
	(define-struct tlist (type) (make-inspector))
	(define-struct tarray (type) (make-inspector))
	(define-struct tvariant (name varnames variantlist) (make-inspector))
	(define-struct tconstructor (argtype result) (make-inspector))
	(define-struct usertype (name))
	(define-struct <voidstruct> (dummy))
	(define-struct <unit> (dummy))
	(define-struct ref (type))
	(define-struct mlexn (name types))
	(define-struct option (type))

	(define <library-names> (make-hash-table 'equal))

	;; The list functions
	(define <list-funcs> (make-hash-table 'equal))
	(hash-table-put! <list-funcs> "hd" (cons (make-arrow (list (make-tlist (make-tvar "'a"))) (make-tvar "'a")) car))
	(hash-table-put! <list-funcs> "tl" (cons (make-arrow (list (make-tlist (make-tvar "'a"))) (make-tlist (make-tvar "'a"))) cdr))
	(hash-table-put! <list-funcs> "rev" (cons (make-arrow (list (make-tlist (make-tvar "'a"))) (make-tlist (make-tvar "'a"))) reverse))
	(hash-table-put! <list-funcs> "map" (cons (make-arrow (list (make-arrow (list (make-tvar "'a")) (make-tvar "'b"))) (make-arrow (list (make-tlist (make-tvar "'a"))) (make-tlist (make-tvar "'b"))))  map))
	(hash-table-put! <list-funcs> "filter" (cons (make-arrow (list (make-tlist (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) "bool")) (make-tlist (make-tvar "'a"))) filter))
	(hash-table-put! <list-funcs> "append" (cons (make-arrow (list (make-tlist (make-tvar "'a")) (make-tlist (make-tvar "'a"))) (make-tlist (make-tvar "'a"))) append))



	(define (array-get arr)
	  (lambda (pos)
;; Don't forget to return invalid argument
	    (vector-ref (+ 1 arr) pos)))

	(define (array-set! arr)
	  (lambda (pos)
	    (lambda (item)
;; Don't forget to raise Invalid arument
	      (begin
		(vector-set! arr (+ 1 pos) item)
		(make-<unit> #f)))))

	(define (make-array k)
	  (lambda (item)
;; Don't forget to raise Invalid_Argument
	    (make-vector k item)))

	(define create-array make-array)

	(define (array-init leng)
	  (lambda (fun)
	    (letrec ([mknum (lambda (num lim)
			      (if (num > lim)
				  null
				  (cons num (mknum (+ 1 num) lim))))])
	      (eval `(vector ,@(map fun (mknum 0 (- leng 1))))))))

	(define (array-make_matrix dimx)
	  (lambda (dimy)
	    (lambda (e)
;; Don't forget to raise Invalid_Argument
	      (letrec ([fillarr (lambda (num)
				  (if (num > dimx)
				      null
				      (cons (make-vector dimy e) (fillarr (+ 1 num)))))])
		(eval `(vector ,@(fillarr 1)))))))

	(define array-create_matrix array-make_matrix)

	(define (array-append v1)
	  (lambda (v2)
	    (eval `(vector ,@(append (vector->list v1) (vector->list v2))))))

	(define (array-concat v1)
	  (if (null? v1)
	      null
	      ((array-append (car v1)) (array-concat (cdr v1)))))

	;; The array functions
	(define <array-funcs> (make-hash-table 'equal))
	(hash-table-put! <array-funcs> "length" (cons (make-arrow (list (make-tarray (make-tvar "'a"))) "int") vector-length))
	(hash-table-put! <array-funcs> "get" (cons (make-arrow (list (make-tarray (make-tvar "'a"))) (make-arrow (list "int") (make-tvar "'a"))) array-get))
	(hash-table-put! <array-funcs> "set" (cons (make-arrow (list (make-tarray (make-tvar "'a"))) (make-arrow (list "int") (make-arrow (list (make-tvar "'a")) "unit"))) array-set!))
	(hash-table-put! <array-funcs> "make" (cons (make-arrow (list "int") (make-arrow (list (make-tvar "'a")) (make-tarray (make-tvar "'a")))) make-array))		(hash-table-put! <array-funcs> "create" (cons (make-arrow (list "int") (make-arrow (list (make-tvar "'a")) (make-tarray (make-tvar "'a")))) create-array))
	(hash-table-put! <array-funcs> "init" (cons (make-arrow (list "int") (make-arrow (list (make-arrow (list "int") (make-tvar "'a"))) (make-tarray (make-tvar "'a")))) array-init))
	(hash-table-put! <array-funcs> "make_matrix" (cons (make-arrow (list "int") (make-arrow (list "int") (make-arrow (list (make-tvar "'a")) (make-tarray (make-tarray (make-tvar "'a")))))) array-make_matrix))
	(hash-table-put! <array-funcs> "create_matrix" (cons (make-arrow (list "int") (make-arrow (list "int") (make-arrow (list (make-tvar "'a")) (make-tarray (make-tarray (make-tvar "'a")))))) array-create_matrix))
	(hash-table-put! <array-funcs> "append" (cons (make-arrow (make-tarray (make-tvar "'a")) (make-arrow (make-tarray (make-tvar "'a")) (make-tarray (make-tvar "'a")))) array-append))
	(hash-table-put! <array-funcs> "concat" (cons (make-arrow (list (make-tlist (make-tarray (make-tvar "'a")))) (make-tarray (make-tvar "'a"))) array-concat))
	
	;; Fill up all the libraries
	(hash-table-put! <library-names> "List" <list-funcs>)
	(hash-table-put! <library-names> "Array" <array-funcs>)

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

	(define (<> a b)
	  (not (equal? a b)))

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

	(define <constructors> (make-hash-table 'equal))
	(hash-table-put! <constructors> "true" (cons "bool" #t))
	(hash-table-put! <constructors> "false" (cons "bool" #f))
	(hash-table-put! <constructors> "[]" (cons (make-tlist (make-tvar "'a")) null))
	(hash-table-put! <constructors> "()" (cons "unit" (make-<unit> #f)))
;	(hash-table-put! <constructors> "::" (cons (make-arrow (list (make-<tuple> (list (make-tvar "'a") (make-tlist (make-tvar "'a"))))) (make-tlist (make-tvar "'a"))) cons))
	(hash-table-put! <constructors> "::" (cons (make-tconstructor (make-<tuple> (list (make-tvar "'a") (make-tlist (make-tvar "'a")))) (make-tlist (make-tvar "'a"))) cons))
	(hash-table-put! <constructors> "list" (cons (make-tlist (make-tvar "'a")) "some error"))
	(hash-table-put! <constructors> "float" (cons "float" "some error"))
	(hash-table-put! <constructors> "int" (cons "int" "some error"))
	(hash-table-put! <constructors> "bool" (cons "bool" "some error"))
	(hash-table-put! <constructors> "string" (cons "string" "some error"))
	(hash-table-put! <constructors> "char" (cons "char" "some error"))
	(hash-table-put! <constructors> "None" (cons (make-option (make-tvar "'a")) (make-option (make-<voidstruct> #f))))
	(hash-table-put! <constructors> "Some" (cons (make-tconstructor (make-tvar "'a") (make-option (make-tvar "'a"))) make-option))
	
	
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