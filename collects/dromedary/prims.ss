(module prims mzscheme
	(require (lib "list.ss")
		 (lib "match.ss")
		 (lib "pretty.ss")
		 (lib "structure.ss")
		 (lib "math.ss"))
	(provide <library-names> built-in-and-user-funcs <constructors> <flatten> <cons>
		 (struct <tuple> (list))
		 (struct arrow (arglist result))
		 (struct tvar (tbox))
		 (struct tlist (type))
		 (struct tarray (type))
		 (struct tvariant (name params varnames variantlist))
		 (struct tabstract (name params type))
		 (struct tconstructor (argtype result))
		 (struct usertype (name params))
		 (struct option (type))
		 (struct <voidstruct> (dummy))
		 (struct <unit> (dummy))
		 (struct ref (type))
		 (struct mlexn (name types))
		 (struct ml-exn ())
		 (struct value-set (name type))
		 (struct <user-type> ())
		 (struct |Some| (tlist))
		 (struct |None| (tlist))
		 != <lt> <gt> <le> <ge> <or> <and> <>
		 float? any?
		 array-get
		 pretty-print
		 (all-from (lib "match.ss"))
		 (all-from (lib "structure.ss")))

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
	(define-struct <voidstruct> (dummy))
	(define-struct <unit> (dummy))
	(define-struct ref (type))
	(define-struct mlexn (name types))
	(define-struct ml-exn ())
	(define-struct option (type))
	(define-struct <user-type> () (make-inspector))

	(define-struct |Some| (tlist) (make-inspector))
	(define-struct |None| (tlist) (make-inspector))

	(define <library-names> (make-hash-table 'equal))

	;; The list functions


	(define (<map> a)
	  (lambda (b)
	    (map a b)))

	(define (<filter> a)
	  (lambda (b)
	    (filter a b)))

	(define (<foldr> a)
	  (lambda (b)
	    (lambda (c)
	      (foldr a c b))))


	(define (<foldrr> a)
	  (lambda (b)
	    (lambda (c)
	      (letrec ([last (lambda (l) (if (null? (cdr l))
					     (car l)
					     (last (cdr l))))]
		       [lcdr (lambda (l) (if (null? (cdr l))
					     null
					     (cons (car l) (lcdr (cdr l)))))])
	      (if (null? b)
		  c
		  (((<foldrr> a) (lcdr b)) ((a (last b)) c)))))))

	(define (<foldll> a)
	  (lambda (b)
	    (lambda (c)
	      (begin
;		(pretty-print (list a b c))
		(if (null? c)
		    
		  b
		  (((<foldll> a) ((a b) (car c))) (cdr c)))))))

	(define (<flattenf> a)
	  (if (null? a)
	      null
	      (append (car a) (<flattenf> (cdr a)))))

	(define <list-funcs> (make-hash-table 'equal))
	(hash-table-put! <list-funcs> "hd" (cons (make-arrow (list (make-tlist (make-tvar "'a"))) (make-tvar "'a")) car))
	(hash-table-put! <list-funcs> "tl" (cons (make-arrow (list (make-tlist (make-tvar "'a"))) (make-tlist (make-tvar "'a"))) cdr))
	(hash-table-put! <list-funcs> "rev" (cons (make-arrow (list (make-tlist (make-tvar "'a"))) (make-tlist (make-tvar "'a"))) reverse))
	(hash-table-put! <list-funcs> "map" (cons (make-arrow (list (make-arrow (list (make-tvar "'a")) (make-tvar "'b"))) (make-arrow (list (make-tlist (make-tvar "'a"))) (make-tlist (make-tvar "'b"))))  <map>))
	(hash-table-put! <list-funcs> "filter" (cons (make-arrow (list (make-arrow (list (make-tvar "'a")) "bool")) (make-arrow (list (make-tlist (make-tvar "'a"))) (make-tlist (make-tvar "'a")))) <filter>))
	(hash-table-put! <list-funcs> "append" (cons (make-arrow (list (make-tlist (make-tvar "'a"))) (make-arrow (list (make-tlist (make-tvar "'a"))) (make-tlist (make-tvar "'a")))) <append>))
	(hash-table-put! <list-funcs> "length" (cons (make-arrow (list (make-tlist (make-tvar "'a"))) "int") length))
	(hash-table-put! <list-funcs> "flatten" (cons (make-arrow (list (make-tlist (make-tlist (make-tvar "'a")))) (make-tlist (make-tvar "'a"))) <flattenf>))
	(hash-table-put!
	 <list-funcs> 
	 "fold_right" (cons 
		       (make-arrow 
			(list 
			 (make-arrow 
			  (list (make-tvar "'a"))
			  (make-arrow 
			   (list (make-tvar "'b"))
			   (make-tvar "'b"))))
			   (make-arrow (list (make-tlist (make-tvar "'a"))) (make-arrow (list (make-tvar "'b")) (make-tvar "'b")))) <foldrr>))
	(hash-table-put!
	 <list-funcs>
	 "fold_left" (cons 
		       (make-arrow 
			(list (make-arrow 
			       (list (make-tvar "'a"))
			       (make-arrow 
				(list (make-tvar "'b"))
				(make-tvar "'a"))))
			   (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tlist (make-tvar "'b"))) (make-tvar "'a")))) <foldll>))


	;; The string functions
	(define (<uppercase> oldstr)
	  (list->string (map char-upcase (string->list oldstr))))

	(define (<string-ref> str)
	  (lambda (int)
	    (string-ref str int)))

	(define <string-funcs> (make-hash-table 'equal))
	(hash-table-put! <string-funcs> "length" (cons (make-arrow (list "string") "int") string-length))
	(hash-table-put! <string-funcs> "uppercase" (cons (make-arrow (list "string") "string") <uppercase>))
	(hash-table-put! <string-funcs> "get" (cons (make-arrow (list "string") (make-arrow (list "int") "char")) <string-ref>))

	;; The array functions
	(define (array-get arr)
	  (lambda (pos)
;; Don't forget to return invalid argument
	    (vector-ref arr pos)))

	(define (array-set! arr)
	  (lambda (pos)
	    (lambda (item)
;; Don't forget to raise Invalid arument
	      (begin
		(vector-set! arr pos item)
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
	      (list->vector (map fun (mknum 0 (- leng 1)))))))

	(define (array-make_matrix dimx)
	  (lambda (dimy)
	    (lambda (e)
;; Don't forget to raise Invalid_Argument
	      (letrec ([fillarr (lambda (num)
				  (if (num > dimx)
				      null
				      (cons (make-vector dimy e) (fillarr (+ 1 num)))))])
		(list->vector (fillarr 1))))))

	(define array-create_matrix array-make_matrix)

	(define (array-append v1)
	  (lambda (v2)
	    (list->vector (append (vector->list v1) (vector->list v2)))))

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
	(hash-table-put! <library-names> "String" <string-funcs>)

	;; Curried primitives
	(define (<+> a)
	  (lambda (b)
	    (+ a b)))

	(define (<*> a)
	  (lambda (b)
	    (* a b)))

	(define (<-> a)
	  (lambda (b)
	    (- a b)))

	(define (<~-> a)
	  (- a))
	
	(define (<quotient> a)
	  (lambda (b)
	    (quotient a b)))

	(define (<remainder> a)
	  (lambda (b)
	    (remainder a b)))

	(define (</> a)
	  (lambda (b)
	    (/ a b)))

	(define (<equal?> a)
	  (lambda (b)
	    (equal? a b)))


	(define (<string-append> a)
	  (lambda (b)
	    (string-append a b)))

	(define (<expt> a)
	  (lambda (b)
	    (expt a b)))


	(define (!= a)
	  (lambda (b)
	    (not (equal? a b))))

	(define (<lt> a)
	  (lambda (b)
	    (cond
	     [(number? a)
	      (< a b)]
	     [(boolean? a)
	      (< (boolean-to-number a) (boolean-to-number b))]
	     [(string? a)
	      (string<? a b)]
	     [else
	      (pretty-print "Uncaught exception: Invalid_argument")])))

	(define (<le> a)
	  (lambda (b)
	    (cond
	     [(number? a)
	      (<= a b)]
	     [(boolean? a)
	      (<= (boolean-to-number a) (boolean-to-number b))]
	     [(string? a)
	      (string<=? a b)]
	     [else
	      (pretty-print "Uncaught exception: Invalid_argument")])))

	(define (<gt> a)
	  (lambda (b)
	    (cond
	     [(number? a)
	      (> a b)]
	     [(boolean? a)
	      (> (boolean-to-number a) (boolean-to-number b))]
	     [(string? a)
	      (string>? a b)]
	     [else
	      (pretty-print "Uncaught exception: Invalid_argument")])))

	(define (<ge> a)
	  (lambda (b)
	    (cond
	     [(number? a)
	      (>= a b)]
	     [(boolean? a)
	      (>= (boolean-to-number a) (boolean-to-number b))]
	     [(string? a)
	      (string>=? a b)]
	     [else
	      (pretty-print "Uncaught exception: Invalid_argument")])))

	(define (<set-box!> b)
	  (lambda (v)
	    (begin (set-box! b v)
		   (make-<unit> #f))))

	(define (<min> x)
	  (lambda (y)
	    (cond
	     [(number? x) (if (< x y) x y)]
	     [(string? x) (if (string<? x y) x y)]
	     [(boolean? x) (if x y x)])))

	(define (<max> x)
	  (lambda (y)
	    (cond
	     [(number? x) (if (> x y) x y)]
	     [(string? x) (if (string>? x y) x y)]
	     [(boolean? x) (if x x y)])))

	(define (<> a)
	  (lambda (b)
	    (not (equal? a b))))

	(define (<or> a)
	  (lambda (b)
	    (or a b)))

	(define (<and> a)
	  (lambda (b)
	    (and a b)))

	(define (print_char a)
	  (begin (display a) (make-<unit> #f)))

	(define (print_float a)
	  (begin (display a) (make-<unit> #f)))

	(define (print_int a)
	  (begin (display a) (make-<unit> #f)))

	(define (print_string a)
	  (begin (display a) (make-<unit> #f)))

	(define (print_newline a)
	  (begin (newline) (make-<unit> #f)))

	(define (boolean-to-number n)
	  (if n 1 0))

	(define (float? n)
	  (number? n))

	(define (any? n) #t)

	(define (<cons> tuple)
	  (cons (car (<tuple>-list tuple)) (cadr (<tuple>-list tuple))))

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
	(hash-table-put! <constructors> "::" (cons (make-tconstructor (make-<tuple> (list (make-tvar "'a") (make-tlist (make-tvar "'a")))) (make-tlist (make-tvar "'a"))) <cons>))
	(hash-table-put! <constructors> "list" (cons (make-tlist (make-tvar "'a")) "some error"))
	(hash-table-put! <constructors> "ref" (cons (make-ref (make-tvar "'a")) "some error"))
	(hash-table-put! <constructors> "array" (cons (make-tarray (make-tvar "'a")) "some error"))
	(hash-table-put! <constructors> "option" (cons (make-option (make-tvar "'a")) "some error"))
	(hash-table-put! <constructors> "float" (cons "float" "some error"))
	(hash-table-put! <constructors> "int" (cons "int" "some error"))
	(hash-table-put! <constructors> "bool" (cons "bool" "some error"))
	(hash-table-put! <constructors> "string" (cons "string" "some error"))
	(hash-table-put! <constructors> "char" (cons "char" "some error"))
	(hash-table-put! <constructors> "exn" (cons (make-ml-exn) "some error"))
	(hash-table-put! <constructors> "in_channel" (cons "in_channel" "some error"))
	(hash-table-put! <constructors> "out_channel" (cons "out_channel" "some error"))
	(hash-table-put! <constructors> "None" (cons (make-tconstructor null (make-option (make-tvar "'a"))) (|make-None| #f)))
	(hash-table-put! <constructors> "Some" (cons (make-tconstructor (make-tvar "'a") (make-option (make-tvar "'a"))) |make-Some|))
	
	
	(define built-in-and-user-funcs (make-hash-table 'equal))
	(hash-table-put! built-in-and-user-funcs "+" (cons (make-arrow (list "int") (make-arrow (list "int") "int")) <+>))
	(hash-table-put! built-in-and-user-funcs "+." (cons (make-arrow (list "float") (make-arrow (list "float") "float")) <+>))
	(hash-table-put! built-in-and-user-funcs "-" (cons (make-arrow (list "int") (make-arrow (list "int") "int")) <->))
	(hash-table-put! built-in-and-user-funcs "-." (cons (make-arrow (list "float") (make-arrow (list "float") "float")) <->))
	(hash-table-put! built-in-and-user-funcs "~-" (cons (make-arrow (list "int") "int") <~->))
	(hash-table-put! built-in-and-user-funcs "~-." (cons (make-arrow (list "float") "float") <~->))
	(hash-table-put! built-in-and-user-funcs "*" (cons (make-arrow (list "int") (make-arrow (list "int") "int")) <*>))
	(hash-table-put! built-in-and-user-funcs "*." (cons (make-arrow (list "float") (make-arrow (list "float") "float")) <*>))
	(hash-table-put! built-in-and-user-funcs "/" (cons (make-arrow (list "int") (make-arrow (list "int") "int")) <quotient>))
	(hash-table-put! built-in-and-user-funcs "/." (cons (make-arrow (list "float") (make-arrow (list "float") "float")) </>))
	(hash-table-put! built-in-and-user-funcs "mod" (cons (make-arrow (list "int") (make-arrow (list "int") "int")) <remainder>))
	(hash-table-put! built-in-and-user-funcs "=" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'b")) "bool")) <equal?>))
	(hash-table-put! built-in-and-user-funcs "==" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'b")) "bool")) <equal?>))
	(hash-table-put! built-in-and-user-funcs "<" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) "bool")) <lt>))
	(hash-table-put! built-in-and-user-funcs "<=" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) "bool")) <le>))
	(hash-table-put! built-in-and-user-funcs ">" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) "bool")) <gt>))
	(hash-table-put! built-in-and-user-funcs ">=" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) "bool")) <ge>))
	(hash-table-put! built-in-and-user-funcs "min" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) (make-tvar "'a"))) <min>))
	(hash-table-put! built-in-and-user-funcs "max" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'a")) (make-tvar "'a"))) <max>))
	(hash-table-put! built-in-and-user-funcs "or" (cons (make-arrow (list "bool") (make-arrow (list "bool") "bool")) <or>))
	(hash-table-put! built-in-and-user-funcs "||" (cons (make-arrow (list "bool") (make-arrow (list "bool") "bool")) <or>))
	(hash-table-put! built-in-and-user-funcs "&&" (cons (make-arrow (list "bool") (make-arrow (list "bool") "bool")) <and>))	
	(hash-table-put! built-in-and-user-funcs "|" (cons (make-arrow (list "bool") (make-arrow (list "bool") "bool")) <or>))
	(hash-table-put! built-in-and-user-funcs "&" (cons (make-arrow (list "bool") (make-arrow (list "bool") "bool")) <and>))
	(hash-table-put! built-in-and-user-funcs "!=" (cons (make-arrow (list (make-tvar "'a")) (make-arrow (list (make-tvar "'b")) "bool")) !=))
	(hash-table-put! built-in-and-user-funcs "not" (cons (make-arrow (list "bool") "bool") not))
	(hash-table-put! built-in-and-user-funcs "~-" (cons (make-arrow (list "int") "int") -))
	(hash-table-put! built-in-and-user-funcs "**" (cons (make-arrow (list "float") (make-arrow (list "float") "float")) <expt>))
	(hash-table-put! built-in-and-user-funcs "sqrt" (cons (make-arrow (list "float") "float") sqrt))
	(hash-table-put! built-in-and-user-funcs "float" (cons (make-arrow (list "int") "float") (lambda (x) x)))
	(hash-table-put! built-in-and-user-funcs "@" (cons (make-arrow (list (make-tlist (make-tvar "'a"))) (make-arrow (list (make-tlist (make-tvar "'a"))) (make-tlist (make-tvar "'a")))) <append>))
	(hash-table-put! built-in-and-user-funcs "^" (cons (make-arrow (list "string") (make-arrow (list "string") "string")) <string-append>))
	(hash-table-put! built-in-and-user-funcs "raise" (cons (make-arrow (list "exception") (make-tvar "'a")) raise))
	(hash-table-put! built-in-and-user-funcs ":=" (cons (make-arrow (list (make-ref (make-tvar "'a"))) (make-arrow (list (make-tvar "'a")) "unit")) <set-box!>))
	(hash-table-put! built-in-and-user-funcs "ref" (cons (make-arrow (list (make-tvar "'a")) (make-ref (make-tvar "'a"))) box))
	(hash-table-put! built-in-and-user-funcs "!" (cons (make-arrow (list (make-ref (make-tvar "'a"))) (make-tvar "'a")) unbox))
	(hash-table-put! built-in-and-user-funcs "print_int" (cons (make-arrow (list "int") "unit") print_int))
	(hash-table-put! built-in-and-user-funcs "print_char" (cons (make-arrow (list "char") "unit") print_char))
	(hash-table-put! built-in-and-user-funcs "print_float" (cons (make-arrow (list "float") "unit") print_float))
	(hash-table-put! built-in-and-user-funcs "print_string" (cons (make-arrow (list "string") "unit") print_string))
	(hash-table-put! built-in-and-user-funcs "print_newline" (cons (make-arrow (list "unit") "unit") print_newline))
	(hash-table-put! built-in-and-user-funcs "pretty_print" (cons (make-arrow (list (make-tvar "'a")) "unit") pretty-print))

	
	(define (<flatten> a-list)
	  (if (null? a-list)
	      null
	      (if (list? (car a-list))
		  (append (<flatten> (car a-list)) (<flatten> (cdr a-list)))
		  (cons (car a-list) (<flatten> (cdr a-list))))))
	)
