(module prims mzscheme
	(require (lib "list.ss")
		 (lib "match.ss")
		 (lib "pretty.ss")
		 (lib "structure.ss")
		 (lib "math.ss")

		 ;; Basic structure needed for ML
		 "libs/basic.ss"

		 ;; ML libraries
		 "libs/built-in.ss"
		 "libs/list.ss"
		 "libs/array.ss"
		 "libs/string.ss"
)
	(provide (all-from "libs/basic.ss")
		 <library-names> built-in-and-user-funcs <constructors> <flatten> <cons>

		 float? any?
		 pretty-print
		 (all-from (lib "match.ss"))
		 (all-from (lib "structure.ss"))
		 )


	(define <library-names> (make-hash-table 'equal))

	;; Fill up all the libraries
	(hash-table-put! <library-names> "List" <list-funcs>)
	(hash-table-put! <library-names> "Array" <array-funcs>)
	(hash-table-put! <library-names> "String" <string-funcs>)

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
	(hash-table-put! <constructors> "()" (cons "unit" (make-<unit>)))
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
	(hash-table-put! built-in-and-user-funcs "pretty_print" (cons (make-arrow (list (make-tvar "'a")) "unit") pretty-print))

	
	(define (<flatten> a-list)
	  (if (null? a-list)
	      null
	      (if (list? (car a-list))
		  (append (<flatten> (car a-list)) (<flatten> (cdr a-list)))
		  (cons (car a-list) (<flatten> (cdr a-list))))))
	)
