(module prims mzscheme
	(require (lib "list.ss")
		 (lib "match.ss"))
	(provide library-names user-types
		 (struct tuple (list))
		 !=
		 float? any?
		 (all-from (lib "match.ss")))

	(define library-names (make-hash-table 'equal))

	;; The list functions
	(define list-funcs (make-hash-table 'equal))
	(hash-table-put! list-funcs "hd" car)
	(hash-table-put! list-funcs "tl" cdr)
	(hash-table-put! list-funcs "rev" reverse)
	(hash-table-put! list-funcs "map" map)
	(hash-table-put! list-funcs "filter" filter)
	(hash-table-put! list-funcs "append" append)

	;; Fill up all the libraries
	(hash-table-put! library-names "List" list-funcs)

	(define-struct tuple (list) (make-inspector))

	(define (!= a b)
	  (not (= a b)))

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
	
)