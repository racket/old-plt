(module prims mzscheme
	(require (lib "list.ss")
		 (lib "match.ss"))
	(provide library-names
		 (struct tuple (list))
		 !=
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
	  (not (= a b))))