(module string mzscheme
	(require "basic.ss")
	(provide <string-funcs>)

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

)