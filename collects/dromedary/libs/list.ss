(module list mzscheme
	(require "basic.ss"
		 (lib "list.ss"))
	(provide <list-funcs>)

		;; The list functions


	(define (<map> a)
	  (lambda (b)
	    (map a b)))

	(define (<filter> a)
	  (lambda (b)
	    (filter a b)))

	(define (<append> a)
	  (lambda (b)
	    (append a b)))

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

)