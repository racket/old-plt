(module array mzscheme
	(require "basic.ss")
	(provide <array-funcs>)

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

)