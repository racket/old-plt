
(define files (vector->list argv))
(read-case-sensitive #t)

;;;;;; Load decls

(define source (make-hash-table))

(define limit 100)

(for-each
 (lambda (f)
   (with-input-from-file f
     (lambda ()
       (let ([fname (let-values ([(base name dir?) (split-path f)])
		      (let* ([m (regexp-match "^(.*)[.]map$" name)]
			     [n (cadr m)])
			(if (> (string-length n) 6)
			    (substring n 0 6)
			    n)))]
	     [cnt 0])
	 (let loop ()
	   (let ([r (read)])
	     (unless (eof-object? r)
	       (case (car r)
		 [(impl) 
		  (set! cnt (add1 cnt))
		  (hash-table-put! source (cadr r) 
				   (if (< cnt limit)
				       fname
				       (format "~a~c" fname 
					       (integer->char
						(+ 65 (quotient cnt limit))))))]
		 [(decl) (hash-table-get source (cadr r)
					 (lambda ()
					   (hash-table-put! source (cadr r) #f)))]
		 [else (void)])
	       (loop))))))))
 files)

;;;;;; Output labels

(with-output-to-file "segmap.h"
  (lambda ()
    (hash-table-for-each
     source
     (lambda (k v)
       (printf "#define SEGOF_~a ~a~n" k
	       (if v
		   (format "__attribute__ ((section (~s)))" v)
		   "/* default */")))))
  'truncate)

(with-output-to-file "mz.def"
  (lambda ()
    (printf "app { \"MzScheme\" MzSc }~n")
    (printf "multiple code { ")
    (let ([t (make-hash-table)])
      (hash-table-for-each
       source
       (lambda (k v)
	 (when v
	   (let ([s (string->symbol v)])
	     (hash-table-get t s
			     (lambda ()
			       (hash-table-put! t s #t)
			       (printf "~s " s))))))))
    (printf "}~n"))
  'truncate)
