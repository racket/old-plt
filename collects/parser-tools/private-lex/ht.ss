(module ht mzscheme

   ;; This is a simple hash table that doubles in size when a load factor is
   ;; reached.  Chaining is used to resolve collisions.  The only values it
   ;; can hash are number lists, and the equal? comparison is used.
   ;; This will allow us to use a cannonical representation for each 
   ;; state (which is an int list) in the dfa and its transitions

  (define-struct table (buckets elements max-elements))

  (provide make-ht ht-put! ht-get)

  (define (make-ht)
    (make-table (make-vector 256 null) 0 192))

  (define-struct element (key value))

  (define (get-hash-code l)
    (apply + l))

  (define (ht-for-each h fun!)
    (let* ((v (table-buckets h)))
      (let loop ((i 0)
		 (chain (vector-ref v 0)))
	(cond
	 ((and (= i (sub1 (vector-length v))) (null? chain))
	  null)
	 ((null? chain)
	  (loop (add1 i) (vector-ref v (add1 i))))
	 (else
	  (fun! (element-key (car chain)) (element-value (car chain)))
	  (loop i (cdr chain)))))))
  
  (define (double-ht! h)
    (let* ((new-size (* 2 (vector-length (table-buckets h))))
	   (v (make-vector new-size null)))
      (ht-for-each h (lambda (key value)
			     (put! v 
				   key
				   value)))
      (set-table-buckets! h v)
      (set-table-max-elements! h (* 2 (table-max-elements h)))))
  
  (define (put! v key value)
    (let* ((bucket (bitwise-and (get-hash-code key) 
				(sub1 (vector-length v)))))
      (vector-set! v bucket (cons (make-element key value)
				  (vector-ref v bucket)))))

  (define (ht-put! h key value)
    (if (>= (table-elements h) (table-max-elements h))
	(double-ht! h))
    (put! (table-buckets h) key value)
    (set-table-elements! h (add1 (table-elements h))))

  (define (ht-get h key fail)
    (let ((bucket (bitwise-and (get-hash-code key) 
			       (sub1 (vector-length (table-buckets h))))))
      (let loop ((chain (vector-ref (table-buckets h) bucket)))
	(cond
	 ((null? chain) (fail))
	 ((equal? (element-key (car chain))
		  key)
	  (element-value (car chain)))
	 (else
	  (loop (cdr chain)))))))

)