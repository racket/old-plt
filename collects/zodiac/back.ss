(define zodiac:scheme-core@
  (unit/sig (make-empty-back-box register-client)
    (import zodiac:misc^ zodiac:interface^)

    (define-struct secure-box (value))

    (define init-value-list '())

    (define register-initial-value
      (lambda (index value)
	(set! init-value-list
	  (append init-value-list
	    (list value)))))

    (define make-initial-value-vector
      (lambda ()
	(let ((v (make-vector current-vector-size)))
	  (let loop ((index 0) (inits init-value-list))
	    (unless (null? inits)
	      (vector-set! v index (car inits))
	      (loop (add1 index) (cdr inits))))
	  v)))

    (define make-empty-back-box
      (lambda ()
	(make-secure-box (make-initial-value-vector))))

    (define current-vector-size 2)

    (define next-client-count
      (let ((count -1))
	(lambda ()
	  (set! count (add1 count))
	  (when (>= count current-vector-size)
	    (set! current-vector-size (* 2 current-vector-size)))
	  count)))

    (define client-registry (make-hash-table))

    (define register-client
      (lambda (client-name default-initial-value)
	(when (hash-table-get client-registry client-name
		(lambda () #f))
	  (internal-error client-name "Attempting duplicate registration"))
	(hash-table-put! client-registry client-name #t)
	(let ((index (next-client-count)))
	  (register-initial-value index default-initial-value)
	  (values
	    (lambda (back)		; getter
	      (let ((v (secure-box-value back)))
		(with-handlers
		  ((exn:application:range:bounds:vector?
		     (lambda (exception)
		       (vector-ref (extend-back-vector back) index))))
		  (vector-ref v index))))
	    (lambda (back value)	; setter
	      (let ((v (secure-box-value back)))
		(with-handlers
		  ((exn:application:range:bounds:vector?
		     (lambda (exception)
		       (vector-set! (extend-back-vector back) index value))))
		  (vector-set! v index value))))))))

    (define extend-back-vector
      (lambda (back-box)
	(let ((v (secure-box-value back-box)))
	  (let ((new-v (make-initial-value-vector)))
	    (let loop ((n (sub1 (vector-length v))))
	      (when (>= n 0)
		(vector-set! new-v n (vector-ref v n))
		(loop (sub1 n))))
	    (set-secure-box-value! back-box new-v)
	    new-v))))

    ))
