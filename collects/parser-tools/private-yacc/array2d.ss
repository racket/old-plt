#cs
(module array2d mzscheme
  
  ;; Macros to treat a vector as a 2 dimensional array
  
  (provide make-array2d array2d-ref array2d-set! array2d-add!)
  
  (define-syntax make-array2d
    (syntax-rules ()
      ((_ d1 d2 def)
       (let ((a (make-vector (add1 (* d1 d2)) def)))
	 (vector-set! a 0 d2)
	 a))))

  (define-syntax array2d-ref
    (syntax-rules ()
      ((_ a i1 i2)
       (let ((bound (vector-ref a 0)))
	 (if (or (< i2 0) (>= i2 bound))
	     (error 'array2d-ref 
		    "index ~a out of range [0, ~a] for array2d"
		    i2
		    (sub1 bound))
	     (vector-ref a (+ 1 i2 (* bound i1))))))))
  
  (define-syntax array2d-set!
    (syntax-rules ()
      ((_ a i1 i2 v)
       (let ((bound (vector-ref a 0)))
	 (if (or (< i2 0) (>= i2 bound))
	     (error 'array2d-set!
		    "index ~a out of range [0, ~a] for array2d"
		    i2
		    (sub1 bound))
	     (vector-set! a (+ 1 i2 (* bound i1)) v))))))

  (define-syntax array2d-add!
    (syntax-rules ()
      ((_ a i1 i2 v)
       (let ((bound (vector-ref a 0)))
         (if (or (< i2 0) (>= i2 bound))
             (error 'array2d-add!
		    "index ~a out of range [0, ~a] for array2d"
		    i2
		    (sub1 bound))
             (let ((idx (+ 1 i2 (* bound i1))))
               (vector-set! a idx (cons v (vector-ref a idx))))))))))