;; growable-vector.ss : provides the datatype gvector, which is a growable vector. It supports
;; the same operations a vector does (list->gvector, gvector->list, gvector-length, gvector-ref,
;; gvector-set!
(module growable-vector mzscheme
  (provide (rename produce-gvector make-gvector)
           gvector
           list->gvector
           vector->gvector
           gvector->vector
           gvector->list
           gvector-length
           gvector-ref
           gvector-set!
           gvector-add!
           gvector-grow!
           gvector->string)
  
(define-struct gvector (items curr-size))
  
;; produce-gvector : natnum -> gvector
;;                   natnum x value -> gvector
;; produces a gvector. has the same constraints make-vector has
(define produce-gvector
  (let ((unspecified (gensym)))
    (case-lambda
     [(k) (produce-gvector k unspecified)]
     [(k fill)
      (if (and (number? k) (>= k 0))
          (make-gvector
           (if (eq? fill unspecified)
               (make-vector k)
               (make-vector k fill))
           k)
          (raise-mismatch-error 'make-gvector "expects k >= 1, given: " k))])))

;; gvector : X ... -> gvector[X]
;; makes a gvector of the given arguments. analagous to the vector function.
(define gvector
  (lambda args
    (list->gvector args)))
  
;; vector->gvector: vector[X] -> gvector[X]
;; produces a gvector with the same initial size and contents as the given
;; vector. The result does not share memory with the input.
(define (vector->gvector vec)
  (let ((newvec (make-vector (vector-length vec))))
    (begin
      (vector-copy! vec newvec)
      (make-gvector newvec (vector-length newvec)))))
  
;; gvector->vector : gvector[X] -> vector[X]
;; produces a vector with the same size and contents as the given gvector.
;; the result vector does not share memory with the gvector.
(define (gvector->vector gvec)
  (let ((newvec (make-vector (gvector-curr-size gvec))))
    (begin
      (vector-copy-n! (gvector-curr-size gvec)
                      (gvector-items gvec)
                      newvec)
      newvec)))
  
;; list->gvector : (listof value) -> gvector[value]
(define (list->gvector l)
  (let ((v (list->vector l)))
    (make-gvector v (vector-length v))))

;; gvector->list : gvector[X] -> (listof X)
;; produces a list version of the given gvector
(define (gvector->list gvec)
  (let ((len (gvector-curr-size gvec))
        (items (gvector-items gvec)))
    (let loop ((i 0))
      (cond
        [(= i len) 
         null]
        [else
         (cons (vector-ref items i) (loop (add1 i)))]))))   
   
;; gvector-length : gvector -> natnum
;; reports the length of the given gvector
(define (gvector-length gvec) (gvector-curr-size gvec))
  
;; gvector-ref : gvector x natnum -> value
;; returns the value at position n of the given gvector
(define (gvector-ref gvec n)
  (if (and (>= n 0) (< n (gvector-length gvec)))
      (vector-ref (gvector-items gvec) n)
      (raise-mismatch-error 'gvector-ref "Index out of bounds: " n)))
  
;; gvector-set! : gvector x natnum x value -> void
;; sets the item at the given index to the given value
(define (gvector-set! gvec n val)
  (if (and (>= n 0) (< n (gvector-length gvec)))
      (vector-set! (gvector-items gvec) n val)
      (raise-mismatch-error 'gvector-set! "Index out of bounds: " n)))

;; gvector-add! : gvector x value -> void
;; adds an item to the end of the given gvector, growing its size
;; by one
(define (gvector-add! gvec new-item)
  (let ((old-size (gvector-curr-size gvec))
        (old-items (gvector-items gvec)))
    (if (> (vector-length old-items) old-size)
        (begin
          (vector-set! old-items old-size new-item)
          (set-gvector-curr-size! gvec (add1 old-size)))
        (let ((new-vec (make-vector (if (zero? old-size) 1 (* 2 old-size)))))
          (begin
            (vector-copy! old-items new-vec)
            (vector-set! new-vec old-size new-item)
            (set-gvector-items! gvec new-vec)
            (set-gvector-curr-size! gvec (add1 old-size)))))))

;; vector-copy! : vector[value] x vector[value] -> void
;; copies the contents of the first vector into the second vector without
;; checking to see if the new vector has enough space.
(define (vector-copy! old new)
  (vector-copy-n! (vector-length old) old new))

;; vector-copy-n! : natnum x vector[value] x vector[value] -> void
;; copies the first n elements of the old vector into the new vector.
;; does not check to see if there is enough room in the new vector.
(define (vector-copy-n! n old-vec new-vec)
  (let loop ((i 0))
    (cond
      [(= i n) (void)]
      [else
       (begin
         (vector-set! new-vec i (vector-ref old-vec i))
         (loop (add1 i)))])))
  
;; gvector-grow! : gvector x number x [value] -> void
;; grows the given gvector to at least the given size. If the gvector is
;; already larger than the given size, it is unchanged. If a third value is provided, 
;; it is the fill for new cells.
(define gvector-grow!
  (let ((grow-internal
         (lambda (gvec n get-vec)
           (if (< (gvector-curr-size gvec) n)
               (begin
                 (set-gvector-curr-size! gvec n)
                 (if (< (vector-length (gvector-items gvec)) n)
                     (let ([old-vec (gvector-items gvec)]
                           [new-vec (get-vec)])
                       (begin
                         (vector-copy! old-vec new-vec)
                         (set-gvector-items! gvec new-vec)))))))))
    (case-lambda 
     [(gvec n)
      (grow-internal gvec n (lambda () (make-vector n)))]
     [(gvec n fill)
      (grow-internal gvec n (lambda () (make-vector n fill)))])))
      
;; gvector->string : gvector -> str
;; produces a human-readable string representing the gvector
(define (gvector->string gvec)
  (apply string-append "#" (gvector-curr-size) (gvector->list gvec))))