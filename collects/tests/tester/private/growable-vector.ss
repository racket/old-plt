;; growable-vector.ss : provides the datatype gvector, which is a growable vector. It supports
;; the same operations a vector does (list->gvector, gvector->list, gvector-length, gvector-ref,
;; gvector-set!
(module growable-vector mzscheme
  (provide (rename produce-gvector make-gvector)
           list->gvector
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

;; list->gvector : (listof value) -> gvector[value]
(define (list->gvector l)
  (let ((v (list->vector l)))
    (make-gvector v (vector-length v))))
  
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

;; vector-copy! : vector x vector -> void
;; copies the contents of the first vector into the second
;; does not check that the new vector is large enough because
;; all code that calls this function knows the vector is big enough anyway
(define (vector-copy! old-vec new-vec)
  (let ((old-size (vector-length old-vec)))
    (let loop ((i 0))
      (cond
        [(= i old-size) (void)]
        [else
         (begin
           (vector-set! new-vec i (vector-ref old-vec i))
           (loop (add1 i)))]))))
  
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