

(load-relative "loadtest.ss")

(SECTION 'serilaization)

(require (lib "serialize.ss"))

;; ----------------------------------------

(define insp (current-inspector))

(define-serializable-struct a () insp)
(define-serializable-struct b (x y) insp)
(define-serializable-struct (c a) (z) insp)
(define-serializable-struct (d b) (w) insp)

(define (same? v1 v2)
  (define ht (make-hash-table))
  (let loop ([v1 v1][v2 v2])
    (cond
     [(hash-table-get ht v1 (lambda () #f))
      => (lambda (x) (eq? x v2))]
     [(and (a? v1)
	   (a? v2)
	   (not (c? v1))
	   (not (c? v2)))
      #t]
     [(and (b? v1)
	   (b? v2)
	   (not (d? v1))
	   (not (d? v2)))
      (hash-table-put! ht v1 v2)
      (and (loop (b-x v1) (b-x v2))
	   (loop (b-y v1) (b-y v2)))]
     [(and (c? v1) (c? v2))
      (hash-table-put! ht v1 v2)
      (loop (c-z v1) (c-z v2))]
     [(and (d? v1) (d? v2))
      (hash-table-put! ht v1 v2)
      (and (loop (b-x v1) (b-x v2))
	   (loop (b-y v1) (b-y v2))
	   (loop (d-w v1) (d-w v2)))]
     [(and (pair? v1)
	   (pair? v2))
      (hash-table-put! ht v1 v2)
      (and (eq? (immutable? v1) (immutable? v2))
	   (loop (car v1) (car v2))
	   (loop (cdr v1) (cdr v2)))]
     [(and (vector? v1)
	   (vector? v2))
      (hash-table-put! ht v1 v2)
      (and (eq? (immutable? v1) (immutable? v2))
	   (= (vector-length v1) (vector-length v2))
	   (andmap loop
		   (vector->list v1)
		   (vector->list v2)))]
     [(and (box? v1) (box? v2))
      (hash-table-put! ht v1 v2)
      (and (eq? (immutable? v1) (immutable? v2))
	   (loop (unbox v1) (unbox v2)))]
     [else
      (and (equal? v1 v2)
	   (eq? (immutable? v1) (immutable? v2)))])))
      

(define (test-ser v)
  (parameterize ([print-graph #t])
    (test #t same? v v)
    (test #t same? v (deserialize (serialize v)))))

;; ----------------------------------------

(test-ser 1)
(test-ser "apple")
(test-ser (string-copy "apple"))
(test-ser #"apple")
(test-ser (bytes-copy #"apple"))
(test-ser #\c)
(test-ser 145.79)
(test-ser 2/3)
(test-ser #t)
(test-ser #f)
(test-ser (void))
(test-ser 'ok)

(test-ser '(1))
(test-ser '#(1))
(test-ser '#&1)

(test-ser (cons 1 2))
(test-ser (cons-immutable 1 2))
(test-ser (vector))
(test-ser (vector 1 2))
(test-ser (vector-immutable))
(test-ser (vector-immutable 1 2))
(test-ser (box 10))
(test-ser (box-immutable 10))

(test-ser (make-a))
(test-ser (make-b 1 2))
(test-ser (make-c 30))
(test-ser (make-d 100 200 300))

;; Simple sharing
(let ([p (cons 1 2)])
  (test-ser (cons p p))
  (test-ser (vector p p))
  (test-ser (make-b p p))
  (test-ser (make-d p 1 p)))
(let ([p (vector 1 2 3)])
  (test-ser (cons p p))
  (test-ser (vector p p))
  (test-ser (make-b p p))
  (test-ser (make-d p 1 p)))
(let ([p (box 1)])
  (test-ser (cons p p))
  (test-ser (vector p p))
  (test-ser (make-b p p))
  (test-ser (make-d p 1 p)))

;; Cycles
(let ([p (cons 1 2)])
  (set-car! p p)
  (test-ser p)
  (set-cdr! p p)
  (test-ser p)
  (test-ser (make-c p))
  (test-ser (make-b p p)))
(let ([p (vector 1 2 3)])
  (vector-set! p 1 p)
  (test-ser p)
  (vector-set! p 2 p)
  (test-ser p)
  (test-ser (make-c p))
  (test-ser (make-b p p)))
(let ([p (box 1)])
  (set-box! p p)
  (test-ser p)
  (test-ser (make-c p))
  (test-ser (make-b p p)))
(let ([p (make-c 1)])
  (set-c-z! p p)
  (test-ser p)
  (test-ser (make-c p)))
(let ([p (make-b 1 2)])
  (set-b-x! p p)
  (test-ser p)
  (set-b-y! p p)
  (test-ser p)
  (set-b-x! p 1)
  (test-ser p)
  (test-ser (make-c p)))
(let ([p (make-d 1 2 3)])
  (set-b-x! p p)
  (test-ser p)
  (set-b-y! p p)
  (test-ser p)
  (set-d-w! p p)
  (test-ser p)
  (set-b-x! p 1)
  (test-ser p)
  (set-b-y! p 2)
  (test-ser p)
  (test-ser (make-c p)))

;; Cycles with immutable parts
(let* ([p1 (cons 1 2)]
       [p2 (cons-immutable 0 p1)])
  (set-cdr! p1 p2)
  (test-ser p1)
  (test-ser p2)
  (test-ser (cons p1 p2))
  (test-ser (cons p2 p1))
  (test-ser (make-c p1))
  (test-ser (make-b p1 p2))
  (test-ser (make-b p2 p1)))
(let* ([p1 (vector 1 2 3)]
       [p2 (vector-immutable 0 p1 4)])
  (vector-set! p1 1 p2)
  (test-ser p1)
  (test-ser p2)
  (test-ser (make-c p1))
  (test-ser (make-b p1 p2))
  (test-ser (make-b p2 p1)))
(let* ([p1 (box 1)]
       [p2 (box-immutable p1)])
  (set-box! p1 p2)
  (test-ser p1)
  (test-ser p2)
  (test-ser (make-c p1))
  (test-ser (make-b p1 p2))
  (test-ser (make-b p2 p1)))

;; ----------------------------------------

(report-errs)
