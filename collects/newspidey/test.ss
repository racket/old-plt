(case-lambda)

((lambda (x y) x)
 "hello")

((case-lambda
  [(x) x]
  [(y z) y])
 42)

((case-lambda
  [(x) x]
  [(y) y])
 64)

((case-lambda
  [x x]
  [y y])
 64)

((case-lambda
  [x x]
  [(y) y])
 64)

((case-lambda
  [(x) x]
  [y y])
 64)

((case-lambda
  [(x) "hello"]
  [(x y) 65])
 17)

((case-lambda
  [(f) (f 99)]
  [(f x y) (f x y)])
 (lambda (z) z))

((lambda (x) x)
 37 94)

((lambda (x y) x)
 1 2 3)

((lambda (x) x)
 3)

((lambda (x) x)
 (lambda (x) x))

(((lambda (x) x)
  (lambda (x) x))
 3)

(((lambda (x) x)
  (lambda (x) x))
 ((lambda (x) x)
  (lambda (x) x)))

((((lambda (x) x)
   (lambda (x) x))
  ((lambda (x) x)
   (lambda (x) x)))
 3)

((case-lambda
  [() 1]
  [(x) 2]))

((case-lambda
  [x 1]
  [(x) 2]))

((lambda (x)
   (if (null? x) x (cons x '())))
 '())

(g1 1)
(define g1 (lambda (x) (f1 x)))
(g1 3)
(define f1 (lambda (x) 2))
(g1 4)
(define f1 (lambda (x) 5))
(g1 6)

(define g2 (lambda (k x) (k x)))
(define f2 (lambda (y) 1))
(g2 f2 3)
(define h2 (lambda (z) 2))
(g2 h2 4)

(define g3 (lambda (f x) (f x)))
(define f3 (lambda (x) 1))
(g3 f3 3)
(define h3 (lambda (x . y) 2))
(g3 h3 4)

(define f4 (lambda (x) x))
(f4 1 2)

(define-struct foo (a b))
(make-foo 1 #t)
(define xfoo (make-foo 2 #f))
(set-foo-a! xfoo 3)
(set-foo-b! "hello")
(foo? xfoo)
(foo-a xfoo)

(cons 4 '())
(cons 5 6 '())
(cons 7 '())

(if #t 1 2)

(car (if #f (cons 1 '()) 2))

(let ([a 1]
      [b (lambda (x) x)])
  (b a))

;(define b5 (lambda (x) x))
;(b5 b5)

(let ([b (lambda (x) x)])
  b)
;(let ([b (lambda (x) x)])
;  (b b))

(let ([a ((lambda (x y) y) 1 cons)] ;; cons
      [b (lambda (x) x)]) ;; Id
  (let ([c ((lambda (x) x) a)] ;; cons
        [e (lambda (x) x)]  ;; Id
        [f (lambda (x) x)]) ;; Id. can't apply b directly to itself below => infinite type
    (let ([d ((e b) (b f))]) ;; Id, f flows into b twice, because the result of (b f) is f and the result of (e b) is b
      (d c)))) ;; cons

