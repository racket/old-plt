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
