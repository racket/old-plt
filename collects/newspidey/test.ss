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
