(module tests mzscheme
  (provide homo-vec-test)

  (require (lib "mz-testing.ss" "tests" "utils"))
  
  (define (homo-vec-test vec->homo   
                         maker
                         homo->vec
                         length
                         getter
                         setter
                         pred
                         sum
                         difference
                         scale
                         norm
                         type-idx
                         filling-maker
                         list->homo
                         homo->list)
    (test #t pred (maker 3 4 5))
    (test #t pred (maker))
    (arity-test pred 1 1)
    (test 3 inexact->exact (getter (maker 3 4 5) 0))
    (test 4 inexact->exact (getter (maker 3 4 5) 1))
    (test 5 inexact->exact (getter (maker 3 4 5) 2))
    (test 3 length (maker 3 4 5))
    (test 0 length (maker))
    (arity-test length 1 1)
    (err/rt-test (length "apple") exn:application:type?)
    (arity-test getter 2 2)
    (err/rt-test (getter "apple" 3) exn:application:type?)
    (err/rt-test (getter (maker 4 5 6) 3) exn:application:mismatch?)
    (err/rt-test (getter (maker) 0) exn:application:mismatch?)
    (err/rt-test (getter (maker) (expt 2 100)) exn:application:mismatch?)
    (err/rt-test (getter (maker 4 5 6) -1))
    (err/rt-test (getter (maker 4 5 6) 2.0))
    (err/rt-test (getter (maker 4 5 6) "2"))
    (let ((vec (maker 4 5 6)))
      (setter vec 1 63)
      (test 63 inexact->exact (getter vec 1)))
    (test `(3 3) map inexact->exact (homo->list (filling-maker 2 3)))
    (test `() homo->list (filling-maker 0))
    (test `() homo->list (filling-maker 0 14))
    (test 2048 length (filling-maker 2048 14))
    (arity-test filling-maker 1 2)
    (err/rt-test (filling-maker "a" 2))
    (err/rt-test (filling-maker 1.0 2))
    (err/rt-test (filling-maker 10.2 2))
    (err/rt-test (filling-maker -1 2))
    (err/rt-test (filling-maker 1000000000000000000000 2) exn:application:type?)
    (arity-test setter 3 3)
    (err/rt-test (setter (maker) 0 'x) exn:application:mismatch?)
    (err/rt-test (setter (maker 1 2 3) -1 'x))
    (err/rt-test (setter (maker 1 2 3) 3 'x) exn:application:mismatch?)
    (err/rt-test (setter (maker 1 2 3) (expt 2 100) 'x) exn:application:mismatch?)
    (err/rt-test (setter '(1 2 3) 2 'x))
    (err/rt-test (setter (maker 1 2 3) "2" 'x))
    (arity-test sum 2 2)
    (arity-test difference 2 2)
    (arity-test norm 1 1)
    (arity-test scale 2 2)
    (test `(3 4 5) map inexact->exact (homo->list (list->homo `(3 4 5))))
    (test `(3 4 5) map inexact->exact (homo->list (maker 3 4 5)))))
