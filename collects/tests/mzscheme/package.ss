
(load-relative "loadtest.ss")
(require (lib "structure.ss"))


(SECTION 'packages)

;; Test suite does not cover:
;;  Ensuring that structures do not leak non-provided variables
;;  open-in-context and other transformer sensitive features
;;    of structure (such as the effect of the context on the
;;    provide-all identifier


;; syntax
(syntax-test #'(dot))             ;;Improve error message
(syntax-test #'(dot 1))
(syntax-test #'(dot 1 2))
(syntax-test #'(dot 1 x))

(syntax-test #'(open))            ;;Improve error message
(syntax-test #'(open 1))
(syntax-test #'(open 1 2))
(syntax-test #'(open 1 x))

(syntax-test #'(open-as))            ;;Improve error message
(syntax-test #'(open-as 1))          ;;Improve error message
(syntax-test #'(open-as x))          ;;Improve error message
(syntax-test #'(open-as 1 2))        ;;Improve error message
(syntax-test #'(open-as 1 x))        ;;Improve error message
(syntax-test #'(open-as x 1))        ;;Improve error message
(syntax-test #'(open-as x y))        ;;Improve error message
(syntax-test #'(open-as 1 x y))
(syntax-test #'(open-as x y 3))
(syntax-test #'(open-as x 2 y))

(syntax-test #'(structure))       ;;Improve error message
(syntax-test #'(structure x))     ;;Improve error message
(syntax-test #'(structure 1))     ;;Improve error message
(syntax-test #'(structure x 1))
(syntax-test #'(structure x x))
(syntax-test #'(structure x (1))) ;;Improve error message



;; Providing
(structure p1 provide-all
  (define x 10)
  (structure y provide-all
    (define x 12)))

(structure p2 ()
  (define x 10))

(structure p3 (x)
  (structure x provide-all
    (define x 10)))

(structure p4 provide-all
  (structure x (x)
    (define x 10)
    (define y 11)))

(err/rt-test x exn:variable?)
(test 10 "" (dot p1 x))
(test 12 "" (dot p1 y x))
(syntax-test #'(dot p2 x))
(test 10 "" (dot p3 x x))
(test 10 "" (dot p4 x x))
(syntax-test #'(dot p4 x y))
(syntax-test #'(structure p (x)))
(syntax-test #'(structure p (x) (structure y (x) (define x 10))))

;; Internal-defines
(let ((p1 1)
      (x 2))
  (define x 1111)
  (structure p1 provide-all
    (define x 10)
    (structure y provide-all
      (define x 12)))
  
  (structure p2 ()
    (define x 10))
  
  (structure p3 (x)
    (structure x provide-all
      (define x 10)))
  
  (structure p4 provide-all
    (structure x (x)
      (define x 10)
      (define y 11)))
  
  (test 10 "" (dot p1 x))
  (test 12 "" (dot p1 y x))
  (syntax-test #'(dot p2 x))
  (test 10 "" (dot p3 x x))
  (test 10 "" (dot p4 x x))
  (syntax-test #'(dot p4 x y)))
(syntax-test #'(let () (structure p (x)) 1))
(syntax-test #'(let () (structure p (x) (structure y (x) (define x 10))) 1))
(syntax-test #'(let ((provide-all 1)) (structure p provide-all (define s 1)) 1))

;; ml-defines
(structure p5 provide-all
  (define-values-ml (x) 10)
  (define-values-ml (f) (lambda () x))
  (define-values-ml (x) 12))
(test 12 "" (dot p5 x))
(test 10 "" ((dot p5 f)))


;; nesting
(structure p6 provide-all
  (structure x provide-all
    (define x 10))
  (structure z provide-all
    (structure a provide-all
      (define z 111)))
  (define y (dot x x))
  (define x 11))

(test 11 "" (dot p6 x))
(test 10 "" (dot p6 y))
(syntax-test #'(dot p6 x x))
(test 111 "" (dot p6 z a z))

;; open
(let ()
  (structure p7 provide-all
    (define a 1)
    (define b 2)
    (define c 3))
  (let ()
    (structure p8 provide-all
      (open p7)
      (define c 4))
    (test 1 "" (dot p8 a))
    (test 1 "" (dot p7 a))
    (test 2 "" (dot p8 b))
    (test 2 "" (dot p7 b))
    (test 4 "" (dot p8 c))
    (test 3 "" (dot p7 c))))

(let ()
  (structure p9 provide-all
    (structure x provide-all
      (define x 1)))
  (let ()
    (open p9)
    (test 1 "" (dot x x))))

(let ()
  (structure p9 provide-all
    (structure x provide-all
      (define x 1)))
  (let ()
    (open p9 x)
    (test 1 "" x)))

(syntax-test #'(open x))
(syntax-test #'(let () (structure y provide-all (structure z ())) (let () (open y a))))
(syntax-test #'(let () (structure y provide-all (structure z ())) (let () (open y z a))))

;; open-as
(let ()
  (structure x provide-all
    (define z 10))
  (open-as a x z)
  (test a "" 10))
(let ()
  (structure x provide-all
    (structure y provide-all
      (define z 10)))
  (open-as a x y)
  (open-as b a z)
  (test 10 "" b))

(syntax-test #'(let () (structure x ()) (open-as a x c) 1))

;; dot
(let ()
  (structure x provide-all
    (define z 10))
  (test 10 "" (dot x z)))
(let ()
  (structure x provide-all
    (structure y provide-all
      (define z 10)))
  (open-as a x y)
  (test 10 "" (dot a z)))
(syntax-test #'(let () (structure x ()) (dot x c)))

(report-errs)