
(load-relative "loadtest.ss")
(require (lib "structure.ss"))


(SECTION 'packages)

;; structure dot open open-as open-in-context

;; syntax
(syntax-test #'(dot))
(syntax-test #'(dot 1))
(syntax-test #'(dot 1 2))
(syntax-test #'(dot 1 x))



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

(test 10 "" (dot p1 x))
(test 12 "" (dot p1 y x))
(syntax-test #'(dot p2 x))
(test 10 "" (dot p3 x x))
(test 10 "" (dot p4 x x))
(syntax-test #'(dot p4 x y))
(syntax-test #'(structure p (x)))
(syntax-test #'(structure p (x) (structure y (x) (define x 10))))

;; ml-defines
(structure p5 provide-all
  (define-values-ml (x) 10)
  (define-values-ml (f) (lambda () x))
  (define-values-ml (x) 12))
(test 12 "" (dot p5 x))
(test 10 "" ((dot p5 f)))

;; Internal-defines
(let ()
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


;; dot



(report-errs)