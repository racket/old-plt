(require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
         (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 1))
         (lib "serialize.ss")
         "../closure.ss")


(define-closure add-y (x) (y) (+ x y))
(define-closure even? (n) (odd?) (or (zero? n)
                                     (odd? (sub1 n))))
(define-closure odd? (n) (even?) (or (= 1 n)
                                     (even? (sub1 n))))

(define even? (make-even? (lambda () odd?)))
(define odd? (make-odd? (lambda () even?)))

;; an interpreter

(define-closure the-empty-env (var) ()
  (error "unbound symbol: " var))

(define-closure extended-env (v) (env var val)
  (if (eqv? v var) val
      (env v)))

(define-serializable-struct proc (var body env))

(define-closure clsr:evaluate (expr env) (evaluate eval-app)
  (match expr
    [('lambda (var) body) (make-proc var body env)]
    [(expr1 expr2)
     (let ([val1 (evaluate expr1 env)]
           [val2 (evaluate expr2 env)])
       (eval-app val1 val2))]
    [(? number? n) n]
    [var (env var)]))

(define-closure clsr:eval-app (val1 val2) (evaluate)
  (cond
    [(proc? val1)
     (evaluate (proc-body val1)
               (make-extended-env
                (lambda () (values (proc-env val1) (proc-var val1) val2))))]
    [else 
     (error "stuck term: " (list val1 val2))]))

(define evaluate (make-clsr:evaluate (lambda () (values evaluate eval-app))))
(define eval-app (make-clsr:eval-app (lambda () evaluate)))

(define closure-tests
  (make-test-suite
   "Tests for closure.ss"
   
   (make-test-case
    "add-y procedure"
    (assert = 2 ((make-add-y (lambda () 1)) 1)))
   
   (make-test-case
    "serialize the add-y procedure"
    (assert = 2 ((deserialize (serialize (make-add-y (lambda () 1)))) 1)))
   
   (make-test-case
    "even? procedure"
    (assert-true (even? 8)))
   
   (make-test-case
    "serialize the even? procedure"
    (assert-true ((deserialize (serialize even?)) 64)))
   
   (make-test-case
    "simple interpreter case"
    (assert = 3 (evaluate 3 (make-the-empty-env))))
   
   (make-test-case
    "serialize simple interpreter case"
    (assert = 3 ((deserialize (serialize evaluate))
                 3
                 (deserialize (serialize (make-the-empty-env))))))
   
   (make-test-case
    "apply identity"
    (assert = 3 (evaluate '((lambda (x) x) 3) (make-the-empty-env))))
   
   (make-test-case
    "serialize environments"
    (let* ([e0 (make-the-empty-env)]
           [e1 (make-extended-env (lambda () (values e0 'x 1)))]
           [e2 (make-extended-env (lambda () (values e1 'y 2)))]
           [e3 (make-extended-env (lambda () (values e2 'z 3)))]
           [e4 (make-extended-env (lambda () (values e3 'x 4)))]
           [e5 (make-extended-env (lambda () (values e4 'y 5)))]
           [e6 (make-extended-env (lambda () (values e5 'z 6)))]
           [env3 (deserialize (serialize e3))]
           [env5 (deserialize (serialize e5))]
           [env6 (deserialize (serialize e6))])
      (assert = 1 (env3 'x))
      (assert = 2 (env3 'y))
      (assert = 3 (env3 'z))
      (assert = 4 (env5 'x))
      (assert = 5 (env5 'y))
      (assert = 3 (env5 'z))
      (assert = 4 (env6 'x))
      (assert = 5 (env6 'y))
      (assert = 6 (env6 'z))))
         
   ))



(test/graphical-ui closure-tests)