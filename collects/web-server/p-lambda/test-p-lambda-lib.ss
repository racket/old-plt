(require "test-helpers.ss")
(require/expose "p-lambda-lib.ss" (lift))

(define (lift-eval expr)
  (let flatten-begins ([e expr])
    (let ([top-e (expand-syntax-to-top-form e)])
      (syntax-case top-e (begin)
        [(begin exprs ...)
         (foldl
          (lambda (e old-val)
            (flatten-begins e))
          (void)
          (syntax->list #'(exprs ...)))]
        [not-a-begin
         (let* ([ex (expand-syntax top-e)])
           (let-values ([(body defs) (lift ex)])
;             (printf "body = ~s~n" body)
;             (printf "defs = ~s~n" defs)
             (for-each eval defs)
             (eval body)))]))))

;; ****************************************
;; tests for lift

(= 3 (lift-eval (syntax 3)))
(string=? "foo" (lift-eval (syntax "foo")))
(eqv? 'foo (lift-eval (syntax 'foo)))

(define plus
  (lift-eval (syntax (lambda (n m) (+ n m)))))

(= 5 (plus 3 2))

(define make-adder0
  (lift-eval (syntax (lambda (n)
                       (lambda (m)
                         (+ n m))))))

(= 1 ((make-adder0 1) 0))
(= 3 ((make-adder0 1) 2))

(define make-adder1
  (lift-eval (syntax (lambda args
                       (lambda (m)
                         (apply + (cons m args)))))))

(= 3 ((make-adder1 1 1 1) 0))
(= 7 ((make-adder1 2 2) 3))

(define make-adder2
  (lift-eval (syntax (lambda (n . restarg)
                       (lambda (m)
                         (apply + (cons n (cons m restarg))))))))

(= 1 ((make-adder2 1) 0))
(= 3 ((make-adder2 1 1) 1))
(= 21 ((make-adder2 1 2 3 4) 11))

(define make-adder3
  (lift-eval (syntax (lambda (n)
                       (lambda restarg
                         (apply + (cons n restarg)))))))

(= 1 ((make-adder3 1) 0))
(= 1 ((make-adder3 1)))
(= 1 ((make-adder3 1) 0 0 0))
(= 7 ((make-adder3 3) 2 1 1))

(define make-adder4
  (lift-eval (syntax (lambda (n)
                       (lambda (m . restarg)
                         (apply + (cons n
                                        (cons m restarg))))))))

(= 1 ((make-adder4 1) 0))
(= 8 ((make-adder4 4) 2 2))

(define make-adder5
  (lift-eval (syntax (case-lambda
                       [() (lambda (m) (+ 0 m))]
                       [(n) (lambda (m) (+ n m))]
                       [(n0 n1) (lambda (m) (+ n0 n1 m))]))))

(= 1 ((make-adder5) 1))
(= 1 ((make-adder5 1) 0))
(= 3 ((make-adder5 1 1) 1))

(define make-adder6
  (lift-eval (syntax (lambda args
                       (case-lambda
                         [() (apply + args)]
                         [(n) (apply + (cons n args))]
                         [(n0 n1) (apply + (cons n0 (cons n1 args)))])))))

(= 1 ((make-adder6 1)))
(= 1 ((make-adder6) 1))
(= 0 ((make-adder6)))
(= 3 ((make-adder6 1) 1 1))
(= 4 ((make-adder6 2) 2))
(= 4 ((make-adder6) 2 2))

(lift-eval (syntax (if #t #t)))
(define simple
  (lift-eval (syntax (lambda (x)
                       (if #t 'simple)))))

(eqv? 'simple (simple 7))

(lift-eval (syntax (if #t #t #f)))
(lift-eval (syntax (if #f #f #t)))

(lift-eval (syntax (let ([x #t]) x)))

(define three-things0
  (lift-eval
   (syntax
    (let ([x (box 0)])
      (lambda ()
        (set-box! x 1)
        (set-box! x 7)
        (unbox x))))))

(= 7 (three-things0))

(define three-things1
  (lift-eval
   (syntax
    (let ([x (box 1)])
      (lambda ()
        (begin
          (set-box! x 1)
          (set-box! x 8)
          (unbox x)))))))

(= 8 (three-things1))

(define three-things2
  (lift-eval
   (syntax
    (let ([x (box #t)])
      (lambda ()
        (begin0 (unbox x) (set-box! x 1) (set-box! x 8) (unbox x)))))))

(three-things2)

(define even0?
  (lift-eval
   (syntax
    (letrec ([e? (lambda (n)
                   (or (zero? n)
                       (o? (sub1 n))))]
             [o? (lambda (n)
                   (and (not (zero? n))
                        (e? (sub1 n))))])
      e?))))

(even0? 0)
(even0? 8)
(not (even0? 3))

(define even1?
  (lift-eval
   (syntax
    (letrec-values ([(e0? e1?)
                     (values
                      (lambda (n)
                        (or (zero? n)
                            (o1? (sub1 n))))
                      (lambda (n)
                        (or (zero? n)
                            (o0? (sub1 n)))))]
                    [(o0? o1?)
                     (values
                      (lambda (n)
                        (and (not (zero? n))
                             (e1? (sub1 n))))
                      (lambda (n)
                        (and (not (zero? n))
                             (e0? (sub1 n)))))])
      e0?))))

(even1? 0)
(not (even1? 1))
(even1? 2)
(not (even1? 3))
(even1? 4)
(not (even1? 5))
(even1? 6)


(= 3 (lift-eval (syntax (((lambda (n)
                            (lambda (m)
                              (+ n m))) 1) 2))))

(= (/ 1 2) (lift-eval (syntax (((lambda (x)
                                  (lambda (y)
                                    (/ x y))) 1) 2))))

(= 6 (lift-eval (syntax ((lambda (f n)
                           (f f n))
                         (lambda (f n)
                           (if (zero? n)
                               1
                               (* n (f f (- n 1)))))
                         3))))

(= 6 (lift-eval (syntax ((lambda (f n)
                           (f f n))
                         (lambda (f n)
                           (cond
                             [(zero? n) 1]
                             [else (* n (f f (sub1 n)))]))
                         3))))

(= 6 (lift-eval (syntax (let ([fact (lambda (f n)
                                      (cond
                                        [(zero? n) 1]
                                        [else (* n (f f (sub1 n)))]))])
                          (fact fact 3)))))

(= 6 (lift-eval (syntax (let ([fix (lambda (f)
                                     (lambda (n)
                                       (f f n)))])
                          (let ([fact (fix (lambda (fact n)
                                             (cond
                                               [(zero? n) 1]
                                               [else (* n (fact fact (sub1 n)))])))])
                            (fact 3))))))

;(= 6 (lift-eval (syntax (begin
;                          (define (fact n)
;                            (if (zero? n) 1
;                                (* n (fact (sub1 n)))))
;                          (fact 3)))))



                