(require "test-helpers.ss")
(require/expose "p-lambda-lib.ss" (lift normalize-term))

(define (normalize-def a-def)
  (syntax-case a-def (define lambda case-lambda)
    [(define f (lambda formals body-exprs ...))
     (let-values ([(new-body body-defs)
                   (normalize-term #'(begin body-exprs ...))])
       (cons
        #`(define f (lambda formals #,new-body))
        body-defs))]
    [(define f
       (case-lambda cases ...))
     (let-values ([(new-cases case-defs)
                   (normalize-cases (syntax->list #'(cases ...)))])
       (cons #`(define f
                 (case-lambda #,@new-cases))
             case-defs))]))

(define (normalize-cases cases)
  (if (null? cases) (values '() '())
      (with-syntax ([(formals body-exprs ...) (car cases)])
        (let-values ([(new-body body-defs)
                      (normalize-term #'(begin body-exprs ...))]
                     [(rest-cases case-defs) (normalize-cases (cdr cases))])
          (values
           (cons #`(formals #,new-body) rest-cases)
           (append body-defs case-defs))))))


(define myprint void)

(define (p-eval expr)
  (let/cc abort
    (parameterize [(current-abort abort)]
      (let flatten-begins ([e expr])
        (let ([top-e (expand-to-top-form e)])
          (syntax-case top-e (begin)
            [(begin exprs ...)
             (foldl
              (lambda (e old-val)
                (flatten-begins e))
              (void)
              (syntax->list #'(exprs ...)))]
            [not-a-begin
             (let ([ex (expand top-e)])
               (let-values ([(body defs) (lift ex)])
                 (let ([defs
                        (let loop ([defs defs])
                          (if (null? defs) '()
                              (append (normalize-def (car defs))
                                      (loop (cdr defs)))))])
                   (let-values ([(body body-defs) (normalize-term body)])
                     (let ([defs (append body-defs defs)])
                       (myprint "defs = ~s~n" defs)
                       (myprint "body = ~s~n" body)
                       (for-each eval defs)
                       (eval body))))))]))))))

;; ****************************************

(define-values (getit setit!)
  (let ([x 0])
    (values
     (lambda () x)
     (lambda (new-x)
       (set! x new-x)))))

(define myloop
  (p-eval (syntax (lambda (proc lis)
                    (unless (null? lis)
                      (proc (car lis))
                      (myloop proc (cdr lis)))))))

(= -1 (p-eval (syntax (let/cc exit
                        (myloop
                         (lambda (n)
                           (when (negative? n)
                             (exit n)))
                         '(12 32 45 64 -1 23 45 6))))))

(and (= -2 (p-eval (syntax (let/cc exit
                             (myloop
                              (lambda (n)
                                (setit! n)
                                (when (negative? n)
                                  (exit n)))
                              '(12 32 45 66 -2 233 456))))))
     (= -2 (getit)))

(= 4 (p-eval (syntax (+ 1 (call/cc
                           (lambda (k)
                             (+ 2 (k 3))))))))

(= 4 (p-eval (syntax (+ 1 (call/cc
                           (lambda (k)
                             (setit! k)
                             (+ 2 (k 3))))))))

(let ([k (getit)])
  (k 3))

(define list-product
  (p-eval (syntax (lambda (s)
                    (call/cc
                     (lambda (exit)
                       (let recur ([s s])
                         (cond
                           [(null? s) 1]
                           [else
                            (setit! (car s))
                            (if (= (car s) 0)
                                (exit 0)
                                (* (car s)
                                   (recur (cdr s))))]))))))))

(= 6 (list-product '(1 2 3)))
(zero? (list-product '(0 1 2 3)))
(zero? (getit))
(zero? (list-product '(1 2 3 0 4 5 6)))
(zero? (getit))