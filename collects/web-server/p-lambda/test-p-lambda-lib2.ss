(require "test-helpers.ss")
(require/expose "p-lambda-lib.ss" (lift normalize-term))

(define (normalize-def a-def)
  (syntax-case a-def (define lambda case-lambda)
    [(define f (lambda formals body-exprs ...))
     (let ([new-body (normalize-term #'(begin body-exprs ...))])
       #`(define f (lambda formals #,new-body)))]
    [(define f
       (case-lambda cases ...))
     (let ([new-cases (normalize-cases (syntax->list #'(cases ...)))])
       #`(define f
           (case-lambda #,@new-cases)))]))

(define (normalize-cases cases)
  (if (null? cases) '()
      (cons
       (with-syntax ([(formals body-exprs ...) (car cases)])
         (let ([new-body (normalize-term #'(begin body-exprs ...))])
           #`(formals #,new-body)))
       (normalize-cases (cdr cases)))))

(define (a-eval expr)
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
             (for-each
              (lambda (a-def)
                (eval (normalize-def a-def)))
              defs)
             (eval (normalize-term body))))]))))

(zero? (a-eval (syntax 0)))
(eqv? 'hello-world
      (a-eval (syntax 'hello-world)))
    