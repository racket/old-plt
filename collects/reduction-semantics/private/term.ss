(module term mzscheme
  (provide term term-let)
  
  (define-syntax (term orig-stx)
    (define (rewrite stx)
      (let loop ([stx stx])
        (syntax-case stx (unquote unquote-splicing)
          [(unquote x)
           (with-syntax ([x-rewrite (loop (syntax x))])
             (syntax (unsyntax x-rewrite)))]
          [(unquote . x)
           (raise-syntax-error 'term "malformed unquote" orig-stx stx)]
          [(unquote-splicing x)
           (with-syntax ([x-rewrite (loop (syntax x))])
             (syntax (unsyntax-splicing x-rewrite)))]
          [(unquote-splicing . x)
           (raise-syntax-error 'term "malformed unquote splicing" orig-stx stx)]
          [(x ...)
           (with-syntax ([(x-rewrite ...) (map loop (syntax->list (syntax (x ...))))])
             (syntax (x-rewrite ...)))]
          [_ stx])))

    (syntax-case orig-stx ()
      [(_ arg)
       (with-syntax ([rewritten (rewrite (syntax arg))])
         (syntax (syntax-object->datum (quasisyntax rewritten))))]))
  
  (define-syntax (term-let stx)
    (syntax-case stx ()
      [(_ ([x rhs] ...) body)
       (syntax
        (with-syntax ([x rhs] ...)
          body))]
      [_ (raise-syntax-error 'term-let "malformed term" stx)]))
#|  
  (define (test stx exp)
    (unless (equal? (eval stx) exp)
      (error 'test "~s" (syntax-object->datum stx))))
  
  (test (quote-syntax (term 1)) 1)
  (test (quote-syntax (term (1 2))) (list 1 2))
  (test (quote-syntax (term (1 ,(+ 1 1)))) (list 1 2))
  (test (quote-syntax (term-let ([x 1]) (term (x x)))) (list 1 1))
  (test (quote-syntax (term-let ([(x ...) (list 1 2 3)]) (term ((y x) ...)))) '((y 1) (y 2) (y 3)))
|#
)
