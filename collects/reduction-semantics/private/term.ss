(module term mzscheme

  (define-syntax (term-let stx)
    (syntax-case stx ()
      [(_ ((name rhs) ...) body)
       (syntax (let-syntax ([name 'rhs] ...) body))]))
  
  (define-syntax (term stx)
    
    (define (rewrite-ellipses stx)
      (let loop ([stx stx]
                 [last #f])
        (syntax-case stx (...)
          [()
           (if last
               (with-syntax ([last last])
                 (syntax (last)))
               (syntax ()))]
          [(dots . y)
           (and (identifier? #'dots) (module-identifier=? #'dots (quote-syntax ...)))
           (begin
             (unless last
               (raise-syntax-error 'term "misplaced ellipses" stx (syntax dots)))
             (with-syntax ([y (loop (syntax y) #f)]
                           [last last])
               (syntax ((dots last) . y))))]
          [(x . y)
           (with-syntax ([y (loop (syntax y) (loop (syntax x) #f))])
             (if last
                 (with-syntax ([last last])
                   (syntax (last . y)))
                 (syntax y)))]
          [id
           (identifier? (syntax id))
           stx])))

    #|
      ;; where else could these go?
    (define (test-re in expected)
      (let ([got (syntax-object->datum (rewrite-ellipses in))])
        (unless (equal? got expected)
          (error 'test-re "for ~s expected ~s got ~s"
                 (syntax-object->datum in)
                 expected 
                 got))))
    
    (test-re (quote-syntax ()) '())
    (test-re (quote-syntax x) 'x)
    (test-re (quote-syntax (a)) '(a))
    (test-re (quote-syntax (a b c)) '(a b c))
    (test-re (quote-syntax (a ...)) '((... a)))
    (test-re (quote-syntax (a b ... c)) '(a (... b) c))
    (test-re (quote-syntax (a b ... c ...)) '(a (... b) (... c)))
    |#    

    (syntax-case stx ()
      [(_ arg)
       (let loop ([arg (syntax arg)])
         (syntax-case arg ()
           [x
            (identifier? (syntax x))
            (let ([v (syntax-local-value (syntax x) (lambda () #f))])
              (if v
                  v
                  (syntax 'x)))]
           [() (syntax '())]
           [(dots y)
            (and (identifier? #'dots) (module-identifier=? #'dots (quote-syntax ...)))
            ...]
           [(x . y)
            (with-syntax ([x-stx (loop (syntax x))]
                          [y-stx (loop (syntax y))])
              (syntax (cons x-stx y-stx)))]))]))
  
  
  (define (test stx exp)
    (unless (equal? (eval stx) exp)
      (error)))
  
  (test
   #'(term-let ([x 111]) (term (y x)))
   (list 'y 111)))
