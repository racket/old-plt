(module 2vals mzscheme
  (provide 2vals let*-2vals 2vals-first 2vals-second)
  
  (define 2vals vector)
  
  (define-syntax (let*-2vals stx)
    (syntax-case stx (let*-2vals)
      [(let*-2vals () body)
       (syntax body)]
      [(let*-2vals ([(id-a id-b) rhs] [(id-c id-d) rhs2] ...) body)
       (syntax (let* ([_a rhs] [id-a (vector-ref _a 0)] [id-b (vector-ref _a 1)])
                 (let*-2vals ([(id-c id-d) rhs2] ...) body)))]))
  
  (define-syntax (2vals-first stx)
    (syntax-case stx (2vals-first)
      [(2vals-first a)
       (syntax (vector-ref a 0))]))
  
  (define-syntax (2vals-second stx)
    (syntax-case stx (2vals-second)
      [(2vals-second a)
       (syntax (vector-ref a 1))]))) 

