(module language-tester mzscheme
  (provide make-module-eval)

  (define-syntax (make-module-eval m-expr)
    (syntax-case m-expr (module)
      [(_ (module m-id . rest))
       #'(let ([ns (make-namespace)])
           (parameterize ([current-namespace ns])
             (eval '(require "../client.ss"
                             (lib "serialize.ss")))
             (eval '(module m-id . rest))
             (eval '(require m-id)))
           
           (lambda (s-expr)
             (parameterize ([current-namespace ns])
               (eval s-expr))))]
      [else
       (raise-syntax-error #f "make-module-evel: dropped through" m-expr)])))