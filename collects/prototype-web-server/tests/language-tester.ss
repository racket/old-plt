(module language-tester mzscheme
  (provide make-module-eval
           make-eval/mod-path)

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
       (raise-syntax-error #f "make-module-evel: dropped through" m-expr)]))
  
  (define (make-eval/mod-path pth)
    (let ([ns (make-namespace)])
      (parameterize ([current-namespace ns])
        (eval `(require (lib "client.ss" "prototype-web-server")
                        (lib "serialize.ss")
                        ,pth))
      (lambda (expr)
        (parameterize ([current-namespace ns])
          (eval expr)))))))