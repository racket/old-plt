(require "test-helpers.ss"
         "prompt.ss")
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

(define the-r-channel (make-channel))

(define test-frame
  (let ([new-es (make-eventspace)])
    (parameterize ([current-eventspace new-es])
      (new prompt-frame% (label "Test Frame") (result-channel the-r-channel)))))

(send test-frame show #t)

(define (do-prompt msg k)
  (send test-frame prompt msg k)
  (channel-get the-r-channel))

(define s/s
  (p-eval (syntax
           (lambda (qtn)
             (let/cc k
               (let ([res (do-prompt qtn k)])
                 ((cadr res) (car res))))))))

(p-eval (syntax
         (s/s
          (format "The answer is: ~a"
                  (+ (string->number (s/s "first number"))
                     (string->number (s/s "second number")))))))