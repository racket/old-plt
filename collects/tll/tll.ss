(module tll mzscheme
  
  (provide
   ;; mzscheme
   #%app #%datum #%top #%module-begin #%plain-module-begin
   cons car cdr null? eq?
   (rename tll-define define)  (rename tll-lambda lambda) (rename tll-cond cond)
   quote and or
   add1 (rename tll-sub1 sub1) zero? 
   )
  
  (define (tll-sub1 n) (if (> n 0) (- n 1) (tll-sub1 n)))
  
  (define-syntax (tll-cond stx) (raise-syntax-error #f "reference to undefined identifier" stx))

  (define-syntax (tll-lambda stx) (raise-syntax-error #f "reference to undefined identifier" stx))

  (define-syntax (tll-define stx)
    (syntax-case stx (tll-lambda tll-cond)
      [(_ <name> 
          (tll-lambda (x y ...)
            (tll-cond
              [<expressionQ> <expressionA>]
              [<expressionQ2> <expressionA2>]
              ...)))
       ;; I don't enforce "else" yet
       (and (identifier? (syntax <name>))
            (andmap identifier? (syntax->list (syntax (x y ...)))))
       (syntax 
        (define <name>
          (lambda (x y ...)
            (cond
              [<expressionQ> <expressionA>]
              [<expressionQ2> <expressionA2>]
              ...))))]
      [(_ <name> 
          (lambda (x y ...)
            (cond
              [<expressionQ> <expressionA>]
              [<expressionQ2> <expressionA2>]
              ...)))
       (andmap identifier? (syntax->list (syntax (x y ...))))
       (let ([TLL-DEFINE
              (string-append 
               " >>> in definition,"
               (format " not an identifier: ~s ~s <<<\n" 
                       (syntax-object->datum (syntax <name>))
                       (identifier? (syntax <name>)))
               "expected:\n"
               "(define <name>\n"
               "  (lambda (<name1> <name2> ...)\n"
               "    (cond\n"
               "      (question answer)\n"
               "      (question2 answer2)\n"
               "      ...\n"
               "      (else answerElse)))\n")])
         (raise-syntax-error #f TLL-DEFINE stx))]
      [(_ <name> 
          (lambda (x y ...)
            (cond
              [<expressionQ> <expressionA>]
              [<expressionQ2> <expressionA2>]
              ...)))
       (identifier? (syntax <name>))
       (let ([TLL-DEFINE
              (string-append 
               " >>> in lambda expression,"
               (format " not a list of identifiers: ~s <<<\n" 
                       (map syntax-object->datum
                            (syntax->list (syntax (x y ...)))))
               "expected:\n"
               "(define <name>\n"
               "  (lambda (<name1> <name2> ...)\n"
               "    (cond\n"
               "      (question answer)\n"
               "      (question2 answer2)\n"
               "      ...\n"
               "      (else answerElse)))\n")])
         (raise-syntax-error #f TLL-DEFINE stx))]
      [(_ <name> 
          (lambda (x y ...)
            (cond
              [question answer])))
       (and (identifier? (syntax <name>))
            (andmap identifier? (syntax->list (syntax (x y ...)))))
       (let ([TLL-DEFINE
              (string-append 
               " >>> a cond-expression requires more than one question-answer pair <<<\n"
               "expected:\n"
               "(define <name>\n"
               "  (lambda (<name1> <name2> ...)\n"
               "    (cond\n"
               "      (question answer)\n"
               "      (question2 answer2)\n"
               "      ...\n"
               "      (else answerElse)))\n")])
         (raise-syntax-error #f TLL-DEFINE stx))]
      [(_ <name> 
          (lambda (x y ...)
            (cond
              line ...)))
       (and (identifier? (syntax <name>))
            (andmap identifier? (syntax->list (syntax (x y ...)))))
       (let ([TLL-DEFINE
              (string-append 
               " >>> a cond-expression requires a sequence of two or more question-answer pairs,\n"
               (format "     given: ~s\n"
                       (map syntax-object->datum
                            (syntax->list (syntax (line ...)))))
               "expected:\n"
               "(define <name>\n"
               "  (lambda (<name1> <name2> ...)\n"
               "    (cond\n"
               "      (question answer)\n"
               "      (question2 answer2)\n"
               "      ...\n"
               "      (else answerElse)))\n")])
         (raise-syntax-error #f TLL-DEFINE stx))]
      [(_ <name> 
          (lambda (x y ...)
            <body>))
       (and (identifier? (syntax <name>))
            (andmap identifier? (syntax->list (syntax (x y ...)))))
       (let ([TLL-DEFINE
              (string-append 
               (format " >>> not a cond-expression: ~s <<<\n"
                       (syntax-object->datum (syntax <body>)))
               "expected:\n"
               "(define <name>\n"
               "  (lambda (<name1> <name2> ...)\n"
               "    (cond\n"
               "      (question answer)\n"
               "      (question2 answer2)\n"
               "      ...\n"
               "      (else answerElse)))\n")])
         (raise-syntax-error #f TLL-DEFINE stx))]
      [(_ <name> (lambda x <body>))
       ; _else  
       (let ([TLL-DEFINE
              (string-append 
               "definition expected:\n"
               (format "<name>: ~s ~s\n" 
                       (symbol->string (syntax-e (syntax name)))
                       (identifier? (syntax name)))
               (format "x y ...: ~s ~s\n"
                       (syntax-object->datum (syntax x))
                       (identifier? (syntax x)))
               "(define <name>\n"
               "  (lambda (<name1> <name2> ...)\n"
               "    (cond\n"
               "      (question answer)\n"
               "      (question2 answer2)\n"
               "      ...\n"
               "      (else answerElse)))\n")])
         (raise-syntax-error #f TLL-DEFINE stx))]))
  )
