(module syntax-utils mzscheme
  (require-for-template mzscheme)
  (provide recertify-dammit
           generate-formal)
  
  ;; syntax syntax -> syntax
  ;; recertify a franken-expression put together from body parts of another expression
  (define (recertify-dammit new-expr old-expr)
    (syntax-recertify new-expr old-expr (current-code-inspector) #f))
  
  ;; generate-formal: -> identifier
  (define (generate-formal sym-name)
    (let ([name (datum->syntax-object #f (gensym sym-name))])
      (with-syntax ([(lambda (formal) ref-to-formal)
                     (if (syntax-transforming?)
                         (local-expand #`(lambda (#,name) #,name) 'expression '())
                         #`(lambda (#,name) #,name))])
        (values #'formal #'ref-to-formal)))))
  
  
  