(module elim-call-cc mzscheme
  (require "syntax-utils.ss")
  (require-for-template "abort-resume.ss" mzscheme)
  (provide elim-call/cc-from-definition
           elim-call/cc)
  
  ;; **************************************************
  ;; LANGUAGE
  ;;
  ;; program ::= definition* expr
  ;;
  ;; definition ::= (define-values (var) expr)
  ;;
  ;; expr ::= w
  ;;       |  (if w expr)
  ;;       |  (if w expr expr)
  ;;       |  (#%app w expr)
  ;;       |  (#%app w w ...)
  ;;       |  (#%app call/cc w)
  ;;
  ;;   w  ::= var | (#%top . var) | value
  ;;   value ::=  (#%datum . datum)
  ;;          | (lambda (var ...) expr)
  
  ;; id: alpha -> alpha
  (define (id x) x)
  
  ;; elim-call/cc: expr -> expr
  ;; eliminate call/cc from an expression
  (define (elim-call/cc expr)
    (elim-call/cc/mark expr id))
   
  ;; elim-call/cc/mark: expr (expr -> expr) -> expr
  ;; eliminate call/cc from an expression given a mark frame function
  (define (elim-call/cc/mark expr markit)
    (syntax-case expr (if #%app call/cc #%top #%datum lambda quote)
      [(if w e)
       (markit #`(if #,(elim-call/cc #'w) #,(elim-call/cc #'e)))]
      [(if w e0 e1)
       (markit #`(if #,(elim-call/cc #'w)
                     #,(elim-call/cc #'e0)
                     #,(elim-call/cc #'e1)))]
      [(#%app call/cc w)
       (let-values ([(cm ref-to-cm) (generate-formal 'current-marks)]
                    [(x ref-to-x) (generate-formal 'x)])
         (markit #`(#%app #,(elim-call/cc #'w)
                          (#%app (lambda (#,cm)
                                   (lambda (#,x)
                                     (#%app abort
                                            (lambda () (#%app resume #,ref-to-cm #,ref-to-x)))))
                                 (#%app reverse (#%app continuation-mark-set->list
                                                       (#%app current-continuation-marks)
                                                       the-cont-key))))))]
      [(#%app w e)
       (syntax-case #'w (lambda)
         [(lambda (formals ...) body)
          (let ([w-prime (datum->syntax-object #f (gensym 'f))])
            #`(let-values ([(#,w-prime) #,(elim-call/cc #'w)])
                #,(markit
                   (recertify-dammit
                    #`(#%app #,w-prime
                             #,(elim-call/cc/mark
                                #'e
                                (lambda (x)
                                  #`(with-continuation-mark the-cont-key #,w-prime #,x))))
                    expr))))]
         [_else
          (let ([w-prime (elim-call/cc #'w)])
            (markit
             (recertify-dammit
              #`(#%app #,w-prime
                       #,(elim-call/cc/mark
                          #'e 
                          (lambda (x)
                            #`(with-continuation-mark the-cont-key #,w-prime #,x))))
              expr)))])]
      [(#%app w rest ...)
       (markit
        (recertify-dammit
         #`(#%app #,(elim-call/cc #'w)
                  #,@(map elim-call/cc (syntax->list #'(rest ...))))
         expr))]
      [(#%top . var) expr]
      [(#%datum . d) expr]
      [(lambda (formals ...) body)
       #`(lambda (formals ...) #,(elim-call/cc #'body))]
      [(quote datum) expr]
      [x (symbol? (syntax-object->datum #'x)) expr]
      [_else
       (raise-syntax-error #f "elim-call/cc/mark dropped through" expr)]))
  
  ;; elim-call/cc-from-definition: definition -> definition
  ;; produce a transformed defintion
  (define (elim-call/cc-from-definition def)
    (syntax-case def ()
      [(define-values (var ...) expr)
       #`(define-values (var ...) #,(elim-call/cc #'expr))]
      [else
       (raise-syntax-error #f "elim-call/cc-from-definition dropped through" def)]))
  )





