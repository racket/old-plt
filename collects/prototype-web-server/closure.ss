(module closure mzscheme
  (require-for-template mzscheme
                        (lib "serialize.ss")
                        (lib "etc.ss"))
  (provide make-closure-definition-syntax)
  
  (define (make-closure-definition-syntax tag formals fvars proc-body)
    (let ([make-id (if (syntax-transforming?)
                       (lambda (str)
                         (syntax-local-module-introduce
                          (datum->syntax-object
                           tag (string->symbol (format str (syntax-object->datum tag))))))
                       (lambda (str)
                         (datum->syntax-object
                          tag (string->symbol (format str (syntax-object->datum tag))))))])
      (with-syntax ([CLOSURE:deserialize-info (make-id "~a:deserialize-info")]
                    [CLOSURE:serialize-info (make-id "~a:serialize-info")]
                    [make-CLOSURE (make-id "make-~a")]
                    [CLOSURE? (make-id "~a?")]
                    [CLOSURE-ref (make-id "~a-ref")]
                    [CLOSURE-set! (make-id "~a-set!")]
                    [CLOSURE-env (make-id "~a-env")]
                    [set-CLOSURE-env! (make-id "set-~a-env!")]
                    [struct:CLOSURE (make-id "struct:~a")])
        (values 
         #'make-CLOSURE
         (list
          #`(define CLOSURE:deserialize-info
              (make-deserialize-info
               
               ;; make-proc: value ... -> CLOSURE
               #,(if (null? fvars)
                     #'(lambda () (make-CLOSURE))
                     #`(lambda #,fvars (make-CLOSURE (lambda () (values #,@fvars)))))
               
               ;; cycle-make-proc: -> (values CLOSURE (CLOSURE -> void))
               (lambda ()
                 (let ([new-closure
                        #,(if (null? fvars)
                              #'(make-CLOSURE)
                              #'(make-CLOSURE (lambda () (error "closure not initialized"))))])
                   (values
                    new-closure
                    #,(if (null? fvars)
                          #'void
                          #'(lambda (clsr)
                              (set-CLOSURE-env! new-closure (CLOSURE-env clsr)))))))))
          
          #`(provide CLOSURE:deserialize-info)
          
          #`(define CLOSURE:serialize-info
              (make-serialize-info
               
               ;; to-vector: CLOSURE -> vector
               #,(if (null? fvars)
                     #'(lambda (clsr) (vector))
                     #'(lambda (clsr)
                         (call-with-values
                          (lambda () ((CLOSURE-env clsr)))
                          vector)))
               
               ;; deserialize-id
               ;(syntax CLOSURE:deserialize-info)
               (syntax CLOSURE:deserialize-info)
               
               ;; can-cycle?
               #t
               
               ;; dir-path
               #,(current-directory)))
          
          #`(define-values (struct:CLOSURE make-CLOSURE CLOSURE? #,@(if (null? fvars)
                                                                        #'()
                                                                        #'(CLOSURE-env set-CLOSURE-env!)))
              (let-values ([(struct:CLOSURE make-CLOSURE CLOSURE? CLOSURE-ref CLOSURE-set!)
                            (make-struct-type '#,tag ;; the tag goes here
                                              #f  ; no super type
                                              #,(if (null? fvars) 0 1)
                                              0   ; number of auto-fields
                                              #f  ; auto-v
                                              
                                              ; prop-vals:
                                              (list (cons prop:serializable CLOSURE:serialize-info))
                                              
                                              #f  ; inspector
                                              
                                              ;; the struct apply proc:
                                              #,(if (null? fvars)
                                                    #`(lambda (clsr #,@formals)
                                                        #,proc-body)
                                                    #`(lambda (clsr #,@formals)
                                                        (let-values ([#,fvars ((CLOSURE-env clsr))])
                                                          #,proc-body)))
                                              )])
                (values struct:CLOSURE make-CLOSURE CLOSURE?
                        #,@(if (null? fvars)
                               #'()
                               #'((lambda (clsr) (CLOSURE-ref clsr 0))
                                  (lambda (clsr new-env) (CLOSURE-set! clsr 0 new-env)))))))))))))