(module closure mzscheme
  (require (lib "serialize.ss"))
  (provide define-closure)
  
  (define-syntax (define-closure stx)
    (syntax-case stx ()
      [(_ tag (formals ...) (fvars ...) proc-body)
       (let ([make-id (lambda (str)
                        (datum->syntax-object
                         #'tag (string->symbol (format str (syntax-object->datum #'tag)))))])
         (with-syntax ([CLOSURE:deserialize-info (make-id "~a:deserialize-info")]
                       [CLOSURE:serialize-info (make-id "~a:serialize-info")]
                       [make-CLOSURE (make-id "make-~a")]
                       [CLOSURE? (make-id "~a?")]
                       [CLOSURE-ref (make-id "~a-ref")]
                       [CLOSURE-set! (make-id "~a-set!")]
                       [CLOSURE-env (make-id "~env")]
                       [set-CLOSURE-env! (make-id "set-~a-env!")]
                       [struct:CLOSURE (make-id "struct:~a")])
           #`(begin
               (define CLOSURE:deserialize-info
                 (make-deserialize-info
                  
                  ;; make-proc: value ... -> CLOSURE
                  #,(if (null? (syntax->list #'(fvars ...)))
                        #'(lambda () (make-CLOSURE))
                        #'(lambda (fvars ...) (make-CLOSURE (lambda () (values fvars ...)))))
                  
                  ;; cycle-make-proc: -> (values CLOSURE (CLOSURE -> void))
                  (lambda ()
                    (let ([new-closure
                           #,(if (null? (syntax->list #'(fvars ...)))
                                 #'(make-CLOSURE)
                                 #'(make-CLOSURE (lambda () (error "closure not initialized"))))])
                      (values
                       new-closure
                       #,(if (null? (syntax->list #'(fvars ...)))
                             #'void
                             #'(lambda (clsr)
                                 (set-CLOSURE-env! new-closure (CLOSURE-env clsr)))))))))
               
               (define CLOSURE:serialize-info
                 (make-serialize-info
                  
                  ;; to-vector: CLOSURE -> vector
                  #,(if (null? (syntax->list #'(fvars ...)))
                        #'(lambda (clsr) (vector))
                        #'(lambda (clsr)
                            (call-with-values
                             (lambda () ((CLOSURE-env clsr)))
                             vector)))
                  
                  ;; deserialize-id
                  (syntax CLOSURE:deserialize-info)
                  
                  ;; can-cycle?
                  #t
                  
                  ;; dir-path
                  (build-path "/")))
               
               (define-values (struct:CLOSURE make-CLOSURE CLOSURE? #,@(if (null? (syntax->list #'(fvars ...)))
                                                                           #'()
                                                                           #'(CLOSURE-env set-CLOSURE-env!)))
                 (let-values ([(struct:CLOSURE make-CLOSURE CLOSURE? CLOSURE-ref CLOSURE-set!)
                               (make-struct-type 'tag ;; the tag goes here
                                                 #f  ; no super type
                                                 #,(if (null? (syntax->list #'(fvars ...)))
                                                       0 1)
                                                 0   ; number of auto-fields
                                                 #f  ; auto-v
                                                 
                                                 ; prop-vals:
                                                 (list (cons prop:serializable CLOSURE:serialize-info))
                                                 
                                                 #f  ; inspector
                                                 
                                                 ;; the struct apply proc:
                                                 (lambda (clsr formals ...)
                                                   (let-values ([(fvars ...) ((CLOSURE-env clsr))])
                                                     proc-body))
                                                 )])
                   (values struct:CLOSURE make-CLOSURE CLOSURE?
                           #,@(if (null? (syntax->list #'(fvars ...)))
                                  #'()
                                  #'((lambda (clsr) (CLOSURE-ref clsr 0))
                                     (lambda (clsr new-env) (CLOSURE-set! clsr 0 new-env))))))))))])))