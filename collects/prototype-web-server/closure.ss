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
                       [struct:CLOSURE (make-id "struct:~a")])
           (with-syntax ([(selectors ...)
                          (map
                           (lambda (fvar-name)
                             (datum->syntax-object
                              #'tag (string->symbol
                                     (format "~a-~a" 
                                             (syntax-object->datum #'tag)
                                             (syntax-object->datum fvar-name)))))
                           (syntax->list #'(fvars ...)))]
                         [(setters ...)
                          (map
                           (lambda (fvar-name)
                             (datum->syntax-object
                              #'tag (string->symbol
                                     (format "set-~a-~a!"
                                             (syntax-object->datum #'tag)
                                             (syntax-object->datum fvar-name)))))
                           (syntax->list #'(fvars ...)))])
             #`(begin
                 (define CLOSURE:deserialize-info
                   (make-deserialize-info
                    
                    ;; make-proc: (listof value) -> CLOSURE
                    (lambda (fvars ...) (make-CLOSURE (lambda () fvars) ...))
                    
                    ;; cycle-make-proc: -> (values CLOSURE (CLOSURE -> void))
                    (lambda ()
                      (let ([new-closure (make-CLOSURE #,@(map (lambda (x) #f) (syntax->list #'(setters ...))))])
                        (values
                         new-closure
                         #,(if (null? (syntax->list #'(setters ...)))
                               #'void
                               #'(lambda (clsr)
                                   (setters new-closure (selectors clsr)) ...)))))))
                 
                 (define CLOSURE:serialize-info
                   (make-serialize-info
                    
                    ;; to-vector: CLOSURE -> vector
                    (lambda (clsr)
                      (list->vector
                       (list (selectors clsr) ...)))
                    
                    ;; deserialize-id
                    (syntax CLOSURE:deserialize-info)
                    
                    ;; can-cycle?
                    #t
                    
                    ;; dir-path
                    (build-path "/")))
                 
                 (define-values (struct:CLOSURE make-CLOSURE CLOSURE? selectors ... setters ...)
                   (let-values ([(struct:CLOSURE make-CLOSURE CLOSURE? CLOSURE-ref CLOSURE-set!)
                                 (make-struct-type 'tag ;; the tag goes here
                                                   #f  ; no super type
                                                   #,(length (syntax->list #'(selectors ...)))
                                                   0   ; number of auto-fields
                                                   #f  ; auto-v
                                                   
                                                   ; prop-vals:
                                                   (list (cons prop:serializable CLOSURE:serialize-info))
                                                   
                                                   #f  ; inspector
                                                   
                                                   ;; the struct apply proc:
                                                   (lambda (clsr formals ...)
                                                     (let-values ([(fvars ...) (values (selectors clsr) ...)])
                                                       proc-body))
                                                   )])
                     (values struct:CLOSURE make-CLOSURE CLOSURE? 
                             #,@(let loop ([selectors (syntax->list #'(selectors ...))]
                                           [n 0])
                                  (if (null? selectors) #'()
                                      (cons
                                       #`(lambda (clsr) ((CLOSURE-ref clsr #,n)))
                                       (loop (cdr selectors) (add1 n)))))
                             #,@(let loop ([setters (syntax->list #'(setters ...))]
                                           [n 0])
                                  (if (null? setters) #'()
                                      (cons
                                       #`(lambda (clsr new-val) (CLOSURE-set! clsr #,n (lambda () new-val)))
                                       (loop (cdr setters) (add1 n))))))))))))])))