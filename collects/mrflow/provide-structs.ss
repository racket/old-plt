; all related structs (i.e. a struct and all its parents) should be provided at the same time

(module provide-structs mzscheme
  
  (require-for-syntax
   (prefix struct: (lib "struct.ss" "syntax"))
   )
  
  (provide
   provide-struct/contract
   )
  
  (define-syntax provide-struct/contract
    (letrec (; stx-object (listof stx-object) (listof (cons symbol (listof stx-object)))
             ; -> (values stx-object (listof stx-object))
             ; get all the field contracts, going up recursively to add the contracts
             ; for the parent fields.
             ; I want to keep everything in one file and there's no define-for-syntax,
             ; so letrec will do.
             [get-all-field-contracts
              (lambda (struct-id field-contracts env)
                (let ([struct-id-flat (syntax-e struct-id)])
                  (if (symbol? struct-id-flat)
                      (values struct-id field-contracts)
                      (values (car struct-id-flat)
                              (let* ([parent-id (cadr struct-id-flat)]
                                     [parent-name (syntax-e parent-id)]
                                     [parent-name-and-field-contracts (assq parent-name env)])
                                (if parent-name-and-field-contracts
                                    (let-values ([(parent-id all-parents-field-contracts)
                                                  (get-all-field-contracts parent-id
                                                                           (append (cdr parent-name-and-field-contracts)
                                                                                   field-contracts)
                                                                           env)])
                                      all-parents-field-contracts)
                                    (raise (make-exn:syntax
                                            (format "define-struct: parent struct type not defined in: ~a" parent-name)
                                            (current-continuation-marks)
                                            parent-id
                                            'define-struct
                                            #f))))))))])
      (lambda (stx)
        (syntax-case stx ()
          [(_ ((struct struct-id (field-id ...)) field-contract ...) ...)
           (let ([struct-ids (syntax-e (syntax (struct-id ...)))]
                 [field-idss (map syntax-e (syntax-e (syntax ((field-id ...) ...))))]
                 [field-contractss (map syntax-e (syntax-e (syntax ((field-contract ...) ...))))])
             (datum->syntax-object
              stx
              `(begin
                 ,@(let loop-struct ([struct-ids struct-ids]
                                     [field-idss field-idss]
                                     [field-contractss field-contractss]
                                     [env '()])
                     (if (null? struct-ids)
                         '()
                         (let*-values ([(struct-id) (car struct-ids)]
                                       [(field-ids) (car field-idss)]
                                       [(field-contracts) (car field-contractss)]
                                       [(struct-id all-field-contracts)
                                        (get-all-field-contracts struct-id field-contracts env)])
                           (let* ([all-ids (cdr (struct:build-struct-names struct-id field-ids #f #f))]
                                  [constructor-id (car all-ids)]
                                  [predicate-id (cadr all-ids)])
                             (cons (datum->syntax-object
                                    struct-id
                                    `(provide/contract
                                      (,constructor-id (,@all-field-contracts . -> . ,predicate-id))
                                      (,predicate-id (any? . -> . boolean?))
                                      ,@(let loop-fields ([field-contracts field-contracts]
                                                          [all-ids (cddr all-ids)])
                                          (if (null? field-contracts)
                                              '()
                                              (let ([field-contract (car field-contracts)])
                                                (cons `(,(car all-ids) (,predicate-id . -> . ,field-contract))
                                                      (cons `(,(cadr all-ids) (,predicate-id ,field-contract . -> . ,void?))
                                                            (loop-fields (cdr field-contracts) (cddr all-ids))))))))
                                    struct-id)
                                   (loop-struct (cdr struct-ids) (cdr field-idss) (cdr field-contractss)
                                                (cons (cons (syntax-e struct-id) field-contracts)
                                                      env))))))))
              stx))]
          [stx
           (let ([stx (syntax stx)])
             (raise (make-exn:syntax
                     "bad provide-struct/contract syntax"
                     (current-continuation-marks)
                     stx
                     'provide-struct/contract
                     #f)))]))))
    
  )
