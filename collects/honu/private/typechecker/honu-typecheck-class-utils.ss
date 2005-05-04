(module honu-typecheck-class-utils mzscheme
  
  (require (lib "struct.ss"))
  (require (lib "list.ss" "srfi" "1"))
  (require (prefix list: (lib "list.ss")))
  
  (require "../../ast.ss")
  (require "../../utils.ss")
  (require "../../tenv.ss")
  (require "honu-type-utils.ss")
  (require "honu-typecheck-exp.ss")
  
  (require "../../read-error-with-stx.ss")

  (provide honu-typecheck-slotdefns)
  (define (honu-typecheck-slotdefns tenv env cenv init-cenv defns)
    (honu-typecheck-slotdefns-helper tenv env cenv init-cenv defns (list)))
  
  (define (honu-typecheck-slotdefns-helper tenv env cenv init-cenv defns new-defns)
    (cond
      [(null? defns) (values (reverse new-defns) env cenv init-cenv)]
      [(honu-init-field? (car defns))
       (let-values (((new-defn new-env new-cenv new-init-cenv)
                     (honu-typecheck-init-field tenv env cenv init-cenv (car defns))))
         (honu-typecheck-slotdefns-helper tenv new-env new-cenv 
                                          new-init-cenv (cdr defns) (cons new-defn new-defns)))]
      [(honu-field? (car defns))
       (let-values (((new-defn new-env new-cenv new-init-cenv)
                     (honu-typecheck-field tenv env cenv init-cenv (car defns))))
         (honu-typecheck-slotdefns-helper tenv new-env new-cenv 
                                          new-init-cenv (cdr defns) (cons new-defn new-defns)))]
      [(honu-method? (car defns))
       (let loop ((mdefns (list (car defns)))
                  (rest-defns (cdr defns)))
         (if (or (null? rest-defns)
                 (not (honu-method? (car rest-defns))))
             (let-values (((new-mdefns new-env new-cenv new-init-cenv)
                           (honu-typecheck-methods tenv env cenv init-cenv mdefns)))
               (honu-typecheck-slotdefns-helper tenv new-env new-cenv new-init-cenv rest-defns
                                                (append (reverse new-mdefns) new-defns)))
             (loop (cons (car rest-defns) mdefns) (cdr rest-defns))))]
      [else (raise-read-error-with-stx
             "Unexpected type of slotdefn."
             (honu-ast-src-stx (car defns)))]))
  
  (define (honu-typecheck-init-field tenv env cenv init-cenv defn)
    (if (honu-type-in-tenv? tenv (honu-init-field-type defn))
        (if (honu-init-field-value defn)
            (let-values (((e1 t1) ((honu-typecheck-exp tenv env init-cenv)
                                   (honu-init-field-value defn))))
              (if (<:_P tenv t1 (honu-init-field-type defn))
                  (values (copy-struct honu-init-field defn
                                       (honu-init-field-value e1))
                          env
                          (extend-env cenv
                                      (honu-init-field-name defn)
                                      (honu-init-field-type defn))
                          (extend-env init-cenv
                                      (honu-init-field-name defn)
                                      (honu-init-field-type defn)))
                  (raise-read-error-with-stx
                   "Default expression for init field not subtype of declared type."
                   (honu-ast-src-stx (honu-init-field-value defn)))))
            (values defn env
                    (extend-env cenv
                                (honu-init-field-name defn)
                                (honu-init-field-type defn))
                    (extend-env init-cenv
                                (honu-init-field-name defn)
                                (honu-init-field-type defn))))
        (raise-read-error-with-stx
         "Type of init field not found in program."
         (honu-ast-src-stx (honu-init-field-type defn)))))
  
  (define (honu-typecheck-field tenv env cenv init-cenv defn)
    (if (honu-type-in-tenv? tenv (honu-field-type defn))
        (let-values (((e1 t1) ((honu-typecheck-exp tenv env init-cenv)
                               (honu-field-value defn))))
          (if (<:_P tenv t1 (honu-field-type defn))
              (values (copy-struct honu-field defn
                        (honu-field-value e1)) env
                      (extend-env cenv
                                  (honu-field-name defn)
                                  (honu-field-type defn))
                      (extend-env init-cenv
                                  (honu-field-name defn)
                                  (honu-field-type defn)))
              (raise-read-error-with-stx
               "Value for field is not subtype of declared type."
               (honu-ast-src-stx (honu-field-value defn)))))
        (raise-read-error-with-stx
         "Type of field not found in program."
         (honu-ast-src-stx (honu-field-type defn)))))

  (define (honu-typecheck-methods tenv env cenv init-cenv mdefns)
    (let* ((new-cenv (fold (lambda (d i)
                             (extend-env i (honu-method-name d)
                                         (make-honu-func-type (honu-ast-src-stx d)
                                                              (honu-method-arg-types d)
                                                              (honu-method-type d))))
                           cenv mdefns))
           (new-init-cenv (fold (lambda (d i)
                                  (extend-env i (honu-method-name d)
                                              (make-honu-func-type (honu-ast-src-stx d)
                                                                   (honu-method-arg-types d)
                                                                   (honu-method-type d))))
                                init-cenv mdefns))
           (new-mdefns (map (lambda (d)
                              (honu-typecheck-method tenv env new-cenv d))
                            mdefns)))
      (values new-mdefns env new-cenv new-init-cenv)))

  (define (honu-typecheck-method tenv env cenv defn)
    (if (or (honu-top-type? (honu-method-type defn)) ;; we allow void only in method return types
            (honu-type-in-tenv? tenv (honu-method-type defn)))
        (let ((new-env (fold (lambda (n t env)
                               (extend-env env n t))
                             env (honu-method-arg-names defn) (honu-method-arg-types defn))))
          (check-arg-types tenv (honu-method-arg-types defn)) ;; will raise exception if one fails
          (let-values (((e1 t1) ((honu-typecheck-exp tenv new-env cenv) (honu-method-body defn))))
            (if (<:_P tenv t1 (honu-method-type defn))
                (copy-struct honu-method defn
                  (honu-method-body e1))
                (raise-read-error-with-stx
                 "Body of method does not have declared return type."
                 (honu-ast-src-stx (honu-method-body defn))))))
        (raise-read-error-with-stx
         "Return type of method does not exist in program."
         (honu-ast-src-stx (honu-method-type defn)))))
  
  (define (check-arg-types tenv types)
    (cond
      [(null? types) #t]
      [(not (honu-type-in-tenv? tenv (car types)))
       (raise-read-error-with-stx
        "Argument type of method does not exist in program."
        (honu-ast-src-stx (car types)))]
      [else (check-arg-types tenv (cdr types))]))

  (provide check-impl-types)
  (define (check-impl-types tenv types)
    (cond
      [(null? types) #t]
      [(not (honu-iface-type-in-tenv? tenv (car types)))
       (raise-read-error-with-stx
        "Type in implements list does not exist in program."
        (honu-ast-src-stx (car types)))]
      [else (check-impl-types tenv (cdr types))]))
  
  (provide honu-typecheck-export)
  (define (honu-typecheck-export tenv cenv expdec)
    (define (check-export-name old new)
      (let ((old-type (cenv old)))
        (if old-type
            (let ((new-type
                   (cond
                     [(honu-prim-type? old-type)
                      (get-field-type tenv (honu-export-type expdec) new)]
                     [(honu-iface-type? old-type)
                      (get-field-type tenv (honu-export-type expdec) new)]
                     [(honu-func-type? old-type)
                      (get-method-type tenv (honu-export-type expdec) new)]
                     [else (raise-read-error-with-stx
                            "Unexpected class of type in check-export-name."
                            (honu-ast-src-stx old-type))])))
              (cond
                [(not new-type)
                 (raise-read-error-with-stx
                  "Public name to be exported to not found in class/mixin type."
                  new)]
                [(and (honu-func-type? old-type)
                      (not (<:_P tenv old-type new-type)))
                 (raise-read-error-with-stx
                  "Method to be exported is not a subtype of the public type."
                  old)]
                [(and (not (honu-func-type? old-type))
                      (not (honu-type-equal? old-type new-type)))
                 (raise-read-error-with-stx
                  "Field to be exported is not an exact type match for the public type."
                  old)]
                [else (void)])) ; The current one checks, we won't have to alter anything.
            (raise-read-error-with-stx
             "Local name to be exported not found in class/mixin."
             old))))
    ;; yes, in the check to make sure they're all exported, I convert to symbols and
    ;; then run the sorting thing, then just use equal?.  I really should make a
    ;; version that tells you _WHICH_ wasn't exported.
    (let ([sorted-type-fields-and-methods
           (sort-names (map (lambda (p) (printable-key (car p)))
                            (get-fields-and-methods tenv (honu-export-type expdec))))]
          [sorted-new-names (sort-names (map printable-key 
                                             (honu-export-new-names expdec)))])
      (if (not (equal? sorted-type-fields-and-methods sorted-new-names))
                 (raise-read-error-with-stx
                  "Not all fields and methods in export type exported."
                  (honu-ast-src-stx expdec))))
    (for-each check-export-name
              (honu-export-old-names expdec)
              (honu-export-new-names expdec)))

  ;; symbol list -> symbol list (sorted)
  (define (sort-names list)
    (list:quicksort list
                    (lambda (a b)
                      (string<? (symbol->string a)
                                (symbol->string b)))))
          
  (provide check-impls-and-exports)
  (define (check-impls-and-exports tenv cenv sub-type impl-types exports)
    (for-each (lambda (i)
                (if (ormap (lambda (t)
                             (honu-type-equal? t i))
                           (map honu-export-type exports))
                    (void)
                    (raise-read-error-with-stx
                     "No export statement for implemented type."
                     (honu-ast-src-stx i))))
              impl-types)
    (if (ormap (lambda (t)
                 (honu-type-equal? t sub-type))
               (map honu-export-type exports))
        (void)
        (raise-read-error-with-stx
         "No export statement for type of this."
         (honu-ast-src-stx sub-type)))
    (for-each (lambda (e)
                (if (or (ormap (lambda (t)
                                 (honu-type-equal? t (honu-export-type e)))
                               impl-types)
                        (honu-type-equal? sub-type (honu-export-type e)))
                    (honu-typecheck-export tenv cenv e)
                    (raise-read-error-with-stx
                     "Export statement for type that is not implemented or type of this."
                     (honu-ast-src-stx e))))
              exports))
  )
