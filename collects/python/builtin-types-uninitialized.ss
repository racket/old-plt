(module builtin-types-uninitialized mzscheme
  (require (lib "etc.ss")
           (lib "list.ss")
           "python-node.ss")
  (provide (all-defined-except *reverse-list-of-types-to-finish-setting-up*))

  (define py-type% (make-python-node #f (make-hash-table) #f))
  (set-python-node-type! py-type% py-type%)

  (define *reverse-list-of-types-to-finish-setting-up* (list (list py-type%
                                                                   'type
                                                                   #f)))

  (define immutable-type
    (opt-lambda (name [base #f])
      (let ([node (make-python-node py-type%
                                    (make-hash-table)
                                    #f)])
        (set! *reverse-list-of-types-to-finish-setting-up*
              (cons (list node name base)
                    *reverse-list-of-types-to-finish-setting-up*))
        node)))

  (define py-object% (immutable-type 'object null))
  (define py-none% (immutable-type #cs'NoneType))
  (define py-number% (immutable-type '|Number (Internal!)|))
  (define py-int% (immutable-type 'int py-number%))
  (define py-float% (immutable-type 'float py-number%))
  (define py-complex% (immutable-type 'complex py-number%))
  (define py-string% (immutable-type 'string))
  (define py-dict% (immutable-type 'dict))
  (define py-list% (immutable-type 'list))
  (define py-tuple% (immutable-type 'tuple))
  (define py-function% (immutable-type 'function))
  (define py-method% (immutable-type '|instance method|))
  (define py-static-method% (immutable-type '|static method|))
  (define py-classmethod% (immutable-type 'classmethod))
  (define py-module% (immutable-type 'module))
  (define py-slice% (immutable-type 'slice))

  (define (set-py-dict! type)
    (set! py-dict% type))

  ;; exceptions
  (define py-exception% (immutable-type #cs'Exception))
  (define py-type-error% (immutable-type #cs'TypeError py-exception%))
  (define py-runtime-error% (immutable-type #cs'RuntimeError py-exception%))
  (define py-assert-error% (immutable-type #cs'AssertError py-exception%))
  (define py-index-error% (immutable-type #cs'IndexError py-exception%))
  (define py-system-error% (immutable-type #cs'SystemError py-exception%))
  (define py-future-warning% (immutable-type #cs'FutureWarning py-exception%))
  (define py-deprecation-warning% (immutable-type #cs'DeprecationWarning py-exception%))
  (define py-key-error% (immutable-type #cs'KeyError py-exception%))

  ;; files
  (define py-file% (immutable-type 'file))


  (define (python-add-members node assoc-list)
    (for-each (lambda (key-value)
                (python-set-member! node (car key-value) (cadr key-value)))
              assoc-list))


  ;; finish setting up
;  (for-each (lambda (type&name&base)
;              (let ([type (first type&name&base)]
;                    [name (second type&name&base)]
;                    [base (third type&name&base)])
;                (printf "setting up ~a~n" name)
;              (python-add-members (first type&name&base)
;                                  `((__name__ ,(second type&name&base))
;                                    (__bases__ ;(list->py-tuple%
;                                                 ,(let ([base (third type&name&base)])
;                                                   (if base
;                                                       (if (null? base)
;                                                           null
;                                                           (list base))
;                                                       (list py-object%))))))))
;            (reverse *reverse-list-of-types-to-finish-setting-up*))
  (for-each (lambda (type&name&base)
              (let ([type (first type&name&base)]
                    [name (second type&name&base)]
                    [base (third type&name&base)])
                (let ([ps (make-python-node py-string% (make-hash-table) #t)])
                  (python-set-member! ps scheme-string-key (symbol->string name))
                  (python-set-member! type '__name__ ps))
                (let ([pt (make-python-node py-tuple% (make-hash-table) #t)])
                  (python-set-member! pt scheme-list-key
                                      (let ([base (third type&name&base)])
                                        (if base
                                            (if (null? base)
                                                null
                                                (list base))
                                            (list py-object%))))
                  (python-set-member! type '__bases__ pt))))
            (reverse *reverse-list-of-types-to-finish-setting-up*))

  )
