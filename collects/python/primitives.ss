(module primitives mzscheme
  (require (lib "list.ss")
           (lib "etc.ss")
           "python-node.ss"
           "builtin-types-uninitialized.ss")
  (provide (all-defined)
           (all-from "python-node.ss")
           (all-from "builtin-types-uninitialized.ss"))
  

  (define-syntax (key->sym stx)
    (syntax-case stx ()
      [(_ key) (let ([key (syntax key)]) (syntax-property key
                                                          'bound-in-source
                                                          (cons
                                                           (syntax-local-introduce key)
                                                           (syntax-local-introduce key))))]))
  
  (define (py-function%->procedure f)
    (python-get-member f scheme-procedure-key #f))

  (define (string->py-string% s)
    (py-create py-string% s))
  
  (define (py-string%->string ps)
    (python-get-member ps scheme-string-key #f))
  
  (define (symbol->py-string% s)
    (string->py-string% (symbol->string s)))
  
  (define (list->py-tuple% l)
    (py-create py-tuple% l))
  
  (define (py-tuple%->list pt)
    (python-get-member pt scheme-list-key #f))

  (define (py-list%->list pl)
    (python-get-member pl scheme-list-key #f))
  
  (define (list->py-list% l)
    (py-create py-list% l))
  
  (define (py-number%->number pn)
    (python-get-member pn scheme-number-key #f))
  
  (define (hash-table->py-dict% ht)
    (py-create py-dict% ht))
  
  (define (assoc-list->py-dict% al)
    (hash-table->py-dict% (assoc-list->hash-table al)))
  
  (python-add-members py-function%
                      `((__init__ ,(lambda (this name v)
                                     (python-set-name! this name)
                                     (python-set-member! this scheme-procedure-key v)))
                        (__call__ ,(lambda (this . args)
                                     (apply (py-function%->procedure this)
                                            args)))))
  
  (define (number->py-number% num)
    (py-create (cond
                 [(integer? num) py-int%]
                 [(real? num) py-float%]
                 [else (error "The runtime system does not support the number" num)])
               num))
  

  ;; py-call: py-object%(X ... -> Y) arg-list -> ?
  (define (py-call functor arg-list)
    (cond
      [(procedure? functor) (unless (procedure-arity-includes? functor (length arg-list))
                              (error "Incorrect number of arguments:"
                                     (map py-object%->string arg-list)))
                            (apply functor arg-list)]
      [(py-is-a? functor py-function%) (py-call (python-get-member functor
                                                                   '__call__
                                                                   false)
                                                (cons functor arg-list))]
      ;; else, it's a py-method% or some other thing that has a __call__ field
      [else (py-call (python-get-member functor
                                        '__call__
                                        false)
                     (cons functor arg-list))]))
  

  ;; py-create: py-type X ... -> py-object%
  ;; create a new instance of a type
  (define (py-create class . init-args)
    (let ([obj (python-method-call class '__new__)]) ; class)]) ;; class is not auto-passed, __new__ is static
      (unless (has-member? class '__init__)
          (raise (format "class ~a has no initializer!" (py-object%->string (python-get-type-name class)))))
      (py-call (python-get-member class '__init__ #f)
               (cons obj init-args))
      obj))
 
  ;; py-is?: python-object python-object -> bool
  ;; determine whether two objects are the same exact thing
  (define py-is? eq?)
  
  (define (py-type? node)
    (py-is? (python-node-type node) py-type%))
   
  ;; python-get-bases: py-object% -> (listof py-type)
  (define (python-get-bases obj)
    (if (py-type? obj)
        (py-tuple%->list (hash-table-get (python-node-dict obj) '__bases__
                                         (lambda ()
                                           (error "object type" (python-get-type-name obj) "has no bases!"))))
        (list (python-node-type obj))))
   
  (define python-get-member
    (opt-lambda (obj member-name [wrap? #t])
      (if (eq? member-name '__dict__)
          (py-create py-dict% (python-node-dict obj))
          (let* ([member (hash-table-get (python-node-dict obj)
                                        member-name
                                        (lambda ()
                                          (let ([bases (python-get-bases obj)])
                                            (if (null? bases)
                                                (error (format "member ~a not found in ~a"
                                                               member-name
                                                               (py-string%->string (python-get-type-name obj))))
                                                    (ormap (lambda (base)
                                                             (python-get-member base member-name wrap?))
                                                           bases)))))]
                [post-wrap (if wrap?
                               (if (and (python-node? member)
                                        (py-is-a? member py-function%))  ;; wrap member functions
                                   (python-wrap-method member
                                                       (if (py-type? obj)  ;; bound or unbound method?
                                                           obj
                                                           (python-node-type obj))
                                                       (if (py-type? obj) py-none obj))
                                   (begin
                                     (when (and (python-node? member)
                                                (py-is-a? member py-method%)) ;; change the binding
                                       (python-set-member! member 'im_self (if (py-type? obj) py-none obj)))
                                     member))
                               member)])
            (if (and (python-node? post-wrap)
                     (py-is-a? post-wrap py-static-method%))
                (let ([fun (python-get-member post-wrap 'static-method-function #f)])
                  (unless fun
                    (error "Uninitialized static method object"))
                  fun)
                post-wrap)))))
    
  
  ;; python-set-name!: py-object% symbol ->
  (define (python-set-name! obj name)
    (python-set-member! obj '__name__ (if (symbol? name)
                                          (symbol->py-string% name)
                                          name)))
  
  ;; python-get-name: py-object% -> py-string%
  (define (python-get-name obj)
    (python-get-member obj '__name__))

  
  ;; py-object%->string: py-object% -> string
  (define (py-object%->string x)
    (cond
      [(py-is-a? x py-string%) (string-append "'" (py-string%->string x) "'")]
      [(py-is-a? x py-number%) (number->string (python-get-member x scheme-number-key))]
      [(py-is-a? x py-list%) (letrec ([list-items-repr
                                       (lambda (l)
                                         (cond
                                           [(empty? l) ""]
                                           [(empty? (rest l)) (py-object%->string (first l))]
                                           [else (string-append (py-object%->string (first l))
                                                                ", "
                                                                (list-items-repr (rest l)))]))])
                               (string-append "["
                                              (list-items-repr (python-get-member x scheme-list-key))
                                              "]"))]
      [(py-is-a? x py-dict%)
       (let ([dict-items-repr
              (lambda (ht)
                (foldr (lambda (str1 str2) (if (> (string-length str2) 0)
                                               (string-append str1 ", " str2)
                                               str1))
                       ""
                       (hash-table-map ht
                                       (lambda (key value)
                                         (string-append (py-object%->string key)
                                                        ": "
                                                        (py-object%->string value))))))])
         (string-append "{"
                        (dict-items-repr (python-get-member x scheme-hash-table-key))
                        "}"))]
      [(py-is-a? x py-function%) (string-append "<function "
                                                (py-string%->string (python-get-name x))
                                                ">")]
      [(py-type? x) (format "<type ~a>"
                                                   (py-object%->string (python-get-type-name x)))]
      [else (format "<~a object>" (py-object%->string (python-get-type-name (python-node-type x))))]))

  ;; python-get-type-name: py-type% -> py-string%
  (define (python-get-type-name type)
    (unless (py-type? type)
      (error "Not a type:" (py-object%->string type)))
    (python-get-member type '__name__))
  
  (define (assoc-list->hash-table al)
    (let ([hash-table (make-hash-table)])
      (for-each (lambda (assoc)
                  (hash-table-put! hash-table (car assoc) (cadr assoc)))
                al)
      hash-table))
  
  

  (define (has-member? class member-name)
    (with-handlers ([exn? (lambda (exn) #f)])
      (python-get-member class member-name #f)
      #t))


  ;; python-get-bases*: py-object% -> (listof py-type%)
  (define (python-get-bases* obj)
    (let ([bases (python-get-bases obj)])
      (if (null? bases)
          bases
          (apply append bases (map python-get-bases* bases)))))
  

  ;; py-is-a? python-object py-type% -> bool
  (define (py-is-a? obj class)
    (unless (py-type? class)
      (error (py-object%->string class) "is not a type."))
    (let ([bases (python-get-bases* obj)])
      (if (or (py-is? class py-object%) ; everything's an object
              (py-is? (python-node-type obj) class)
              (ormap (lambda (base)
                       (py-is? base class))
                       ;;;(or (py-is? base class)
                           ;;(py-is-a? base class))
                     bases))
          #t #f)))
  
  
  


  ;; python-wrap-method: (union procedure py-function%) class instance -> python-method%
  (define (python-wrap-method method class obj)
    (py-create py-method%
               (if (procedure? method)  ;; if it's not wrapped as a proc already...
                   (py-create py-function% 'python-function method)
                   method)
               class
               obj))
  
  ;; py-repr: python-node -> py-string%
  (define py-repr (lambda (x)
    (string->py-string% (py-object%->string x))))


  ;; python-unwrap-method: py-method% -> py-function%
  ;; grab the function wrapped by the method object
  (define (python-unwrap-method method)
    (python-get-member method 'im_func #f))
  
  
  ;; python-method-call: python-object symbol X ... -> ?
  (define (python-method-call obj method-name . args)
    (let ([fn (python-get-member obj method-name false)])
      (py-call fn
               (if (and (python-node? fn)
                        (py-is-a? fn py-static-method%))
                   args
                   (cons obj args)))))
  


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;; new forms for introducing procedures ;;;;;;;;;;;;;;;;;;;;;

  (define-syntax py-lambda
    (lambda (stx)
      (syntax-case stx ()
        [(_ name (arg ...) expr ...)
         (let ([expr-stx (syntax->list (syntax (list expr ...)))])
           #`(py-create py-function%
                        #,(syntax name)
                        (lambda #,(syntax (arg ...))
                                        #,@expr-stx)))]
        [(_ name ((arg ...) . rest-args) expr ...)
           #`(py-create py-function%
                        #,(syntax name)
                        (lambda (#,@(syntax (arg ...)) . (syntax rest-args))
                                        #,(syntax (expr ...))))])))
  

  (define-syntax py-opt-lambda
    (lambda (stx)
      (syntax-case stx ()
        [(_ name (arg ...) expr ...)
         (let ([expr-stx (syntax->list (syntax (list expr ...)))])
           #`(py-create py-function%
                        #,(syntax name)
                        (opt-lambda #,(syntax (arg ...))
                                        #,@expr-stx)))])))

  
  
  ;;;;;;;;;;;;;;; complete the basic types now ;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (python-add-members py-static-method%
                      `((__new__ ,(lambda (class)
                                     (unless (or (py-is? class py-static-method%)
                                                 (py-is-a? class py-static-method%))
                                       (error (py-object%->string class)
                                              "is not a subtype of staticmethod."))
                                     (let ([sm (python-method-call py-object%
                                                                   '__new__
                                                                   class)])
                                       (python-set-member! sm 'static-method-function #f)
                                       sm)))
                        (__init__ ,(lambda (this function-to-wrap)
                                     (python-set-member! this 'static-method-function
                                                         function-to-wrap)))))
  
  
  (python-add-members py-object%
                      `((__new__ ,python-new-object);,(let ([sm (python-new-object py-static-method%)])
                                 ;   (python-method-call sm '__init__ python-new-object)
                                 ;   sm)) ;; object.__new__ is a staticmethod
                        (__init__ ,(lambda (this) (void)))
                        (__call__ ,py-create)
                        (__repr__ ,py-repr)))


  (define (py-bin-op op)
    (lambda (lhs rhs)
      (number->py-number% (apply-bin-op op lhs rhs))))

  
  (define (apply-bin-op op lhs rhs)
    (let ([check (lambda (obj)
                   (unless (py-is-a? obj py-number%)
                     (error (py-object%->string obj) "is not a number.")))])
      (check lhs)
      (check rhs))
    (op (python-get-member lhs scheme-number-key)
        (python-get-member rhs scheme-number-key)))
  
  (python-add-members py-number%
                      `((__init__ ,(lambda (this v)
                                     (python-set-member! this scheme-number-key v)))
                        (__add__ ,(py-bin-op +))
                        (__sub__ ,(py-bin-op -))
                        (__mul__ ,(py-bin-op *))
                        (__div__ ,(py-bin-op /))
                        (__mod__ ,(py-bin-op modulo))))
  
  
  (python-add-members py-string%
                      `((__init__ ,(lambda (this value)
                                     (python-set-member! this scheme-string-key value)))))
  
  (python-add-members py-none%
                      `((__init__ ,(lambda (this)
                                     (error "Cannot create"
                                            (python-get-type-name py-none%)
                                            "instances")))))

  
  (define py-none (make-python-node py-none% (make-hash-table) #f))

  (define (python-method-bound? method)
    (not (py-is? (python-get-member method 'im_self)
                 py-none)))
  
  
  (python-add-members py-method%
                        `((__init__ ,(opt-lambda (this fun class [self py-none])
                                       (python-set-member! this 'im_class class)
                                       (python-set-member! this 'im_func fun)
                                       (python-set-member! this 'im_self self)))
                          (__call__ ,(py-create py-function%
                                                'wrapped-method
                                                (lambda (this-method . args)
                                                  (py-call (python-get-member this-method 'im_func false)
                                                           (if (python-method-bound? this-method)
                                                               (cons (python-get-member this-method 'im_self)
                                                                     args)
                                                               (let ([class (python-get-member this-method
                                                                                               'im_class)])
                                                                 (if (and (not (null? args))
                                                                          (py-is-a? (car args) class))
                                                                     args
                                                                     (error (py-object%->string this-method)
                                                                            "must be called with"
                                                                            (py-string%->string
                                                                             (python-get-type-name class))
                                                                            "instance as first argument."
                                                                            "Got"
                                                                            (if (null? args)
                                                                                "no arguments!"
                                                                                (py-object%->string (car args)))))))))))
                          (im_func ,py-none) ; py-procedure%
                          (im_self ,py-none))) ; py-object%
  
  (python-add-members py-tuple%
                      `((__init__ ,(py-lambda '__init__ (this v)
                                              (python-set-member! this
                                                                  scheme-list-key
                                                                  v)))))

  (python-add-members py-list%
                      `((__init__ ,(py-lambda '__init__ (this v)
                                              (python-set-member! this
                                                                  scheme-list-key
                                                                  v)))))
  
  (python-add-members py-dict%
                      `((__init__ ,(py-lambda '__init__ (this v)
                                              (python-set-member! this
                                                                  scheme-hash-table-key
                                                                  (if (list? v)
                                                                      (assoc-list->hash-table v)
                                                                      v))))))
  


  

  ;; more setup for PY-TYPE
  (python-set-member! py-type% '__new__ python-new-object)
  (python-set-member! py-type% '__init__
                      (lambda (this name bases defs)
                        (python-set-name! this name)
                        (python-set-member! this '__bases__
                                            (if (list? bases)
                                                (list->py-tuple% bases)
                                                (if (py-is-a? bases py-tuple%)
                                                    bases
                                                    (error "Invalid input: "
                                                           (py-object%->string bases)
                                                           " is not a tuple."))))
                        (for-each (lambda (wrapped-key&value)
                                    (let ([key&value (wrapped-key&value this)])
                                      (python-set-member! this
                                                          (first key&value)
                                                          (second key&value))))
                                  defs)))

  
  (define py-true (py-create py-int% 1))
  (define py-false (py-create py-int% 0))

 
  )