(module primitives mzscheme
  (require (lib "list.ss")
           (lib "etc.ss"))
  (provide (all-defined))
  
  ;; python-node is a:
  ;; (make-python-node python-node (hash-table-of dict-member) bool)
  
  ;; a dict-member is one of:
  ;;  symbol python-node
  ;;  gensym scheme-value
  
  
  (define-struct python-node (type dict mutable?) (make-inspector))
  
  
  ;; these are hidden keys in built-in data types to hold their actual scheme values
  (define scheme-number-key (gensym 'number-key))
  (define scheme-string-key (gensym 'string-key))
  (define scheme-list-key (gensym 'list-key))
  (define scheme-procedure-key (gensym 'procedure-key))
  (define scheme-hash-table-key (gensym 'hash-table-key))

  (define (py-function%->procedure f)
    (python-get-member f scheme-procedure-key #f))
  
  
  ;; py-call: py-object%(X ... -> Y) arg-list -> ?
  (define (py-call functor arg-list)
    (cond
      [(procedure? functor) (apply functor arg-list)]
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
  (define (py-create class . rest)
    (let ([obj (make-python-node class (make-hash-table) #t)])
      (if (has-member? class '__init__)
          (let ([init (python-get-member obj '__init__ #f)])
            (if (procedure? init)
                (apply init (cons obj rest))
                (py-call init
                         (cons obj rest))))
          (if (not (null? rest))
              (raise (format "class ~a has no initializer!" (py-object%->string (python-get-type-name class))))))
      obj))

 
  ;; py-is?: python-object python-object -> bool
  ;; determine whether two objects are the same exact thing
  (define py-is? eq?)
  
  (define (py-type? node)
    (py-is? (python-node-type node) py-type%))
   
  ;; python-get-bases: py-object% -> (listof py-type)
;  (define (python-get-bases obj)
;    (if (py-type? obj)
;        (py-tuple%->list (python-get-member obj '__bases__))
;        (list (python-node-type obj))))
   
  (define (python-get-bases obj)
    (if (py-type? obj)
        (py-tuple%->list (hash-table-get (python-node-dict obj) '__bases__
                                         (lambda ()
                                           (error "object " obj "has no bases!"))))
        (list (python-node-type obj))))
   
  (define python-get-member
    (opt-lambda (obj member-name [wrap? #t])
      (if (eq? member-name '__dict__)
          (py-create py-dict% (python-node-dict obj))
          (let ([member (hash-table-get (python-node-dict obj)
                                        member-name
                                        (lambda ()
                                          (let ([bases (python-get-bases obj)])
                                            (if (null? bases)
                                                (error (format "member ~a not found in ~a"
                                                               member-name
                                                               (python-get-type-name obj)))
                                                    (ormap (lambda (base)
                                                             (python-get-member base member-name wrap?))
                                                           bases)))))])
                (if wrap?
                    (if (and (python-node? member)
                             (py-is-a? member py-function%))  ;; wrap member functions
                        (python-wrap-method member (if (py-type? obj) #f obj))
                        (begin
                          (when (and (python-node? member) (py-is-a? member py-method%))
                            (python-set-member! member 'im_self (if (py-type? obj) #f obj)))
                          member))
                    member)))))
    
  (define (string->py-string% s)
    (py-create py-string% s))
  
  (define (py-string%->string ps)
    (python-get-member ps scheme-string-key #f))
  
  (define (list->py-tuple% l)
    (py-create py-tuple% l))
  
  (define (py-tuple%->list pt)
    (python-get-member pt scheme-list-key #f))
  
  ;; python-set-name!: py-object% symbol ->
  (define (python-set-name! obj name)
    (python-set-member! obj '__name__ (string->py-string% (symbol->string name))))
  
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
  
  (define py-type% (make-python-node #f (make-hash-table) #f))
  (set-python-node-type! py-type% py-type%)
  

  (define (has-member? class member-name)
    (with-handlers ([exn? (lambda (exn) #f)])
      (python-get-member class member-name #f)
      #t))

  (define *reverse-list-of-types-to-finish-setting-up* empty)
  
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

  (define (python-set-member! obj name value)
    (hash-table-put! (python-node-dict obj) name value))

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
                  (python-set-member! pt scheme-list-key (let ([base (third type&name&base)])
                                                   (if base
                                                       (if (null? base)
                                                           null
                                                           (list base))
                                                       (list py-object%))))
                  (python-set-member! type '__bases__ pt))))
            (reverse *reverse-list-of-types-to-finish-setting-up*))

  ;; python-get-bases*: py-object% -> (listof py-type%)
  (define (python-get-bases* obj)
    (let ([bases (python-get-bases obj)])
      (if (null? bases)
          bases
          (apply append bases (map python-get-bases* bases)))))
  

  ;; py-is-a? python-object python-class -> bool
  (define (py-is-a? obj class)
    (let ([bases (python-get-bases* obj)])
      (if (or (py-is? class py-object%) ; everything's an object
              (py-is? (python-node-type obj) class)
              (ormap (lambda (base)
                       (py-is? base class))
                       ;;;(or (py-is? base class)
                           ;;(py-is-a? base class))
                     bases))
          #t #f)))
  
  
  (define (number->py-number num)
    (py-create (cond
                 [(integer? num) py-int%]
                 [(real? num) py-float%]
                 [else (error "The runtime system does not support the number" num)])
               num))
  
  
  (define (py-bin-op op)
    (py-lambda (lhs rhs)
      (number->py-number (apply-bin-op op lhs rhs))))

  
  (define (apply-bin-op op lhs rhs)
    (let ([check (lambda (obj)
                   (unless (py-is-a? obj py-number%)
                     (error (py-object%->string obj) "is not a number.")))])
      (check lhs)
      (check rhs))
    (op (python-get-member lhs scheme-number-key)
        (python-get-member rhs scheme-number-key)))


  ;; python-wrap-method: (union procedure py-function%) python-object -> python-method%
  (define (python-wrap-method method obj)
    (py-create py-method%
               (if (procedure? method)  ;; if it's not wrapped as a proc already...
                   (py-create py-function% method)
                   method)
               obj))
  
  
  (python-set-member! py-function% '__init__ (lambda (this v)
                                               (python-set-member! this scheme-procedure-key v)))
;                      (let ([pf (make-python-node py-function% (make-hash-table) #t)])
;                        (python-set-member! pf scheme-procedure-key
;                                            (lambda (this v)
;                                              (python-set-member! this scheme-procedure-key v)))
;                        pf))
  (python-set-member! py-function% '__call__ (lambda (this . args)
                                               (apply (py-function%->procedure this)
                                                      args)))
;                      (let ([pf (make-python-node py-function% (make-hash-table) #t)])
;                        (python-set-member! pf scheme-procedure-key
;                                            (lambda (this . args)
;                                              (py-call this args)))
;                        pf))
  
  ;; py-repr: python-node -> py-string%
  (define py-repr (py-lambda (x)
    (string->py-string% (py-object%->string x))))


  (define-syntax py-lambda
    (lambda (stx)
      (syntax-case stx ()
        [(_ (arg ...) expr ...)
         (let ([expr-stx (syntax->list (syntax (list expr ...)))])
           #`(py-create py-function% (lambda #,(syntax (arg ...))
                                        #,@expr-stx)))]
        [(_ ((arg ...) . rest-args) expr ...)
           #`(py-create py-function% (lambda (#,@(syntax (arg ...)) . (syntax rest-args))
                                        #,(syntax (expr ...))))])))
  

  (define-syntax py-opt-lambda
    (lambda (stx)
      (syntax-case stx ()
        [(_ (arg ...) expr ...)
         (let ([expr-stx (syntax->list (syntax (list expr ...)))])
           #`(py-create py-function% (opt-lambda #,(syntax (arg ...))
                                        #,@expr-stx)))])))

  
  (python-add-members py-object%
                      `((__repr__ ,py-repr)))

  
  (python-add-members py-number%
                      `((__init__ ,(py-lambda (this v)
                                     (python-set-member! this scheme-number-key v)))
                        (__add__ ,(py-bin-op +))
                        (__sub__ ,(py-bin-op -))
                        (__mul__ ,(py-bin-op *))
                        (__div__ ,(py-bin-op /))
                        (__mod__ ,(py-bin-op modulo))))
  
  
  (python-add-members py-string%
                      `((__init__ ,(py-lambda (this value)
                                     (python-set-member! this scheme-string-key value)))))
  
  (python-add-members py-none%
                      `((__init__ ,(py-lambda (this)
                                     (error "Cannot create"
                                            (python-get-type-name py-none%)
                                            "instances")))))

  
;  (python-add-members py-function%
 ;                      `(;(__init__ ,(py-lambda (this v)
                         ;             (python-set-member! this scheme-procedure-key v)))
                         ;(__call__ ,(py-lambda ((this) . args)
                         ;             (py-call this args)))
                         ;(__name__ ,(string->py-string% "anonymous-procedure"))))
                      
  

  (define py-none (make-python-node py-none% (make-hash-table) #f))

  
  (python-add-members py-method%
                        `((__init__ ,(py-opt-lambda (this fun [self py-none])
                                       (python-set-member! this 'im_func fun)
                                       (python-set-member! this 'im_self self)))
                          (__call__ ,(py-create py-function% (lambda (this-method . args)
                                       (py-call (python-get-member this-method 'im_func false)
                                                (let ([self (python-get-member this-method 'im_self)])
                                                  (if (not (py-is? self py-none))
                                                      (cons self args)
                                                      args))))))
                          (im_func ,py-none) ; py-procedure%
                          (im_self ,py-none))) ; py-object%
  
  
  (python-add-members py-dict%
                      `((__init__ ,(py-lambda (this v)
                                              (python-set-member! this
                                                                  scheme-hash-table-key
                                                                  (if (list? v)
                                                                      (assoc-list->hash-table v)
                                                                      v))))))
  


  
  ;; python-unwrap-method: py-method% -> py-function%
  ;; grab the function wrapped by the method object
  (define (python-unwrap-method method)
    (python-get-member method 'im_func #f))
  
  
  ;; python-method-call: python-object symbol X ... -> ?
  (define (python-method-call obj method-name . args)
    (py-call (python-get-member obj method-name false)
             (cons obj args)))
  

  

  
  (define py-true (py-create py-int% 1))
  (define py-false (py-create py-int% 0))

 
  )