(module primitives mzscheme
  (require (lib "list.ss")
           (lib "etc.ss")
           "python-node.ss"
           "builtin-types-uninitialized.ss")
  (provide (all-defined)
           (all-from "python-node.ss")
           (all-from "builtin-types-uninitialized.ss"))
  
  (define python-runtime-debugging? false)
  
  (define dprintf
    (if python-runtime-debugging?
        (lambda (format . args)
          (apply printf
                 (cons format args)))
        (lambda (f . a) (if #f #f))))
  
  (dprintf "1~n")

  
  (define python-get-member
    (opt-lambda (obj member-name [wrap? #t] [orig-call? #t])
      (cond
        ;; special case #1: __class__ is actually the type
        [(eq? member-name '__class__) (python-node-type obj)]
         ;; special case #2: __dict__ is the internal dictionary (well, usually... let's just assume always)
         [(eq? member-name '__dict__) (py-create py-dict% (python-node-dict obj))]
         ;; special case #3: __call__ is implicitly defined for type objects
         ;;   but not py-type%, because type() creates a new type.... :P
;         [(and orig-call?
;               (eq? member-name '__call__)
;               (py-type? obj)
;               (not (py-is? obj py-type%))) (procedure->py-function% python-create-object
;                                                                  'python-create-object)]
         [else
          (local ((define (pgm o)
                   (with-handlers ([symbol? (lambda (exn-sym)
                                                  (unless (eq? exn-sym 'member-not-found)
                                                    (raise exn-sym))
                                                  (error (format "member ~a not found in ~a"
                                                                 member-name
                                                                 (py-object%->string obj))))])
                         (hash-table-get (python-node-dict o)
                                         member-name
                                         (lambda ()
                                           (let ([bases (python-get-bases o)])
                                             (if (null? bases)
                                                 (raise 'member-not-found)
                                                 (ormap (lambda (base)
                                                          (pgm base))
                                                        bases))))))))
            (let ([member (pgm obj)])
              (if (and (eq? member-name '__new__)
                       (or (procedure? member)
                           (py-is-a? member py-function%)))
                  ;; special case #4: __new__ should always be a staticmember function
                  ;;  if it's a proc, make it a py-function% and try again
                  ;;  if it's a py-function%, make it a py-static-method% and try again
                  (begin
                        (python-set-member! obj '__new__
                                            ((if (procedure? member)
                                                 procedure->py-function%
                                                 py-function%->py-static-method%) member))
                        (python-get-member obj '__new__ wrap? #f))
                  (if wrap?
                      (if (py-type? obj)
                          (python-wrap-member member obj py-none)
                          (python-wrap-member member (python-node-type obj) obj))
                      member))))])))
  
  
  (define (py-function%->procedure f)
    (python-get-member f scheme-procedure-key #f))
  
  (define procedure->py-function%
    (opt-lambda (proc [name #f])
      (let ([fn (python-new-object py-function%)])
        (py-function%-init fn name proc)
        fn)))

  (define (py-string%-init this value)
    (python-set-member! this scheme-string-key value))
  
  (define (string->py-string% s)
    (let ([ps (python-new-object py-string%)])
      (py-string%-init ps s)
      ps))
  
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
  
  (define (py-dict%->hash-table pd)
    (python-get-member pd scheme-hash-table-key #f))
  
  (define (assoc-list->py-dict% al)
    (hash-table->py-dict% (assoc-list->hash-table al)))

  (define (py-sequence%->list s)
    (cond
      [(py-is-a? s py-list%) (py-list%->list s)]
      [(py-is-a? s py-tuple%) (py-tuple%->list s)]
      [else (error (format "~a is not a sequence" (py-object%->string s)))]))
  
  ;; python-set-name!: py-object% symbol ->
  (define (python-set-name! obj name)
    (python-set-member! obj '__name__ (if (symbol? name)
                                          (symbol->py-string% name)
                                          name)))
  
  (define (py-function%-init this name v)
    (python-set-name! this name)
    (python-set-member! this scheme-procedure-key v))

  
  ;; py-is?: python-object python-object -> bool
  ;; determine whether two objects are the same exact thing
  (define py-is? eq?)
  
  
  ;; python-new-static-method: py-type% -> py-static-method%
  ;; "allocate" a new static-method object
  (define (python-new-static-method class)
    (unless (or (py-is? class py-static-method%)
                (py-is-a? class py-static-method%))
      (error (py-object%->string class)
             "is not a subtype of staticmethod."))
    (let ([sm (python-new-object class)])
      (python-set-member! sm 'static-method-function #f)
      sm))

  (define (py-type? node)
    (py-is? (python-node-type node) py-type%))
  
  
  ;; python-get-bases: py-object% -> (listof py-type)
  (define (python-get-bases obj)
    (if (py-type? obj)
        (py-tuple%->list (hash-table-get (python-node-dict obj) '__bases__
                                         (lambda ()
                                           (error "object type" (python-get-type-name obj) "has no bases!"))))
        (list (python-node-type obj))))
    
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
  
  
  (define (py-static-method%-init this function-to-wrap)
;    (unless (py-is-a? function-to-wrap py-function%)
;      (error (format "staticmethod.__init__: ~a is not a function"
;                     (py-object%->string function-to-wrap))))
    (python-set-member! this 'static-method-function
                        function-to-wrap))
  
  (define (py-function%->py-static-method% fn)
    (let ([sm (python-new-static-method py-static-method%)])
      (py-static-method%-init sm fn)
      sm))
  
  (define (py-classmethod%-new class)
    (unless (or (py-is? class py-classmethod%)
                (py-is-a? class py-classmethod%))
      (error (py-object%->string class)
             "is not a subtype of classmethod."))
    (let ([cm (python-new-object class)])
      (python-set-member! cm 'classmethod-function #f)
      cm))
  
  (define (py-classmethod%-init this function-to-wrap)
    (python-set-member! this 'classmethod-function
                        function-to-wrap))
  
  (define (py-function%->py-classmethod% fn)
    (let ([cm (py-classmethod%-new py-classmethod%)])
      (py-classmethod%-init cm fn)
      cm))
  
  (define py-bad-new (py-function%->py-static-method%
                      (procedure->py-function%
                       (lambda (which-class)
                         (error "Cannot create instances of "
                                (py-object%->string which-class)))
                       '|bad\__new__/bad|)))
  
  (dprintf "2~n")
  (python-add-members py-function%
                      `((__new__ ,py-bad-new)
                        (__init__ ,py-function%-init)
                        (__call__ ,(lambda (this . args)
                                     (apply (py-function%->procedure this)
                                            args)))))

  (python-set-member! py-object% '__new__ (py-function%->py-static-method%
                                           (procedure->py-function% python-new-object
                                                                    '__new__)))
  
  (dprintf "3~n")
  
  (define (number->py-number% num)
    (py-create (cond
                 [(integer? num) py-int%]
                 [(real? num) py-float%]
                 [else (error "The runtime system does not support the number" num)])
               num))
  
  
  ;; py-call: py-object%(X ... -> Y) arg-list -> ?
  (define (py-call functor arg-list)
    (dprintf "DEBUG: py-call~n")
    (cond
      [(procedure? functor) (dprintf "DEBUG: py-call got a procedure~n")
       (with-handlers ([exn:application:arity? (lambda (exn)
                                                 (error "Incorrect number of arguments:"
                                                        (map py-object%->string arg-list)))])
         (apply functor arg-list))]
      [(and (py-type? functor)
            (not (py-is? functor py-type%))) (py-call python-create-object
                                                      (cons functor arg-list))]
      [(py-is-a? functor py-function%) (dprintf "DEBUG: py-call got a py-function%~n")
       (py-call (py-function%->procedure functor)
                arg-list)]
      ;; else, it's a py-method% or some other thing that has a __call__ field
      [else (dprintf "DEBUG: py-call got something else~n")
       (py-call (python-get-member functor
                                   '__call__
                                   false)
                (cons functor arg-list))]))
  
  ;; create instance object
  (define (python-create-object class . init-args)
    (dprintf "DEBUG: python-create-object~n")
    (let ([obj (py-call (begin0
                          (python-get-member class '__new__) ;; static methods become functions when wrapped
                          (dprintf "DEBUG: python-create-object found __new__~n"))
                        (list class))])
      (dprintf "DEBUG: python-create-object allocated~n")
      (py-call (python-get-member class '__init__ #f)
               (cons obj init-args))
      (dprintf "DEBUG: python-create-object initialized~n")
      obj))
  
  ;; create a new type
  (define (python-create-type name base-types member-dict)
    (let ([type (py-call (python-get-member py-type% '__new__)
                         (list py-type%))])
      (py-call (python-get-member py-type% '__init__ #f)
               (list type name base-types member-dict))
      type))
  
  
  ;; py-create: py-type X ... -> py-object%
  ;; create a new instance of a type
  #|  (define (py-create class . init-args)
        (let ([obj (python-method-call class '__new__)]) ; class)]) ;; class is not auto-passed, __new__ is static
          (py-call (python-get-member class '__init__ #f)
                   (cons obj init-args))
          obj))
  |#
  (define py-create python-create-object)
  
  

  
  ;; python-get-name: py-object% -> py-string%
  (define (python-get-name obj)
    (cond
      [(py-is-a? obj py-method%) (python-get-name (python-get-member obj 'im_func false))]
      [else (python-get-member obj '__name__ false)]))
  
  
  ;; py-object%->string: py-object% -> string
  (define (py-object%->string x)
    (cond
      [(py-type? x) (format "<type ~a>"
                            (py-object%->string (python-get-type-name x)))]
      [(py-is-a? x py-string%) (string-append "'" (py-string%->string x) "'")]
      [(py-is-a? x py-number%) (number->string (py-number%->number x))]
      [(py-is-a? x py-list%) (letrec ([list-items-repr
                                       (lambda (l)
                                         (cond
                                           [(empty? l) ""]
                                           [(empty? (rest l)) (py-object%->string (first l))]
                                           [else (string-append (py-object%->string (first l))
                                                                ", "
                                                                (list-items-repr (rest l)))]))])
                               (string-append "["
                                              (list-items-repr (py-list%->list x))
                                              "]"))]
      [(py-is-a? x py-tuple%) (letrec ([tuple-items-repr
                                       (lambda (l)
                                         (cond
                                           [(empty? l) ""]
                                           [(empty? (rest l)) (py-object%->string (first l))]
                                           [else (string-append (py-object%->string (first l))
                                                                ", "
                                                                (tuple-items-repr (rest l)))]))])
                               (string-append "("
                                              (tuple-items-repr (py-tuple%->list x))
                                              ")"))]
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
                        (dict-items-repr (py-dict%->hash-table x))
                        "}"))]
      [(py-is-a? x py-function%) (string-append "<function "
                                                (py-string%->string (python-get-name x))
                                                ">")]
      [(py-is-a? x py-method%) (let ([bound? (python-method-bound? x)])
       (string-append "<" (if bound?
                              "bound"
                              "unbound")
                      " method "
                      (py-string%->string
                       (python-get-name (python-get-member x
                                                           'im_class)))
                      "."
                      (py-string%->string (python-get-name x))
                      (if bound?
                          (string-append " of "
                                         (py-object%->string (python-get-member x 'im_self)))
                          "")
                      ">"))]
      [else (format "<~a object>" (py-string%->string (python-get-type-name (python-node-type x))))]))
  
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
  
  
  ;; python-index: (union py-list% py-tuple% py-dict%) number -> py-object%
  (define (python-index indexable index)
    (cond
      [(py-is-a? indexable py-list%) (list-ref (py-list%->list indexable) index)]
      [(py-is-a? indexable py-tuple%) (list-ref (py-tuple%->list indexable) index)]
      [(py-is-a? indexable py-dict%) (error "python-index: dictionaries not yet supported")]
      [else (error (format "python-index: cannot index into this: ~a"
                           (py-object%->string indexable)))]))
  
  
  (define (has-member? class member-name)
    (with-handlers ([exn? (lambda (exn) #f)])
      (python-get-member class member-name #f)
      #t))
  

  
  
  
  
  ;;(union procedure py-function% py-classmethod% py-static-method%) class instance
  ;;  ->
  ;;    (union py-method% py-function%)
  (define (python-wrap-member member class obj)
    (cond
      [(or (procedure? member)
            (py-is-a? member py-function%))
        (python-create-object py-method%
                              (if (procedure? member)  ;; if it's not wrapped as a proc already...
                                  (procedure->py-function% member 'python-function)
                                  member)
                              class
                              obj)]
      ;; x.my_static_method returns a plain py-function%
      [(py-is-a? member py-static-method%)
       (let ([fn (python-get-member member 'static-method-function #f)])
         (unless fn
           (error "Uninitialized static method object"))
         fn)]
      ;; x.my_classmethod returns a wrapped member function...
      [(py-is-a? member py-classmethod%)
       (let ([fn (python-get-member member 'classmethod-function #f)])
         (unless fn
           (error "Uninitialized classmethod object"))
         (python-wrap-member fn py-type% class))]
      ;; otherwise, it's just a member variable...
      [else member]))


  ;; py-repr: python-node -> py-string%
  (define py-repr (lambda (x)
                    (string->py-string% (py-object%->string x))))
  
  ;; python-method-call: python-object symbol X ... -> ?
  (define (python-method-call obj method-name . args)
    (let ([fn (python-get-member obj method-name)])
      (py-call fn
               (if (and (python-node? fn)
                        (or (py-is-a? fn py-function%) ; static method
                            (python-method-bound? fn))) ; bound method
                   args
                   (cons obj args))))) ; add the object to the arg-list for unbound methods or scheme procs
  
  
  (dprintf "4~n")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;; new forms for introducing procedures ;;;;;;;;;;;;;;;;;;;;;
  
  (define-syntax py-lambda
    (lambda (stx)
      (syntax-case stx ()
        [(_ name (arg ...) expr ...)
         (let ([expr-stx (syntax->list (syntax (list expr ...)))])
           #`(procedure->py-function%
                        (lambda #,(syntax (arg ...))
                          #,@expr-stx)
                        #,(syntax name)))]
        [(_ name ((arg ...) . rest-args) expr ...)
         #`(procedure->py-function%
                      (lambda (#,@(syntax (arg ...)) . (syntax rest-args))
                        #,(syntax (expr ...)))
                      #,(syntax name))])))
  
  
  (define-syntax py-opt-lambda
    (lambda (stx)
      (syntax-case stx ()
        [(_ name (arg ...) expr ...)
         (let ([expr-stx (syntax->list (syntax (list expr ...)))])
           #`(procedure->py-function%
                        (opt-lambda #,(syntax (arg ...))
                          #,@expr-stx)
                        #,(syntax name)))])))
  
  
  (dprintf "5~n")
  
  ;;;;;;;;;;;;;;; complete the basic types now ;;;;;;;;;;;;;;;;;;;;;;;;;;

  
  (python-add-members py-static-method%
                      `((__new__ ,(py-function%->py-static-method%
                                   (procedure->py-function% python-new-static-method
                                                            'python-new-static-method)))
                        (__init__ ,py-static-method%-init)))
  
  (python-add-members py-classmethod%
                      `((__new__ ,(py-function%->py-static-method%
                                   (procedure->py-function% py-classmethod%-new
                                                            'py-classmethod%-new)))
                        (__init__ ,py-classmethod%-init)))
  
  (dprintf "6~n")
  
  (python-add-members py-object%
                      `((__init__ ,(procedure->py-function% (lambda (this) (void))
                                                            '__init__))
                        (__repr__ ,py-repr)))
  
  
  (dprintf "7~n")
  
  (define (py-bin-op op)
    (lambda (lhs rhs)
      (number->py-number% (apply-bin-op op lhs rhs))))
  
  
  (define (apply-bin-op op lhs rhs)
    (let ([check (lambda (obj)
                   (unless (py-is-a? obj py-number%)
                     (error (py-object%->string obj) "is not a number.")))])
      (check lhs)
      (check rhs))
    (op (py-number%->number lhs)
        (py-number%->number rhs)))
  
  (python-add-members py-number%
                      `((__init__ ,(lambda (this v)
                                     (python-set-member! this scheme-number-key v)))
                        (__add__ ,(py-bin-op +))
                        (__sub__ ,(py-bin-op -))
                        (__mul__ ,(py-bin-op *))
                        (__div__ ,(py-bin-op /))
                        (__mod__ ,(py-bin-op modulo))
                        (__neg__ ,(lambda (this)
                                    (number->py-number% (- (py-number%->number this)))))))
  
  
  (python-add-members py-string%
                      `((__init__ ,(procedure->py-function% py-string%-init))))
  
  (python-add-members py-none%
                      `((__init__ ,(lambda (this)
                                     (error "Cannot create"
                                            (python-get-type-name py-none%)
                                            "instances")))))
  
  (python-add-members py-slice%
                      `((__init__ ,(opt-lambda (this lower upper [step py-none])
                                     (python-set-member! this 'start lower)
                                     (python-set-member! this 'stop upper)
                                     (python-set-member! this 'step step)))))
  
  (define py-none (make-python-node py-none% (make-hash-table) #f))
  
  (define (python-method-bound? method)
    (not (py-is? (python-get-member method 'im_self)
                 py-none)))
  
  (dprintf "8~n")
  
  (python-add-members py-method%
                      `((__init__ ,(opt-lambda (this fun class [self py-none])
                                     (python-set-member! this 'im_class class)
                                     (python-set-member! this 'im_func fun)
                                     (python-set-member! this 'im_self self)))
                        (test ,(dprintf "test1~n"))
                        (__call__ ,(procedure->py-function%
                                    (lambda (this-method . args)
                                      (dprintf "DEBUG: method.__call__~n")
                                      (py-call (python-get-member this-method 'im_func false)
                                               (if (python-method-bound? this-method)
                                                   (cons (python-get-member this-method 'im_self)
                                                         args)
                                                   (let ([class (python-get-member this-method
                                                                                   'im_class)])
                                                     (dprintf "DEBUG: method.__call__ unbound~n")
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
                                                                    (py-object%->string (car args)))))))))
                                    'wrapped-method))
                        (im_func ,py-none) ; py-procedure%
                        (im_self ,py-none))) ; py-object%
  (dprintf "9~n")
  
  (python-add-members py-tuple%
                      `((__init__ ,(py-lambda '__init__ (this v)
                                              (python-set-member! this
                                                                  scheme-list-key
                                                                  v)))
                        (__getitem__ ,(py-lambda '__getitem__ (this key)
                                                 (simple-get-item this key list->py-tuple% py-tuple%->list)))
                        (__len__ ,(py-lambda '__len__ (this)
                                             (number->py-number% (length (py-tuple%->list this)))))))
  
  (python-add-members py-list%
                      `((__init__ ,(py-lambda '__init__ (this v)
                                              (python-set-member! this
                                                                  scheme-list-key
                                                                  v)))
                        (__getitem__ ,(py-lambda '__getitem__ (this key)
                                                 (simple-get-item this key list->py-list% py-list%->list)))
                        (__len__ ,(py-lambda '__len__ (this)
                                             (number->py-number% (length (py-list%->list this)))))))
                                                                          

  (define (simple-get-item this key list-to-sequence sequence-to-list)
  (cond
                                                   [(py-is-a? key py-number%)
                                                 (list-ref (sequence-to-list this)
                                                           (py-number%->number key))]
                                                   [(py-is-a? key py-slice%)
                                                    (let* ([len (py-number%->number
                                                                 (python-method-call this '__len__))]
                                                           [start (py-number%->number
                                                                   (python-get-member key
                                                                                      'start))]
                                                           [stop (min len
                                                                      (py-number%->number
                                                                       (python-get-member key
                                                                                          'stop)))]
                                                           [step (let ([s (python-get-member key 'step)])
                                                                   (if (py-is? s py-none)
                                                                       1
                                                                       (py-number%->number s)))])
                                                      (list-to-sequence
                                                      (map (lambda (i)
                                                             (python-method-call this '__getitem__
                                                                                 (number->py-number% i)))
                                                           (build-list (floor (/ (- stop start) step))
                                                                       (lambda (i) (* (+ i start) step))))))]
                                                   [else (error "Invalid key for __getitem__")]))
  (python-add-members py-dict%
                      `((__init__ ,(py-lambda '__init__ (this v)
                                              (python-set-member! this
                                                                  scheme-hash-table-key
                                                                  (if (list? v)
                                                                      (assoc-list->hash-table v)
                                                                      v))))
                        (__getitem__ ,(py-lambda '__getitem__ (this key)
                                                 (hash-table-get (python-get-member this
                                                                                    scheme-hash-table-key
                                                                                    false)
                                                                  key)))))
  
  
  
  
  
  ;; more setup for PY-TYPE
;  (python-set-member! py-type% '__new__ python-new-object)
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
  (python-set-member! py-type% '__call__
                      (case-lambda
                        [(this obj) (python-node-type obj)]
                        [(this name base-types member-dict)
                         (python-create-type name base-types member-dict)]))
  
  
  (define py-true (py-create py-int% 1))
  (define py-false (py-create py-int% 0))
  
  
  )