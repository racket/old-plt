(module primitives mzscheme
  (require (lib "list.ss")
           (lib "etc.ss"))
  (provide (all-defined))

  ;; python-object is a:
  ;; (make-python-object (listof python-class) (listof (symbol sexp)))
  
  ;; python-class is a:
  ;; (make-python-class (listof python-class) (listof (symbol sexp)) symbol)
  
  (define-struct python-object (super-classes members) (make-inspector))

  (define-struct (python-class python-object) (name) (make-inspector))

  (define-struct (python-instance python-object) () (make-inspector))
  
  ;; python-repr: python-object -> py-string%
  (define (py-repr x)
    (py-create py-string%
    (cond
      [(py-is-a? x py-string%) (string-append "'" (python-get-member x '_value) "'")]
      [(py-is-a? x py-number%) (number->string (python-get-member x '_value))]
      [(py-is-a? x py-list%) (letrec ([list-items-repr
                           (lambda (l)
                             (cond
                               [(empty? l) ""]
                               [(empty? (rest l)) (py-repr (first l))]
                               [else (string-append (py-repr (first l))
                                                     ", "
                                                     (list-items-repr (rest l)))]))])
                   (string-append "["
                                  (list-items-repr (python-get-member x '_value))
                                  "]"))]
      [else (format "~a" x)])))
  
  (define (assoc-list->hash-table al)
    (let ([hash-table (make-hash-table)])
      (for-each (lambda (assoc)
                  (hash-table-put! hash-table (car assoc) (cadr assoc)))
                al)
      hash-table))

  (define py-object%
    (make-python-class '()
                       (assoc-list->hash-table `((__repr__ ,py-repr)))
                       'object))
  
  (define (has-member? class member-name)
    (with-handlers ([exn? (lambda (exn) #f)])
      (python-get-member class member-name #f)
      #t))
  
  (define (make-builtin-class name)
    (make-python-class (list py-object%)
                       (assoc-list->hash-table
                        `((__init__ ,(lambda (this v)
                                       (python-set-member! this '_value v)))))
                       name))
  
  ;; py-gbov: bo -> Value
  ;; python-get-builtin-object-value
  (define (py-gbov bo)
    (python-get-member bo '_value false))
  

  (define py-procedure%
    (make-python-class (list py-object%)
                       (assoc-list->hash-table
                        `((__init__ ,(lambda (this v)
                                       (python-set-member! this '_value v)))
                          (__call__ ,(lambda (this . args)
                                       (py-call (py-gbov this)
                                                args)))
                          (__name__ 'anonymous-procedure)))
                       'procedure))
  
  
  (define py-number%
    (make-python-class (list py-object%)
                       (assoc-list->hash-table
                        `((__init__ ,(lambda (this v)
                                       (python-set-member! this '_value v)))
                          (__add__ ,(lambda (this rhs)
                                      (py-create py-number% (+ (py-gbov this) (py-gbov rhs)))))
                          (__sub__ ,(lambda (this rhs)
                                      (py-create py-number% (- (py-gbov this) (py-gbov rhs)))))))
                       'number))
   

  (define py-method%
    (make-python-class (list py-object%)
                       (assoc-list->hash-table
                        `((__init__ ,(opt-lambda (this fun [self #f])
                                       (python-set-member! this 'im_func fun)
                                       (python-set-member! this 'im_self self)))
                          (__call__ ,(lambda (this-method . args)
                                       (py-call (python-get-member this-method 'im_func false)
                                                (let ([self (python-get-member this-method 'im_self)])
                                                  (if self
                                                      (cons self args)
                                                      args)))))
                          (im_func ,#f) ; py-procedure%
                          (im_self ,#f))) ; py-object%
                       'method))
  

  (define py-string% (make-builtin-class 'string))
  (define py-list% (make-builtin-class 'list))
  (define py-tuple% (make-builtin-class 'tuple))
  (define py-dict% (make-builtin-class 'dict))
  
  (define python-get-member
    (opt-lambda (obj member-name [wrap? #t])
      (if (eq? member-name '__dict__)
          (python-object-members obj)
          (let ([member (hash-table-get (python-object-members obj)
                                        member-name
                                        (lambda ()
                                          (let ([super-classes (python-object-super-classes obj)])
                                            (if (null? super-classes)
                                                (raise (format "member not found: ~a" member-name))
                                                (ormap (lambda (super-class)
                                                         (python-get-member super-class member-name wrap?))
                                                       super-classes)))))])
            (if wrap?
                (if (procedure? member) ;; if this member is a method...
                    (python-wrap-method member obj)
                    (if (and (python-object? member)
                             (py-is-a? member py-method%)) ;; if this was wrapped by a recursive call...
                        (python-wrap-method (python-unwrap-method member)
                                            obj)
                        member))
                member)))))

  ;; python-wrap-method: (union procedure py-procedure%) python-object -> python-method%
  (define (python-wrap-method method obj)
    (py-create py-method%
               (if (procedure? method)  ;; if it's not wrapped as a proc already...
                   (py-create py-procedure% method)
                   method)
               obj))
  
  ;; python-unwrap-method: python-method% -> python-procedure%
  ;; grab the function wrapped by the method object
  (define (python-unwrap-method method)
    (python-get-member method 'im_func))
  
  (define (python-set-member! obj name value)
    (hash-table-put! (python-object-members obj) name value))
  
  ;; python-method-call: python-object symbol X ... -> ?
  ;; if the method is a py-method%, no need to pass self, otherwise include it
  (define (python-method-call obj method-name . args)
    (let ([method (python-get-member obj method-name false)])
      (if (procedure? method)
          (py-call method (cons obj args))
          (py-call method args))))
  
  
  ;; py-is?: python-object python-object -> bool
  ;; determine whether two objects are the same exact thing
  (define py-is? eq?)
  
  ;; py-type: python-instance -> python-class
  ;; get the type of an object
  (define (py-type obj)
    (first (python-object-super-classes obj)))

  ;; py-call: callable-python-object arg-list -> ?
  (define (py-call functor arg-list)
    (cond
      [(procedure? functor) (apply functor arg-list)]
      [else (py-call (python-get-member functor
                                        '__call__
                                        false)
                     (cons functor arg-list))]))
  
  ;; py-create: python-class X ... -> python-instance
  (define (py-create class . rest)
    (let ([obj (make-python-instance (list class) (make-hash-table))])
      (if (has-member? class '__init__)
          (let ([init (python-get-member obj '__init__ #f)])
            (if (procedure? init)
                (apply init (cons obj rest))
                (py-call init
                         (cons init (cons obj rest)))))
          (if (not (null? rest))
              (raise (format "class ~a has no initializer!" (python-class-name class)))))
      obj))
  
  
  ;; py-is-a? python-object python-class -> bool
  (define (py-is-a? obj class)
    (let ([super-classes (python-object-super-classes obj)])
      (if (or (and (python-class? obj)
                   (py-is? obj class))
              (ormap (lambda (super-class)
                       (or (py-is? super-class class)
                           (py-is-a? super-class class)))
                     super-classes))
          #t #f)))

  (define-syntax py-lambda
    (lambda (stx)
      (syntax-case stx ()
        [(_ (arg ...) expr ...)
         (let ([args (syntax->list (syntax (arg ...)))]
               [exprs (syntax->list (syntax (expr ...)))])
           #`(py-create py-procedure% (lambda (#,@args)
                                       #,@exprs)))])))
  
  
  )