(module primitives mzscheme
  (require (lib "list.ss"))
  (provide (all-defined))

  ;; python-object is a:
  ;; (make-python-object (listof python-class) (listof (symbol sexp)))
  
  ;; python-class is a:
  ;; (make-python-class (listof python-class) (listof (symbol sexp)) symbol)
  
  (define-struct python-object (super-classes members) (make-inspector))

  (define-struct (python-class python-object) (name) (make-inspector))

  (define-struct (python-instance python-object) () (make-inspector))
  
  ;; repr: python-object -> string
  (define (repr x)
    (cond
      [(py-is-a? x py-string%) (string-append "'" (get-python-member x '_value) "'")]
      [(py-is-a? x py-number%) (number->string (get-python-member x '_value))]
      [(py-is-a? x py-list%) (letrec ([list-items-repr
                           (lambda (l)
                             (cond
                               [(empty? l) ""]
                               [(empty? (rest l)) (repr (first l))]
                               [else (string-append (repr (first l))
                                                     ", "
                                                     (list-items-repr (rest l)))]))])
                   (string-append "["
                                  (list-items-repr (get-python-member x '_value))
                                  "]"))]
      [else (format "~a" x)]))
  
  (define (assoc-list->hash-table al)
    (let ([hash-table (make-hash-table)])
      (for-each (lambda (assoc)
                  (hash-table-put! hash-table (car assoc) (cadr assoc)))
                al)
      hash-table))

  (define py-object%
    (make-python-class '()
                       (assoc-list->hash-table `((__repr__ ,repr)))
                       'object))
  
  (define py-builtin-object%
    (make-python-class (list py-object%)
                       (assoc-list->hash-table `((_value 0)
                                                 (__init__ ,(lambda (this v)
                                                              (python-set-member! this '_value v)))))
                       'builtin))
  
  (define (make-builtin-class name)
    (make-python-class (list py-builtin-object%)
                       (make-hash-table)
                       name))
  
  ;; py-gbov: bo -> Value
  ;; python-get-builtin-object-value
  (define (py-gbov bo)
    (get-python-member bo '_value))
  
  (define py-number%
    (make-python-class (list py-builtin-object%)
                       (assoc-list->hash-table
                        `((__add__ ,(lambda (this rhs)
                                      (py-create py-number% (+ (py-gbov this) (py-gbov rhs)))))
                          (__sub__ ,(lambda (this rhs)
                                      (py-create py-number% (- (py-gbov this) (py-gbov rhs)))))))
                       'number))
   
  (define py-string% (make-builtin-class 'string))
  (define py-list% (make-builtin-class 'list))
  (define py-tuple% (make-builtin-class 'tuple))
  (define py-dict% (make-builtin-class 'dict))
  
  (define (get-python-member obj method-name)
    (hash-table-get (python-object-members obj)
                    method-name
                    (lambda ()
                      (let ([super-classes (python-object-super-classes obj)])
                        (if (null? super-classes)
                            (raise (format "method not found: ~a" method-name))
                            (ormap (lambda (super-class)
                                     (get-python-member super-class method-name))
                                   super-classes))))))
  
  (define (has-member? class member-name)
    (with-handlers ([exn? (lambda (exn) #f)])
      (get-python-member class member-name)
      #t))
  

  (define (python-set-member! obj name value)
    (hash-table-put! (python-object-members obj) name value))
  
  ;; python-method-call: python-object symbol X ... -> ?
  (define (python-method-call obj method-name . args)
    (apply (get-python-member obj method-name)
           (cons obj args)))
  
  ;; py-create: python-class X ... -> python-instance
  (define (py-create class . rest)
    (let ([obj (make-python-instance (list class) (make-hash-table))])
      (if (has-member? class '__init__)
          (apply python-method-call
                 (cons obj (cons '__init__ rest)))
          (if (not (null? rest))
              (raise (format "class ~a has no initializer!" class))))
      obj))
  
  ;; py-is?: python-object python-object -> bool
  ;; determine whether two objects are the same exact thing
  (define py-is? eq?)
  
  ;; py-type: python-instance -> python-class
  ;; get the type of an object
  (define (py-type obj)
    (first (python-object-super-classes obj)))

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
  
  )