#cs(module primitives mzscheme
  (require (lib "list.ss")
           (lib "etc.ss")
           "python-node.ss"
           "builtin-types-uninitialized.ss"
           "python-import.ss"
           )
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


  (define-struct (exn:not-found exn) () (make-inspector))
  (define-struct (exn:python exn) (type value) (make-inspector))

  (define python-current-exception
    (let ([current-exception (make-exn:python "" (current-continuation-marks) #f #f)])
      (case-lambda
        [() current-exception]
        [(exn) (set! current-exception exn)])))

  ; py-raise: (union #f py-type%) (union #f python-node) (union #f py-traceback%) ->
  (define (py-raise type obj traceback)
    (unless type
      (raise (python-current-exception)))
    (let* ([exn-type (cond
                       [(py-type? type) type]
                       [(py-is-a? type py-string%) type]
                       [(not (py-is? type py-none) (python-node-type type))]
                       [else (py-raise py-type-error%
                                       (format "exceptions must be strings, classes, or instances, not ~a"
                                               (py-object%->string type))
                                       #f)])]
           [exn-value (cond
                        [(or (not obj)
                             (py-is? obj py-none)) (cond
                                                     [(py-type? type) (py-create type)]
                                                     [(py-is-a? type py-string%) py-none]
                                                     [else type])]
                        [(py-is-a? type py-string%) obj]
                        [(and (not (py-type? type))
                              (not (py-type? obj))) (py-raise py-type-error%
                                                              (string->py-string%
                                                              "instance exception may not have a separate value")
                                                              #f)]
                        [(py-is-a? obj exn-type) obj]
                        [(py-is-a? obj py-tuple%) (py-call py-create
                                                           (cons type
                                                                 (py-tuple%->list obj)))]
                        [else (py-create type obj)])]
           [exn (make-exn:python (format "~a~a"
                                         (py-object%->string exn-type)
                                         (if (py-is? exn-value py-none)
                                             ""
                                             (format ": ~a"
                                                     (py-object%->string
                                                      (with-handlers ([exn:not-found? (lambda (exn) exn-value)])
                                                        (python-get-member exn-value 'args))))))
                                 (current-continuation-marks)
                                 exn-type
                                 exn-value)])
      (python-current-exception exn)
      (raise exn)))

  ;; there are four special cases in this function.
  (define python-get-member
    (opt-lambda (obj member-name [wrap? #t] [orig-call? #t])
      (cond
        ;; special case #1: __class__ is actually the type
        [(eq? member-name '__class__) (python-node-type obj)]
         ;; special case #2: __dict__ is the internal dictionary (well, usually... let's just assume always)
         [(eq? member-name '__dict__) (py-create py-dict% (python-node-dict obj))]
         ;;;;;;;;;;;;;; SPECIAL CASE 3 NOW IRRELEVANT.  __call__ DEFINED AS A MEMBER OF py-type%
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
                                                  (raise (make-exn:not-found (format "member ~a not found in ~a"
                                                                                     member-name
                                                                                     (py-object%->string obj))
                                                                             (current-continuation-marks))))])
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
    (opt-lambda (proc [name #f] [pos '()] [key '()] [seq #f] [dict #f])
      (let ([fn (python-new-object py-function%)])
        (py-function%-init fn)
        (python-set-name! fn name)
        (python-set-member! fn scheme-procedure-key proc)
        (python-set-member! fn python-function-pos-ids-key pos)
        (python-set-member! fn python-function-key-ids-key key)
        (python-set-member! fn python-function-seq-id-key seq)
        (python-set-member! fn python-function-dict-id-key dict)
        fn)))

  (define (py-string%-init this value)
    (python-set-member! this scheme-string-key value))

  (define (string->py-string% s)
    (let ([ps (python-new-object py-string%)])
      (py-string%-init ps s)
      ps))

  (define (py-string%->string ps)
    (python-get-member ps scheme-string-key #f))

  (define (py-string%->symbol ps)
    (string->symbol (py-string%->string ps)))

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

  (define (py-object%->bool x)
    (cond
      [(py-is-a? x py-number%) (not (zero? (py-number%->number x)))]
      [(py-is? x py-none) #f]
      [(py-type? x) #t]
      [else (with-handlers ([exn:not-found? (lambda (exn) #t)])
              (py-object%->bool (python-method-call x '__len__)))]))


  (define (bool->py-number% x)
    (number->py-number% (if x 1 0)))


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

  (define (py-function%-init this)
    (void))


  (define (py-file%->port pf)
    (python-get-member pf scheme-port-key #f))


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
                                           (error "object type"
                                                  (python-get-type-name obj)
                                                  "has no bases!"))))
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
     
  (define (py-static-method%->py-function% sm)
    (python-get-member sm 'static-method-function #f))

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

     (define py-object%-new (py-function%->py-static-method%
                                           (procedure->py-function% python-new-object
                                                                    '__new__)))
     
  (python-set-member! py-object% '__new__ py-object%-new)

  (dprintf "3~n")

  (define (number->py-number% num)
    (py-create (cond
                 [(integer? num) py-int%]
                 [(real? num) py-float%]
                 [else (error "The runtime system does not support the number" num)])
               num))


  ;; py-call: py-object%(X ... -> Y) arg-list -> ?
  (define py-call
    (opt-lambda (functor arg-list [kw-args '()])
    (dprintf "DEBUG: py-call~n")
    (cond
      [(procedure? functor) (dprintf "DEBUG: py-call got a procedure~n")
       (with-handlers ([exn:application:arity? (lambda (exn)
                                                 (printf "function: ~a" functor)
                                                 (error "Incorrect number of arguments:"
                                                        (map py-object%->string arg-list)
                                                        (map py-object%->string kw-args)))])
         (apply functor arg-list))]
      [(and (py-type? functor)
            (not (py-is? functor py-type%))) (py-call python-create-object
                                                      (cons functor arg-list))]
      [(py-is-a? functor py-function%) ;(dprintf "DEBUG: py-call got a py-function%~n")
       ;;; TODO: cannot handle f(x=1, x=2) yet
       (let ([pos (python-get-member functor python-function-pos-ids-key #f)]
             [key (python-get-member functor python-function-key-ids-key #f)]
             [seq (python-get-member functor python-function-seq-id-key #f)]
             [dict (python-get-member functor python-function-dict-id-key #f)])
         (let ([min-arity (length pos)]
               [num-pos-args (length arg-list)])
         ;  (when (< num-pos-args min-arity)
         ;    (error "Not enough arguments for " (py-object%->string functor)))
           (let* ([all-parms (append pos key)]
                  [to-go (if (> num-pos-args (+ min-arity (length key)))
                             '()
                             (list-tail all-parms num-pos-args))])
             (for-each (lambda (parm)
                         (when (assq parm kw-args)
                           (error "cannot use a keyword argument to define a parameter already passed as a positional argument")))
                       (list-head all-parms (- (length all-parms) (length to-go))))
             (let ([rest-must-be-dict? false])
               (for-each (lambda (parm)
                           (cond
                             [(assq parm kw-args) => (lambda (x)
                                                       (when rest-must-be-dict?
                                                         (error "Invalid order for keyword arguments"))
                                                       (set! arg-list
                                                             (append arg-list (cons (second x) empty)))
                                                       (set! kw-args
                                                             (nassq parm kw-args)))]
                             [else (set! rest-must-be-dict? true)]))
                         to-go))
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;; to do here...
             ;;;;;;;;; check when arg-list > pos (dump the rest to uhh... rest)
           (py-call (py-function%->procedure functor)
                    (if dict
                        (begin ;(printf "keywords args: ~a~n" kw-args)
                               (cons (assoc-list->py-dict% kw-args) arg-list))
                        arg-list)))))]
      ;; else, it's a py-method% or some other thing that has a __call__ field
      [else (dprintf "DEBUG: py-call got something else~n")
       (py-call (python-get-member functor
                                   '__call__
                                   false)
                (cons functor arg-list)
                kw-args)])))

  (define (list-head list items)
    (reverse (list-tail (reverse list)
                        (- (length list) items))))

(define (nassq sym assl)
  (filter (lambda (x)
            (not (eq? sym (first x))))
          assl))


  ;; create instance object
  (define (python-create-object class . init-args)
   ; (printf "DEBUG: python-create-object~n")
    (let* ([alloc (begin0 (python-get-member class '__new__ #f)
                       ;   (printf "DEBUG: python-create-object found __new__~n")
                          )]
           [obj (if (eq? alloc py-object%-new)
                    (begin ;(printf "DEBUG: python-create-object calling object.__new__: ~a~n"
                           ;        (py-object%->string alloc))
                      (py-call (py-static-method%->py-function% alloc)
                             (list class)))
                    (py-call alloc (cons class init-args)))])
      ;(printf "DEBUG: python-create-object allocated~n")
      (when (py-is-a? obj class)
        (py-call (python-get-member class '__init__ #f)
                 (cons obj init-args))
        ;(printf "DEBUG: python-create-object initialized~n")
        )
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
      [(py-is? x py-none) "None"]
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
                                         (if (and (symbol? key) ;; special internal Spy value
                                                  (not (python-node? value)))
                                             (string-append "<internal Spy key "
                                                            (symbol->string key)
                                                            ">: "
                                                            (format "~a" value))
                                             ;;;; the key is either a number or a symbol
                                             (string-append (if (number? key)
                                                                (number->string key)
                                                                (string-append "'"
                                                                               (symbol->string key)
                                                                               "'"))
                                                            ; (py-object%->string key)
                                                            ": "
                                                            (py-object%->string value)))))))])
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
                                         (py-object%->string (python-get-member x 'im_self #f)))
                          "")
                      ">"))]
      [(py-is-a? x py-module%) (format "<module '~a' from '~a'>"
                                       (py-string%->string (python-get-name x))
                                       (py-string%->string (python-get-member x '__file__ #f)))]
      [else (format "<~a object~a>" (py-string%->string (python-get-type-name (python-node-type x)))
                    (if (spy-ext-object? x)
                        (string-append ": " ((eval 'spy-ext-object->string) x))
                        ""))]))
     
     (define (spy-ext-object? obj)
       (hash-table-get (python-node-dict (python-node-type obj)) 'spy-ext-type (lambda () #f)))
     

  (define (python-get-attribute obj attr-sym)
   ; (printf "python-get-attribute is looking for ~a~n" attr-sym)
    (py-call (python-get-member (python-node-type obj) '__getattribute__)
             (cons obj (list (symbol->py-string% attr-sym)))))
;    (python-method-call obj
;                        (python-get-member (python-node-type obj) '__getattribute__)
;                        (list (symbol->py-string% attr-sym))))

     ;; python-set-attribute: py-object% symbol py-object% -> void
  (define (python-set-attribute! obj attr-sym value)
    (py-call (python-get-member (python-node-type obj) '__setattr__)
             (cons obj (list (symbol->py-string% attr-sym)
                             value))))

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
;    (python-method-call indexable '__getitem__ (list index)))
    (cond
      [(py-is-a? indexable py-list%) (list-ref (py-list%->list indexable) index)]
      [(py-is-a? indexable py-tuple%) (list-ref (py-tuple%->list indexable) index)]
      [(py-is-a? indexable py-dict%) (error "python-index: dictionaries not yet supported")]
      [(py-type? indexable) (error (format "Unsubscriptable object: ~a" (py-object%->string indexable)))]
      [else (python-method-call indexable '__getitem__
                                (list (if (number? index)
                                          (number->py-number% index)
                                          index)))]))
       ;(error (format "python-index: cannot index into this: ~a"
       ;                    (py-object%->string indexable)))]))


  (define (has-member? class member-name)
    (with-handlers ([exn? (lambda (exn) #f)])
      (python-get-member class member-name #f)
      #t))



  ;; py-compatible-exn? py-object% exn:python -> bool
  ;; determine whether obj (from a try/except clause) matches the thrown exception
  (define (py-compatible-exn? obj exn)
    (or (py-is? obj
         (exn:python-value exn))
        (and (py-type? obj)
             (py-is-a? (exn:python-value exn) obj))
        (and (py-is-a? obj py-tuple%)
             (ormap (lambda (item)
                      (py-compatible-exn? item exn))
                    (py-tuple%->list obj)))))



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

  ;; python-method-call: python-object (U symbol py-method%) (listof X) (listof (cons Symbol X) -> ?
  (define python-method-call
    (opt-lambda (obj method [pos-args '()] [key-args '()])
;      (printf "python-method-call is looking for ~a~n" (if (symbol? method)
;                                                           method
;                                                           (py-object%->string method)))
      (let ([fn (if (symbol? method)
                    (python-get-attribute obj method)
                    method)])
;        (if (py-type? obj) ;; Class.method(obj, arg...)
;            (
        (py-call fn
                 (if (and (python-node? fn)
                          (or (py-is-a? fn py-function%) ; static method
                              (python-method-bound? fn) ; bound method
                              (py-type? obj) ;  Class.method(obj, arg...)
                              ))
                     pos-args
                     (cons obj pos-args)) ; add the object to the arg-list for unbound methods or scheme procs
                 key-args))))


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
                        (__init__ ,py-static-method%-init)
                        (__call__ ,(procedure->py-function% (lambda (this . args)
                                                              (py-call (py-static-method%->py-function% this)
                                                                       args))
                                                            '__call__))))

  (python-add-members py-classmethod%
                      `((__new__ ,(py-function%->py-static-method%
                                   (procedure->py-function% py-classmethod%-new
                                                            'py-classmethod%-new)))
                        (__init__ ,py-classmethod%-init)))

  (dprintf "6~n")

  (python-add-members py-object%
                      `((__init__ ,(procedure->py-function% (lambda (this . args) (void))
                                                            '__init__))
                        (__repr__ ,py-repr)
                        (__str__ ,py-repr)
                        (__getattribute__ ,(py-lambda '__getattribute__ (this key)
                                               (python-get-member this (py-string%->symbol key))))
                        (__setattr__ ,(py-lambda '__setattr__ (this key value)
                                                (python-set-member! this (py-string%->symbol key) value)))))

;  (python-add-members py-type%
;                      `((__getattribute__ ,python-get-member)
;                        (__setattr__ ,python-set-member!)))
  (dprintf "7~n")

  (define (py-bin-op op)
    (lambda (lhs rhs)
      (number->py-number% (apply-bin-op op lhs rhs))))


  (define (apply-bin-op op lhs rhs)
    (let ([check (lambda (obj)
                   (unless (py-is-a? obj py-number%)
                     (py-raise py-type-error%
                               (format "~a is not a number."
                                       (py-object%->string obj)))))])
      (check lhs)
      (check rhs))
    (op (py-number%->number lhs)
        (py-number%->number rhs)))

  (define (py-mult-num lhs rhs)
    (cond
      [(and (py-is-a? lhs py-number%)
            (py-is-a? rhs py-number%) (number->py-number% (* (py-number%->number lhs)
                                                             (py-number%->number rhs))))]
      [(and (py-is-a? lhs py-string%)
            (py-is-a? rhs py-number%)) (string->py-string% (repeat-string (py-string%->string lhs)
                                                                          (py-number%->number rhs)))]
      [(and (py-is-a? rhs py-string%)
            (py-is-a? lhs py-number%)) (string->py-string% (repeat-string (py-string%->string rhs)
                                                                          (py-number%->number lhs)))]
      [else (py-raise py-type-error%
                      (format "cannot multiply ~a and ~a"
                              (py-object%->string lhs)
                              (py-object%->string rhs)))]))

  (define (repeat-string s n)
    (if (zero? n)
        ""
        (string-append s (repeat-string s (sub1 n)))))

  (python-add-members py-number%
                      `((__init__ ,(lambda (this v)
                                     (python-set-member! this scheme-number-key v)))
                        (__add__ ,(py-bin-op +))
                        (__sub__ ,(py-bin-op -))
                        (__mul__ ,py-mult-num)
                        (__div__ ,(py-bin-op /))
                        (__mod__ ,(py-bin-op modulo))
                        (__neg__ ,(lambda (this)
                                    (number->py-number% (- (py-number%->number this)))))))


  (python-add-members py-string%
                      `((__init__ ,(procedure->py-function% py-string%-init))
                        (__add__ ,(py-lambda '+ (this s)
                                          (string->py-string% (string-append (py-string%->string this)
                                                                               (py-string%->string s)))))
                        (__mul__ ,py-mult-num)))

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


  (python-add-members py-exception%
                      `((__init__ ,(lambda (this . args)
                                     (python-set-member! this 'args
                                                         (list->py-tuple% args))))
                        (__getitem__ ,(py-lambda '__getitem__ (this key)
                                                 (python-method-call (python-get-member this 'args #f)
                                                                     '__getitem__
                                                                     (list key))))))

  (define py-none (make-python-node py-none% (make-hash-table) #f))

  (define (py-none? obj) (py-is? obj py-none))

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
                                                         (error (format "~a must be called with ~a instance as first argument (got ~a instead)"
                                                                        (py-object%->string this-method)
                                                                        (py-string%->string
                                                                         (python-get-type-name class))
                                                                        (if (null? args)
                                                                            "nothing"
                                                                            (string-append (py-string%->string
                                                                                            (python-get-type-name (python-node-type (car args))))
                                                                                           " instance")))))))))
                                    'wrapped-method))
                        (im_func ,py-none) ; py-procedure%
                        (im_self ,py-none))) ; py-object%
  (dprintf "9~n")

  (python-add-members py-tuple%
                      `((__init__ ,(py-lambda '__init__ (this v)
                                              (cond
                                                [(list? v)
                                                 ;(printf "PT LIST~n")
                                                 (python-set-member! this
                                                                     scheme-list-key
                                                                     v)]
                                                [(number? v)
                                                 ;(printf "PT NUM: ~a~n" v)
                                                 (python-set-member! this
                                                                     scheme-list-key
                                                                     (build-list v identity))]
                                                [(py-is-a? v py-tuple%)
                                                 ;(printf "PT TUP~n")
                                                 (python-set-member! this
                                                                     scheme-list-key
                                                                     (py-tuple%->list v))]
                                                [else (error "Invalid argument to tuple constructor")])
                                              ;(printf "new tuple is ~a~n" (py-tuple%->list this))
					      ))
                        (__getitem__ ,(py-lambda '__getitem__ (this key)
                                                 (simple-get-item this key list->py-tuple% py-tuple%->list)))
                        (__setitem__ ,(py-lambda '__setitem__ (this key value)
                                                 (simple-set-item this key value py-tuple%->list)))
                        (__len__ ,(py-lambda '__len__ (this)
                                             (number->py-number% (length (py-tuple%->list this)))))))

  (python-add-members py-list%
                      `((__init__ ,(py-lambda '__init__ (this v)
                                              (python-set-member! this
                                                                  scheme-list-key
                                                                  v)))
                        (__getitem__ ,(py-lambda '__getitem__ (this key)
                                                 (simple-get-item this key list->py-list% py-list%->list)))
                        (__setitem__ ,(py-lambda '__setitem__ (this key value)
                                                 (simple-set-item this key value py-list%->list)))
                        (__len__ ,(py-lambda '__len__ (this)
                                             (number->py-number% (length (py-list%->list this)))))))


  (define (simple-set-item this key value sequence-to-list)
    ;(printf "SIMPLE-SET-ITEM~n")
    (cond
      [(py-is-a? key py-number%)
       (set-car! (list-tail (sequence-to-list this)
                            (py-number%->number key))
                 value)]
      [(py-is-a? key py-slice%)
       (let* ([list-len (py-number%->number
                        (python-method-call this '__len__))]
             [start (py-number%->number
                     (python-get-member key 'start))]
             [stop (py-number%->number
                    (python-get-member key 'stop))]
             [slice-len (- stop start)]
             [value-len (py-number%->number
                         (python-method-call value '__len__))])
         (error "I refuse to assign to slices right now, try again later"))]
      [else (error "Invalid key for __setitem__")])
    ;(printf "SIMPLE-SET-ITEM finished, sequence is now: ~a~n" (py-object%->string this))
    )

  (define (simple-get-item this key list-to-sequence sequence-to-list)
    (cond
      [(py-is-a? key py-number%)
       (list-ref (sequence-to-list this)
                 (py-number%->number key))]
      [(py-is-a? key py-slice%)
       (let* ([len (py-number%->number
                    (python-method-call this '__len__))]
              [start (py-number%->number
                      (python-get-member key 'start))]
              [stop (min len
                         (py-number%->number
                          (python-get-member key 'stop)))]
              [step (let ([s (python-get-member key 'step)])
                      (if (py-is? s py-none)
                          1
                          (py-number%->number s)))])
         (list-to-sequence
          (map (lambda (i)
                 (python-method-call this '__getitem__
                                     (list (number->py-number% i))))
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
                                                                  (->scheme key))))))


  (python-add-members py-module%
                      `((__getattribute__ ,(py-lambda '__getattribute__ (this key)
                                           (parameterize ([current-namespace
                                                           (py-module%->namespace this)])
                                             (let ([key (py-string%->symbol key)])
                                               (namespace-variable-value key
                                                                         #t
                                                                         (lambda ()
                                                                           (python-get-member this
                                                                                              key)))))))
                        (__setattr__ ,(py-lambda '__setattr__ (this key value)
                                           (parameterize ([current-namespace
                                                           (py-module%->namespace this)])
                                             (namespace-set-variable-value! (py-string%->symbol key)
                                                                            value))))))

  ; namespace->py-module% namespace [string] [string] -> py-module%
  ; turn a scheme namespace into a python module
  (define namespace->py-module%
    (opt-lambda (namespace [name ""] [path ""])
      (let ([module (python-create-object py-module%)])
        (python-set-member! module '__name__ (string->py-string% name))
        (python-set-member! module '__file__ (string->py-string% path))
        (python-set-member! module scheme-namespace-key namespace)
        module)))

  (define (py-module%->namespace module)
    (python-get-member module scheme-namespace-key #f))



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
                        (if (list? defs)
                            (for-each (lambda (wrapped-key&value)
                                        (let ([key&value (wrapped-key&value this)])
                                          (python-set-member! this
                                                              (first key&value)
                                                              (second key&value))))
                                      defs)
                            (hash-table-for-each (py-dict%->hash-table defs)
                                                 (lambda (key val)
                                                   (python-set-member! this
                                                                       (cond
                                                                         [(symbol? key) key]
                                                                         [(string? key) (string->symbol key)]
                                                                         [(py-is-a? key py-string%) (string->symbol (py-string%->string key))]
                                                                         [(python-node? key)
                                                                          (error (format "py-type%::__init__: Sorry, this value is not yet supported as a valid dictionary key: ~a" (py-object%->string key)))]
                                                                         [else (error "py-type%::__init__: invalid key: ~a" key)])
                                                                       val))))))
  (python-set-member! py-type% '__call__
                      (case-lambda
                        [(this obj) (python-node-type obj)]
                        [(this name base-types member-dict)
                         (python-create-type name base-types member-dict)]))



  (define py-true (py-create py-int% 1))
  (define py-false (py-create py-int% 0))


  ;;; convert python objects to scheme objects through dynamic dispatch
  (define python-to-scheme-method (gensym 'python-to-scheme-method))
  (for-each (lambda (t&fn)
              (python-set-member! (car t&fn) python-to-scheme-method
                                  (py-lambda python-to-scheme-method (this)
                                             ((cadr t&fn) this))))
            `((,py-number% ,py-number%->number)
              (,py-string% ,py-string%->string)
              (,py-tuple% ,py-tuple%->list)
              (,py-list% ,(lambda (pl)
                            (map ->scheme (py-list%->list pl))))
              (,py-dict% ,py-dict%->hash-table)
              (,py-function% ,py-function%->procedure)
              (,py-module% ,py-module%->namespace)
              (,py-none% ,void)))

  (define (->scheme py-obj)
    (py-call (python-get-member py-obj python-to-scheme-method)
             null))

  (define (->python sxp)
    (cond
      [(number? sxp) (number->py-number% sxp)]
      [(string? sxp) (string->py-string% sxp)]
      [(list? sxp) (list->py-list% (map ->python sxp))]
      [(hash-table? sxp) (hash-table->py-dict% sxp)]
      [(boolean? sxp) (bool->py-number% sxp)]
      [(procedure? sxp) (procedure->py-function% sxp)]
      [(void? sxp) py-none]
      [else (error (format "Don't know how to make this a python object: ~a"
                           sxp))]))

  (define-syntax (define-pfn stx)
    (syntax-case stx ()
      [(_ pfn (arg ...) sfn) (let ([args (syntax->list (syntax (arg ...)))])
                               #`(define pfn
                                 (py-lambda '#,(syntax pfn) (#,@args)
                                   (->python (apply sfn
                                                    (map ->scheme
                                                         (list #,@args)))))))]))


  (define (python-add-extension-method method-name)
    (printf "python-add-extension-method: adding ~a~n" method-name)
    (namespace-set-variable-value! (string->symbol method-name)
                                   (lambda arg-list
                                     (printf "now applying ~a with ~a~n" method-name arg-list)
                                     (apply (eval 'scheme-python-dispatch-method)
                                            (cons method-name
                                                  arg-list))))
    (printf "python-add-extension-method: added ~a~n" method-name))

  ;; py-ext-init-module: symbol -> py-module%
  ;; create a new python module, put it in the current namespace, and return it
  (define (py-ext-init-module name)
    (let ([mod (namespace->py-module% (make-python-namespace) (symbol->string name))])
      (namespace-set-variable-value! name
                                     mod)
      mod))

  ;; py-ext-module-add-object: py-module% symbol py-object% -> void
  (define (py-ext-module-add-object pymod name obj)
;    (write "Adding an extension object...") (newline)
    #cs(parameterize ([current-namespace (py-module%->namespace pymod)])
      (namespace-set-variable-value! #csname obj))
    (if (py-type? obj)
        (begin 
 ;         (printf "obj is a type object.~n")
          (python-set-member! obj 'spy-ext-type #t)
          ((eval 'init-spy-ext-method-table) pymod name obj))
        (printf "obj is not a type object~n")))

  ;; python-wrap-ext-function: cptr symbol -> py-function%
  (define (python-wrap-ext-function fn-ptr name)
    (procedure->py-function%
     (lambda args
       (apply (eval 'spy-ext-call-fn)
              (cons fn-ptr args)))
     name))

  ;; python-wrap-ext-method: cptr symbol -> py-function%
  (define (python-wrap-ext-method-varargs fn-ptr name)
;    (printf "PYTHON WRAP EXT METHOD: ~a~n" name)
    (procedure->py-function% (lambda (self . args)
                               (apply (eval 'spy-ext-call-bound-varargs)
                                      (cons fn-ptr (cons self args))))
                             name))

  ;; python-wrap-ext-method: cptr symbol -> py-function%
  (define (python-wrap-ext-method-noargs fn-ptr name)
 ;   (printf "PYTHON WRAP EXT METHOD: ~a~n" name)
    (procedure->py-function% (lambda (self)
                               (apply (eval 'spy-ext-call-bound-noargs)
                                      (cons fn-ptr (cons self null))))
                             name))
  
  ;; TODO: add support for keyword args (applying kw-args funcs as varargs fns now)
  ;; python-wrap-ext-method: cptr symbol -> py-function%
  (define (python-wrap-ext-method-kwargs fn-ptr name)
  ;  (printf "PYTHON WRAP EXT METHOD: ~a~n" name)
    (procedure->py-function% (lambda (self . args)
                               (apply (eval 'spy-ext-call-bound-kwargs)
                                      (cons fn-ptr
                                            (cons self
                                                  (cons (assoc-list->py-dict% '())
                                                        args)))))
                             name))
  
     
     
  ;; hash-table -> hash-table
  ;; duplicate a hash table
  (define (copy-hash-table ht)
    (let ([new-ht (make-hash-table)])
      (hash-table-for-each ht
                           (lambda (key val)
                             (hash-table-put! new-ht key val)))
      new-ht))

  
  )
