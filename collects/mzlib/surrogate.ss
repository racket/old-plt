(module surrogate mzscheme
  (require (lib "class.ss"))

  (provide host<%>
           surrogate)
  
  (define host<%> 
    (interface ()
      set-surrogate
      get-surrogate))
  
  (define-syntax (surrogate stx)
    
    (define (make-empty-method method-spec)
      (syntax-case method-spec ()
        [(name argspec ...)
         (identifier? (syntax name))
         (with-syntax ([(cases ...) (map make-empty-lambda-case
                                         (syntax->list (syntax (argspec ...))))])
           (syntax
            (begin
              (define/public name
                (case-lambda cases ...)))))]))
    
    (define (make-empty-lambda-case spec)
      (syntax-case spec ()
        [(id ...) (syntax [(ths id ...) (void)])]
        [id
         (identifier? (syntax id))
         (syntax [(ths . name) (void)])]))
    
    (define (make-overriding-method method-spec)
      (syntax-case method-spec ()
        [(name argspec ...)
         (identifier? (syntax name))
         (let ([super-name
                (datum->syntax-object 
                 (syntax name)
                 (string->symbol
                  (string-append
                   "super-"
                   (symbol->string
                    (syntax-object->datum
                     (syntax name))))))])
           (with-syntax ([(cases ...) (map (make-lambda-case (syntax name) super-name)
                                           (syntax->list (syntax (argspec ...))))]
                         [super-name super-name])
             (syntax
              (begin
                (rename [super-name name])
                (define/override name
                  (case-lambda cases ...))))))]))
    
    (define (extract-id method-spec)
      (syntax-case method-spec ()
        [(name argspec ...) 
         (syntax name)]))
    
    (define (make-lambda-case name super-name)
      (with-syntax ([super-name super-name]
                    [name name])
        (lambda (spec)
          (syntax-case spec ()
            [(id ...) (syntax [(id ...) 
                               (when surrogate
                                 (send surrogate name this id ...))
                               (super-name id ...)])]
            [id
             (identifier? (syntax id))
             (syntax [name 
                      (when surrogate
                        (send surrogate name this . id))
                      (super-name . id)])]))))
    
    (syntax-case stx ()
      [(_ method-spec ...)
       (with-syntax ([(ids ...) (map extract-id (syntax->list (syntax (method-spec ...))))]
                     [(overriding-methods ...)
                      (map make-overriding-method 
                           (syntax->list
                            (syntax (method-spec ...))))]
                     [(empty-methods ...)
                      (map make-empty-method
                           (syntax->list
                            (syntax (method-spec ...))))])
         (syntax
          (let ([surrogate<%>
                 (interface ()
                   on-disable-surrogate
                   on-enable-surrogate
                   ids ...)])
            (values
             (lambda (super%)
               (class* super% (delegating<%>)
                 (field [surrogate #f])
                 (define/public (set-surrogate d)
                   (when surrogate
                     (send surrogate on-disable-surrogate this))
                   (when d
                     (unless (object? d)
                       (error 'set-surrogate "expected an object, got: ~e" d))
                     (let ([methods-to-impl '(on-enable on-disable ids ...)]
                           [i (object-interface d)])
                       (for-each (lambda (x) 
                                   (unless (method-in-interface? x i)
                                     (error 'set-surrogate "expected object to implement an ~s method" x)))
                                 methods-to-impl))
                     (set! surrogate d)
                     (send surrogate on-enable-surrogate this)))
                 (define/public (get-surrogate) surrogate)
                 
                 overriding-methods ...
                 
                 (super-new)))
             
             (class* object% (surrogate<%>)
               (define/public (on-enable-surrogate) (void))
               (define/public (on-disable-surrogate) (void))
               empty-methods ...
               (super-new))
             surrogate<%>))))])))
