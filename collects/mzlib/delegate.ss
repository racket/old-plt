(module delegate mzscheme
  (require (lib "class.ss"))

  (provide delegating<%>
           delegate)
  
  (define delegating<%> 
    (interface ()
      set-delegate
      get-delegate))
  
  (define-syntax (delegate stx)
    
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
        [(id ...) (syntax [(id ...) (void)])]
        [id
         (identifier? (syntax id))
         (syntax [name (void)])]))
    
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
                               (when delegate
                                 (send delegate name id ...))
                               (super-name id ...)])]
            [id
             (identifier? (syntax id))
             (syntax [name 
                      (when delegate
                        (send delegate name . id))
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
          (let ([delegate<%>
                 (interface ()
                   ids ...)])
            (values
             (lambda (super%)
               (class* super% (delegating<%>)
                 (field [delegate #f])
                 (define/public (set-delegate d)
                   (when delegate
                     (send delegate on-disable))
                   (when d
                     (let ([methods-to-impl '(on-enable on-disable ids ...)]
                           [i (object-interface d)])
                       (for-each (lambda (x) 
                                   (unless (method-in-interface? x i)
                                     (error 'set-delegate "expected object to implement an ~s method" x)))
                                 methods-to-impl))
                     (set! delegate d)
                     (send delegate on-enable)))
                 (define/public (get-delegate) delegate)
                 
                 overriding-methods ...
                 
                 (super-new)))
             
             (class* object% (delegate<%>)
               (define/public (on-enable) (void))
               (define/public (on-disable) (void))
               empty-methods ...
               (super-new))
             delegate<%>))))])))
