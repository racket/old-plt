(module surrogate mzscheme
  (require (lib "class.ss"))

  (provide surrogate)
  
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
        [(id ...) (syntax [(ths super-call id ...) (super-call id ...)])]
        [id
         (identifier? (syntax id))
         (syntax [(ths super-call . name) (apply super-call name)])]))
    
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
                     (syntax name))))))]
               [super-call-name
                (datum->syntax-object 
                 (syntax name)
                 (string->symbol
                  (string-append
                   "super-proc-"
                   (symbol->string
                    (syntax-object->datum
                     (syntax name))))))])
           (with-syntax ([(cases ...) 
                          (map (make-lambda-case (syntax name)
						 super-name
						 super-call-name)
                               (syntax->list (syntax (argspec ...))))]
                         [(super-proc-cases ...)
                          (map (make-super-proc-case super-name)
                               (syntax->list (syntax (argspec ...))))]
                         [super-name super-name]
                         [super-call-name super-call-name])
             (syntax
              (begin
                (rename [super-name name])
                (field [super-call-name
			(case-lambda super-proc-cases ...)])
                (define/override name
                  (case-lambda cases ...))))))]))
    
    (define (extract-id method-spec)
      (syntax-case method-spec ()
        [(name argspec ...) 
         (syntax name)]))
    
    (define (make-super-proc-case super-name)
      (lambda (spec)
        (with-syntax ([super-name super-name])
          (syntax-case spec ()
            [(id ...) (syntax [(id ...)
                               (super-name id ...)])]
            [id 
             (identifier? (syntax id))
             (syntax [id (super-name . id)])]))))
    
    (define (make-lambda-case name super-name super-call)
      (with-syntax ([super-name super-name]
                    [name name]
                    [super-call super-call])
        (lambda (spec)
          (syntax-case spec ()
            [(id ...) (syntax [(id ...)
                               (if surrogate
                                   (send surrogate name this super-call id ...)
                                   (super-call id ...))])]
            [id
             (identifier? (syntax id))
             (syntax [name 
                      (if surrogate
                          (send surrogate name this super-call . id)
                          (super-name . id))])]))))
    
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
         (syntax/loc stx
          (let ([surrogate<%>
                 (interface ()
                   on-disable-surrogate
                   on-enable-surrogate
                   ids ...)]
		[host<%> 
		 (interface ()
		   set-surrogate
		   get-surrogate
		   ids ...)])
            (values
             (lambda (super%)
               (class* super% (host<%>)
                 (field [surrogate #f])
                 (define/public (set-surrogate d)
                   (when surrogate
                     (send surrogate on-disable-surrogate this))
                   
                   ;; error checking
                   (when d
                     (unless (object? d)
                       (error 'set-surrogate "expected an object, got: ~e" d))
                     (let ([methods-to-impl '(on-enable-surrogate on-disable-surrogate ids ...)]
                           [i (object-interface d)])
                       (for-each (lambda (x) 
                                   (unless (method-in-interface? x i)
                                     (error 'set-surrogate "expected object to implement an ~s method" x)))
                                 methods-to-impl)))
                   
                   (set! surrogate d)
                   (when surrogate
                     (send surrogate on-enable-surrogate this)))
                 (define/public (get-surrogate) surrogate)
                 
                 overriding-methods ...
                 
                 (super-new)))
             host<%>

             (class* object% (surrogate<%>)
               (define/public (on-enable-surrogate x) (void))
               (define/public (on-disable-surrogate x) (void))
               empty-methods ...
               (super-new))
             surrogate<%>))))])))
