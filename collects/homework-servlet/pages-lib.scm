(module pages-lib mzscheme
  (require (lib "unitsig.ss" "mzlib")
           (lib "servlet-sig.ss" "web-server")
           (lib "servlet-helpers.ss" "web-server")
           (lib "url.ss" "net"))

  (provide make-binding-selector
           make-binding-predicate
           add-binding-to-url-string
           send/suspend/struct
           send/forward/struct)
    
  (define-syntax (send/suspend/struct/help stx)
    (syntax-case stx ()
      [(_ where () body) 
       (with-syntax ([send/suspend (datum->syntax-object #'where 'send/suspend)])
         #'(request-bindings (send/suspend body)))]
      [(_  where (selector-name selector-names ...) body)
       (with-syntax ([struct-selector-as-id
                      (datum->syntax-object #'where (syntax-object->datum #'selector-name))])
         #'(let ([struct-selector-as-id (symbol->string 'selector-name)])
             (send/suspend/struct/help where (selector-names ...) body)))]))
  
  (define-syntax (send/suspend/struct stx)
    (syntax-case stx ()
      [(_ s-name () body) #`(send/suspend/struct/help #,stx () body)]
      [(_ s-name (field fields ...) body)
       (with-syntax ([(selector-names ...)
                      (datum->syntax-object #'body (map (lambda (a-field)
                                                          (string->symbol
                                                           (string-append
                                                            (symbol->string (syntax-object->datum #'s-name)) "-"
                                                            (symbol->string (if (list? a-field)
                                                                                (car a-field)
                                                                                a-field)))))
                                                        (syntax-object->datum #'(field fields ...))))]
                     [constructor
                      (datum->syntax-object #'body (string->symbol
                                                    (string-append "make-"
                                                                   (symbol->string (syntax-object->datum #'s-name)))))])
         (with-syntax ([(selector-exprs ...)
                        (datum->syntax-object
                         #'body
                         (map (lambda (a-field a-name)
                                (if (list? a-field)
                                    (with-syntax ([(a-name _ proc) (datum->syntax-object #'body (cons a-name a-field))])
                                      #'(lambda (bindings)
                                          (proc (extract-bindings 'a-name bindings))))
                                    (with-syntax ([a-name (datum->syntax-object #'body a-name)])
                                      #'(lambda (bindings)
                                          (extract-binding/single 'a-name bindings)))))
                              (syntax-object->datum #'(field fields ...))
                              (syntax-object->datum #'(selector-names ...))))])
           #`((lambda (bindings)
                (constructor (selector-exprs bindings) ...))
              (send/suspend/struct/help #,stx (selector-names ...) body))))]))
  
  
  
  
  (define-syntax (send/forward/struct/help stx)
    (syntax-case stx ()
      [(_ where () body) 
       (with-syntax ([send/forward (datum->syntax-object #'where 'send/forward)])
         #'(request-bindings (send/forward body)))]
      [(_  where (selector-name selector-names ...) body)
       (with-syntax ([struct-selector-as-id
                      (datum->syntax-object #'where (syntax-object->datum #'selector-name))])
         #'(let ([struct-selector-as-id (symbol->string 'selector-name)])
             (send/forward/struct/help where (selector-names ...) body)))]))
  
  (define-syntax (send/forward/struct stx)
    (syntax-case stx ()
      [(_ s-name () body) #`(send/forward/struct/help #,stx () body)]
      [(_ s-name (field fields ...) body)
       (with-syntax ([(selector-names ...)
                      (datum->syntax-object #'body (map (lambda (a-field)
                                                          (string->symbol
                                                           (string-append
                                                            (symbol->string (syntax-object->datum #'s-name)) "-"
                                                            (symbol->string (if (list? a-field)
                                                                                (car a-field)
                                                                                a-field)))))
                                                        (syntax-object->datum #'(field fields ...))))]
                     [constructor
                      (datum->syntax-object #'body (string->symbol
                                                    (string-append "make-"
                                                                   (symbol->string (syntax-object->datum #'s-name)))))])
         (with-syntax ([(selector-exprs ...)
                        (datum->syntax-object
                         #'body
                         (map (lambda (a-field a-name)
                                (if (list? a-field)
                                    (with-syntax ([(a-name _ proc) (datum->syntax-object #'body (cons a-name a-field))])
                                      #'(lambda (bindings)
                                          (proc (extract-bindings 'a-name bindings))))
                                    (with-syntax ([a-name (datum->syntax-object #'body a-name)])
                                      #'(lambda (bindings)
                                          (extract-binding/single 'a-name bindings)))))
                              (syntax-object->datum #'(field fields ...))
                              (syntax-object->datum #'(selector-names ...))))])
           #`((lambda (bindings)
                (constructor (selector-exprs bindings) ...))
              (send/forward/struct/help #,stx (selector-names ...) body))))]))
  
 
  ;; make-binding-selector: string -> (request -> string)
  (define make-binding-selector
    (lambda (binding-str)
      (lambda (req)
        (extract-binding/single (string->symbol binding-str) (request-bindings req)))))
  
  ;; make-binding-predicate: string -> (request -> boolean)
  (define make-binding-predicate
    (lambda (str)
      (lambda (req)
        (exists-binding? (string->symbol str) (request-bindings req)))))
  
  ;; add-binding-to-url-string: string string string -> string
  (define add-binding-to-url-string
    (lambda (url-str name val)
      (let ([url (string->url url-str)])
        (url->string
         (make-url (url-scheme url)
                   (url-host url)
                   (url-port url)
                   (url-path url)
                   (url-params url) ;; This is the continuation web-noise
                   (format "~a=~a" name val)
                   #f))))))