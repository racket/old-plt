(module plot-extend mzscheme
  (require
   (lib "class.ss")
   (lib "view.ss" "plot")
   (lib "renderer-helpers.ss" "plot")
   )
  
  
  
  (define-syntax (define-plot-type stx)
    (define (join-identifier prefix ident)
      (datum->syntax-object 
       ident 
       (string->symbol 
        (string-append (symbol->string prefix )
                       (symbol->string (syntax-e ident)))) ))
    (syntax-case stx ()
      [(_ name data view ((var default) ...) body)
       #'(r-lambda-internal-test name data view ((var default) ...) () body)]
      [(_ name data view (field ...) ((var default) ...) body)
       (let ((accessors (map (lambda (f) (join-identifier 'get- f)) (syntax-e #'(field ...)))))
         (with-syntax (((getter ...) accessors))
           #'(r-lambda-internal-test name data view ((var default) ...) ((field getter) ...) body)))]))

 #|
  (define-syntax r-lambda-internal-test    
    (syntax-rules ()
      [(_ name data view ((var default) ...) ((value accessor) ...) body)
       (define-syntax name 
         (lambda (stx-2)           
           (define (find-val sym lst)
             (cond 
               [(null? lst) #f]
               [(eq? (car (syntax-object->datum (car lst))) sym)
                (cadr (syntax-object->datum (car lst)))]
               [else
                (find-val sym (cdr lst))]))
           
           ;; there is probably a better way to do this
           (define (subst-names original overrides)             
             (map
              (lambda (default-pair)
                (let ((pair-id (car (syntax-object->datum default-pair))))
                  (cond
                    [(find-val pair-id overrides)
                     => (lambda (new-val)
                          (datum->syntax-object 
                           default-pair
                           (list pair-id new-val)))]
                    [else 
                     default-pair])))
              original))
           
           (syntax-case stx-2 ()
             [(_ val)
              #'(let ((var default) ...
                      (data val))
                  (lambda (view)
                    (let ((value (send view accessor)) ...)
                      body)))]
             [(_ val (override-name override-value) (... ...) )
              
              (let ((new-defaults (subst-names
                                   (syntax-e #'((var default) ...))
               
                                   (syntax-e #'((override-name override-value) (... ...))))))
                (with-syntax ((((def new-def-val) (... ...)) new-defaults))
                  #'(let ((def new-def-val) (... ...)
                          (data val))
                      (lambda (view)
                        (let ((value (send view accessor)) ...)
                          body)))))])))]))
|#
    (define-syntax r-lambda-internal-test    
      (syntax-rules ()
        [(_ name data view ((var default) ...) ((value accessor) ...) body)
         (define-syntax name 
           (lambda (stx-2)                    
             (syntax-case stx-2 ()
               [(_ val)
                #'(let ((var default) ...
                        (data val))
                    (lambda (view)
                      (let ((value (send view accessor)) ...)
                        body)))]
               [(_ val (override-name override-value) (... ...) )              
                (let ((new-overrides 
                       (map 
                        (lambda (pair) 
                          (datum->syntax-object 
                           #'((var default) ...)
                           (syntax-object->datum pair)))                        
                        (syntax-e #'((override-name override-value) (... ...))))))
                  (with-syntax ((((new-name new-override) (... ...)) new-overrides))
                    #'(let ((var default) ...
                            (data val))
                        (let
                            ((new-name override-value) (... ...))                          
                          (lambda (view)
                            (let ((value (send view accessor)) ...)
                              body))))))])))])) 

  
  (provide 
   define-plot-type   
   (all-from (lib "view.ss" "plot"))
   (all-from (lib "renderer-helpers.ss" "plot"))))

