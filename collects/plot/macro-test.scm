(require
 (lib "etc.ss"))
   


(define-syntax (r-lambda stx)
  (define (join-identifier prefix ident)
    (datum->syntax-object 
     ident 
     (string->symbol (string-append (symbol->string prefix )(symbol->string (syntax-e ident)))) ))
  (syntax-case stx ()
    [(_ data view ((var default) ...) body)
     #'(r-lambda-internal data view ((var default) ...) () body)]
    [(_ data view (field ...) ((var default) ...) body)
     (let ((accessors (map (lambda (f) (join-identifier 'get- f)) (syntax-e #'(field ...)))))
       (with-syntax (((getter ...) accessors))
         #'(r-lambda-internal data view ((var default) ...) ((field getter) ...) body)))]))


(define-syntax r-lambda-internal
    (syntax-rules ()
      [(_ data view ((var default) ...) ((value accessor) ...) body)
       (opt-lambda [data (args null)]
         (let ((var (cond [(assq 'var args) => cadr]
                          [else default]))
               ...)
           (lambda (view)
             (let ((value (send view accessor)) ...)
               body))))]))

(expand-once 
   #'(r-lambda-internal 
      (lambda (x) x) 
      view 
      ((x 10) (y 20)) 
      ((x-min get-x-min) (y-min get-y-min))
      (lambda (y) (add 1 y))))

(expand-once (expand-once
 #'(r-lambda data view (x-min x-max y-min y-max) ((x 10))
     (+ x-min x-max))))