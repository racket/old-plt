(module closure mzscheme
  (require (lib "contract.ss"))
  (provide p-lambda
           closure-proc
           closure-env
           closure?)  
  
  ;; macro is due to Felix Clock
  (define-syntax define-procedural-struct
    (lambda (stx)
      (syntax-case stx ()
        [(define-procedural-struct (PROC-STRUCT MAIN-STRUCT) PROC)
         (let* ((++ (lambda sym (string->symbol (apply string-append (map symbol->string sym)))))
                (so (lambda (sym) (datum->syntax-object #'define-procedural-struct sym)))
                (maker (so (++ 'make- (syntax-object->datum #'PROC-STRUCT))))
                (pred  (so (++ (syntax-object->datum #'PROC-STRUCT) '?)))
                (super (so (++ 'struct: (syntax-object->datum #'MAIN-STRUCT)))))
           #`(define-values (#,maker #,pred)
               (let ()
                 (define-values (struct:anon make-anon anon? anon-ref anon-set!)
                   (make-struct-type (quote PROC-STRUCT) #,super 1 0 #f null #f
                                     (lambda args
                                       (apply (anon-ref (car args) 0) args))))
                 (values (lambda l
                           (apply make-anon (append l (list PROC))))
                         anon?))))])))
  
  ;; ****************************************
  ;; a closure is a structure
  ;; (make-closure procedure (-> (listof value))
  ;; Note: change env to a thunk that produces the values
  ;;       this helps implement letrec
  (define-struct closure-base (proc env) (make-inspector))
  (define closure-proc closure-base-proc)
  (define closure-env closure-base-env)
  
  (define-procedural-struct (closure closure-base)
    (lambda (the-closure . rest-args)
      (apply (closure-base-proc the-closure)
             (append ((closure-base-env the-closure)) rest-args))))
  
  (define (p-lambda proc env)
    (make-closure proc env))
   
;  (provide/contract
;   [p-lambda ((procedure?) list? . ->* . (closure?))]
;   [closure-proc (closure? . -> . procedure?)]
;   [closure-env (closure? . -> . list?)]
;   [closure? (any? . -> . boolean?)])
  )