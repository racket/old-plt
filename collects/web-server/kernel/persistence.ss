(module persistence mzscheme
  (require (lib "list.ss")
           (lib "match.ss")
           )

  (provide servlet-entry?
           servlet-entry->symbol
           provide-servlet-entry

           apply-callback
           (struct callback-application (entry args))
           )

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

  ;; **************************************************
  ;; servlet-entry stuff:

  ;; servlet-entry
  ;; an entry is a structure
  ;; (make-entry procedure)
  (define-struct entry (proc proc-name))
  (define-procedural-struct (servlet-entry entry)
    (lambda (the-entry . args)
      (apply (entry-proc the-entry) args)))

  (define servlet-entry->symbol entry-proc-name)

  ;; provide-servlet-entry
  (define-syntax provide-servlet-entry
    (lambda (stx)
      (syntax-case stx ()
        [(_ (servlet-entry-name args ...) body-exprs ...)
         (syntax (begin (provide servlet-entry-name)
                        (define servlet-entry-name
                          (make-servlet-entry
                           (lambda (args ...) body-exprs ...)
                           'servlet-entry-name))))]
        [(_ (servlet-entry-name args ... . rest-arg) body-exprs ...)
         (syntax (begin (provide servlet-entry-name)
                        (define servlet-entry-name
                          (make-servlet-entry
                           (lambda (args ... . rest-arg) body-exprs ...)
                           'servlet-entry-name))))])))

  ;; **************************************************
  ;; **************************************************
  ;; apply-callback stuff

  ;; a callback-application is a structure
  ;; (make-callback-application servlet-entry (listof value))
  (define-struct callback-application (entry args))

  ;; apply-callback: servlet-entry value ... -> callback-application
  (define (apply-callback entry . args)
    (make-callback-application entry args))


  )
