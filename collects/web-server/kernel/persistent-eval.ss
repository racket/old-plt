(module persistent-eval mzscheme
  (require (lib "list.ss")
           (lib "match.ss")
           ;"test-harness.ss"
           )

  (provide persistent-eval
           provide-servlet-entry
           servlet-entry?
           servlet-entry->symbol
           serializable-closure?
           serializable?)

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

  ;; an environment is either
  ;; (make-empty-env)
  ;; (make-environment symbol value environment)
  (define-struct empty-environment ())
  (define-struct extended-environment (sym val enclosing))

  (define-struct (exn:persistent-eval:unbound-variable exn) ())

  ;; empty-env-proc: symbol -> bust
  (define-procedural-struct (empty-env-proc empty-environment)
    (lambda (the-env sym)
      (raise (make-exn:persistent-eval:unbound-variable
              (format "Unbound variable: ~a" sym) (current-continuation-marks)))))

  ;; env-proc: symbol -> (union serializable bust)
  (define-procedural-struct (env-proc extended-environment)
    (lambda (the-env sym)
      (cond
        [(eqv? sym (extended-environment-sym the-env))
         (extended-environment-val the-env)]
        [else
         ((extended-environment-enclosing the-env) sym)])))

  ;; extend-env: symbol serializable (symbol -> serializable) -> (symbol -> serializable)
  (define (extend-env sym val env)
    (make-env-proc sym val env))

  ;; extend-env*: (symbol -> serializable) (listof symbol) (listof serializable)
  ;;              -> (symbol -> serializable)
  (define (extend-env* syms vals env)
    (foldl extend-env env syms vals))

  ;; The empty environment
  (define empty-env (make-empty-env-proc))

  ;; ********************************************************************************
  ;; tests
  ;(define e1 (extend-env 'foo "bar" empty-env))
  ;(string=? "bar" (e1 'foo))
  ;
  ;(define e2 (extend-env* (list 'one 'two 'three) (list "one" "two" "three") e1))
  ;
  ;(string=? "bar" (e2 'foo))
  ;(string=? "one" (e2 'one))
  ;(string=? "two" (e2 'two))
  ;(string=? "three" (e2 'three))

  ;; ********************************************************************************

  ;; a closure is a structure
  ;; (make-closure (listof symbol) expression environment)
  (define-struct closure (formals body env))

  ;; make-closure-proc: (listof symbol) expression environment ->
  ;;                    (listof expression) -> serializable
  (define-procedural-struct (serializable-closure closure)
    (lambda (the-closure . the-args)
      (persistent-eval/env (closure-body the-closure)
                       (extend-env* (closure-formals the-closure)
                                    the-args
                                    (closure-env the-closure)))))
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
        [(_ (servlet-entry-name args ...) body)
         (syntax (begin (provide servlet-entry-name)
                        (define servlet-entry-name
                          (make-servlet-entry
                           (lambda (args ...) body)
                           'servlet-entry-name))))])))

  (define-struct (exn:persistent-eval:application exn) ())

  ;; serializable?: x -> boolean
  (define (serializable? x)
    (or (serializable-closure? x)
        (servlet-entry? x)
        (string? x)
        (number? x)
        (null? x)
        (and (list? x)
             (serializable? (car x))
             (serializable? (cdr x)))))

  ;; ********************************************************************************
  ;; an expression is either:
  ;; This comment is begginning to show its age, I need to write out
  ;; the semantics for this language

  ;;   <serializable?>
  ;;   x                           ; varriable reference
  ;;   (expression ...)            ; application
  ;;   (lambda (x ...) expression) ; abstraction

  ;; a value is either:
  ;;   <serializable?>
  ;;   (lambda (x ...) expression)

  (define (persistent-eval expr)
    (persistent-eval/env expr empty-env))

  ;; persistent-eval/env: expression (symbol -> serializable) -> serializable
  (define (persistent-eval/env expr env)
    (match expr
      [(? serializable? x) x]
      [('lambda (formals ...) body)
       (make-serializable-closure formals body env)]
      [(rator rands ...)
       (let ([rator (persistent-eval/env rator env)]
             [rands (map
                     (lambda (rand)
                       (persistent-eval/env rand env))
                     rands)])
         (if (or (serializable-closure? rator)
                 (servlet-entry? rator))
             (apply rator rands)
             (raise (make-exn:persistent-eval:application
                     (format "rator not a serializable-closure nor a servlet-entry: ~a" rator)
                     (current-continuation-marks)))))]
      [(? symbol? x) (env x)]
      ))


  ;; ********************************************************************************
  ;; ********************************************************************************
  ;; ********************************************************************************

  ;; persistent-eval/capture: expression (symbol -> serializable) -> serializable
  ;; Bad idea to use eval to capture into free vars (see tests)
  ;; Instead, perhaps this should be done with a macro.
;  (define (persistent-eval/capture expr env)
;    (match expr
;      [(? serializable? x) x]
;      [('lambda (formals ...) body)
;       (let-values ([(free-vars vals) (compute-closure expr env)])
;         (let ([body (if (null? free-vars)
;                         body
;                         `((lambda ,free-vars body) ,@vals))])
;           (make-serializable-closure formals body env)))]
;      [(rator rands ...)
;       (let ([rator (persistent-eval/capture rator env)]
;             [rands (map
;                     (lambda (rand)
;                       (persistent-eval/capture rand env))
;                     rands)])
;         (rator rands))]
;
;      [(? symbol? x)
;       (with-handlers ([exn:unbound-var?
;                        (lambda (the-exn)
;                          (let ([val (eval x)])
;                            (or (and (serializable? val) val)
;                                (raise the-exn))))])
;         (env x))]
;
;      ))
;
;  ;; compute-closure: expression environment -> (listof symbol) (listof serializable)
;  ;; find the free-vars in an expression and figure out some serializable values
;  ;; to bind them to.
;  (define (compute-closure expr env)
;    (match expr
;      [(? serializable? x) (values '() '())]
;      [('lambda (formals ...) body)
;       (compute-closure body (extend-env* formals (map (lambda (x) #t) formals) env))]
;      [(rator rands ...)
;       (compute-closure* (cons rator rands) env)]
;      [(? symbol? x)
;       (with-handlers ([exn:unbound-var?
;                        (lambda (the-exn)
;                          (let ([val (eval x)])
;                            (if (serializable? val)
;                                (values (list x) (list val))
;                                (raise the-exn))))])
;         (and (env x)
;              (values '() '())))]))
;
;  ;; compute-closure*: (listof expression) environment -> (listof symbol) (listof serializable)
;  ;; as compute-closure but for several
;  (define (compute-closure* exprs env)
;    (if (null? exprs)
;        (values '() '())
;        (let-values ([(first-free-vars first-vals) (compute-closure (car exprs) env)])
;          (let-values ([(rest-free-vars rest-vals) (compute-closure* (cdr exprs) env)])
;            (parallel-union first-free-vars rest-free-vars first-vals rest-vals)))))
;
;  ;; parallel-union: (listof symbol) (listof symbol) (listof serializable) (listof serializable)
;  ;;                 -> (listof symbol) (listof serializable)
;  ;; Take the union in parallel so that symbols and values still line up properly
;  (define (parallel-union first-free-vars rest-free-vars first-vals rest-vals)
;    (if (null? first-free-vars)
;        (values rest-free-vars rest-vals)
;        (let-values ([(rest-free-vars rest-vals)
;                      (parallel-union (cdr first-free-vars) rest-free-vars
;                                      (cdr first-vals) rest-vals)])
;          (let-values ([(rest-free-vars rest-vals)
;                        (sans (car first-free-vars) rest-free-vars rest-vals)])
;            (values
;             (cons (car first-free-vars) rest-free-vars)
;             (cons (car first-vals) rest-vals))))))
;
;  ;; sans: symbol (listof symbol) (listof val) -> (listof symbol) (listof val)
;  ;; return the (listof symbol) sans the symbol and the (listof val) sans the corresponding val
;  (define (sans which where-syms where-vals)
;    (let sans ([where-syms where-syms]
;               [where-vals where-vals])
;      (if (null? where-syms) (values '() '())
;          (let-values ([(rest-syms rest-vals)
;                        (sans (cdr where-syms) (cdr where-vals))])
;            (if (eqv? which (car where-syms))
;                (values rest-syms rest-vals)
;                (values
;                 (cons (car where-syms) rest-syms)
;                 (cons (car where-vals) rest-vals)))))))

  ;; ********************************************************************************
  ;; tests

;  (test "TODO: check for duplicates in formals"
;        (lambda () #f))
;
;  (define (compute-closure-test expr expected-vars expected-vals)
;    (test (format "complete-closure-test: ~s" expr)
;          (lambda ()
;            (let-values ([(vars vals) (compute-closure expr empty-env)])
;              (and (equal? vars expected-vars)
;                   (equal? vals expected-vals))))))
;
;  (compute-closure-test "foo" '() '())
;  (compute-closure-test '(lambda (x) "foo") '() '())
;  (compute-closure-test '(lambda () "foo") '() '())
;  (compute-closure-test '(lambda (x y z) "foo") '() '())
;  (compute-closure-test '(lambda (x) x) '() '())
;  (compute-closure-test '(lambda (x y) (x y)) '() '())
;
;  (define a-defined-string "a defined string")
;  (provide a-defined-string)
;  (test "trivial eval"
;        (lambda ()
;          (string=? "a defined string" (eval 'a-defined-string))))
;
;  (compute-closure-test 'a-defined-string '(a-defined-string) '("a defined string"))
;  (compute-closure-test '(lambda () a-defined-string) '(a-defined-string) '("a defined string"))
;  (compute-closure-test '(lambda (x) a-defined-string) '(a-defined-string) '("a defined string"))
;  (compute-closure-test '((lambda (x y) (x y)) a-defined-string a-defined-string)
;                        '(a-defined-string) '("a defined string"))
;
;  (test "persistent-eval/capture: id"
;        (lambda ()
;          (serializable-closure? (persistent-eval/capture '(lambda (x) x) empty-env))))
;
;  (test "persistent-eval/capture: string"
;        (lambda ()
;          (string=? "foo" (persistent-eval/capture "foo" empty-env))))
;
;  (test "persistent-eval/capture: application"
;        (lambda ()
;          (string=? "foo" (persistent-eval/capture '((lambda (x) x) "foo") empty-env))))
  )

