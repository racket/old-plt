(module env mzscheme
  (require (lib "contract.ss")
           "util.ss"
           "dbg.ss")

  (provide (all-defined))

  (define create-env (lambda () '()))
  (define create-tenv (lambda () '()))
  
  (define (env-of? domain range)
    (list-immutable/c (cons-immutable/c (list-immutable/c domain)
                                          (vector-immutable/c range))))
  (define tenv? (listof (cons/p (listof symbol?) (vectorof any?))))
  
  (dbg-define/contract extend-tenv
    (tenv? (listof symbol?) (listof any?) . ->d .
           (lambda (env vars binders)
             (unless (= (length vars) (length binders))
               (error 'extend-tenv "Must have one handle for each var~n~a~n~a" vars binders))
             tenv?))
    (lambda (env vars binders)
      (cons (cons vars (list->vector binders)) env)))

  (dbg-define/contract extend-env
    ((env-of? symbol? any?) (list-immutable/c symbol?) (list-immutable/c any?) . ->d .
           (lambda (env vars binders)
             (unless (= (length vars) (length binders))
               (error 'extend-tenv "Must have one handle for each var~n~a~n~a" vars binders))
             tenv?))
    (lambda (env vars binders)
      (cons (cons vars (list->immutable-vector binders)) env)))
  
  (dbg-define/contract generic-lookup-symbol
    ((any? . -> . any?) . -> . (tenv? any? . -> . any?))
    (lambda (not-found-function)
      (lambda (tenv var)
        (let loop-env ([env tenv])
          (if (null? env)
              (not-found-function var)
              (let* ([rib (car env)]
                     [syms (car rib)]
                     [types (cdr rib)])
                (let loop-rib ([syms syms] [i 0])
                  (cond
                    [(null? syms) (loop-env (cdr env))]
                    [(equal? (car syms) var) (vector-ref types i)]
                    [else
                     (loop-rib (cdr syms) (+ i 1))]))))))))
  
  (dbg-define/contract lookup-symbol (tenv? symbol? . -> . any?)
    (generic-lookup-symbol
     (lambda (var)
       (error 'get-state "Unknown type variable in environment: ~a " var))))
  
  (dbg-define/contract maybe-lookup-symbol (tenv? symbol? . -> . (union false? any?))
    (generic-lookup-symbol (lambda (_) #f)))
  )