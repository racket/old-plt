;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; environment.ss
;; Richard Cobbe
;; $Id: environment.ss,v 1.1 2004/07/27 22:41:36 cobbe Exp $
;;
;; Functions that define a standard environment data type.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module environment mzscheme

  ;; env?           :: Any -> Bool
  ;; make-empty-env :: () -> (Env X)
  ;; lookup         :: (Env X) Symbol (() -> Y) -> (union X Y)
  ;; extend-env     :: (Env X) (listof Symbol) (listof X) -> (Env X)
  ;; env->alist     :: (Env X) -> (Listof (List Symbol X))

  (require (lib "contract.ss"))

  (define-struct env ())
  (define-struct (empty-env env) ())
  (define-struct (rib-env env) (env ids bindings))
  ;; (Env X) ::= (make-empty-env)
  ;;           | (make-rib-env (Env X) (listof Symbol) (listof X))

  (define extend-env
    (lambda (env syms bindings)
      (unless (= (length syms) (length bindings))
        (raise (make-exn:fail:contract
                "extend-env: IDs and bindings must have same length"
                (current-continuation-marks))))
      (make-rib-env env syms bindings)))

  (define (lookup env id fk)
    (let outer-loop ([env env])
      (if (empty-env? env)
          (fk)
          (let inner-loop ([ids (rib-env-ids env)]
                           [bindings (rib-env-bindings env)])
            (cond
              [(null? ids) (outer-loop (rib-env-env env))]
              [(eq? (car ids) id) (car bindings)]
              [else (inner-loop (cdr ids) (cdr bindings))])))))

  (define env->alist
    (lambda (env)
      (cond
       [(empty-env? env) null]
       [else (append (map list (rib-env-ids env) (rib-env-bindings env))
                     (env->alist (rib-env-env env)))])))

  (define-syntax env-macro
    (lambda (stx)
      (syntax-case stx ()
        [(_) #'(make-empty-env)]
        [(_ (key val) ...)
         (andmap identifier? (syntax-e #'(key ...)))
         #'(extend-env (make-empty-env)
                       (list (quote key) ...)
                       (list val ...))])))

  (provide (rename env-macro env))

  (provide/contract (env? (-> any/c boolean?))
                    (make-empty-env (-> env?))
                    (lookup (-> env? symbol? (-> any) any))
                    (extend-env (-> env? (listof symbol?) list? env?))
                    (env->alist (-> env? (listof (cons/c symbol? any/c))))))
