(require (lib "list.ss")
         (lib "match.ss")
         ;"test-harness.ss"
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



(define-struct (exn:persistent-eval:application exn) ())

(define id (lambda (x) x))

;; serializable?: x -> boolean
(define (serializable? x)
  (or (serializable-closure/cps? x)
      (eq? x id)
      (string? x)
      (number? x)
      (null? x)
      (and (list? x)
           (serializable? (car x))
           (serializable? (cdr x)))))

(define (persistent-eval expr)
  (persistent-eval/cps expr empty-env id))

;; persistent-eval/cps: expression (symbol -> serializable) (serializable -> alpha)
;;                         -> alpha
(define (persistent-eval/cps expr env k)
  (match expr
    [(? serializable? x) (k x)]
    [('lambda (formals ...) body)
     (k (make-serializable-closure/cps formals body env))]
    [(rator rands ...)
     (persistent-eval/cps*
      (cons rator rands) env
      (make-rands-k k))]
    [(? symbol? x) (k (env x))]))


;; persistent-eval/cps*: (listof expression) (symbol -> serializable)
;;                       (serializable -> alpha) -> alpha
(define (persistent-eval/cps* rator+rands env k)
  (cond
    [(null? rator+rands) (k '())]
    [else
     (persistent-eval/cps
      (car rator+rands) env
      (make-firstarg-k k rator+rands env)
      )]))

;; ***************************
;; closures and continuations:

;; a closure is a structure
;; (make-closure (listof symbol) expression environment)
(define-struct value ())
(define-struct (closure value) (formals body env))
(define-struct (rands-k-base value) (k))
(define-struct (firstarg-k-base value) (k rator+rands env))
(define-struct (restargs-k-base value) (k first))

;; make-rands-k: (serializable -> alpha) -> (listof expression) -> alpha
(define-procedural-struct (rands-k rands-k-base)
  (lambda (the-rands-k rator+rands)
    (let ([rator (car rator+rands)]
          [rands (cdr rator+rands)])
      (if (not (closure? rator))
          (raise (make-exn:persistent-eval:application
                  (format "rator not a serializable-closure: ~a" rator)
                  (current-continuation-marks)))
          (apply rator (cons (rands-k-base-k the-rands-k) rands))))))

;; make-firstarg-k: (listof expression) environment (serializable -> alpha) -> alpha
(define-procedural-struct (firstarg-k firstarg-k-base)
  (lambda (the-firstarg-k first)
    (persistent-eval/cps*
     (cdr (firstarg-k-base-rator+rands the-firstarg-k))
     (firstarg-k-base-env the-firstarg-k)
     (make-restargs-k (firstarg-k-base-k the-firstarg-k) first))))

;; make-restargs-k: expression (serializable -> alpha) -> (listof expression) -> alpha
(define-procedural-struct (restargs-k restargs-k-base)
  (lambda (the-restargs-k rest)
    ((restargs-k-base-k the-restargs-k)
     (cons (restargs-k-base-first the-restargs-k) rest))))

;; make-closure-proc: (listof symbol) expression environment ->
;;                    (serializable -> alpha) (listof expression) -> alpha
(define-procedural-struct (serializable-closure/cps closure)
  (lambda (the-closure k . the-args)
    (persistent-eval/cps (closure-body the-closure)
                         (extend-env* (closure-formals the-closure)
                                      the-args
                                      (closure-env the-closure))
                         k)))
  
;; ************************************************************
;; ************************************************************
;; some tests

(eq? id (persistent-eval id))
(= 4 (persistent-eval 4))
(serializable-closure/cps? (persistent-eval '(lambda (x) x)))
(= 7 (persistent-eval '((lambda (x) x) 7)))
(serializable-closure/cps? (persistent-eval '((lambda (x) x) (lambda (x) x))))





