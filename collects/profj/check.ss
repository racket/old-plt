#cs
(module check mzscheme
  
  (require "ast.ss"
           "types.ss"
           "parameters.ss"
           (lib "etc.ss")
           (lib "class.ss")
           (lib "list.ss"))
  (provide check-defs check-interactions-types)
  
  ;symbol-remove-last: symbol->symbol
  (define (symbol-remove-last s)
    (let ((str (symbol->string s)))
      (string->symbol (substring str 0 (sub1 (string-length str))))))

  ;Constant empty environment
  (define empty-env (list null null))

  ;; env => (list (list type-bound) (list var-type))
  ;; var-type => (make-var-type string type boolean boolean)
  (define-struct var-type (var type local? static?))
  ;; type-bound => (make-type-bound symbol type)
  (define-struct type-bound (var bound) (make-inspector))
  
  ;; add-var-to-env: string type boolean boolean env -> env
  (define add-var-to-env
    (lambda (name type local? static? oldEnv)
      (list (car oldEnv)
            (cons (make-var-type name type local? static?)
                  (cadr oldEnv)))))

  ;; lookup-var-in-env: string env -> (U var-type boolean)
  (define (lookup-var-in-env name env)
    (letrec ((lookup
              (lambda (env)
                (if (null? env)
                    #f
                    (if (string=? name (var-type-var (car env)))
                        (car env)
                        (lookup (cdr env)))))))
      (lookup (cadr env))))
  
  ;; add-type-to-env: string type env -> env
  (define add-type-to-env
    (lambda (name type oldEnv)
      (list (cons (make-type-bound name type)
                  (car oldEnv))
            (cadr oldEnv))))

  ;; lookup-type-in-env: string env -> (U type-spec boolean)
  (define (lookup-type-in-env name env)
    (letrec ((lookup
              (lambda (env)
                (if (null? env)
                    #f
                    (if (string=? name (type-bound-var (car env)))
                        (type-bound-bound (car env))
                        (lookup (cdr env)))))))
      (lookup (car env))))

  
  ;; Unimplemented functions
  (define-values (...) (values null))
  
  (define (raise-error)
    (error 'type-error "This file has a type error in the statements but more likely expressions"))
  
  ;; set-expr-type: expr type -> type
  (define (set-expr-type exp t)
    (set-expr-types! exp t)
    t)
       
  ;; name->type-var: name env -> (U name type-var)
  (define (name->type-var name env)
    (if (null? (name-path name))
        (let ((look (lookup-type-in-env (string->symbol (id-string (name-id name))) env)))
          (if look
              (make-type-var (string->symbol (id-string (name-id name)))
                                 look
                                 (name-src name))
              name))
        name))
  
  ;; fixup-java-name: name env -> name
  (define (fixup-java-name name env)
    (name->type-var name env))
  
  ;Doesn't check much
  ;; check-type-spec: type-spec env type-recs-> 
  (define (check-type-spec ts env type-recs)
    (if (not (or (symbol? (type-spec-name ts))
                 (type-var? (type-spec-name ts))))
        (set-type-spec-name! ts (fixup-java-name (type-spec-name ts) env))))

  ;;Checks
  
  ;; 5.6.1
  ;;unary-promotion: symbol -> symbol
  (define (unary-promotion t)
    (case t
      ((byte short char) 'int)
      (else t)))
  
  ;; 5.6.2
  ;; binary-promotion: symbol symbol -> symbol
  (define (binary-promotion t1 t2)
    (cond
      ((or (symbol=? 'double t1) (symbol=? 'double t2)) 'double)
      ((or (symbol=? 'float t1) (symbol=? 'float t2)) 'float)
      ((or (symbol=? 'long t1) (symbol=? 'long t2)) 'long)
      (else 'int)))
       
  
  ;; check-bin-op: symbol type type src-loc -> symbol
  (define (check-bin-op op l r src)
    (case op
      
      ;; 15.17
      ((* / %)
       (if (and (prim-numeric-type? l) (prim-numeric-type? r))
           (binary-promotion l r)
           (raise-error)))
      
      ;; 15.18
      ((+ -)
       (cond
         ((and (symbol=? '+ op) (or (symbol=? 'string l) (symbol=? 'string r))) 'string)
         ((and (prim-numeric-type? l) (prim-numeric-type? r))
          (binary-promotion l r))
         (else
          (raise-error))))
      
      ;; 15.19
      ((<< >> >>>)
       (if (and (prim-integral-type? l) (prim-integral-type? r))
           (unary-promotion l)
           (raise-error)))

      ;; 15.20
      ((< > <= >=)
       (if (and (prim-numeric-type? l) (prim-numeric-type? r))
           'boolean
           (raise-error)))

      ;; 15.21
      ((== !=)
       (if (or (and (prim-numeric-type? l) (prim-numeric-type? r))
               (and (eq? 'boolean l) (eq? 'boolean r))
               (and (reference-type? l) (reference-type? r)))
           'boolean
           (raise-error)))
      
      ;; 15.22
      ((& ^ or)
       (cond
         ((and (prim-numeric-type? l) (prim-numeric-type? r))
          (binary-promotion l r))
         ((and (symbol=? 'boolean l) (symbol=? 'boolean r)) 'boolean)
         (else (raise-error))))
      
      ;; 15.23, 15.24
      ((&& oror)
       (if (and (symbol=? 'boolean l) (symbol=? 'boolean r))
           'boolean
           (raise-error)))))
           
  ;; field-lookup: string type type-records -> type
  (define (field-lookup fname obj-type type-recs)
    (cond
      ((ref-type? obj-type)
       (field-record-type 
        (get-field-record fname 
                          (get-record 
                           (send type-recs get-class-record obj-type 
                                                            ((get-importer type-recs) obj-type type-recs))
                           type-recs)
                          (lambda () (raise-error)))))
      ((array-type? obj-type)
       (if (equal? fname "length")
           'int
           (raise-error)))
      (else (raise-error))))

  ;; build-field-accesses: access (list id) -> field-access
  (define (build-field-accesses start accesses)
    (cond
      ((null? accesses) (access-name start))
      (else
       (build-field-accesses
        (make-access #f (expr-src start) 
                         (make-field-access start (car accesses) #f))
        (cdr accesses)))))
  
  ;;find-static-class: (list access) type-recs bool-> (U (list class-record (list access)) #f)
  (define find-static-class 
    (lambda (accs type-recs field?)
      (let ((path (send type-recs lookup-path (id-string (car accs)) (lambda () #f))))
        (if path
            (list (let* ((name (cons (id-string (car accs)) path))
                         (record (get-record (send type-recs get-class-record name ((get-importer type-recs) name type-recs))
                                             type-recs)))
                    (if field? 
                        (get-field-record (id-string (cadr accs)) record (lambda () (raise-error)))
                        (get-method-records (id-string (cadr accs)) record)))
                  (cdr accs))
            (letrec ((assemble-path
                      (lambda (f r)
                        (if (null? r)
                            (error 'find-static-class 
                                   "Internal error: find static class needs to look in class path for exisiting path")
                            (let* ((name (cons (id-string (car r)) f))
                                   (record (send type-recs get-class-record (cons (id-string (car r)) f)
                                                (lambda () #f))))
                              (if record
                                  (list (if field?
                                            (get-field-record (id-string (cadr r)) record (lambda () (raise-error)))
                                            (get-method-records (id-string (cadr r)) record))
                                        (cdr r))
                                  (assemble-path (append f (list (car r))) (cdr r))))))))
              (assemble-path (list (car accs)) (cdr accs)))))))
  
  ;; check-expr: expression environment type-records (U string #f)-> type
  (define (check-expr exp env type-recs current-class)
    (cond
      ((literal? exp) (expr-types exp))
      ((bin-op? exp)
       (set-expr-type exp (check-bin-op (bin-op-op exp)
                                        (check-expr (bin-op-left exp) env type-recs current-class)
                                        (check-expr (bin-op-right exp) env type-recs current-class)
                                        (expr-src exp))))

      ;; SKIP - set access
      ((access? exp)
       (let ((acc (access-name exp)))
         (cond
           ((field-access? acc)
            (if (field-access-object acc)
                (let* ((expr-type (check-expr (field-access-object acc) env type-recs current-class))
                       (type (field-lookup (id-string (field-access-field acc)) expr-type type-recs))
                       (record (if (array-type? expr-type)
                                   (make-field-record "length" `() `(array) 'int)
                                   (get-field-record 
                                    (id-string (field-access-field acc))
                                    (get-record (send type-recs get-class-record expr-type ((get-importer type-recs) expr-type type-recs))
                                                type-recs)
                                    (lambda () (raise-error))))))
                  (unless (equal? (car current-class) (car (field-record-class record)))
                    (send type-recs add-req (make-req (car (field-record-class record)) (cdr (field-record-class record)))))
                  (set-field-access-access! 
                   acc
                   (make-var-access 
                    (memq 'static (field-record-modifiers record))
                    (field-record-class record)))
                  (set-expr-type exp type))
                (let ((record (get-field-record (id-string (field-access-field acc))
                                                (let ((name (var-access-class (field-access-access acc))))
                                                  (get-record (send type-recs get-class-record name
                                                                    ((get-importer type-recs) name type-recs))
                                                              type-recs))
                                                (lambda () (raise-error)))))
                  (unless (equal? (car current-class) (car (field-record-class record)))
                    (send type-recs add-req (make-req (car (field-record-class record)) (cdr (field-record-class record)))))
                  (set-expr-type exp (field-record-type record)))))
           ((local-access? acc) 
            (set-expr-type exp (var-type-type (lookup-var-in-env 
                                                 (id-string (local-access-name acc))
                                                 env))))
           (else
            (let* ((first-acc (id-string (car acc)))
                   (first-binding (lookup-var-in-env first-acc env))
                   (new-acc
                    (cond
                      ((and (not first-binding) (> (length acc) 1))
                       (let ((static-class (find-static-class acc type-recs #t)))
                         (if (not static-class)
                             (raise-error)
                             (let ((accs (cadr static-class)))
                               (build-field-accesses 
                                (make-access #f 
                                                 (expr-src exp)
                                                 (make-field-access 
                                                  #f
                                                  (car accs)
                                                  (make-var-access #t (field-record-class (car static-class)))))
                                (cdr accs))))))
                      ((and first-binding (var-type-local? first-binding))
                       (build-field-accesses
                        (make-access #f (expr-src exp) (make-local-access (car acc)))
                        (cdr acc)))
                      (first-binding
                       (if (var-type-static? first-binding)
                           (build-field-accesses
                            (make-access #f (expr-src exp)
                                             (make-field-access #f
                                                                    (car acc)
                                                                    (make-var-access #t current-class)))
                            (cdr acc))
                           (build-field-accesses
                            (make-access #f (expr-src exp)
                                             (make-field-access (make-special-name #f (expr-src exp)
                                                                                           "this")
                                                                    (car acc)
                                                                    #f))
                            (cdr acc))))
                      (else (raise-error)))))
              (set-access-name! exp new-acc)
              (check-expr exp env type-recs current-class))))))

      ((special-name? exp)
       (if (string=? "this" (special-name-name exp))
           (let ((type-this (lookup-var-in-env "this" env)))
             (if type-this
                 (set-expr-type exp (var-type-type type-this))
                 (raise-error)))
           (raise-error)))
      
      ;; 15.12
      ;; SKIP - worrying about modifiers
      ;; SKIP - ability for super.METHOD() and this.METHOD()
      ((call? exp)
       (let* ((arg-types (map (lambda (a) (check-expr a env type-recs current-class)) (call-args exp)))
              (methods 
               (cond 
                 ((and (special-name? (call-method-name exp))
                       (string=? (special-name-name (call-method-name exp)) "super"))
                  (let* ((parent (car (class-record-parents (send type-recs get-class-record 
                                                                  (var-type-type (lookup-var-in-env "this" env)) 
                                                                  (lambda () (raise-error))))))
                         (parent-name (car parent)))
                    (get-method-records parent-name 
                                        (send type-recs get-class-record parent (lambda () (raise-error))))))
                 ((and (special-name? (call-method-name exp))
                       (string=? (special-name-name (call-method-name exp)) "this"))
                  (let ((crecord (send type-recs get-class-record 
                                       (var-type-type (lookup-var-in-env "this" env))
                                       (lambda () (raise-error)))))
                    (get-method-records (car (class-record-name crecord)) crecord)))
                 (else
                  (cond
                    ((special-name? (call-expr exp)) 
                     (get-method-records (id-string (call-method-name exp))
                                         (send type-recs get-class-record (let ((var-t (lookup-var-in-env "this" env)))
                                                                            (if var-t
                                                                                (var-type-type var-t)
                                                                                (raise-error)))
                                               (lambda () (raise-error)))))
                    ((call-expr exp)
                     (let ((call-exp (with-handlers ((void (lambda (exn)
                                                             (let ((members (car (find-static-class (append (access-name (call-expr exp))
                                                                                                            (list (call-method-name exp)))
                                                                                                    type-recs #f))))
                                                               (when (not (null? members))
                                                                 (set-call-expr! exp #f)
                                                                 (when (not (equal? (car (method-record-class (car members)))
                                                                                    (car current-class)))
                                                                   (send type-recs add-req (make-req (car (method-record-class (car members)))
                                                                                                         (cdr (method-record-class (car members)))))))
                                                               members))))
                                       (check-expr (call-expr exp) env type-recs current-class))))
                       (cond
                         ((list? call-exp) call-exp)
                         ((array-type? call-exp) 
                          (get-method-records (id-string (call-method-name exp))
                                              (send type-recs get-class-record object-type (lambda () (raise-error)))))
                         (else (get-method-records (id-string (call-method-name exp))
                                                   (get-record 
                                                    (send type-recs get-class-record call-exp ((get-importer type-recs) call-exp type-recs))
                                                    type-recs))))))
                    (else 
                     (get-method-records (id-string (call-method-name exp))
                                         (send type-recs get-class-record 
                                               (let ((var-t (lookup-var-in-env "this" env)))
                                                 (if var-t 
                                                     (var-type-type var-t)
                                                     (cons (car current-class)
                                                           (send type-recs lookup-path (car current-class) (lambda () (raise-error))))))
                                               (lambda () raise-error))))))))
              (method-record (resolve-overloading methods arg-types (lambda () (raise-error)) type-recs)))
         (set-call-method-record! exp method-record)
         (set-expr-type 
          exp
          (method-record-rtype method-record))))
                                                         
      ;; 15.9
      ;; SKIP - all checking
      ((class-alloc? exp)
       (set-class-alloc-name! exp (fixup-java-name (class-alloc-name exp) env))
       (for-each (lambda (x) (check-expr x env type-recs current-class)) (class-alloc-args exp))
       (let ((type (java-name->type (class-alloc-name exp) type-recs)))
         (unless (equal? (ref-type-class/iface type) (car current-class))
           (send type-recs add-req (make-req (ref-type-class/iface type) (ref-type-path type))))
         (set-expr-type exp type)))
      
      ;; 15.10
      ((array-alloc? exp)
       (check-type-spec (array-alloc-name exp) env type-recs)
       (let ((element-type (type-spec-to-type (array-alloc-name exp) type-recs)))
         (for-each
          (lambda (e)
            (let ((t (check-expr e env type-recs current-class)))
              (if (or (not (prim-integral-type? t))
                      (not (symbol=? (unary-promotion t) 'int)))
                  (raise-error))))
          (array-alloc-size exp))
         (make-array-type element-type (+ (length (array-alloc-size exp))
                                          (array-alloc-dim exp)))))
      
      ;; 15.25
      ((cond-expression? exp)
       (let ((test-type (check-expr (cond-expression-cond exp) env type-recs current-class))
             (then-type (check-expr (cond-expression-then exp) env type-recs current-class))
             (else-type (check-expr (cond-expression-else exp) env type-recs current-class)))
         (cond
           ((not (eq? 'boolean test-type))
            (raise-error))
           ((and (eq? 'boolean then-type) (eq? 'boolean else-type))
            (set-expr-type exp 'boolean))
           ((and (prim-numeric-type? then-type) (prim-numeric-type? else-type))
            ;; This is not entirely correct
            (set-expr-type exp (binary-promotion then-type else-type)))
           ((and (eq? 'null then-type) (reference-type? else-type))
            (set-expr-type else-type))
           ((and (eq? 'null else-type) (reference-type? then-type))
            (set-expr-type then-type))
           ((and (reference-type? then-type) (reference-type? else-type))
            (if (assignment-conversion then-type else-type type-recs) 
                (set-expr-type exp then-type)
                (if (assignment-conversion else-type then-type type-recs)
                    (set-expr-type exp else-type))))
           (else (raise-error)))))
            
      ;; 15.13
      ((array-access? exp)
       (let ((ref-type (check-expr (array-access-name exp) env type-recs current-class))
             (idx-type (check-expr (array-access-index exp) env type-recs current-class)))
         (if (not (array-type? ref-type))
             (raise-error))
         (if (or (not (prim-integral-type? idx-type))
                 (not (symbol=? 'int (unary-promotion idx-type))))
             (raise-error))
         (set-expr-type exp (if (= 1 (array-type-dim ref-type))
                                ;I think this is correct: Could be a PROBLEM
                                (array-type-type ref-type)
                                (make-array-type (array-type-type ref-type)
                                                 (sub1 (array-type-dim ref-type)))))))
       
      ;; 15.14
      ;;Skips checking of whether it's expression is a variable or a value
      ((post-expr? exp)
       (let ((type (check-expr (post-expr-expr exp) env type-recs current-class)))
         (if (prim-numeric-type? type)
             (set-expr-type exp type)
             (raise-error))))
      
      ;; 15.15
      ;;Skips checking of whether expr is variable or value
      ((pre-expr? exp)
       (let ((type (check-expr (pre-expr-expr exp) env type-recs current-class)))
         (if (prim-numeric-type? type)
             (set-expr-type exp type)
             (raise-error))))
      
      ;; 15.15
      ((unary? exp)
       (let ((t (check-expr (unary-expr exp) env type-recs current-class)))
         (case (unary-op exp)
           ((+ -)
            (if (prim-numeric-type? t)
                (set-expr-type exp (unary-promotion t))
                (raise-error)))
           ((~)
            (if (prim-integral-type? t)
                (set-expr-type exp (unary-promotion t))
                (raise-error)))
           ((!)
            (if (symbol=? 'boolean t)
                'boolean
                (raise-error))))))
      
      ;; 15.16
      ;; SKIP - doing the checking
      ((cast? exp)
       (check-expr (cast-expr exp) env type-recs current-class)
       (check-type-spec (cast-type exp) env type-recs)
       (let ((type (type-spec-to-type (cast-type exp) type-recs)))
         (unless (equal? (car current-class) (ref-type-class/iface type))
           (send type-recs add-req (make-req (ref-type-class/iface type) (ref-type-path type))))
         (set-expr-type exp type)))
              
      ;; 15.20.2
      ;; SKIP - doing the checking
      ((instanceof? exp)
       (check-expr (instanceof-expr exp) env type-recs current-class)
       (check-type-spec (instanceof-type exp) env type-recs)
       (let ((type (type-spec-to-type (instanceof-type exp) type-recs)))
         (unless (equal? (car current-class) (ref-type-class/iface type))
           (send type-recs add-req (make-req (ref-type-class/iface type) (ref-type-path type)))))
       (set-expr-type exp 'boolean))

      ;; 15.26
      ;; SKIP - worrying about final - doing the check for compound assignment
      ((assignment? exp)
       (let ((ltype (check-expr (assignment-left exp) env type-recs current-class))
             (rtype (check-expr (assignment-right exp) env type-recs current-class)))
         (case (assignment-op exp)
           ((=)
            (if (assignment-conversion ltype rtype type-recs)
                (set-expr-type exp ltype)
                (raise-error)))
           ((+= *= /= %= -= <<= >>= >>>= &= ^= or=)
            (let* ((t (check-bin-op (symbol-remove-last (assignment-op exp))
                                    ltype
                                    rtype
                                    (expr-src exp))))
              (set-expr-type exp ltype))))))))
              
  ;; SKIP - most checking
  ;; Checks that return has correct type 
  ;;check-statement: statement type env type-records (U #f string)-> type
  (define check-statement
    (lambda (statement return env type-recs current-class)
      (cond
        ((ifS? statement)
         (check-expr (ifS-cond statement) env type-recs current-class)
         (check-statement (ifS-then statement) return env type-recs current-class)
         (check-statement (ifS-else statement) return env type-recs current-class))
        ((throw? statement)
         (send type-recs add-req (make-req "Throwable" (list "java" "lang")))
         (check-expr (throw-expr statement) env type-recs current-class))
        ((return? statement)
         (when (not (assignment-conversion return (check-expr (return-expr statement) env type-recs current-class) type-recs))
           (display return) (display (check-expr (return-expr statement) env type-recs current-class)) (raise-error)))
        ((while? statement)
         (check-expr (while-cond statement) env type-recs current-class)
         (check-statement (while-loop statement) return env type-recs current-class))
        ((doS? statement)
         (check-expr (doS-cond statement) env type-recs current-class)
         (check-statement (doS-loop statement) return env type-recs current-class))
        ((for? statement)
         (let ((newEnv (if (and (not (null? (for-init statement)))
                                (or (var-init? (car (for-init statement)))
                                    (var-decl? (car (for-init statement)))))
                           (check-for-vars (for-init statement) env type-recs current-class)
                           (begin (map (lambda (e) (check-expr e env type-recs current-class)) (for-init statement))
                                  env))))
           (check-expr (for-cond statement) newEnv type-recs current-class)
           (map (lambda (e) (check-expr e newEnv type-recs current-class)) (for-incr statement))
           (check-statement (for-loop statement) return newEnv type-recs current-class)))
        ((try? statement)
         (check-statement (try-body statement) return env type-recs current-class)
         ...)
        ((switch? statement)
         (check-expr (switch-expr statement) return env type-recs current-class)
         ...)
        ((block? statement)
         (check-block (block-stmts statement)
                      (block-src statement)
                      return
                      env type-recs
                      current-class))
        ((break? statement)
         void)
        ((continue? statement)
         void)
        ((label? statement)
         (check-statement (label-stmt statement) return env type-recs current-class))
        ((synchronized? statement)
         (check-expr (synchronized-expr statement) env type-recs current-class)
         (check-statement (synchronized-stmt statement) return env type-recs current-class))
        ((statement-expression? statement)
         (check-expr statement env type-recs current-class)))))
  
  ;Performs no checks
  ;check-for-vars: (list (U var-init var-decl)) env type-records (U (list string) #f)-> env
  (define check-for-vars
    (lambda (vars env type-recs current-class)
      (if (null? vars)
          env
          (check-for-vars (cdr vars) (check-local-var (car vars) env type-recs current-class) type-recs current-class))))
  
  ;Performs no checks
  ;check-local-var: (U var-init var-decl) env type-records (U #f (list string))-> env
  (define check-local-var 
    (lambda (local env type-recs current-class)
      (let* ((is-var-init? (var-init? local))
             (newEnv 
              (begin
                (check-type-spec (field-type local) env type-recs)
                (add-var-to-env (id-string (field-name local))
                                (type-spec-to-type (field-type local) type-recs)
                                #t
                                #f
                                env))))
        (if is-var-init?
            (begin
              (check-var-init (var-init-init local) newEnv type-recs current-class)
              newEnv)
            newEnv))))
  
  ;Performs no checks
  ;check-var-init (U expression array-init) env type-records (U string #f) -> void
  (define check-var-init
    (lambda (init env type-recs current-class)
      (if (array-init? init)
          (check-array-init (array-init-vals init) env type-recs current-class)
          (check-expr init env type-recs current-class))))      
  
  ;Performs no checks
  ;check-array-init (list (U expression array-init)) env type-records (U #f string)-> void
  (define check-array-init
    (lambda (inits env type-recs current-class)
      (cond
        ((null? inits) void)
        ((array-init? (car inits))
         (for-each (lambda (a) (check-array-init (array-init-vals a) env type-recs current-class))
                   inits))
        (else
         (for-each (lambda (e) (check-expr e env type-recs current-class)) inits)))))                   

  ;check-block: (list (U statement variable)) src type env type-records (U #f (list string))-> void
  (define check-block
    (lambda (stmts src return env type-recs current-class)
      (cond 
        ((null? stmts) void)
        ((or (var-decl? (car stmts)) (var-init? (car stmts)))
         (check-block (cdr stmts) src return (check-local-var (car stmts) env type-recs current-class) type-recs current-class))
        (else
         (check-statement (car stmts) return env type-recs current-class)
         (check-block (cdr stmts) src return env type-recs current-class)))))
  
  ;build-method-env: (list var-decl) env type-records-> env
  (define build-method-env
    (lambda (parms env type-recs)
      (cond
        ((null? parms) env)
        (else
         (add-var-to-env (id-string (var-decl-name (car parms)))
                         (type-spec-to-type (var-decl-type (car parms)) type-recs)
                         #t
                         #f
                         (build-method-env (cdr parms) env type-recs))))))
  
  ;Should check that all branches have a return; as Java requires
  ;Always returns #t  
  ;reachable-return?: statement -> bool
  (define reachable-return?
    (lambda (body)
      #t))
  
  ;check-method: method env type-records (U #f (list string))-> void
  (define check-method
    (lambda (method field-env type-recs current-class)
      (let ((return (type-spec-to-type (method-type method) type-recs)))
        (if (or (eq? return 'void)
                (reachable-return? (method-body method)))
            (check-statement (method-body method)
                             return
                             (build-method-env (method-parms method) field-env type-recs)
                             type-recs
                             current-class)
            (raise-error)))))
  
  ;create-field-env: (list field) env -> env
  (define create-field-env
    (lambda (fields env)
      (cond
        ((null? fields) env)
        (else (add-var-to-env (field-record-name (car fields))
                              (field-record-type (car fields))
                              (not (memq 'static (field-record-modifiers (car fields))))
                              (memq 'static (field-record-modifiers (car fields)))
                              (create-field-env (cdr fields) env))))))
  
  ;get-static-env: env -> env
  (define get-static-env
    (lambda (env)
      (list (car env)
            (filter var-type-static? (cadr env)))))

  ;check-members: env type-records (U #f (list string)) -> (member -> void)
  (define (check-members env type-recs current-class)
    (lambda (members)
      (let* ((fields (class-record-fields (send type-recs get-class-record 
                                                (var-type-type (lookup-var-in-env "this" env)) 
                                                (lambda () (raise-error)))))
             (field-env (create-field-env fields env)))
        (for-each (lambda (member)
                    (cond
                      ((method? member)
                       (if (memq 'static (map modifier-kind (method-modifiers member)))
                           (check-method member (get-static-env field-env) type-recs current-class)
                           (check-method member field-env type-recs current-class)))
                      ((initialize? member)
                       (if (initialize-static member)
                           (check-statement (initialize-block member) null type-recs current-class)
                           (check-statement (initialize-block member) field-env type-recs current-class)))
                      ((var-init? member)
                       (check-var-init (var-init-init member) field-env type-recs current-class))
                      (else void)))
                  members))))
  
  ;check-static-members -> env type-records -> (member -> void)
  (define check-static-members
    (lambda (env type-recs)
      (lambda (members)
        void)))
  
  ;check-interface: interface-def env type-records
  (define check-interface
    (lambda (iface p-name type-recs)
      (send type-recs set-location! (interface-def-file iface))
      (send type-recs set-class-reqs (interface-def-uses iface))
      ;does no gj type paramterizations
      ((check-static-members empty-env type-recs) (interface-def-members iface))
      (set-interface-def-uses! iface (send type-recs get-class-reqs))))
  
  ;check-class: class-def (list string) type-records
  (define check-class
    (lambda (class package-name type-recs)
      (send type-recs set-location! (class-def-file class))
      (send type-recs set-class-reqs (class-def-uses class))
      (let ((this-ref (make-ref-type (id-string (header-id (class-def-info class)))
                                     package-name)))
        (let ((env empty-env))
          ((check-members (add-var-to-env "this" this-ref #t #f env) 
                          type-recs 
                          (list (id-string (header-id (class-def-info class)))))
           (class-def-members class))))
      (set-class-def-uses! class (send type-recs get-class-reqs))))

  ;check-def: ast type-records -> void
  (define (check-defs def type-recs)
    (when (not (null? (check-list)))
      (check-list (cdr (check-list))))
    (send type-recs set-location! (def-file def))
    (let ((package-name (send type-recs lookup-path 
                              (id-string (def-name def)) 
                              (lambda () (raise-error)))))
      (if (interface-def? def)
          (check-interface def package-name type-recs)
          (check-class def package-name type-recs)))
    (packages (cons def (packages)))
    (when (not (null? (check-list)))
      (check-defs (car (check-list)) type-recs)))
  
  (define (check-interactions-types prog type-recs)
    (let ((env (add-var-to-env "this" (make-ref-type "scheme-interactions" null) #t #f
                               (create-field-env (send type-recs get-interactions-fields) empty-env)))
          (current-class (list "scheme-interactions")))
      (cond
        ((var-init? prog) 
         (check-var-init (var-init-init prog) env type-recs current-class))
        ((var-decl? prog) (void))
        ((statement? prog)
         (check-statement prog null env type-recs current-class))
        ((expr? prog)
         (check-expr prog env type-recs current-class))
        (else
         (error 'check-interactions "Internal error: check-interactions-types got ~a" prog)))))
    
    )