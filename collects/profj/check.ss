#cs
(module check mzscheme
  
  (require "ast.ss"
           "types.ss"
           "parameters.ss"
           (lib "etc.ss")
           (lib "match.ss")
           (lib "class.ss")
           (lib "list.ss"))
  (provide check-defs check-interactions-types)
  
  ;symbol-remove-last: symbol->symbol
  (define (symbol-remove-last s)
    (let ((str (symbol->string s)))
      (string->symbol (substring str 0 (sub1 (string-length str))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Environment functions
  
  ;Constant empty environment
  (define empty-env null)

  ;; env => (list (list type-bound) (list var-type))
  ;; var-type => (make-var-type string type boolean boolean)
  (define-struct var-type (var type local? static?))
  
  ;; add-var-to-env: string type boolean boolean env -> env
  (define add-var-to-env
    (lambda (name type local? static? oldEnv)
      (cons (make-var-type name type local? static?)
            oldEnv)))

  ;; lookup-var-in-env: string env -> (U var-type boolean)
  (define (lookup-var-in-env name env)
    (letrec ((lookup
              (lambda (env)
                (if (null? env)
                    #f
                    (if (string=? name (var-type-var (car env)))
                        (car env)
                        (lookup (cdr env)))))))
      (lookup env)))
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Generic helper functions
  
  ;; set-expr-type: expr type -> type
  (define (set-expr-type exp t)
    (set-expr-types! exp t) t)

  ;lookup-this type-records env -> class-record
  (define (lookup-this type-recs env)
    (send type-recs get-class-record 
          (var-type-type (lookup-var-in-env "this" env)) (lambda () #f)))  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Checking functions

  ;check-def: ast symbol type-records -> void
  (define (check-defs def level type-recs)
    (when (not (null? (check-list)))
      (check-list (cdr (check-list))))
    (send type-recs set-location! (def-file def))
    (check-location (def-file def))
    (let ((package-name 
           (send type-recs lookup-path 
                 (id-string (def-name def)) 
                 (lambda () 
                   (error 'check-defs 
                          "Internal error: Current def does not have a record entry")))))
      (if (interface-def? def)
          (check-interface def package-name type-recs)
          (check-class def package-name type-recs)))
    (packages (cons def (packages)))
    (when (not (null? (check-list)))
      (check-defs (car (check-list)) level type-recs)))
  
  ;check-interactions-types: ast symbol location type-records -> void
  (define (check-interactions-types prog level loc type-recs)
    (check-location loc)
    (let ((env (add-var-to-env "this" (make-ref-type "scheme-interactions" null) #t #f
                               (create-field-env (send type-recs get-interactions-fields)
                                                 empty-env)))
          (current-class (list "scheme-interactions")))
      (cond
        ((pair? prog)
         (for-each (lambda (p)
                     (check-interactions-types p level loc type-recs)) prog))
        ((var-init? prog) 
         (check-var-init (var-init-init prog) env type-recs current-class))
        ((var-decl? prog) (void))
        ((statement? prog)
         (check-statement prog null env type-recs current-class))
        ((expr? prog)
         (check-expr prog env  level type-recs current-class))
        (else
         (error 'check-interactions "Internal error: check-interactions-types got ~a" prog)))))
  
  ;check-interface: interface-def env type-records
  (define check-interface
    (lambda (iface p-name type-recs)
      (send type-recs set-location! (interface-def-file iface))
      (send type-recs set-class-reqs (interface-def-uses iface))
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

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Functions to check class members

  ;check-members: env type-records (U #f (list string)) -> (member -> void)
  (define (check-members env type-recs current-class)
    (lambda (members)
      (let* ((fields (class-record-fields (send type-recs get-class-record 
                                                (var-type-type (lookup-var-in-env "this" env)) 
                                                (lambda () (raise-error #f #f)))))
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
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  ;;Checks
  
           
  ;; field-lookup: string type type-records -> type
  (define (field-lookup fname obj-type type-recs)
    (cond
      ((ref-type? obj-type)
       (field-record-type 
        (get-field-record fname 
                          (get-record 
                           (send type-recs get-class-record obj-type 
                                                            ((get-importer type-recs) obj-type type-recs 'full))
                           type-recs)
                          (lambda () (raise-error #f #f)))))
      ((array-type? obj-type)
       (if (equal? fname "length")
           'int
           (raise-error #f #f)))
      (else (raise-error #f #f))))

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
                         (record (get-record (send type-recs get-class-record name ((get-importer type-recs) name type-recs 'full))
                                             type-recs)))
                    (if field? 
                        (get-field-record (id-string (cadr accs)) record (lambda () (raise-error #f #f)))
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
                                            (get-field-record (id-string (cadr r)) record (lambda () (raise-error #f #f)))
                                            (get-method-records (id-string (cadr r)) record))
                                        (cdr r))
                                  (assemble-path (append f (list (car r))) (cdr r))))))))
              (assemble-path (list (car accs)) (cdr accs)))))))

              
  ;; SKIP - most checking
  ;; Checks that return has correct type 
  ;;check-statement: statement type env type-records (U #f string)-> type
  (define check-statement
    (lambda (statement return env type-recs current-class)
      (cond
        ((ifS? statement)
         (check-expr (ifS-cond statement) env 'full type-recs current-class)
         (check-statement (ifS-then statement) return env type-recs current-class)
         (check-statement (ifS-else statement) return env type-recs current-class))
        ((throw? statement)
         (send type-recs add-req (make-req "Throwable" (list "java" "lang")))
         (check-expr (throw-expr statement) env 'full type-recs current-class))
        ((return? statement)
         (when (not (assignment-conversion return (check-expr (return-expr statement) env 'full type-recs current-class) type-recs))
           (display return) (display (check-expr (return-expr statement) env 'full type-recs current-class)) (raise-error #f #f)))
        ((while? statement)
         (check-expr (while-cond statement) env 'full type-recs current-class)
         (check-statement (while-loop statement) return env type-recs current-class))
        ((doS? statement)
         (check-expr (doS-cond statement) env 'full type-recs current-class)
         (check-statement (doS-loop statement) return env type-recs current-class))
        ((for? statement)
         (let ((newEnv (if (and (not (null? (for-init statement)))
                                (or (var-init? (car (for-init statement)))
                                    (var-decl? (car (for-init statement)))))
                           (check-for-vars (for-init statement) env type-recs current-class)
                           (begin (map (lambda (e) (check-expr e env 'full type-recs current-class)) (for-init statement))
                                  env))))
           (check-expr (for-cond statement) newEnv 'full type-recs current-class)
           (map (lambda (e) (check-expr e newEnv 'full type-recs current-class)) (for-incr statement))
           (check-statement (for-loop statement) return newEnv type-recs current-class)))
        ((try? statement)
         (check-statement (try-body statement) return env type-recs current-class))
        ((switch? statement)
         (check-expr (switch-expr statement) return env 'full type-recs current-class))
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
         (check-expr (synchronized-expr statement) env 'full type-recs current-class)
         (check-statement (synchronized-stmt statement) return env type-recs current-class))
        ((statement-expression? statement)
         (check-expr statement env 'full type-recs current-class)))))
  
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
              (add-var-to-env (id-string (field-name local))
                              (type-spec-to-type (field-type local) type-recs)
                              #t
                              #f
                              env)))
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
          (check-expr init env 'full type-recs current-class))))      
  
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
         (for-each (lambda (e) (check-expr e env 'full type-recs current-class)) inits)))))                   

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
            (raise-error #f #f)))))
  
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

    
  ;; check-expr: expression env symbol type-records (U string #f)-> type
  (define (check-expr exp env level type-recs current-class)
    (let ((check-sub-expr 
           (lambda (expr) (check-expr expr env level type-recs current-class))))
      (cond
        ((literal? exp) (expr-types exp))
        ((bin-op? exp)
         (set-expr-type exp 
                        (check-bin-op (bin-op-op exp)
                                      (check-sub-expr (bin-op-left exp))
                                      (check-sub-expr (bin-op-right exp))
                                      (expr-src exp)
                                      level
                                      type-recs)))
        
        ;; SKIP - set access
        ((access? exp)
         (let ((acc (access-name exp)))
           (cond
             ((field-access? acc)
              (if (field-access-object acc)
                (let* ((expr-type (check-expr (field-access-object acc) env level type-recs current-class))
                       (type (field-lookup (id-string (field-access-field acc)) expr-type type-recs))
                       (record (if (array-type? expr-type)
                                   (make-field-record "length" `() `(array) 'int)
                                   (get-field-record 
                                    (id-string (field-access-field acc))
                                    (get-record (send type-recs get-class-record expr-type ((get-importer type-recs) expr-type type-recs 'full))
                                                type-recs)
                                    (lambda () (raise-error #f #f))))))
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
                                                                    ((get-importer type-recs) name type-recs 'full))
                                                              type-recs))
                                                (lambda () (raise-error #f #f)))))
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
                             (raise-error #f #f)
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
                      (else (raise-error #f #f)))))
              (set-access-name! exp new-acc)
              (check-expr exp env level type-recs current-class))))))

      ((special-name? exp)
       (if (string=? "this" (special-name-name exp))
           (let ((type-this (lookup-var-in-env "this" env)))
             (if type-this
                 (set-expr-type exp (var-type-type type-this))
                 (raise-error #f #f)))
           (raise-error #f #f)))
      
      ((call? exp)
       (set-expr-type exp (check-call exp
                                      (call-expr exp)
                                      (call-method-name exp)
                                      (map check-sub-expr (call-args exp))
                                      check-sub-expr
                                      current-class
                                      level
                                      env
                                      type-recs
                                      #t ;constructor?
                                      #f))) ;static?
      ((class-alloc? exp)
       (set-expr-type exp
                      (check-class-alloc (class-alloc-name exp)
                                         (map check-sub-expr (class-alloc-args exp))
                                         (expr-src exp)
                                         type-recs
                                         current-class)))
      ((array-alloc? exp)
       (set-expr-type exp
                      (check-array-alloc (array-alloc-name exp)
                                         (array-alloc-size exp)
                                         (array-alloc-dim exp)
                                         (expr-src exp)
                                         check-sub-expr
                                         type-recs)))
      ((cond-expression? exp)
       (set-expr-type exp
                      (check-cond-expr (check-sub-expr (cond-expression-cond exp))
                                       (check-sub-expr (cond-expression-then exp))
                                       (check-sub-expr (cond-expression-else exp))
                                       (expr-src exp)
                                       (expr-src (cond-expression-cond exp))
                                       type-recs)))
      ((array-access? exp)
       (set-expr-type exp
                      (check-array-access (check-sub-expr (array-access-name exp))
                                          (check-sub-expr (array-access-index exp))
                                          (expr-src exp))))
      ((post-expr? exp)
       (set-expr-type exp
                      (check-pre-post-expr (check-sub-expr (pre-expr-expr exp))
                                           (post-expr-op exp)
                                           (expr-src exp))))
      ((pre-expr? exp)
       (set-expr-type exp
                      (check-pre-post-expr (check-expr (pre-expr-expr exp))
                                           (pre-expr-op exp)
                                           (expr-src exp))))      
      ((unary? exp)
       (set-expr-type exp
                      (check-unary (check-sub-expr (unary-expr exp))
                                   (unary-op exp)
                                   (expr-src exp))))
      ((cast? exp)
       (set-expr-type exp
                      (check-cast (check-sub-expr (cast-expr exp))
                                  (cast-type exp)
                                  (expr-src exp)
                                  current-class 
                                  type-recs)))
      ((instanceof? exp)
       (set-expr-type exp
                      (check-instanceof (check-sub-expr (instanceof-expr exp))
                                        (instanceof-type exp)
                                        (expr-src exp)
                                        current-class
                                        type-recs)))
      ((assignment? exp)
       (set-expr-type exp
                      (check-assignment (assignment-op exp)
                                        (check-sub-expr (assignment-left exp))
                                        (check-sub-expr (assignment-right exp))
                                        (expr-src exp)
                                        #t ;constructor?
                                        level
                                        type-recs))))))

  ;;added assignment ops so that error messages will be correct
  ;;check-bin-op: symbol type type src-loc symbol type-records -> type
  (define (check-bin-op op l r src level type-recs)
    (case op
      ((* / % *= /= %=)       ;; 15.17
       (prim-check prim-numeric-type? binary-promotion 'num l r op src))
      ((+ - += -=)      ;; 15.18
       (if (and (not (or (eq? level 'beginner) (eq? level 'intermediate)))
                (symbol=? '+ op) (or (eq? 'string l) (eq? 'string r)))
           'string
           (prim-check prim-numeric-type? binary-promotion 'num l r op src)))
      ((<< >> >>> <<= >>= >>>=)      ;; 15.19
       (prim-check prim-integral-type? 
                   (lambda (l r) (unary-promotion l)) 'int l r src))
      ((< > <= >=)      ;; 15.20
       (prim-check prim-numeric-type? (lambda (l r) 'boolean) 'num l r op src))
      ((== !=)      ;; 15.21
       (cond
         ((or (and (prim-numeric-type? l) (prim-numeric-type? r))
              (and (eq? 'boolean l) (eq? 'boolean r)))
          'boolean)
          ((and (reference-type? l) (reference-type? r))
           (let ((right-to-left (assignment-conversion l r type-recs))
                 (left-to-right (assignment-conversion r l type-recs)))
             (cond
               ((and right-to-left left-to-right) 'boolean)
               (right-to-left (raise-error (list src 'dummy l r op) bin-op-eq-left))
               (left-to-right (raise-error (list src 'dummy l r op) bin-op-eq-right))
               (else (raise-error (list src 'dummy l r op) bin-op-eq-both)))))
          (else 
           (raise-error (list src 'dummy l r op) bin-op-eq-prim))))
      ((& ^ or &= ^= or=)      ;; 15.22
       (cond
         ((and (prim-numeric-type? l) (prim-numeric-type? r)) (binary-promotion l r))
         ((and (eq? 'boolean l) (eq? 'boolean r)) 'boolean)
         (else (raise-error (list src l r) 'bin-op-bitwise))))
      ((&& oror)      ;; 15.23, 15.24
       (prim-check (lambda (b) (eq? b 'boolean)) 
                   (lambda (l r) 'boolean) 'bool l r op src))))

  ;prim-check: (type -> bool) (type type -> type) type type src -> type
  (define (prim-check ok? return expt l r op src)
    (cond
      ((and (ok? l) (ok? r)) (return l r))
      ((ok? l) (raise-error (list src expt l r op) bin-op-prim-right))
      ((ok? r) (raise-error (list src expt l r op) bin-op-prim-left))
      (else (raise-error (list src expt l r op) bin-op-prim-both))))

  ;; 5.6.1
  ;;unary-promotion: symbol -> symbol
  (define (unary-promotion t)
    (case t ((byte short char) 'int) (else t)))
  
  ;; 5.6.2
  ;; binary-promotion: symbol symbol -> symbol
  (define (binary-promotion t1 t2)
    (cond
      ((or (eq? 'double t1) (eq? 'double t2)) 'double)
      ((or (eq? 'float t1) (eq? 'float t2)) 'float)
      ((or (eq? 'long t1) (eq? 'long t2)) 'long)
      (else 'int)))

  

  ;wrong-code: (list src string)
  (define (ctor-not-ctor ct)
    (format "calls to ~a may only occur in other constructors" ct))
  ;wrong-code: (list src type level)
  (define (prim-call type level)
    (format "attempted to call method on ~a, instead of a ~a type"
            (case level 
              ((beginner) "class")
              ((intermediate) "class or interface")
              (else "class, interface, or array"))))
  
  ;; 15.12
  ;; SKIP - worrying about modifiers
  ;; SKIP - ability for super.METHOD() and this.METHOD()
  ;;SKIP - worrying about statics calling this and super, will worry about that later
  ;check-call: call expr MethodName (list type) (expr->type) (list string) env symbol type-records bool bool-> type
  (define (check-call call expr name args check-sub-expr c-class level env type-recs ctor? static?)
    (let* ((src (expr-src call))
           (methods 
            (cond 
              ((special-name? name)
               (unless ctor?
                 (raise-error (list src (special-name-name name)) ctor-not-ctor))
               (if (string=? (special-name-name name) "super")
                   (let ((parent (car (class-record-parents (lookup-this type-recs env)))))
                     (get-method-records (car parent)
                                         (send type-recs get-class-record parent (lambda () #f))))
                   (let ((this (lookup-this type-recs env)))
                     (get-method-records (car (class-record-name this)) this))))
              (else
               (cond
                 ((special-name? expr)
                  ;Not right?
                  (get-method-records (id-string name) (lookup-this type-recs env)))
                 (expr
                  (let ((call-exp 
                         (with-handlers 
                             ;Occurs if an error is raised checking expr
                             ((exn:syntax? 
                               (lambda (exn)
                                 (let ((members (car (find-static-class (append (access-name expr) (list name))
                                                                        type-recs #f))))
                                   (unless (null? members)
                                     (set-call-expr! call #f)
                                     (let ((class (method-record-class (car members))))
                                       (unless (equal? (car class) (car c-class))
                                         (send type-recs add-req (make-req (car class) (cdr class))))))
                                   members))))
                           (check-sub-expr expr))))
                    (cond
                      ;List of methods found
                      ((list? call-exp) call-exp)
                      ((array-type? call-exp)
                       (get-method-records (id-string name)
                                           (send type-recs get-class-record object-type (lambda () #f))))
                      ((reference-type? call-exp)
                       (get-method-records (id-string name)
                                                (get-record 
                                                 (send type-recs get-class-record call-exp 
                                                       ((get-importer type-recs) call-exp type-recs 'full))
                                                 type-recs)))
                      (else (raise-error (list (expr-src expr) call-exp level) prim-call)))))
                 (else 
                  (get-method-records (id-string name)
                                      (if static?
                                          (send type-recs get-class-record c-class (lambda () #f))
                                          (lookup-this type-recs env))))))))
              (method-record (resolve-overloading methods args (lambda () (raise-error #f #f)) type-recs)))
      (set-call-method-record! call method-record)
      (method-record-rtype method-record)))

  
  ;;Skip check to make sure such a constructor exists, and is non-private
  ;; 15.9
  ;;check-class-alloc: name (list type) src type-records (list string) -> type
  (define (check-class-alloc name args src type-recs current-class)
    (let* ((type (java-name->type name type-recs))
           (class-record (send type-recs get-class-record type (lambda () #f))))
      (unless (equal? (ref-type-class/iface type) (car current-class))
        (send type-recs add-req (make-req (ref-type-class/iface type) (ref-type-path type))))
      (when (memq 'abstract (class-record-modifiers class-record))
        (raise-error (list (name-src name) (ref-type-class/iface type) type) class-alloc-abstract))
      (when (class-record-class? class-record)
        (raise-error (list (name-src name) (ref-type-class/iface type) type) class-alloc-interface))
      type))
  
  ;; 15.10
  ;;check-array-alloc type-spec (list expression) int src (expr->type) type-records -> type
  (define (check-array-alloc elt-type exps dim src check-sub-exp type-recs)
    (let ((type (type-spec-to-type elt-type type-recs)))
      (for-each (lambda (e)
                  (let ((t (check-sub-exp e)))
                    (unless (prim-integral-type? t)
                      (raise-error (list (expr-src e) 'array-alloc t) array-alloc-not-int))))
                exps)
      (make-array-type type (+ (length exps) dim))))
  
  ;; 15.25
  ;check-cond-expr: type type type src src type-records -> type
  (define (check-cond-expr test then else-t src test-src type-recs)
    (unless (eq? 'boolean test)
      (raise-error (list test-src '? test) cond-not-bool))
    (cond
      ((and (eq? 'boolean then) (eq? 'boolean else-t)) 'boolean)
      ((and (prim-numeric-type? then) (prim-numeric-type? else-t))
       ;; This is not entirely correct, but close enough due to using scheme ints
       (binary-promotion then else-t))
      ((and (eq? 'null then) (reference-type? else-t)) else-t)
      ((and (eq? 'null else-t) (reference-type? then)) then)
      ((and (reference-type? then) (reference-type? else-t))
       (if (assignment-conversion then else-t type-recs) 
           then
           (if (assignment-conversion else-t then type-recs)
               else-t)))
      (else (raise-error (list src then else-t '?) cond-type-mismatch))))

  ;; 15.13
  ;check-array-access: type type src -> type
  (define (check-array-access ref-type idx-type src)
    (unless (array-type? ref-type)
      (raise-error (list src 'access ref-type) array-ac-non-array))
    (when (or (not (prim-integral-type? idx-type))
              (not (symbol=? 'int (unary-promotion idx-type))))
      (raise-error (list src idx-type 'int) array-ac-idx))
    (if (= 1 (array-type-dim ref-type))
        (array-type-type ref-type)
        (make-array-type (array-type-type ref-type)
                         (sub1 (array-type-dim ref-type)))))
  
  ;; 15.14 & 15.15
  ;;Skips checking of whether expr is variable or value, and whether that variable is final
  ;;check-pre-post-expr: type symbol src -> type
  (define (check-pre-post-expr type op src)
    (if (prim-numeric-type? type)
        type
        (raise-error (list src type op 'num) unary-error)))
  
  ;; 15.15
  ;check-unary: type symbol src -> type
  (define (check-unary expr-type op src)
    (case op
      ((+ -)
       (if (prim-numeric-type? expr-type)
           (unary-promotion expr-type)
           (raise-error (list src expr-type op 'num) unary-error)))
      ((~)
       (if (prim-integral-type? expr-type)
           (unary-promotion expr-type)
           (raise-error (list src expr-type op 'int) unary-error)))
      ((!)
       (if (eq? 'boolean expr-type)
           'boolean
           (raise-error (list src expr-type op 'bool) unary-error)))))
    
  ;; 15.16
  ;check-cast: type type-spec src (list string) type-records -> type
  (define (check-cast exp-type cast-type src current-class type-recs)
    (let ((type (type-spec-to-type cast-type type-recs)))
      (unless (equal? (car current-class) (ref-type-class/iface type))
        (send type-recs add-req (make-req (ref-type-class/iface type) (ref-type-path type))))
      (cond
        ((and (reference-type? exp-type) (reference-type? type)) type)
        ((and (not (reference-type? exp-type)) (not (reference-type? type))) type)
        ((reference-type? exp-type)
         (raise-error (list src exp-type type 'cast) cast-ref-prim))
        (else
         (raise-error (list src exp-type type 'cast) cast-prim-ref)))))

  ;; 15.20.2
  ;check-instanceof type type-spec src (list string) type-records -> type
  (define (check-instanceof exp-type inst-type src current-class type-recs)
    (let ((type (type-spec-to-type inst-type type-recs)))
      (unless (equal? (car current-class) (ref-type-class/iface type))
        (send type-recs add-req (make-req (ref-type-class/iface type) (ref-type-path type))))
      (cond 
        ((and (ref-type? exp-type) (ref-type? type) (is-subclass? exp-type type type-recs)) 'boolean)
        ((and (ref-type? exp-type) (ref-type? type))
         (raise-error (list src type exp-type 'instanceof) instance-not-subtype))
        ((ref-type? exp-type)
         (raise-error (list (type-spec-src inst-type) 'instanceof type) instance-type-not-ref))
        (else
         (raise-error (list src 'instanceof exp-type) instance-exp-not-ref)))))     
  
  ;; 15.26
  ;; SKIP - worrying about final - doing the check for compound assignment
  ;check-assignment: symbol type type src bool symbol type-records -> type
  (define (check-assignment op ltype rtype src constructor? level type-recs)
    (when (eq? level 'beginner)
      (unless constructor?
        (raise-error (list op src) illegal-beginner-assignment)))
    (case op
      ((=)
       (if (assignment-conversion ltype rtype type-recs)
           ltype
           (raise-error (list src 'dummy ltype rtype op) assignment-convert-fail)))
      ((+= *= /= %= -= <<= >>= >>>= &= ^= or=)
       (check-bin-op op ltype rtype src level type-recs)
       ltype)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Error code

  ;;Binop errors
  (define (bin-op-prim-side side select)
    (lambda (op expt left right)
      (format "~a hand side of ~a should be of type ~a, but given ~a" 
              side op expt (select left right))))
  (define bin-op-prim-right (bin-op-prim-side "Right" (lambda (l r) r)))
  (define bin-op-prim-left (bin-op-prim-side "Left" (lambda (l r) l)))
  (define (bin-op-prim-both op expt left right)
    (format "~a expects arguments of type ~a, but given ~a and ~a" op expt left right))

  (define (bin-op-eq-side side select)
    (lambda (op d left right)
      (format "~a hand side of ~a should be assignable to ~a" op (select left right))))
  (define bin-op-eq-left (bin-op-eq-side "Left" (lambda (l r) r)))
  (define bin-op-eq-right (bin-op-eq-side "Right" (lambda (l r) l)))
  (define (bin-op-eq-prim op dummy left right)
    (format "~a expects arguments to be of equivalent types, given non-equivalent ~a and ~a" op left right))
  (define (bin-op-eq-both op dummy left right)
    (format "~a expects arguments to be assignable to each other, ~a and ~a cannot" op left right))

  ;;Class Alloc errors
  (define (class-alloc-abstract type)
    (format "abstract class ~a may not be constructed" type))
  (define (class-alloc-interface type)
    (format "interface ~a may not be constructed" type))

  
  ;;Array Alloc error
  (define (array-alloc-not-int type)
    (format "allocation of an array requires an integer for the size: given ~a" type))
  
  ;;Conditional Expression errors
  (define (cond-not-bool type)
    (format "conditional expression requires the first expression be a boolean, given ~a" type))
  ;wrong-code: (list src type type)
  (define (cond-type-mismatch then else)
    (format "conditional expression requires then and else branch have equivalent types: given ~a and ~a"
            then else))
  
  ;;Array Access errors
  (define (array-ac-non-array ref-type)
    (format "array access expects an array, given ~a" ref-type))
  ;wrong-code: (list src type symbol)
  (define (array-ac-idx expt idx-type)
    (format "array access expects a ~a as index, given ~a" expt idx-type))
  
  ;;Unary error
  (define (unary-error op expt type)
    (format "~a expects a ~a, given ~a" op expt type))
  
  ;;Cast errors
  (define (cast-msg from to)
    (lambda (exp-type cast-type)
      (format "Illegal cast from ~a type, ~a, to ~a type, ~a" from to exp-type cast-type)))
  (define cast-ref-prim (cast-msg "primitive" "class or interface"))
  (define cast-prim-ref (cast-msg "class or interface" "primitive"))
  
  ;;Instanceof errors
  (define (instance-not-subtype inst-type exp-type)
    (format "instanceof requires that ~a, type of the expression, be a subtype of ~a, given type" 
            exp-type inst-type))
  (define (instance-type-not-ref inst-type)
    (format "instanceof requires that ~a, the given type, be a class or interface type"
            inst-type))
  (define (instance-exp-not-ref exp-type)
    (format "instanceof require that ~a, the expression type, be a class or interface type" exp-type)) 

  ;;Assignment errors
  (define (illegal-beginner-assignment)
    "Assignment expressions are only allowed in constructors")
  (define (assignment-convert-fail op d ltype rtype)
    (format "~a requires that the right hand type be equivalent to or a subtype of ~a: given ~a" 
            op ltype rtype))
  
  (define (raise-error wrong-code make-msg)
    (match wrong-code
      ;Covers bin-op-* assignment-convert-fail
      ;src symbol type type symbol
      [(src expected ltype rtype op) 
       (raise-syntax-error #f (make-msg op (get-expected expected)
                                        (type->ext-name ltype)
                                        (type->ext-name rtype))
                           (make-so op src))]

      ;Covers illegal-beginner-assignment
      ;symbol src
      [(op src) (raise-syntax-error #f (make-msg) (make-so op src))]

      ;Covers instance-not-subtype cast-*
      [(src (? type? type1) (? type? type2) (? symbol? op)) 
       (raise-syntax-error #f (make-msg (type->ext-name type1)
                                        (type->ext-name type2))
                              (make-so op src))]

      ;Covers instance-*-not-* array-ac-non* cond* array-alloc-not-int class-alloc-*
      [(src (? symbol? op) (? type? type)) 
       (raise-syntax-error #f (make-msg (type->ext-name type))
                           (make-so op src))]
      ;Covers unary-error
      [(src (? type? type) (? symbol? op) (? symbol? expt)) 
       (raise-syntax-error #f
                           (make-msg op (get-expected expt) (type->ext-name type))
                           (make-so op src))] 
      ;Covers array-ac-idx
      [(src (? type? type) (? symbol? expt)) 
       (raise-syntax-error #f
                           (make-msg (get-expected expt) (type->ext-name type))
                           (make-so 'index src))]
      [_ 
       (error 'type-error "This file has a type error in the statements but more likely expressions")]))
      
  ;type?: ~a -> bool
  (define (type? t)
    (or (reference-type? t)
        (array-type? t)
        (eq? t 'boolean)
        (prim-numeric-type? t)))
  
  ;type->ext-name: type -> (U symbol string)
  (define (type->ext-name t)
    (cond 
      ((ref-type? t) (ref-type-class/iface t))
      ((array-type? t) 
       (format "~a~a" (type->ext-name (array-type-type t))
                      (let ((dims ""))
                        (let loop ((d (array-type-dim t)))
                          (if (= d 0)
                              dims
                              (begin (set! dims (string-append dims "[]"))
                                     (loop (sub1 d))))))))
      (else t)))

  ;get-expected: symbol-> string
  (define (get-expected e)
    (case e
      ((bool) 'boolean)
      ((int) "int, short, byte or char")
      ((num) "double, float, long, int, short, byte or char")
      (else "dummy")))
  
  ;make-so: symbol src -> syntax-object
  (define (make-so id src)
    (datum->syntax-object #f id (build-src-list src)))
  
  (define check-location (make-parameter #f))
  
  ;build-src-list: src -> (U bool (list loc int int int int))
  (define (build-src-list src)
    (if (not src)
        src
        (if (and (= (src-line src) 0)
                 (= (src-col src) 0)
                 (= (src-pos src) 0)
                 (= (src-span src) 0))
            #f
            (list (check-location) (src-line src) (src-col src) (src-pos src) (src-span src)))))
      
  )