#cs
(module check mzscheme
  
  (require "ast.ss"
           "types.ss"
           "parameters.ss"
           "error-messaging.ss"
           "profj-pref.ss"
           (lib "class.ss")
           (lib "list.ss"))
  (provide check-defs check-interactions-types)
  
  ;symbol-remove-last: symbol->symbol
  (define (symbol-remove-last s)
    (let ((str (symbol->string s)))
      (string->symbol (substring str 0 (sub1 (string-length str))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Environment functions

  ;(make-environment (list var-type) (list type) (list string))
  (define-struct environment (type-env exn-env label-env))
  
  ;Constant empty environment
  (define empty-env (make-environment null null null))

  ;; env => (list (list type-bound) (list var-type))
  ;; var-type => (make-var-type string type boolean boolean boolean)
  (define-struct var-type (var type local? static? field? final?))
  
  ;; add-var-to-env: string type boolean boolean boolean boolean env -> env
  (define (add-var-to-env name type local? static? field? final? oldEnv)
    (make-environment (cons (make-var-type name type local? static? field? final?)
                            (environment-type-env oldEnv))
                      (environment-exn-env oldEnv)
                      (environment-label-env oldEnv)))
  
  ;; lookup-var-in-env: string env -> (U var-type boolean)
  (define (lookup-var-in-env name env)
    (letrec ((lookup
              (lambda (env)
                (if (null? env)
                    #f
                    (if (string=? name (var-type-var (car env)))
                        (car env)
                        (lookup (cdr env)))))))
      (lookup (environment-type-env env))))
  
  ;remove-var-from-env string env -> env
  (define (remove-var-from-env name env)
    (letrec ((remove-from-env
              (lambda (env)
                (cond
                  ((null? env) null)
                  ((equal? name (var-type-var (car env)))
                   (remove-from-env (cdr env)))
                  (else (cons (car env) (remove-from-env (cdr env))))))))
      (make-environment (remove-from-env (environment-type-env env))
                        (environment-exn-env env)
                        (environment-label-env env))))
  
  ;;add-exn-to-env: type env -> env
  (define (add-exn-to-env exn env)
    (make-environment (environment-type-env env)
                      (cons exn (environment-exn-env env))
                      (environment-label-env env)))
  
  ;;add-exns-to-env: (list type) env -> env
  (define (add-exns-to-env exns env)
    (if (null? exns)
        env
        (add-exns-to-env (cdr exns)
                         (add-exn-to-env (car exns) env))))
  
  ;;lookup-exn: type env type-records-> bool
  (define (lookup-exn type env type-recs)
    (ormap (lambda (lookup)
             (assignment-conversion lookup type type-recs))
           (environment-exn-env env)))
  
  ;;add-label-to-env: string env -> env
  (define (add-label-to-env label env)
    (make-environment (environment-type-env env)
                      (environment-exn-env env)
                      (cons label (environment-label-env env))))
  
  ;;lookup-label: string env -> bool
  (define (lookup-label label env)
    (member label (environment-label-env env)))
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Generic helper functions
  
  ;; set-expr-type: expr type -> type
  (define (set-expr-type exp t)
    (set-expr-types! exp t) t)

  ;lookup-this type-records env -> class-record
  (define (lookup-this type-recs env)
    (send type-recs get-class-record 
          (var-type-type (lookup-var-in-env "this" env))))  

  ;add-required (list string) string (list string) type-records -> void
  (define (add-required test-class class path type-recs)
    (unless (equal? (car test-class) class)
      (send type-recs add-req (make-req class path))))
  
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
          (check-interface def package-name level type-recs)
          (check-class def package-name level type-recs)))
    (packages (cons def (packages)))
    (when (not (null? (check-list)))
      (check-defs (car (check-list)) level type-recs)))
  
  ;check-interactions-types: ast symbol location type-records -> void
  (define (check-interactions-types prog level loc type-recs)
    (check-location loc)
    (send type-recs set-location! 'interactions)
    (send type-recs set-class-reqs null)
    (let ((env (add-var-to-env "this" (make-ref-type "scheme-interactions" null) #t #f #f #f
                               (create-field-env (send type-recs get-interactions-fields)
                                                 empty-env)))
          (c-class (list "scheme-interactions")))
      (cond
        ((pair? prog)
         (for-each (lambda (p)
                     (check-interactions-types p level loc type-recs)) prog))
        ((var-init? prog) 
         (let ((name (id-string (field-name prog))))
           (check-var-init (var-init-init prog)
                           (lambda (e) 
                             (check-expr e (remove-var-from-env name env) level type-recs c-class #f #t))
                           (type-spec-to-type (field-type prog) type-recs)
                           (string->symbol name)
                           type-recs)))
        ((var-decl? prog) (void))
        ((statement? prog)
         (check-statement prog null env level type-recs c-class #f #t #f #f))
        ((expr? prog)
         (check-expr prog env level type-recs c-class #f #t))
        (else
         (error 'check-interactions "Internal error: check-interactions-types got ~a" prog)))))
  
  ;check-class: class-def (list string) symbol type-records -> void
  (define (check-class class package-name level type-recs)
    (send type-recs set-location! (class-def-file class))
    (send type-recs set-class-reqs (class-def-uses class))
    (let ((this-ref (make-ref-type (id-string (header-id (class-def-info class)))
                                   package-name)))
      (check-members (class-def-members class)
                     (add-var-to-env "this" this-ref #t #f #f #f empty-env)
                     level
                     type-recs 
                     (list (id-string (header-id (class-def-info class))))))
    (set-class-def-uses! class (send type-recs get-class-reqs)))

  ;check-interface: interface-def (list string) symbol type-recs -> void
  (define (check-interface iface p-name level type-recs)
    (send type-recs set-location! (interface-def-file iface))
    (send type-recs set-class-reqs (interface-def-uses iface))
    (check-members (interface-def-members iface) empty-env level type-recs 
                   (list (id-string (header-id (interface-def-info iface)))))
    (set-interface-def-uses! iface (send type-recs get-class-reqs)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Member checking methods
  
  ;check-members: (list member) env symbol type-records (list string) -> void
  (define (check-members members env level type-recs c-class)
    (let* ((class-record (send type-recs get-class-record (var-type-type (lookup-var-in-env "this" env))))
           (fields (class-record-fields class-record))
           (field-env (create-field-env fields env))
           (ctor-throw-env (consolidate-throws 
                            (get-constructors (class-record-methods class-record)) field-env))
           (static-env (get-static-fields-env field-env))
           (setting-fields null))
      (let loop ((rest members) (statics empty-env) (fields env))
        (unless (null? rest)
          (let ((member (car rest)))
            (cond
              ((method? member)
               (if (memq 'static (map modifier-kind (method-modifiers member)))
                   (check-method member static-env level type-recs c-class #t)
                   (check-method member field-env level type-recs c-class #f))
               (loop (cdr rest) statics fields))
              ((initialize? member)
               (if (initialize-static member)
                   (check-statement (initialize-block member) 'void static-env
                                    level type-recs c-class #f #t #f #f)
                   (check-statement (initialize-block member) 'void ctor-throw-env level
                                    type-recs c-class #f #f #f #f))
               (loop (cdr rest) statics fields))
              ((field? member)
               (let ((static? (memq 'static (map modifier-kind (field-modifiers member))))
                     (name (id-string (field-name member)))
                     (type (type-spec-to-type (field-type member) type-recs)))
                 (if (var-init? member)
                     (check-var-init (var-init-init member)
                                     (lambda (e) 
                                       (check-expr e 
                                                   (if static? statics fields)
                                                   level type-recs c-class #f 
                                                   static?))
                                     type
                                     (string->symbol name)
                                     type-recs)
                     (when (field-needs-set? member level)
                       (set! setting-fields (cons member setting-fields))))
                 (if static?
                     (loop (cdr rest) 
                           (add-var-to-env name type #f #t #t #f statics) 
                           (add-var-to-env name type #f #t #t #f fields))
                     (loop (cdr rest) statics 
                           (add-var-to-env name type #t #f #t #f fields)))))))))
      (let ((assigns (get-assigns members level (car c-class)))
            (static-assigns (get-static-assigns members level)))
        (for-each (lambda (field)
                    (if (memq 'static (map modifier-kind (field-modifiers field)))
                        (andmap
                         (lambda (assign)
                           (field-set? field assign (car c-class) level #t)) static-assigns)
                        (andmap
                         (lambda (assign)
                           (field-set? field assigns (car c-class) level #f)) assigns)))
                  setting-fields))))
    
  ;create-field-env: (list field-record) env -> env
  (define (create-field-env fields env)
    (cond
      ((null? fields) env)
      (else
       (let* ((field (car fields))
              (static? (memq 'static (field-record-modifiers field))))
         (create-field-env (cdr fields)
                           (add-var-to-env (field-record-name field)
                                           (field-record-type field)
                                           (not static?)
                                           static?
                                           #t
                                           #f
                                           env))))))
  
  ;get-constrcutors: (list method-record) -> (list method-record)
  (define (get-constructors methods)
    (filter (lambda (mr)
              (eq? 'ctor (method-record-rtype mr))) methods))

  ;consolidate-throws: (list method-record) env -> env
  (define (consolidate-throws mrs env)
    (let ((first-throws (method-record-throws (car mrs)))
          (other-throws (map method-record-throws (cdr mrs))))
      (add-exns-to-env (filter (lambda (throw)
                                 (andmap (lambda (throws)
                                           (member throw throws))
                                         other-throws))
                               first-throws) env)))
  
  ;get-static-fields-env: env -> env
  (define (get-static-fields-env env)
    (make-environment (filter var-type-static? (environment-type-env env))
                      (environment-exn-env env)
                      (environment-label-env env)))

  ;field-needs-set?: field symbol -> bool
  (define (field-needs-set? field level)
    (cond
      ((eq? level 'beginner) #t)
      ((memq 'final (map modifier-kind (field-modifiers field))) #t)
      (else #f)))
  
  ;get-assigns: (list member) symbol string -> (list (list assignment))
  (define (get-assigns members level class)
    (if (eq? level 'beginner)
        (list (get-beginner-assigns members class))
        (get-instance-assigns members)))
  
  ;get-beginner-assigns: (list member) string-> (list assignment)
  (define (get-beginner-assigns members class)
    (cond
      ((null? members) null)
      ((field? (car members)) (get-beginner-assigns (cdr members) class))
      ((method? (car members)) 
       (if (eq? (method-type (car members)) 'ctor)
           (if (block? (method-body (car members)))
               (get-b-assigns (block-stmts (method-body (car members))) class)
               null)
           (get-beginner-assigns (cdr members) class)))))
  
  ;get-b-assigns: (list statement) string-> (list assignment)
  (define (get-b-assigns stmts class)
    (cond
      ((ifS? (car stmts)) 
       (beginner-ctor-error class (car stmts) (ifS-src (car stmts))))
      ((return? (car stmts)) 
       (beginner-ctor-error class (car stmts) (ifS-src (car stmts))))
      (else (cons (get-b-assigns-expr (car stmts) class)
                  (get-b-assigns (cdr stmts) class)))))
  
  ;get-b-assigns-expr: Expression string -> assignment
  (define (get-b-assigns-expr body class)
    (if (assignment? body)
        body
        (beginner-ctor-error class body (expr-src body))))

  ;get-instance-assigns: (list member) -> (list (list assignment))
  (define (get-instance-assigns members)
    (cond
      ((null? members) null)
      ((method? (car members))
       (if (eq? 'ctor (method-type (car members)))
           (cons (get-stmt-assigns (method-body (car members)))
                 (get-instance-assigns (cdr members)))
           (get-instance-assigns (cdr members))))
      (else (get-instance-assigns (cdr members)))))
  
  ;get-stmt-assigns: statement -> (list assign)
  (define (get-stmt-assigns b)
    (cond
      ((or (not b) (switch? b) (break? b) (continue? b)) null)
      ((ifS? b)
       (append (get-assigns-exp (ifS-cond b))
               (get-stmt-assigns (ifS-then b))
               (get-stmt-assigns (ifS-else b))))
      ((throw? b) (get-assigns-exp (throw-expr b)))
      ((return? b) (get-assigns-exp (return-expr b)))
      ((while? b) (append (get-assigns-exp (while-cond b))
                          (get-stmt-assigns (while-loop b))))
      ((doS? b) (append (get-assigns-exp (doS-cond b))
                        (get-stmt-assigns (doS-loop b))))
      ((for? b) (append (get-assigns-forInit (for-init b))
                        (get-assigns-exp (for-cond b))
                        (apply append (map get-assigns-exp (for-incr b)))
                        (get-stmt-assigns (for-loop b))))
      ((try? b) (get-stmt-assigns (try-body b)))
      ((block? b) (get-assigns-body (block-stmts b)))
      ((label? b) (get-stmt-assigns (label-stmt b)))
      ((synchronized? b) (append (get-assigns-exp (synchronized-expr b))
                                 (get-stmt-assigns (synchronized-stmt b))))
      (else (get-assigns-exp b))))
  
  ;get-assigns-forInit: (list forInit) -> (list assignment)
  (define (get-assigns-forInit b-list)
    (cond
      ((null? b-list) null)
      ((field? (car b-list)) null)
      (else (apply append (map get-assigns-exp b-list)))))
  
  ;get-assigns-body: (list statement) -> (list assignment)
  (define (get-assigns-body b-list)
    (cond
      ((null? b-list) null)
      ((field? (car b-list)) (get-assigns-body (cdr b-list)))
      (else (append (get-stmt-assigns (car b-list))
                    (get-assigns-body (cdr b-list))))))

  ;get-assigns-exp: expression -> (list assignment) 
  (define (get-assigns-exp exp)
    (cond
      ((or (not exp) (literal? exp) (special-name? exp)
           (class-alloc? exp)) null)
      ((bin-op? exp) (append (get-assigns-exp (bin-op-left exp))
                             (get-assigns-exp (bin-op-right exp))))
      ((access? exp) (if (field-access? (access-name exp))
                         (get-assigns-exp (field-access-object (access-name exp)))
                         null))
      ((call? exp) (get-assigns-exp (call-expr exp)))
      ((array-alloc? exp) (apply append (map get-assigns-exp (array-alloc-size exp))))
      ((cond-expression? exp)
       (append (get-assigns-exp (cond-expression-cond exp))
               (get-assigns-exp (cond-expression-then exp))
               (get-assigns-exp (cond-expression-else exp))))
      ((array-access? exp)
       (append (get-assigns-exp (array-access-name exp))
               (get-assigns-exp (array-access-index exp))))
      ((post-expr? exp) (get-assigns-exp (post-expr-expr exp)))
      ((pre-expr? exp) (get-assigns-exp (pre-expr-expr exp)))
      ((unary? exp) (get-assigns-exp (unary-expr exp)))
      ((cast? exp) (get-assigns-exp (cast-expr exp)))
      ((instanceof? exp) (get-assigns-exp (instanceof-expr exp)))
      ((assignment? exp) (list exp))))
  
  (define (get-static-assigns m l) null)
  
  ;field-set?: field (list assignment) string symbol bool -> bool
  (define (field-set? field assigns class level static?)
    (if 
     (null? assigns) (field-not-set-error (field-name field)
                                          class
                                          (if (memq level '(beginner intermediate))
                                              level
                                              (if static? 'static 'instance))
                                          (field-src field))
     (let* ((assign (car assigns))
            (left (assignment-left assign)))
       (or (cond
             ((local-access? left) 
              (equal? (id-string (local-access-name left))
                      (id-string (field-name field))))
             ((field-access? left)
              (and (special-name? (field-access-object left))
                   (equal? "this" (special-name-name (field-access-object left)))
                   (equal? (id-string (field-access-field left))
                           (id-string (field-name field))))))
           (field-set? field
                       (if (assignment? (assignment-right assign))
                           (cons (assignment-right assign)
                                 (cdr assigns))
                           (cdr assigns))
                       class level static?)))))
  
  ;check-method: method env type-records (list string) boolean-> void
  (define (check-method method env level type-recs c-class static?)
    (let* ((ctor? (eq? 'ctor (type-spec-name (method-type method))))
           (name (method-name method))
           (sym-name (string->symbol (id-string name)))
           (body (method-body method))
           (mods (map modifier-kind (method-modifiers method)))
           (return (if ctor? 
                       'void
                       (type-spec-to-type (method-type method) type-recs))))
      (if (or (memq 'abstract mods) (memq 'native mods))
          (when body
            (method-error (if (memq 'abstract mods) 'abstract 'native)
                          sym-name (id-src name)))
          (begin
            (when (and (not (eq? return 'void))
                       (not (reachable-return? body)))
              (method-error 'no-reachable sym-name (id-src name)))
            (check-statement body
                             return
                             (add-exns-to-env (map (lambda (n)
                                                     (java-name->type n type-recs)) 
                                                   (method-throws method))
                                              (build-method-env (method-parms method) env type-recs))
                             level
                             type-recs
                             c-class
                             ctor?
                             static?
                             #f
                             #f)
            ))))
  
  ;build-method-env: (list field) env type-records-> env
  (define (build-method-env parms env type-recs)
    (cond
      ((null? parms) env)
      (else
       (build-method-env (cdr parms)
                         (add-var-to-env (id-string (field-name (car parms)))
                                         (type-spec-to-type (field-type (car parms)) type-recs)
                                         #t
                                         #f
                                         #f
                                         #f
                                         env)
                         type-recs))))

  ;reachable-return?: statement -> bool
  (define (reachable-return? body)
    (cond
      ((ifS? body) 
       (if (ifS-else body)
           (and (reachable-return? (ifS-then body))
                (reachable-return? (ifS-else body)))))
      ((throw? body) #t)
      ((return? body) #t)
      ((while? body) (reachable-return? (while-loop body)))
      ((doS? body) (reachable-return? (doS-loop body)))
      ((for? body) (reachable-return? (for-loop body)))
      ((try? body) 
       (and (reachable-return? (try-body body))
            (or (and (try-finally body)
                     (reachable-return? (try-finally body)))
                #t)
            (andmap reachable-return? (map catch-body (try-catches body)))))
      ((switch? body) #f)
      ((block? body)
       (reachable-return? (list-ref (block-stmts body)
                                    (sub1 (length (block-stmts body))))))
      ((break? body) #f)
      ((continue? body) #f)
      ((label? body) (reachable-return? (label-stmt body)))
      ((synchronized? body) (reachable-return? (synchronized-stmt body)))
      (else #f)))

  ;check-var-init: expression (expression -> type) type symbol type-records -> type
  (define (check-var-init init check-e dec-type name type-recs)
    (if (array-init? init)
        (if (array-type? dec-type)
            (check-array-init (array-init-vals init) check-e 
                              (array-type-type dec-type) name type-recs)
            (var-init-error name dec-type (array-init-src init)))
        (check-e init)))
  
  ;check-array-init (U (list array-init) (list Expression)) (expression->type) type symbol type-records -> type
  (define (check-array-init inits check-e dec-type name type-recs)
    (cond
      ((null? inits) (make-array-type dec-type 1))
      ((array-init? (car inits))
       (let ((array-types (map (lambda (a) 
                                 (check-array-init (array-init-vals a) check-e dec-type name type-recs))
                               inits)))
         (make-array-type dec-type (add1 (array-type-dim (car array-types))))))
      (else
       (for-each (lambda (e) 
                   (let ((new-type (check-e e)))
                     (unless (assignment-conversion dec-type new-type type-recs)
                       (array-init-error dec-type new-type (expr-src e)))))
                 inits)
       (make-array-type dec-type 1))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Member errors

  (define (method-error kind method src)
    (raise-error method
                 (case kind
                   ((no-return)
                    (format "method ~a does not have a reachable return" method))
                   ((abs)
                    (format "abstract method ~a has an implementation. A ';' should come after the header"
                            method))
                   ((native)
                    (format "native method ~a has an implementation." method)))
                 method src))
  
  ;var-init-error: symbol type src -> void
  (define (var-init-error name dec-type src)
    (raise-error name
                 (format "~a declared to be of type ~a, given an array" 
                         name (type->ext-name dec-type))
                 name src))

  ;array-init-error: type type src -> void
  (define (array-init-error dec-type given src)
    (let ((d (type->ext-name dec-type))
          (g (type->ext-name given)))
      (raise-error 
       g
       (format "Error initializing declared array of ~a, given element with incompatible type ~a"
               d g)
       d src)))

  ;field-not-set-error: id string symbol src
  (define (field-not-set-error name class kind src)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (format "Field ~a from ~a must be set in the ~a"
                           n
                           class
                           (case kind
                             ((beginner intermediate) "constructor")
                             ((instance) "constructor or instance initialization")
                             ((static) "static initialization")))
                   n src)))

  ;beginner-ctor-error: string statement src -> void
  (define (beginner-ctor-error class kind src)
    (let ((exp (statement->ext-name kind)))
      (raise-error exp
                   (format "Constructor for ~a may only assign the fields of ~a. Found illegal statement ~a"
                           class class exp)
                   exp src)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Statement checking functions
  
  ;;check-statement: statement type env symbol type-records (U #f string) bool bool bool bool-> type
  (define (check-statement statement return env level type-recs c-c ctor? static? in-loop? in-switch?)
    (let* ((check-s (lambda (stmt env in-l? in-s?)
                      (check-statement stmt return env level type-recs c-c ctor? static? in-l? in-s?)))
           (check-s-env-change (lambda (smt env) (check-s smt env in-loop? in-switch?)))
           (check-s-no-change (lambda (stmt) (check-s stmt env in-loop? in-switch?)))
           (check-e (lambda (exp env)
                      (check-expr exp env level type-recs c-c ctor? static?)))
           (check-e-no-change (lambda (exp) (check-e exp env))))
      (cond
        ((ifS? statement) 
         (check-ifS (check-e-no-change (ifS-cond statement))
                    (expr-src (ifS-cond statement)))
         (check-s-no-change (ifS-then statement))
         (when (ifS-else statement) (check-s-no-change (ifS-else statement))))
        ((throw? statement)
         (check-throw (check-e-no-change (throw-expr statement))
                      (expr-src (throw-expr statement))
                      env
                      type-recs))
        ((return? statement)
         (check-return (return-expr statement)
                       return
                       check-e-no-change
                       (return-src statement)
                       type-recs))
        ((while? statement) 
         (check-while (check-e-no-change (while-cond statement))
                      (expr-src (while-cond statement)))
         (check-s (while-loop statement) env #t #f))
        ((doS? statement) 
         (check-do (check-e-no-change (doS-cond statement))
                   (expr-src (doS-cond statement)))
         (check-s (doS-loop statement) env #t #f))
        ((for? statement)
         (check-for (for-init statement)
                    (for-cond statement)
                    (for-incr statement)
                    (for-loop statement)
                    check-e
                    check-s
                    env
                    type-recs
                    in-switch?))
        ((try? statement) 
         (check-try (try-body statement)
                    (try-catches statement)
                    (try-finally statement)
                    env
                    check-s-env-change
                    type-recs))
        ((switch? statement)
         (check-switch (check-e-no-change (switch-expr statement))
                       (expr-src (switch-expr statement))
                       (switch-cases statement)
                       in-loop?
                       env
                       check-e-no-change
                       check-s))
        ((block? statement)
         (check-block (block-stmts statement)
                      env
                      check-s-env-change
                      check-e
                      type-recs))
        ((break? statement)
         (check-break (break-label statement)
                      (break-src statement)
                      in-loop?
                      in-switch?
                      env))
        ((continue? statement)
         (check-continue (continue-label statement)
                         (continue-src statement)
                         env
                         in-loop?))
        ((label? statement)
         (check-label (label-stmt statement)
                      (label-label statement)
                      check-s-env-change
                      env))
        ((synchronized? statement)
         (check-synchronized (check-e-no-change (synchronized-expr statement))
                             (expr-src (synchronized-expr statement)))
         (check-s-no-change (synchronized-stmt statement)))
        ((statement-expression? statement)
         (check-e-no-change statement)))))
  
  ;check-cond: symbol -> (type src -> void)
  (define (check-cond kind)
    (lambda (cond cond-src)
      (unless (eq? 'boolean cond)
        (kind-condition-error kind cond cond-src))))
        
  ;check-ifS: type src -> void
  (define check-ifS (check-cond 'if))

  ;check-throw: type src env type-records -> void
  (define (check-throw exp-type src env type-recs)
    (cond
      ((or (not (ref-type? exp-type))
           (not (is-eq-subclass? exp-type throw-type type-recs)))
       (throw-error 'not-throwable exp-type src))
      ((not (is-eq-subclass? exp-type runtime-exn-type type-recs))
       (unless (lookup-exn exp-type env type-recs)
         (throw-error 'not-declared exp-type src)))
      (else
       (send type-recs add-req (make-req "Throwable" (list "java" "lang"))))))

  ;check-return: expression type (expression -> type) src type-records -> void
  (define (check-return ret-expr return check src type-recs)
    (cond
      ((and ret-expr (not (eq? 'void return)))
       (let ((ret-type (check ret-expr)))
         (unless (assignment-conversion return ret-type type-recs)
           (return-error 'not-equal ret-type return src))))
      ((and ret-expr (eq? 'void return))
       (return-error 'void #f return src))
      ((and (not ret-expr) (not (eq? 'void return)))
       (return-error 'val #f return src))))
  
  ;check-while: type src -> void
  (define check-while (check-cond 'while))
  
  ;check-do: type src void -> void
  (define check-do (check-cond 'do))
  
  ;check-for: forInit Expression (list Expression) Statement (Expression env -> type) 
  ;           (Statement env bool bool-> void) env type-records bool -> void
  (define (check-for init cond incr loop check-e check-s env type-recs in-switch?)
    (let ((newEnv (if (and (not (null? init))
                           (field? (car init)))
                      (check-for-vars init env (lambda (e) (check-e e env)) type-recs)
                      (begin (for-each (lambda (e) (check-e e env)) init)
                             env))))
      ((check-cond 'for) (check-e cond newEnv) (expr-src cond))
      (map (lambda (e) (check-e e newEnv)) incr)
      (check-s loop newEnv #t in-switch?)))

  ;check-for-vars: (list field) env (expression -> type) type-records -> env
  (define (check-for-vars vars env check-e types)
    (if (null? vars)
        env
        (check-for-vars (cdr vars) 
                        (check-local-var (car vars) env check-e types) check-e types)))
  
  ;Need to allow shadowing of field names
  ;check-local-var: field env (expression -> type) type-records -> env
  (define (check-local-var local env check-e type-recs)
    (let* ((is-var-init? (var-init? local))
           (name (id-string (field-name local)))
           (in-env? (lookup-var-in-env name env))
           (sym-name (string->symbol name))
           (type (type-spec-to-type (field-type local) type-recs)))
      (when (and in-env? (not (var-type-field? in-env?)))
        (illegal-redefinition (field-name local) (field-src local)))
      (when is-var-init?
        (let ((new-type (check-var-init (var-init-init local) check-e type sym-name type-recs)))
          (unless (assignment-conversion type new-type type-recs)
            (variable-type-error (field-name local) new-type type (var-init-src local)))))
      (add-var-to-env name type #t #f #f #f env)))

  ;check-try: statement (list catch) (U #f statement) env (statement env -> void) type-records -> void
  (define (check-try body catches finally env check-s type-recs)
    (let ((new-env
           (let loop ((catches catches) (new-env env))
             (if (null? catches)
                 new-env
                 (let* ((catch (car catches))
                        (type (field-type (catch-cond catch))))
                   (unless (and (ref-type? type)
                                (is-eq-subclass? type throw-type type-recs))
                     (catch-error type (field-src (catch-cond catch))))
                   (loop (cdr catches) (add-exn-to-env type env)))))))
      (check-s body new-env)
      (for-each (lambda (catch)
                  (let* ((field (catch-cond catch))
                         (name (id-string (field-name field)))
                         (in-env? (lookup-var-in-env name env)))
                    (if (and in-env? (not (var-type-field? in-env?)))
                        (illegal-redefinition (field-name field) (field-src field))
                        (check-s (catch-body catch)
                                 (add-var-to-env name (field-type field) #t #f #f #f env)))))
                catches)
      (when finally (check-s finally env))))

  ;Skipping proper checks of the statements + proper checking that constants aren't repeated
  ;check-switch: type src (list caseS) bool env (expression -> type) (statement env bool bool -> void) -> void
  (define (check-switch expr-type expr-src cases in-loop? env check-e check-s)
    (when (or (eq? expr-type 'long)
              (not (prim-integral-type? expr-type)))
      (switch-error 'switch-type 'switch expr-type #f expr-src))
    (for-each (lambda (case)
                (let* ((constant (caseS-constant case))
                       (cons-type (unless (eq? 'default constant) (check-e constant))))
                  (if (or (eq? 'default constant)
                          (type=? cons-type expr-type))
                      void
                      (switch-error 'incompat 'case cons-type expr-type (expr-src constant)))))
              cases))
  
  ;check-block: (list (U statement field)) env (statement env -> void) (expr -> type) type-records -> void
  (define (check-block stmts env check-s check-e type-recs)
    (let loop ((stmts stmts) (block-env env) (check-e (lambda (e) (check-e env))))
      (cond 
        ((null? stmts) (void))
        ((field? (car stmts))
         (loop (cdr stmts) (check-local-var (car stmts) block-env check-e type-recs)
               (lambda (e) (check-e e block-env))))
        (else
         (check-s (car stmts) block-env)
         (loop (cdr stmts) block-env (lambda (e) (check-e e block-env)))))))
      
  ;check-break: (U id #f) src env bool bool -> void
  (define (check-break label src env in-loop? in-switch?)
    (cond
      (label
       (unless (lookup-label (id-string label) env)
         (illegal-label 'break (id-string label) (id-src label))))
      ((or (not in-loop?) (not in-switch?))
       (break-error src))))

  ;check-continue: (U id #f) src env bool -> void
  (define (check-continue label src env in-loop?)
    (cond
      (label
       (unless (lookup-label (id-string label) env)
         (illegal-label 'continue (id-string label) (id-src label))))
      ((not in-loop?)
       (continue-error src))))
  
  ;check-label: statement string (statement env -> void) env -> void
  (define (check-label stmt label check-s env)
    (check-s stmt (add-label-to-env label env)))

  ;check-synchronized: type src -> void
  (define (check-synchronized e-type e-src)
    (unless (reference-type? e-type)
      (synch-error e-type e-src)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Statement error messages

  ;make-condition-error: symbol type src -> void
  (define (kind-condition-error kind cond src)
    (raise-error kind
                 (format "~a condition must be a boolean: Given ~a" 
                         kind (type->ext-name cond))
                 kind src))
    
  ;throw-error: symbol type src -> void
  (define (throw-error kind thrown src)
    (let ((t (type->ext-name thrown)))
      (raise-error 'throw
                   (case kind
                     ((not-throwable)
                      (format "Expression for throw must be a subtype of Throwable: given ~a" t))
                     ((not-caught)
                      (format "Thrown type ~a must be declared as thrown or caught" t)))
                   'throw src)))
  
  ;return-error: symbol type type src -> void
  (define (return-error kind given expected src)
    (let ((g (type->ext-name given))
          (e (type->ext-name expected)))
      (raise-error 'return
                   (case kind
                     ((not-equal)
                      (format "type of returned expression must be equal to or a subclass of ~a: given ~a"
                              e g))
                     ((void) "No value should be returned from void method.")
                     ((val)
                      (format "Expected a return value assignable to ~a. No value was given" e)))
                     'return src)))

  ;illegal-redefinition: id src -> void
  (define (illegal-redefinition field src)
    (let ((f (id->ext-name field)))
      (raise-error f
                   (format "Variable ~a already exists. Another name must be chosen" f)
                   f src)))
  
  ;variable-type-error: id type type src -> void
  (define (variable-type-error field given expt src)
    (let ((f (id->ext-name field)))
      (raise-error 
       f
       (format "Variable ~a declared to be ~a, which is incompatible with the initial value type of ~a"
               f (type->ext-name expt) (type->ext-name given))
       f src)))

  ;catch-error: type src -> void
  (define (catch-error given src)
    (raise-error 'catch
                 (format "catch clause must catch an argument of subclass Throwable: Given ~a"
                         (type->ext-name given))
                 'catch src))
  
  ;switch-error symbol symbol type type src -> void
  (define (switch-error kind syn given expected src)
    (raise-error
     syn
     (case kind
       ((switch-type) 
        (format "switch expression must be of type byte, short, int or char. Given: ~a" 
                (type->ext-name given)))
       ((incompat)
        (format "switch case must be same type as switch expression. Given ~a: expected ~a"
                (type->ext-name given) (type->ext-name expected))))
     syn src))
  
  ;illegal-label: symbol string src -> void
  (define (illegal-label kind label src)
    (raise-error kind
                 (format "~a references label ~a, no enclosing statement has this label"
                         kind label)
                 kind src))
                 
  ;break-error: src -> void
  (define (break-error src)
    (raise-error 'break "break must be in either a loop or a switch"
                 'break src))
  (define (continue-error src)
    (raise-error 'continue "continue must be in a loop" 'continue src))

  ;synch-error: type src -> void
  (define (synch-error given src)
    (raise-error 'synchronize
                 (format "synchronization expression must be a subtype of Object: Given ~a"
                         (type->ext-name given))
                 'synchronize src))                                        
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Expression checking functions
  
  ;; check-expr: expression env symbol type-records (U string #f) bool bool-> type
  (define (check-expr exp env level type-recs current-class ctor? static?)
    (let ((check-sub-expr 
           (lambda (expr) (check-expr expr env level type-recs current-class ctor? static?))))
      (cond
        ((literal? exp) 
         (when (memq (expr-types exp) `(String string))
           (add-required current-class "String" `("java" "lang") type-recs))
         (expr-types exp))
        ((bin-op? exp)
         (set-expr-type exp 
                        (check-bin-op (bin-op-op exp)
                                      (check-sub-expr (bin-op-left exp))
                                      (check-sub-expr (bin-op-right exp))
                                      (expr-src exp)
                                      level
                                      type-recs)))
        ((access? exp)
         (set-expr-type exp
                        (check-access exp check-sub-expr env level type-recs current-class)))
        ((special-name? exp)
         (set-expr-type exp (check-special-name exp env static? #f))) ;last bool is interactions. PROBLEM
        ((call? exp)
         (set-expr-type exp (check-call exp
                                        (map check-sub-expr (call-args exp))
                                        check-sub-expr
                                        current-class
                                        level
                                        env
                                        type-recs
                                        ctor?
                                        static?)))
        ((class-alloc? exp)
         (set-expr-type exp
                        (check-class-alloc (class-alloc-name exp)
                                           (map check-sub-expr (class-alloc-args exp))
                                           (expr-src exp)
                                           type-recs
                                           current-class
                                           env
                                           level)))
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
                                            (expr-src exp)
                                            type-recs)))
        ((post-expr? exp)
         (set-expr-type exp
                        (check-pre-post-expr (check-sub-expr (post-expr-expr exp))
                                             (post-expr-op exp)
                                             (expr-src exp))))
        ((pre-expr? exp)
         (set-expr-type exp
                        (check-pre-post-expr (check-sub-expr (pre-expr-expr exp))
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
                                          ctor?
                                          level
                                          type-recs))))))

  ;;added assignment ops so that error messages will be correct
  ;;check-bin-op: symbol type type src-loc symbol type-records -> type
  (define (check-bin-op op l r src level type-recs)
    (case op
      ((* / % *= /= %=)       ;; 15.17
       (prim-check prim-numeric-type? binary-promotion 'num l r op src))
      ((+ - += -=)      ;; 15.18
       (if (and (memq level '(advanced full))
                (eq? '+ op) (or (is-string-type? l) (is-string-type? r)))
           string-type
           (prim-check prim-numeric-type? binary-promotion 'num l r op src)))
      ((<< >> >>> <<= >>= >>>=)      ;; 15.19
       (prim-check prim-integral-type? 
                   (lambda (l r) (unary-promotion l)) 'int l r op src))
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
               (right-to-left (bin-op-equality-error 'left op l r src))
               (left-to-right (bin-op-equality-error 'right op l r src))
               (else (bin-op-equality-error 'both op l r src)))))
          (else 
           (bin-op-equality-error 'prim op l r src))))
      ((& ^ or &= ^= or=)      ;; 15.22
       (cond
         ((and (prim-numeric-type? l) (prim-numeric-type? r)) (binary-promotion l r))
         ((and (eq? 'boolean l) (eq? 'boolean r)) 'boolean)
         (else (bin-op-bitwise-error op l r src))))
      ((&& oror)      ;; 15.23, 15.24
       (prim-check (lambda (b) (eq? b 'boolean)) 
                   (lambda (l r) 'boolean) 'bool l r op src))))

  ;prim-check: (type -> bool) (type type -> type) type type src -> type
  (define (prim-check ok? return expt l r op src)
    (cond
      ((and (ok? l) (ok? r)) (return l r))
      ((ok? l) (bin-op-prim-error 'right op expt l r src))
      ((ok? r) (bin-op-prim-error 'left op expt l r src))
      (else (bin-op-prim-error 'both op expt l r src))))

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

  ;;check-access: expression (expr -> type) env symbol type-records (list string) -> type
  (define (check-access exp check-sub-expr env level type-recs c-class)
    (let ((acc (access-name exp)))
      (cond
        ((field-access? acc)
         (let ((obj (field-access-object acc))
               (fname (id-string (field-access-field acc)))
               (record null))
           (if obj
               (begin 
                 (set! record (field-lookup fname (check-sub-expr obj) obj type-recs))
                 (set-field-access-access! acc (make-var-access 
                                                (memq 'static (field-record-modifiers record))
                                                (field-record-class record))))
               (set! record 
                     (let ((name (var-access-class (field-access-access acc))))
                       (get-field-record fname
                                         (get-record 
                                          (send type-recs get-class-record name
                                                ((get-importer type-recs) name type-recs 'full))
                                          type-recs)
                                         (lambda () 
                                           (field-lookup-error 'not-found
                                                               (string->symbol fname)
                                                               (make-ref-type name null)
                                                               (expr-src exp)))))))
           (add-required c-class (car (field-record-class record)) 
                         (cdr (field-record-class record)) type-recs)
           (field-record-type record)))
        
        ((local-access? acc) 
         (var-type-type (lookup-var-in-env (id-string (local-access-name acc)) env)))
        
        (else
         (let* ((first-acc (id-string (car acc)))
                (first-binding (lookup-var-in-env first-acc env))
                (new-acc
                 (cond
                   ((and (eq? level 'full) (not first-binding) (> (length acc) 1))
                    (let* ((static-class (find-static-class acc type-recs))
                           (accs (cadr static-class)))
                      (build-field-accesses 
                       (make-access #f 
                                    (expr-src exp)
                                    (make-field-access 
                                     #f
                                     (car accs)
                                     (make-var-access #t (class-record-name (car static-class)))))
                       (cdr accs))))
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
                                                         (make-var-access #t c-class)))
                         (cdr acc))
                        (build-field-accesses
                         (make-access #f (expr-src exp)
                                      (make-field-access (make-special-name #f (expr-src exp)
                                                                            "this")
                                                         (car acc)
                                                         #f))
                         (cdr acc))))
                   (else (variable-not-found-error (car acc) (id-src (car acc)))))))
           (set-access-name! exp new-acc)
           (check-sub-expr exp))))))
  
  ;; field-lookup: string type expression type-records -> field-record
  (define (field-lookup fname obj-type obj type-recs)
    (let ((src (expr-src obj))
          (name (string->symbol fname)))
      (cond
        ((reference-type? obj-type)
         (let ((obj-record (send type-recs get-class-record obj-type
                                 ((get-importer type-recs) obj-type type-recs 'full))))
           (get-field-record fname 
                             (get-record obj-record type-recs)
                             (lambda () 
                               (field-lookup-error 'not-found name obj-type src)))))
        ((array-type? obj-type)
         (unless (equal? fname "length")
           (field-lookup-error 'array name obj-type src))
         (make-field-record "length" `() `(array) 'int))
        (else (field-lookup-error 'primitive name obj-type src)))))
  
  ;; build-field-accesses: access (list id) -> field-access
  (define (build-field-accesses start accesses)
    (cond
      ((null? accesses) (access-name start))
      (else
       (build-field-accesses
        (make-access #f (expr-src start) 
                     (make-field-access start (car accesses) #f))
        (cdr accesses)))))
  
  ;;find-static-class: (list access) type-recs -> (list class-record (list access))
  (define (find-static-class accs type-recs)
    (let ((path (send type-recs lookup-path (id-string (car accs)) (lambda () #f))))
      (if path
          (list (let* ((name (cons (id-string (car accs)) path))
                       (record (get-record 
                                (send type-recs get-class-record name 
                                      ((get-importer type-recs) name type-recs 'full))
                                type-recs)))
                  record)
                (cdr accs))
          (let ((found? (find-static (list (car accs)) (cdr accs))))
            (if (car found?)
                (list (get-record (send type-recs get-class-record (car found?)) type-recs)
                      (cdr found?))
                (class-lookup-error (cadr found?) (id-src (car accs))))))))
    
  ;find-static: (list id) (list id) -> (list (U #f (list id)) (list string)))
  (define (find-static test-path remainder)
    (let ((string-path (map id-string test-path)))
      (cond
        ((null? (cdr remainder))
         (list #f (list (apply build-path string-path))))
        ((find-directory string-path) =>
         (lambda (directory)
           (if (class-exists? directory (id-string (car remainder)))
               (list (cdr remainder) (cons (id-string (car remainder)) string-path))
               (find-static (append string-path (list (id-string (car remainder))))
                            (cdr remainder)))))
      (else (list #f (apply build-path (append string-path (list (id-string (car remainder))))))))))
  
  ;find-directory: (list string) -> (U string bool)
  (define (find-directory path)
    (if (null? path)
        (build-path 'same)
        (let loop ((paths (get-classpath)))
          (cond
            ((null? paths) #f)
            ((directory-exists? (build-path (car paths)
                                            (apply build-path path)))
             (build-path (car paths) (apply build-path path)))
            (else (loop (cdr paths)))))))
  
  ;class-exists?: string string -> bool
  (define (class-exists? path class)
    (or (file-exists? (string-append (build-path path class) ".java"))
        (file-exists? (string-append (build-path path "compiled" class) ".jinfo"))))
  
  ;check-special-name: expression env bool bool-> type
  (define (check-special-name exp env static? interact?)
    (when static? 
      (special-error (expr-src exp) interact?))
    (var-type-type (lookup-var-in-env "this" env)))
  
  ;;Skipping package access constraints
  ;; 15.12
  ;check-call: call (list type) (expr->type) (list string) symbol env type-records bool bool-> type
  (define (check-call call args check-sub c-class level env type-recs ctor? static?)
    (let* ((this (unless static? (lookup-this type-recs env)))
           (src (expr-src call))
           (name (call-method-name call))
           (expr (call-expr call))
           (exp-type #f)
           (handle-call-error 
            (lambda (exn)
              (unless (or (access? expr) (not (eq? level 'full))) (raise exn))
              (let ((record (car (find-static-class 
                                   (append (access-name expr) (list name))
                                   type-recs))))
                (set-call-expr! call #f)
                (unless (equal? (class-record-name record) c-class)
                  (send type-recs add-req (make-req (car (class-record-name record))
                                                    (cdr (class-record-name record)))))
                (get-method-records name record))))
           (methods 
            (cond 
              ((special-name? name)
               (let ((n (special-name-name name)))
                 (unless ctor? (illegal-ctor-call n src level))
                 (if (string=? n "super")
                     (let ((parent (car (class-record-parents this))))
                       (get-method-records (car parent)
                                           (send type-recs get-class-record parent)))
                     (get-method-records (car (class-record-name this)) this))))
              (else
               (cond
                 ((special-name? expr)
                  (if (equal? (special-name-name expr) "super")
                      (let ((parent (car (class-record-parents this))))
                        (set! exp-type 'super)
                        (get-method-records (id-string name)
                                            (send type-recs get-class-record parent)))
                      (get-method-records (id-string name) this)))
                 (expr
                  (let ((call-exp 
                         (with-handlers ((exn:syntax? handle-call-error))
                           (check-sub expr))))
                    (cond
                      ;List of methods found
                      ((list? call-exp) call-exp)
                      ((array-type? call-exp)
                       (set! exp-type call-exp)
                       (get-method-records (id-string name)
                                           (send type-recs get-class-record object-type)))
                      ((reference-type? call-exp)
                       (set! exp-type call-exp)
                       (get-method-records (id-string name)
                                           (get-record 
                                            (send type-recs get-class-record call-exp 
                                                  ((get-importer type-recs) call-exp type-recs 'full))
                                            type-recs)))
                      (else (prim-call-error call-exp name src level)))))
                 (else 
                  (get-method-records (id-string name)
                                      (if static?
                                          (send type-recs get-class-record c-class)
                                          this))))))))
      
      (when (null? methods)
        (cond 
          ((eq? exp-type 'super) (no-method-error 'super exp-type name src))
          (exp-type (no-method-error 'class exp-type name src))
          (else (no-method-error 'this exp-type name src))))
      
      (when (and (not ctor?)
                 (eq? (method-record-rtype (car methods)) 'ctor))
        (ctor-called-error exp-type name src))
      
      (let* ((method-record 
              (if (memq level '(full advanced))
                  (resolve-overloading methods 
                                       args
                                       (lambda () (call-arg-error 'number name args exp-type src))
                                       (lambda () (call-arg-error 'conflict name args exp-type src))
                                       (lambda () (call-arg-error 'no-match name args exp-type src))
                                       type-recs)
                  (when (check-method-args args (method-record-atypes (car methods)) name exp-type src type-recs)
                    (car methods))))
             (mods (method-record-modifiers method-record)))
        (when (memq 'abstract mods) (call-access-error 'abs name exp-type src))
        (when (and (memq 'protected mods) (reference-type? exp-type) 
                   (not (is-eq-subclass? this exp-type)))
          (call-access-error 'pro name exp-type src))
        (when (and (memq 'private mods)
                   (reference-type? exp-type)
                   (not (eq? this (send type-recs get-class-record exp-type))))
          (call-access-error 'pri name exp-type src))
        (when (eq? level 'full)
          (for-each (lambda (thrown)
                      (unless (lookup-exn thrown env type-recs)
                        (thrown-error (ref-type-class/iface thrown) name exp-type src)))
                    (method-record-throws method-record)))
        (set-call-method-record! call method-record)
        (method-record-rtype method-record))))

  ;check-method-args: (list type) (list type) id type src type-records -> void
  (define (check-method-args args atypes name exp-type src type-recs)
    (unless (= (length args) (length atypes))
      (method-arg-error 'number args atypes name exp-type src))
    (for-each (lambda (arg atype)
                (unless (assignment-conversion arg atype type-recs)
                  (method-arg-error 'type (list arg) (cons atype atypes) name exp-type src)))
              args atypes))
  
  ;;Skip package access controls
  ;; 15.9
  ;;check-class-alloc: name (list type) src type-records (list string) env symbol -> type
  (define (check-class-alloc name args src type-recs c-class env level)
    (let* ((type (java-name->type name type-recs))
           (class-record (send type-recs get-class-record type))
           (methods (get-method-records (ref-type-class/iface type) class-record)))
      (unless (equal? (ref-type-class/iface type) (car c-class))
        (send type-recs add-req (make-req (ref-type-class/iface type) (ref-type-path type))))
      (when (memq 'abstract (class-record-modifiers class-record))
        (class-alloc-error 'abstract type (name-src name)))
      (unless (class-record-class? class-record)
        (class-alloc-error 'interface type (name-src name)))
      (let* ((const (if (memq level `(full advanced))
                        (resolve-overloading methods 
                                             args 
                                             (lambda () (ctor-overload-error 'number name args src))
                                             (lambda () (ctor-overload-error 'conflict name args src))
                                             (lambda () (ctor-overload-error 'no-match name args src))
                                             type-recs)
                        (when (check-ctor-args args (method-record-atypes (car methods)) type src type-recs)
                          (car methods))))
             (mods (method-record-modifiers const))
             (this (lookup-this type-recs env)))
        (when (eq? level 'full)
          (for-each (lambda (thrown)
                      (unless (lookup-exn thrown env type-recs)
                        (ctor-throws-error (ref-type-class/iface thrown) type src)))
                    (method-record-throws const)))
        (when (and (memq 'private mods) (not (eq? class-record this)))
          (class-access-error 'pri type src))
        (when (and (memq 'protected mods) (not (is-eq-subclass? this type)))
          (class-access-error 'pro type src))
        type)))

  ;check-method-args: (list type) (list type) type src type-records -> void
  (define (check-ctor-args args atypes name src type-recs)
    (unless (= (length args) (length atypes))
      (ctor-arg-error 'number args atypes name src))
    (for-each (lambda (arg atype)
                (unless (assignment-conversion arg atype type-recs)
                  (ctor-arg-error 'type (list arg) (cons atype atypes) name src)))
              args atypes))
  
  ;; 15.10
  ;;check-array-alloc type-spec (list expression) int src (expr->type) type-records -> type
  (define (check-array-alloc elt-type exps dim src check-sub-exp type-recs)
    (send type-recs add-req (make-req 'array null))
    (let ((type (type-spec-to-type elt-type type-recs)))
      (for-each (lambda (e)
                  (let ((t (check-sub-exp e)))
                    (unless (prim-integral-type? t)
                      (array-size-error type t (expr-src e)))))
                exps)
      (make-array-type type (+ (length exps) dim))))
  
  ;; 15.25
  ;check-cond-expr: type type type src src type-records -> type
  (define (check-cond-expr test then else-t src test-src type-recs)
    (unless (eq? 'boolean test)
      (condition-error test test-src))
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
      (else (condition-mismatch-error then else-t src))))

  ;; 15.13
  ;check-array-access: type type src -> type
  (define (check-array-access ref-type idx-type src type-recs)
    (send type-recs add-req (make-req 'array null))
    (unless (array-type? ref-type)
      (illegal-array-access ref-type src))
    (when (or (not (prim-integral-type? idx-type))
              (not (eq? 'int (unary-promotion idx-type))))
      (array-access-error ref-type idx-type src))
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
        (unary-error op 'num type src)))
  
  ;; 15.15
  ;check-unary: type symbol src -> type
  (define (check-unary expr-type op src)
    (case op
      ((+ -)
       (if (prim-numeric-type? expr-type)
           (unary-promotion expr-type)
           (unary-error op 'num expr-type src)))
      ((~)
       (if (prim-integral-type? expr-type)
           (unary-promotion expr-type)
           (unary-error op 'int expr-type src)))
      ((!)
       (if (eq? 'boolean expr-type)
           'boolean
           (unary-error op 'bool expr-type src)))))
    
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
         (cast-error 'from-prim exp-type type src))
        (else
         (cast-error 'from-ref exp-type type src)))))

  ;; 15.20.2
  ;check-instanceof type type-spec src (list string) type-records -> type
  (define (check-instanceof exp-type inst-type src current-class type-recs)
    (let ((type (type-spec-to-type inst-type type-recs)))
      (unless (equal? (car current-class) (ref-type-class/iface type))
        (send type-recs add-req (make-req (ref-type-class/iface type) (ref-type-path type))))
      (cond 
        ((and (ref-type? exp-type) (ref-type? type) (is-eq-subclass? exp-type type type-recs)) 'boolean)
        ((and (ref-type? exp-type) (ref-type? type))
         (instanceof-error 'not-subtype type exp-type src))
        ((ref-type? exp-type)
         (instanceof-error 'not-class type exp-type src))
        (else
         (instanceof-error 'not-ref type exp-type src)))))
  
  ;; 15.26
  ;; SKIP - worrying about final - doing the check for compound assignment
  ;check-assignment: symbol type type src bool symbol type-records -> type
  (define (check-assignment op ltype rtype src constructor? level type-recs)
    (when (eq? level 'beginner)
      (unless constructor?
        (illegal-assignment src)))
    (case op
      ((=)
       (if (assignment-conversion ltype rtype type-recs)
           ltype
           (assignment-error op ltype rtype src)))
      ((+= *= /= %= -= <<= >>= >>>= &= ^= or=)
       (check-bin-op op ltype rtype src level type-recs)
       ltype)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Expression Errors

  ;;Binop errors
  ;;bin-op-prim-error: symbol symbol symbol type type src -> void
  (define (bin-op-prim-error side op expect left right src)
    (let ((ext-out (get-expected expect))
          (rt (type->ext-name right))
          (lt (type->ext-name left)))
      (raise-error
       op
       (case side
         ((right) (format "Right hand side of ~a should be of type ~a, but given ~a" 
                          op ext-out rt))
         ((left) (format "Left hand side of ~a should be of type ~a, but given ~a"
                         op ext-out lt))
         (else
          (format "~a expects arguments of type ~a, but given ~a and ~a" op ext-out lt rt)))
       op src)))
  
  ;bin-op-equality-error symbol symbol type type src -> void
  (define (bin-op-equality-error type op left right src)
    (let ((rt (type->ext-name right))
          (lt (type->ext-name left)))
      (raise-error 
       op
       (case type
         ((right) 
          (format "Right hand side of ~a should be assignable to ~a. Given ~a" op lt rt))
         ((left) 
          (format "Left hand side of ~a should be assignable to ~a. Given ~a" op rt lt))
         ((both) 
          (format "~a expects its arguments to be assignable to each other, ~a and ~a cannot" op lt rt))          
         (else 
          (format "~a expects its arguments to be equivalent types, given non-equivalent ~a and ~a" 
                  op lt rt)))
       op src)))

  ;bin-op-bitwise-error symbol type type src -> void
  (define (bin-op-bitwise-error op left right src)
    (let ((lt (type->ext-name left))
          (rt (type->ext-name right))
          (prim-list "double, float, long, int, short, byte or char"))
      (raise-error 
       op 
       (cond
         ((prim-numeric-type? left) 
          (format "~a expects the right hand side to be a ~a when the left is ~a. Given ~a"
                  op prim-list lt rt))
         ((prim-numeric-type? right)
          (format "~a expects the left hand side to be a ~a when the left is ~a. Given ~a"
                  op prim-list rt lt))
         ((eq? left 'boolean)
          (format "~a expects the right hand side to be a ~a when the left is ~a. Given ~a"
                  op "boolean" lt rt))
         ((eq? right 'boolean)
          (format "~a expects the left hand side to be a ~a when the right is ~a. Given ~a"
                  op "boolean" rt lt))
         (else
          (format "~a expects its arguments to both be either booleans, or ~a. Given ~a and ~a"
                  op prim-list lt rt)))
       op src)))

  ;;check-access errors
  
  ;variable-not-found-error: id src -> void
  (define (variable-not-found-error var src)
    (let ((name (id->ext-name var)))
      (raise-error
       name
       (format "reference to undefined identifier ~a" name)
       name src)))
  
  ;field-lookup-error: symbol symbol type src -> void
  (define (field-lookup-error kind field exp src)
    (let ((t (type->ext-name exp)))
      (raise-error
       field
       (case kind
         ((not-found)
          (format "field ~a not found for object with type ~a" field t))
         ((array)
          (format "~a only has a length field, attempted to access ~a" t field))
         ((primitive)     
          (format "attempted to access field ~a on ~a, this type does not have fields" field t)))
       field src)))
  
  ;;special-name errors
  ;special-error: src bool -> void
  (define (special-error src interactions?)
    (raise-error 'this 
                 (format "use of 'this' is not allowed in ~a"
                         (if interactions? 
                             "the interactions window"
                             "static code"))
                 'this src))
  
  ;;Call errors

  ;prim-call-error type id src symbol -> void
  (define (prim-call-error exp name src level)
    (let ((n (id->ext-name name))
          (t (type->ext-name exp)))
      (raise-error n
                   (format "attempted to call method ~a on ~a, only ~a types have methods"
                           n t
                           (case level 
                             ((beginner) "class")
                             ((intermediate) "class or interface")
                             (else "class, interface, or array")))
                   n src)))
  
  ;no-method-error: symbol type id src -> void
  (define (no-method-error kind exp name src)
    (let ((t (type->ext-name exp))
          (n (id->ext-name name)))
      (raise-error n
                   (format "~a does not contain a method ~a"
                           (case kind
                             ((class) t)
                             ((super) "This class's super class")
                             ((this) "The current class"))
                           n)
                   n src)))
  
  ;ctor-called-error: type id src -> void
  (define (ctor-called-error exp name src)
    (let ((t (type->ext-name exp))
          (n (id->ext-name name)))
      (raise-error n
                   (format "Constructor ~a from ~a cannot be used as a method"
                           n t)
                   n src)))
  
  (define (illegal-ctor-call name src level)
    (let ((n (string->symbol name)))
      (raise-error n (format "calls to ~a may only occur in ~a"
                             n
                             (if (memq level `(full advanced))
                                 "other constructors"
                                 "the constructor"))
                   n src)))

  ;method-arg-error symbol (list type) (list type) id type src -> void
  (define (method-arg-error kind args atypes name exp-type src)
    (let ((n (id->ext-name name))
          (e (get-call-type exp-type))
          (givens (map type->ext-name args))
          (expecteds (map type->ext-name atypes))
          (awitht "arguments with types"))
      (raise-error n
                   (case kind
                     ((number)
                      (format "method ~a from ~a expects ~a ~a ~a. Given ~a ~a ~a"
                              n e (length expecteds) awitht expecteds (length givens) awitht givens))
                     ((type)
                      (format "method ~a from ~a expects ~a ~a, but given a ~a instead of ~a for one argument"
                              n e awitht (cdr atypes) (car givens) (car atypes))))
                   n src)))

  ;call-access-error: symbol id type src -> void
  (define (call-access-error kind name exp src)
    (let ((n (id->ext-name name))
          (t (get-call-type exp)))
    (raise-error n
                 (case kind
                   ((abs) (format "Abstract methods may not be called. ~a from ~a is abstract"
                                  n t))
                   ((pro) (format "Protected method ~a from ~a may not be called here" 
                                  n t))
                   ((pri) (format "Private method ~a from ~a may not be called here"
                                  n t)))
                 n src)))

  ;call-arg-error: symbol id (list type) type src -> void
  (define (call-arg-error kind name args exp src)
    (let ((n (id->ext-name name))
          (t (get-call-type exp))
          (as (map type->ext-name exp)))
      (raise-error n
                   (case kind
                     ((number)
                      (format "method ~a from ~a has no definition with ~a arguments. Given ~a"
                              n t (length as) as))
                     ((no-match)
                      (format "method ~a from ~a has no definition with compatible types as the given types: ~a"
                              n t as))
                     ((conflict)
                      (format "method ~a from ~a has multiple compatible definitions with given arguments: ~a"
                              n t as)))
                   n src)))
  
  ;thrown-error: string id type src -> void
  (define (thrown-error thrown name exp src)
    (let ((n (id->ext-name name))
          (t (get-call-type exp)))
      (raise-error n
                   (format "called method ~a from ~a throws exception ~a, which is not caught or listed as thrown"
                           n t thrown)
                   n src)))
      
  ;;Class Alloc errors

  ;class-alloc-error: symbol type src -> void
  (define (class-alloc-error kind type src)
    (let ((cl (type->ext-name type)))
      (raise-error cl
                   (case kind
                     ((abstract) "Class ~a is abstract and may not be constructed" cl)
                     ((interface) "Interface ~a is an interface: only classes may be constructed" cl))
                   cl src)))

  ;ctor-arg-error symbol (list type) (list type) type src -> void
  (define (ctor-arg-error kind args atypes name src)
    (let ((n (type->ext-name name))
          (givens (map type->ext-name args))
          (expecteds (map type->ext-name atypes))
          (awitht "arguments with types"))
      (raise-error n
                   (case kind
                     ((number)
                      (format "Constructor for ~a expects ~a ~a ~a. Given ~a ~a ~a"
                              n (length expecteds) awitht expecteds (length givens) awitht givens))
                     ((type)
                      (format "Constructor for ~a expects ~a ~a, but given a ~a instead of ~a for one argument"
                              n awitht (cdr atypes) (car givens) (car atypes))))
                   n src)))

  ;ctor-overload-error: symbol type (list type) src -> void
  (define (ctor-overload-error kind name args src)
    (let ((n (type->ext-name name))
          (as (map type->ext-name exp)))
      (raise-error 
       n
       (case kind
         ((number)
          (format "No constructor for ~a exists with ~a arguments. Given ~a"
                  n (length as) as))
         ((no-match)
          (format "No constructor for ~a exists with compatible types as the given types: ~a"
                  n as))
         ((conflict)
          (format "Multiple constructors for ~a exist with compatible definitions for the given arguments: ~a"
                  n as)))
       n src)))
  
  ;class-access-error: symbol type src -> void
  (define (class-access-error kind name src)
    (let ((n (type->ext-name name)))
      (raise-error n
                   (case kind
                     ((pro) (format "This constructor for ~a may only be used by ~a and its subclasses"
                                    n n))
                   ((pri) (format "This constructor for ~a may only be used by ~a"
                                  n n)))
                 n src)))

  ;ctor-throws-error: string type src -> void
  (define (ctor-throws-error thrown name src)
    (let ((n (type->ext-name name)))
      (raise-error n
                   (format "Constructor for ~a throws exception ~a, which is not caught or listed as thrown"
                           n thrown)
                   n src)))
  
  ;;Array Alloc error
  ;array-size-error: type type src -> void
  (define (array-size-error array dim src)
    (let ((a (type->ext-name array))
          (d (type->ext-name dim)))
      (raise-error a
                   (format "Allocation of array of ~a requires an integer for the size. Given ~a"
                           a d)
                   a src)))
    
  ;;Conditional Expression errors

  ;condition-error: type src -> void
  (define (condition-error type src)
    (let ((t (type->ext-name type)))
      (raise-error '?
                   (format "? requires that the first expression have type boolean. Given ~a" t)
                   '? src)))

  ;condition-mismatch-error: type type src -> void
  (define (condition-mismatch-error then else src)
    (raise-error '?
                 (format "? requires that the then and else branches have equivalent types: given ~a and ~a"
                         (type->ext-name then) (type->ext-name else))
                 '? src))
    
  ;;Array Access errors
  ;illegal-array-access: type src -> void
  (define (illegal-array-access type src)
    (let ((n (type->ext-name type)))
      (raise-error n
                   (format "Expression of type ~a accessed as if it were an array" n)
                   n src)))
  
  ;array-access-error: type type src -> void
  (define (array-access-error array idx src)
    (let ((n (type->ext-name array))
          (i (type->ext-name idx)))
      (raise-error n
                   (format "~a should be indexed with an integer, given ~a" n i)
                   n src)))
  
  ;;Unary error
  ;unary-error: symbol symbol type src -> void
  (define (unary-error op expect type src)
    (raise-error op
                 (format "~a expects a ~a, given ~a"
                         op (get-expected expect) (type->ext-name type))
                 op src))
  
  ;;Cast errors
  ;cast-error: symbol type type src -> void
  (define (cast-error kind cast exp src)
    (raise-error 'cast
                 (case kind
                   ((from-prim) 
                    (format "Illegal cast from primitive, ~a, to class or interface ~a"
                            (type->ext-name exp) (type->ext-name cast)))
                   ((from-ref)
                    (format "Illegal cast from class or interface ~a to primitive, ~a"
                            (type->ext-name exp) (type->ext-name cast))))
                 'cast src))
  
  ;;Instanceof errors
  ;instanceof-error: symbol type type src -> void
  (define (instanceof-error kind inst exp src)
    (let ((i (type->ext-name inst))
          (e (type->ext-name exp)))
      (raise-error 
       'instanceof
       (case kind
         ((not-subtype)
          (format "instanceof requires that its expression be a subtype of the given type: ~a is not a subtype of ~a"
                  e i))
         ((not-class)
          (format "instance of requires the expression to be compared to a class or interface: Given ~a" i))
         ((not-ref)
          (format "instance of requires the expression, compared to ~a, to be a class or interface: Given ~a"
                  i e)))
       'instanceof src)))
  
  ;;Assignment errors
  ;illegal-assignment: src -> void
  (define (illegal-assignment src)
    (raise-error '= "Assignment is only allowed in the constructor" '= src)) 

  (define (assignment-error op ltype rtype src)
    (raise-error op
                 (format "~a requires that the right hand type be equivalent to or a subtype of ~a: given ~a"
                         op (type->ext-name ltype) (type->ext-name rtype))
                 op src))
  
  (define (illegal-beginner-assignment)
    "Assignment expressions are only allowed in constructors")
  (define (assignment-convert-fail op d ltype rtype)
    (format "~a requires that the right hand type be equivalent to or a subtype of ~a: given ~a" 
            op ltype rtype))

  ;implicit import error
  ;class-lookup-error: string src -> void
  (define (class-lookup-error class src)
    (raise-error class
                 (format "Implicit import of class ~a failed as this class does not exist at the specified location"
                         class)
                 class src))
  
  (define check-location (make-parameter #f))
  
  (define raise-error (make-error-pass check-location))
      
  )