#cs
(module check mzscheme
  
  (require "ast.ss"
           "types.ss"
           "parameters.ss"
           "error-messaging.ss"
           (lib "match.ss")
           (lib "file.ss")
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
  ;; var-type => (make-var-type string type boolean boolean)
  (define-struct var-type (var type local? static?))
  
  ;; add-var-to-env: string type boolean boolean env -> env
  (define (add-var-to-env name type local? static? oldEnv)
    (make-environment (cons (make-var-type name type local? static?)
                            (environment-type-env oldEnv))
                      (environment-exn-env oldEnv)
                      (environment-label-env oldEnv)))
  
  ;;add-var-group (list var-type) env -> env
  (define (add-var-group vars env)
    (make-environment (append vars (environment-type-env env))
                      (environment-exn-env env)
                      (environment-label-env env)))

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
  
  ;;add-exn-to-env: type env -> env
  (define (add-exn-to-env exn env)
    (make-environment (environment-type-env env)
                      (cons exn (environment-exn-env env))
                      (environment-label-env env)))
  
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
          (check-interface def package-name type-recs)
          (check-class def package-name level type-recs)))
    (packages (cons def (packages)))
    (when (not (null? (check-list)))
      (check-defs (car (check-list)) level type-recs)))
  
  ;check-interactions-types: ast symbol location type-records -> void
  (define (check-interactions-types prog level loc type-recs)
    (check-location loc)
    (send type-recs set-location! 'interactions)
    (send type-recs set-class-reqs null)
    (let ((env (add-var-to-env "this" (make-ref-type "scheme-interactions" null) #t #f
                               (create-field-env (send type-recs get-interactions-fields)
                                                 empty-env)))
          (c-class (list "scheme-interactions")))
      (cond
        ((pair? prog)
         (for-each (lambda (p)
                     (check-interactions-types p level loc type-recs)) prog))
        ((var-init? prog) 
         (check-var-init (var-init-init prog)
                         (lambda (e) 
                           (check-expr e env level type-recs c-class #f #t))
                         (field-type prog)
                         (string->symbol (id-string (field-name prog)))
                         type-recs))
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
                     (add-var-to-env "this" this-ref #t #f empty-env)
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
  
  ;check-members: (list member) env symbol type-records (U #f (list string)) -> void
  (define (check-members members env level type-recs c-class)
    (let* ((fields (class-record-fields (send type-recs get-class-record 
                                              (var-type-type (lookup-var-in-env "this" env)))))
           (field-env (create-field-env fields env)))
      (for-each (lambda (member)
                  (cond
                    ((method? member)
                     (if (memq 'static (map modifier-kind (method-modifiers member)))
                         (check-method member (get-static-fields-env field-env) level type-recs c-class #t)
                         (check-method member field-env level type-recs c-class #f)))
                    ((initialize? member)
                     (if (initialize-static member)
                         (check-statement (initialize-block member) 'void 
                                          (get-static-fields-env field-env)
                                          level type-recs c-class #f #t #f #f)
                         (check-statement (initialize-block member) 'void field-env level
                                          type-recs c-class #f #f #f #f)))
                    ((var-init? member)
                     (let ((static? (memq 'static (map modifier-kind (field-modifiers member)))))
                       (check-var-init (var-init-init member)
                                       (lambda (e) 
                                         (check-expr e 
                                                     (if static? 
                                                         (get-static-fields-env field-env)
                                                         field-env)
                                                     level type-recs c-class #f 
                                                     static?))
                                       (field-type member)
                                       (string->symbol (id-string (field-name member)))
                                       type-recs)))
                    (else void)))
                members)))
  
  ;check-static-members -> env type-records -> (member -> void)
  (define check-static-members
    (lambda (env type-recs)
      (lambda (members)
        void)))  

  
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
                                           env))))))
  
  ;get-static-fields-env: env -> env
  (define (get-static-fields-env env)
    (make-environment (filter var-type-static? (environment-type-env env))
                      (environment-exn-env env)
                      (environment-label-env env)))

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
            (raise-member-error (id-src name)
                                (list sym-name (memq 'abstract mods))
                                impl-for-abs))                                
          (begin
            (when (and (not (eq? return 'void))
                       (not (reachable-return? body)))
              (raise-member-error (id-src name) sym-name no-reachable-return)) 
            (check-statement body
                             return
                             (build-method-env (method-parms method) env type-recs)
                             level
                             type-recs
                             c-class
                             ctor?
                             static?
                             #f
                             #f)))))
  
  
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
                                         env)
                         type-recs))))

  ;Should check that all branches have a return; as Java requires
  ;Always returns #t  
  ;reachable-return?: statement -> bool
  (define (reachable-return? body) #t)

  ;check-var-init: expression (expression -> type) type symbol type-records -> type
  (define (check-var-init init check-e dec-type name type-recs)
    (if (array-init? init)
        (if (array-type? dec-type)
            (check-array-init (array-init-vals init) check-e 
                              (array-type-type dec-type) name type-recs)
            (raise-statement-error dec-type (array-init-src init) name array-not-expected))
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
                       (raise-statement-error (list dec-type new-type) 
                                              (expr-src e) name init-incompatible))))
                 inits)
       (make-array-type dec-type 1))))
  
  ;Member errors
  
  (define (no-reachable-return method)
    (format "method ~a does not have a reachable return" method))

  (define (impl-for-abs name abs?)
    (format "~a method ~a must not have an implementation. A ';' should appear after the header"
            (if abs? 'abstract 'native) name))
  
  (define (raise-member-error src code msg)
    (match code
      ((? symbol? code)
       (raise-syntax-error code (msg code) (make-so code src)))
      (((? symbol? name) (? boolean? cond))
       (raise-syntax-error name (msg name cond) (make-so name src)))
      (_
       (error 'raise-member-error "Given ~a" code))))
  
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
         (check-s-no-change (ifS-else statement)))                           
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
                         env))
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
        (raise-statement-error cond cond-src kind (statement-cond-not-bool kind)))))
        
  ;check-ifS: type src -> void
  (define check-ifS (check-cond 'if))

  ;check-throw: type src env type-records -> void
  (define (check-throw exp-type src env type-recs)
    (cond
      ((or (not (ref-type? exp-type))
           (not (is-subclass? exp-type throw-type type-recs)))
       (raise-statement-error exp-type src 'throw throw-not-throwable))
      ((not (is-subclass? exp-type runtime-exn-type type-recs))
       (unless (lookup-exn exp-type env type-recs)
         (raise-statement-error exp-type src 'throw thrown-not-declared)))
      (else
       (send type-recs add-req (make-req "Throwable" (list "java" "lang"))))))

  ;check-return: expression type (expression -> type) src type-records -> void
  (define (check-return ret-expr return check src type-recs)
    (cond
      ((and ret-expr (not (eq? 'void return)))
       (let ((ret-type (check ret-expr)))
         (unless (assignment-conversion return ret-type type-recs)
           (raise-statement-error (list ret-type return) (expr-src ret-expr) 'return return-not-match))))
      ((and ret-expr (eq? 'void return))
       (raise-statement-error return (expr-src ret-expr) 'return return-on-void))
      ((and (not ret-expr) (not (eq? 'void return)))
       (raise-statement-error return src 'return no-val-to-return))))
  
  ;check-while: type src -> void
  (define check-while (check-cond 'while))
  
  ;check-do: type src void -> void
  (define check-do (check-cond 'do))
  
  ;check-for: forInit Expression (list Expression) Statement (Expression env -> type) (Statement env bool bool-> void) env 
  ;           type-records bool -> void
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
                        (check-local-var (car vars) env check-e types) types)))
  
  ;check-local-var: field env (expression -> type) type-records -> env
  (define (check-local-var local env check-e type-recs)
    (let* ((is-var-init? (var-init? local))
           (name (id-string (field-name local)))
           (sym-name (string->symbol name))
           (type (type-spec-to-type (field-type local) type-recs)))
      (when (lookup-var-in-env name env)
        (raise-statement-error name (field-src local) sym-name name-already-defined))
      (when is-var-init?
        (let ((new-type (check-var-init (var-init-init local) check-e type sym-name type-recs)))
          (unless (assignment-conversion type new-type type-recs)
            (raise-statement-error (list new-type type) (var-init-src local) sym-name incompatible-type))))
      (add-var-to-env name type #t #f env)))  
  

  ;check-try: statement (list catch) (U #f statement) env (statement env -> void) type-records -> void
  (define (check-try body catches finally env check-s type-recs)
    (let ((new-env
           (let loop ((catches catches) (new-env env))
             (if (null? catches)
                 new-env
                 (let* ((catch (car catches))
                        (type (field-type (catch-cond catch))))
                   (unless (and (ref-type? type)
                                (is-subclass? type throw-type type-recs))
                     (raise-statement-error type (field-src (catch-cond catch)) 'catch catch-throwable))
                   (loop (cdr catches) (add-exn-to-env type env)))))))
      (check-s body new-env)
      (for-each (lambda (catch)
                  (let* ((field (catch-cond catch))
                         (name (id-string (field-name field))))
                    (if (lookup-var-in-env name env)
                        (raise-statement-error name (field-src field) (string->symbol name) name-already-defined)
                        (check-s (catch-body catch) 
                                 (add-var-to-env name (field-type field) #t #f env)))))
                catches)
      (when finally (check-s finally env))))

  ;Skipping proper checks of the statements + proper checking that constants aren't repeated
  ;check-switch: type src (list caseS) bool env (expression -> type) (statement env bool bool -> void) -> void
  (define (check-switch expr-type expr-src cases in-loop? env check-e check-s)
    (when (or (eq? expr-type 'long)
              (not (prim-integral-type? expr-type)))
      (raise-statement-error expr-type expr-src 'switch wrong-switch-type))
    (for-each (lambda (case)
                (let* ((constant (caseS-constant))
                       (cons-type (unless (eq? 'default constant) (check-e constant))))
                  (if (or (eq? 'default constant)
                          (type=? cons-type expr-type))
                      void
                      (raise-statement-error (list cons-type expr-type) 
                                             (expr-src constant) 
                                             'switch 
                                             incompatible-case))))
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
         (raise-statement-error (id-string label) (id-src label) 'break (no-label 'break))))
      ((or (not in-loop?) (not in-switch?))
       (raise-statement-error 'break src 'break illegal-break))))

  ;check-continue: (U id #f) src env bool -> void
  (define (check-continue label src env in-loop?)
    (cond
      (label
       (unless (lookup-label (id-string label) env)
         (raise-statement-error (id-string label) (id-src label) 'continue (no-label 'continue))))
      ((not in-loop?)
       (raise-statement-error 'continue src 'continue continue-not-in-loop))))
  
  ;check-label: statement string (statement env -> void) env -> void
  (define (check-label stmt label check-s env)
    (check-s stmt (add-label-to-env env)))

  ;check-synchronized: type src -> void
  (define (check-synchronized e-type e-src)
    (unless (reference-type? e-type)
      (raise-statement-error e-type e-src 'synchronized synch-wrong-exp)))  
  
  ;Statement error messages
  
  (define (statement-cond-not-bool kind)
    (lambda (given)
      (format "condition for ~a must be boolean, given ~a" kind given)))
  
  (define (throw-not-throwable given)
    (format "throw expression must be a subtype of class Throwable: given ~a" given))
  (define (thrown-not-declared given)
    (format "thrown type ~a must be declared in the throws clause or in a catch clause of the surrounding try block"
            given))

  (define (return-not-match given expected)
    (format "type ~a of returned expression must be equal to or a subclass of declared return ~a"
            given expected))
  (define (return-on-void given)
    "No value is expected to be returned from void method. Value found")
  (define (no-val-to-return expected)
    (format "Expected a return value of type ~a, no value was given" expected))
  
  (define (name-already-defined given)
    (format "Redefinition of ~a is not allowed. Another name must be chosen" given))
  (define (incompatible-type given expected)
    (format "Variable declared to be of type ~a, which is incompatible with given type ~a" expected given))
  
  (define (init-incompatible given expected)
    (format "types of all expressions in array initialization must be compatible with declared type. 
             ~a is not compatible with declared type ~a" given expected))
  (define (array-not-expected expected)
    (format "variable declared to be of type ~a, given an array" expected))

  (define (catch-throwable given)
    (format "catch clauses must be given an argument that is a subclass of Throwable. Given ~a" given))
  
  (define (wrong-switch-type given)
    (format "Switch expression must be of type byte, short, int or char. Given: ~a" given))
  (define (incompatible-case given expected)
    (format "Each switch case must be the same type as switch expression. Given ~a: expected ~a" given expected))

  (define (no-label kind)
    (lambda (label)
      (format "~a attempting to ~a to label ~a when no statement has that label" kind kind label)))
  (define (illegal-break given)
    "this break statement must be in either a loop or a switch")
  (define (continue-not-in-loop given)
    "this continue statement must be in a loop")
  
  (define (synch-wrong-exp given)
    (format "The expression for synchronization must be a subtype of Object. Given ~a" given))
  
  ;raise-statement-error: ast src symbol ( 'a -> string) -> void
  (define (raise-statement-error code src kind msg)
    (match code
      ;Covers statement-cond-not-bool throw-not-throwable thrown-not-declared
      ;array-not-expected catch-throwable wrong-switch-type synch-wrong-exp
      ((? type? c) (raise-syntax-error kind (msg (type->ext-name c)) (make-so kind src)))
      ;Covers return-not-match init-incompatible incompatibe-type incompatible-case
      (((? type? fst) (? type? snd)) 
       (raise-syntax-error kind (msg (type->ext-name fst) (type->ext-name snd)) (make-so kind src)))
      ;name-already-defined no-label
      ((? string? name)
       (raise-syntax-error kind (msg name) (make-so kind src)))
      ;illegal-break continue-not-in-loop
      ((? symbol? name)
       (raise-syntax-error kind (msg name) (make-so kind src)))
      (_ (error 'raise-statement-error "Given ~a" code)))) 
                                        
  
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
       (if (and (not (or (eq? level 'beginner) (eq? level 'intermediate)))
                (eq? '+ op) (or (eq? 'string l) (eq? 'string r)))
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
  (define find-static-class 
    (lambda (accs type-recs)
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
                  (raise-error (list (id-src (car accs)) (cadr found?)) class-lookup-failed)))))))
  
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
  
  ;get-classpath: -> (list string)
  (define (get-classpath)
    (cons (build-path 'same)
          (get-preference 'classpath
                          (lambda ()
                            (let ((libs (map (lambda (p) (build-path "drj" "libs"))
                                             (current-library-collection-paths))))
                              (put-preferences `(classpath) (list libs))
                              libs)))))

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
              (unless (or (access? expr) (eq? level 'full)) (raise exn))
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
                   (not (is-subclass? this exp-type)))
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
        (when (and (memq 'protected mods) (not (is-subclass? this type)))
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
  (define (check-array-access ref-type idx-type src type-recs)
    (send type-recs add-req (make-req 'array null))
    (unless (array-type? ref-type)
      (raise-error (list src 'access ref-type) array-ac-non-array))
    (when (or (not (prim-integral-type? idx-type))
              (not (eq? 'int (unary-promotion idx-type))))
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

  ;implicit import error
  (define (class-lookup-failed class)
    (format "Implicit import of class ~a failed as this class does not exist at the specified location"
            class))
  
  (define (old-raise-error wrong-code make-msg)
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
      [((? symbol? op) src) (raise-syntax-error #f (make-msg) (make-so op src))]

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
      ;Covers prim-call
      [(src (? type? type) (? level? level))
       (raise-syntax-error #f (make-msg (type->ext-name type) level) 
                           (make-so 'call src))]
      ;Covers array-ac-idx
      [(src (? type? type) (? symbol? expt)) 
       (raise-syntax-error #f
                           (make-msg (get-expected expt) (type->ext-name type))
                           (make-so 'index src))]
      ;Covers super-meth-* local-meth-* *-meth-called ctor-not-ctor special* variable-not-found
      ;       class-lookup-failed
      [(src (? string? name))
       (raise-syntax-error #f (make-msg name) (make-so (string->symbol name) src))]
      ;Covers call-arg-error
      [(src (? string? name) (? number? num))
       (raise-syntax-error #f (make-msg name num) (make-so (string->symbol name) src))]
      ;Covers call-conflict
      [(src (? string? name) (? list? args))
       (raise-syntax-error #f (make-msg name (map type->ext-name args)) (make-so (string->symbol name) src))]
      ;Covers meth-not-found
      [(src (? string? name) (? type? type))
       (raise-syntax-error #f (make-msg (type->ext-name type) name)
                           (make-so (string->symbol name) src))]
      ;Covers field-not-found array-field
      [(src (? type? type) (? string? name))
       (raise-syntax-error #f (make-msg (type->ext-name type) name) 
                           (make-so (string->symbol name) src))]
      
      ;Covers prim-field-acc
      [(src (? type? type))
       (raise-syntax-error #f (make-msg type) (make-so 'field-access src))]

      [_ 
       (error 'type-error "This file has a type error in the statements but more likely expressions")]))
      
  ;level?: ~a -> bool
  (define (level? l)
    (memq l '(beginner intermediate advanced full)))
  
  ;type?: ~a -> bool
  (define (type? t)
    (or (reference-type? t)
        (array-type? t)
        (eq? t 'boolean)
        (prim-numeric-type? t)))
  
  (define check-location (make-parameter #f))
  
  (define raise-error (make-error-pass check-location))
      
  )