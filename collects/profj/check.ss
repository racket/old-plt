#cs
(module check mzscheme
  
  (require "ast.ss"
           "types.ss"
           "parameters.ss"
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
          (check-class def package-name type-recs)))
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
          (current-class (list "scheme-interactions")))
      (cond
        ((pair? prog)
         (for-each (lambda (p)
                     (check-interactions-types p level loc type-recs)) prog))
        ((var-init? prog) 
         (check-var-init (var-init-init prog)
                         (lambda (e) (check-expr e env level type-recs current-class #f #t))
                         (field-type prog)
                         (string->symbol (id-string (field-name prog)))
                         type-recs))
        ((var-decl? prog) (void))
        ((statement? prog)
         (check-statement prog null env null level type-recs current-class #f #t))
        ((expr? prog)
         (check-expr prog env level type-recs current-class #f #t))
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
                           (check-statement (initialize-block member) null (get-static-env field-env) null
                                            'full type-recs current-class #f #t)
                           (check-statement (initialize-block member) null field-env null 'full
                                            type-recs current-class #f #f)))
                      ((var-init? member)
                       (check-var-init (var-init-init member) 
                                       (lambda (e) (check-expr e field-env 'full type-recs current-class #f #t))
                                       (field-type member)
                                       (string->symbol (id-string (field-name member)))
                                       type-recs))
                      (else void)))
                  members))))
  
  ;check-static-members -> env type-records -> (member -> void)
  (define check-static-members
    (lambda (env type-recs)
      (lambda (members)
        void)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  ;;Checks      

  ;check-block: (list (U statement variable)) src type env type-records (U #f (list string))-> void
  (define check-block
    (lambda (stmts src return env type-recs current-class)
      (cond 
        ((null? stmts) void)
        ((or (var-decl? (car stmts)) (var-init? (car stmts)))
         (check-block (cdr stmts) src return 
                      (check-local-var (car stmts) env type-recs current-class) type-recs current-class))
        (else
         (check-statement (car stmts) return env null 'full type-recs current-class #t #f)
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
                             null
                             'full
                             type-recs
                             current-class
                             #t #f)
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
  (define (get-static-env env)
    (filter var-type-static? env))  

    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Statement checking functions
  
  ;;check-statement: statement type env (list type) symbol type-records (U #f string) bool bool-> type
  (define check-statement
    (lambda (statement return env exn-env level type-recs current-class ctor? static?)
      (let ((check-s (lambda (stmt env exn-env)
                       (check-statement stmt return env exn-env level type-recs current-class ctor? static?)))
            (check-e (lambda (exp env exn-env)
                       (check-expr exp env level type-recs current-class ctor? static?))))
      (cond
        ((ifS? statement) 
         (check-ifS (check-e (ifS-cond statement) env exn-env)
                    (expr-src (ifS-cond statement)))
         (check-s (ifS-then statement) env exn-env)
         (check-s (ifS-else statement) env exn-env))                                     
        ((throw? statement)
         (check-throw (check-e (throw-expr statement) env exn-env)
                      (expr-src (throw-expr statement))
                      exn-env
                      type-recs))
        ((return? statement)
         (check-return (return-expr statement)
                       return
                       (lambda (e) (check-e e env exn-env))
                       (return-src statement)
                       type-recs))
        ((while? statement) 
         (check-while (check-e (while-cond statement) env exn-env)
                      (expr-src (while-cond statement)))
         (check-s (while-loop statement) env exn-env))
        ((doS? statement) 
         (check-do (check-e (doS-cond statement) env exn-env)
                   (expr-src (doS-cond statement)))
         (check-s (doS-loop statement) env exn-env))
        ((for? statement)
         (check-for (for-init statement)
                    (for-cond statement)
                    (for-incr statement)
                    (for-loop statement)
                    (lambda (exp env) (check-e exp env exn-env))
                    (lambda (stmt env) (check-s stmt env exn-env))
                    env
                    type-recs))
        ((try? statement) 
         (check-try (try-body statement)
                    (try-catches statement)
                    (try-finally statement)
                    env
                    exn-env
                    check-s
                    type-recs))
        ((switch? statement)
         (check-switch (check-e (switch-expr statement) env exn-env)
                       (expr-src (switch-expr statement))
                       (switch-cases statement)
                       env
                       (lambda (e) (check-e e env exn-env))
                       (lambda (s env) (check-s s env exn-env))))
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
         (check-s (label-stmt statement) env exn-env))
        ((synchronized? statement)
         (check-e (synchronized-expr statement) env exn-env)
         (check-s (synchronized-stmt statement) env exn-env))
        ((statement-expression? statement)
         (check-e statement env exn-env))))))
  
  ;check-cond: symbol -> (type src -> void)
  (define (check-cond kind)
    (lambda (cond cond-src)
      (unless (eq? 'boolean cond)
        (raise-statement-error cond cond-src kind (statement-cond-not-bool kind)))))
        
  ;check-ifS: type src -> void
  (define check-ifS (check-cond 'if))

  ;check-throw: type src (list type) type-records -> void
  (define (check-throw exp-type src exn-env type-recs)
    (cond
      ((or (not (ref-type? exp-type))
           (not (is-subclass? exp-type throw-type type-recs)))
       (raise-statement-error exp-type src 'throw throw-not-throwable))
      ((not (is-subclass? exp-type (make-ref-type "RuntimeException" (list "java" "lang")) type-recs))
       (unless (ormap 
                (lambda (caught) (assignment-conversion caught exp-type exn-env type-recs))
                exn-env)
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
  
  ;check-for: forInit Expression (list Expression) Statement (Expression env -> type) (Statement env -> void) env 
  ;           type-records -> void
  (define (check-for init cond incr loop check-e check-s env type-recs)
    (let ((newEnv (if (and (not (null? init))
                           (field? (car init)))
                      (check-for-vars init env (lambda (e) (check-e e env)) type-recs)
                      (begin (for-each (lambda (e) (check-e e env)) init)
                             env))))
      ((check-cond 'for) (check-e cond newEnv) (expr-src cond))
      (map (lambda (e) (check-e e newEnv)) incr)
      (check-s loop newEnv)))

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

  ;check-try: statement (list catch) (U #f statement) env (list type) 
  ;           (statement env (list type) -> void) type-records -> void
  (define (check-try body catches finally env exn-env check-s type-recs)
    (let ((cought-types (map 
                         (lambda (catch)
                           (let ((type (field-type (catch-cond catch))))
                             (unless (and (ref-type? type)
                                          (is-subclass? type throw-type type-recs))
                               (raise-statement-error type (field-src (catch-cond catch)) 'catch catch-throwable))
                             type))
                         catches)))
      (check-s body env (append cought-types exn-env))
      (for-each (lambda (catch)
                  (let* ((field (catch-cond catch))
                         (name (id-string (field-name field))))
                    (if (lookup-var-in-env name env)
                        (raise-statement-error name (field-src field) (string->symbol name) name-already-defined)
                        (check-s (catch-body catch) 
                                 (add-var-to-env name (field-type field) #t #f env)
                                 exn-env))))
                catches)
      (when finally
        (check-s finally env exn-env))))

  ;Skipping proper checks of the statements + proper checking that constants aren't repeated
  ;check-switch: type src (list caseS) (expression -> type) (statement env -> void) -> void
  (define (check-switch expr-type expr-src cases env check-e check-s)
    (when (or (eq? expr-type 'long)
              (not (prim-integral-type? expr-type)))
      (raise-statement-error expr-type expr-src 'switch wrong-switch-type))
    (for-each (lambda (case)
                (let* ((constant (caseS-constant))
                       (cons-type (unless (eq? 'default constant) (check-e constant))))
                  (if (or (eq? 'default constant)
                          (type=? cons-type expr-type))
                      void
                      (raise-statement-error (list cons-type expr-type) (expr-src constant) 'switch incompatible-case))))
              cases))
    
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
    (format "Each case of a switch statement must be of the type of the expression. Given ~a: expected ~a" given expected))
  
  ;raise-statement-error: ast src symbol ( 'a -> string) -> void
  (define (raise-statement-error code src kind msg)
    (match code
      ;Covers statement-cond-not-bool throw-not-throwable thrown-not-declared
      ;array-not-expected catch-throwable wrong-switch-type
      ((? type? c) (raise-syntax-error kind (msg (type->ext-name c)) (make-so kind src)))
      ;Covers return-not-match init-incompatible incompatibe-type incompatible-case
      (((? type? fst) (? type? snd)) 
       (raise-syntax-error kind (msg (type->ext-name fst) (type->ext-name snd)) (make-so kind src)))
      ;name-already-defined
      ((? string? name)
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
                        (check-access exp check-sub-expr env type-recs current-class)))
      ((special-name? exp)
       (set-expr-type exp (check-special-name exp env #f)))
      
      ((call? exp)
       (set-expr-type exp (check-call exp
                                      (map check-sub-expr (call-args exp))
                                      check-sub-expr
                                      current-class
                                      level
                                      env
                                      type-recs
                                      ctor? ; #t
                                      static?))) ;#f
      ((class-alloc? exp)
       (set-expr-type exp
                      (check-class-alloc (class-alloc-name exp)
                                         (map check-sub-expr (class-alloc-args exp))
                                         (expr-src exp)
                                         type-recs
                                         current-class
                                         env)))
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
                                        ctor? ;#t
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

  ;;check-access: expression (expr -> type) env type-records (list string) -> type
  (define (check-access exp check-sub-expr env type-recs c-class)
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
               (begin 
                 (set! record 
                       (let ((name (var-access-class (field-access-access acc))))
                         (get-field-record fname
                                           (get-record 
                                            (send type-recs get-class-record name
                                                  ((get-importer type-recs) name type-recs 'full))
                                            type-recs)
                                           (lambda () (raise-error (list (expr-src exp)
                                                                         (make-ref-type name null)
                                                                         fname)
                                                                   field-not-found)))))))
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
                                                         (make-var-access #t c-class)))
                         (cdr acc))
                        (build-field-accesses
                         (make-access #f (expr-src exp)
                                      (make-field-access (make-special-name #f (expr-src exp)
                                                                            "this")
                                                         (car acc)
                                                         #f))
                         (cdr acc))))
                   (else (raise-error (list (id-src (car acc)) (id-string (car acc)))
                                      variable-not-found)))))
           (set-access-name! exp new-acc)
           (check-sub-expr exp))))))
  
  ;; field-lookup: string type expression type-records -> field-record
  (define (field-lookup fname obj-type obj type-recs)
    (let ((src (expr-src obj)))
      (cond
        ((reference-type? obj-type)
         (let ((obj-record (send type-recs get-class-record obj-type
                                 ((get-importer type-recs) obj-type type-recs 'full))))
           (get-field-record fname 
                             (get-record obj-record type-recs)
                             (lambda () 
                               (raise-error (list src obj-type fname) field-not-found)))))
        ((array-type? obj-type)
         (unless (equal? fname "length")
           (raise-error (list src obj-type fname) array-field))
         (make-field-record "length" `() `(array) 'int))
        (else (raise-error (list src obj-type) prim-field-acc)))))
  
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
                         (record (get-record 
                                  (send type-recs get-class-record name 
                                        ((get-importer type-recs) name type-recs 'full))
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
  
  ;check-special-name: expression env bool -> type
  (define (check-special-name exp env static?)
    (when static? 
      (raise-error (list (expr-src exp) "this") special-in-static))
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
              (unless (access? expr) (raise exn))
              (let ((members (car (find-static-class 
                                   (append (access-name expr) (list name))
                                   type-recs #f))))
                (unless (null? members)
                  (set-call-expr! call #f)
                  (let ((class (method-record-class (car members))))
                    (unless (equal? (car class) (car c-class))
                      (send type-recs add-req (make-req (car class) (cdr class))))))
                members)))
           (methods 
            (cond 
              ((special-name? name)
               (let ((n (special-name-name name)))
                 (unless ctor? (raise-error (list src n) ctor-not-ctor))
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
                      (else (raise-error (list (expr-src expr) call-exp level) prim-call)))))
                 (else 
                  (get-method-records (id-string name)
                                      (if static?
                                          (send type-recs get-class-record c-class)
                                          this))))))))
      
      (when (null? methods)
        (cond 
          ((eq? exp-type 'super) (raise-error (list src (id-string name)) super-meth-not-found))
          (exp-type (raise-error (list src (id-string name) exp-type)) meth-not-found)
          (else (raise-error (list src (id-string name)) local-meth-not-found))))

      (let* ((overload-list 
              (cond 
                ((eq? exp-type 'super) (list src (id-string name)))
                (exp-type (list src (id-string name)))
                (else
                 (list src
                       (if (special-name? name) 
                           (special-name-name name)
                           (id-string name))))))
             (method-record (resolve-overloading methods 
                                                 args
                                                 (lambda () (raise-error (append overload-list 
                                                                                 (list (length args))) call-arg-error))
                                                 (lambda () (raise-error (append overload-list (list args))
                                                                         call-conflict))
                                                 (lambda () (raise-error (append overload-list (list exp-type))
                                                                         full-meth-not-found))
                                                 type-recs))
             (mods (method-record-modifiers method-record))
             (err-list (list src (method-record-name method-record))))
        (when (memq 'abstract mods) (raise-error err-list abs-meth-called))
        (when (and (memq 'protected mods) (reference-type? exp-type) 
                   (not (is-subclass? this exp-type)))
          (raise-error err-list pro-meth-called))
        (when (and (memq 'private mods)
                   (reference-type? exp-type)
                   (not (eq? this (send type-recs get-class-record exp-type))))
          (raise-error err-list pri-meth-called))
        (set-call-method-record! call method-record)
        (method-record-rtype method-record))))
     
  ;;Skip package access controls
  ;; 15.9
  ;;check-class-alloc: name (list type) src type-records (list string) env -> type
  (define (check-class-alloc name args src type-recs c-class env)
    (let* ((type (java-name->type name type-recs))
           (class-record (send type-recs get-class-record type))
           (methods (get-method-records (ref-type-class/iface type) class-record))
           (err-list (list (name-src name) (string->symbol (ref-type-class/iface type)) type)))
      (unless (equal? (ref-type-class/iface type) (car c-class))
        (send type-recs add-req (make-req (ref-type-class/iface type) (ref-type-path type))))
      (when (memq 'abstract (class-record-modifiers class-record))
        (raise-error err-list class-alloc-abstract))
      (unless (class-record-class? class-record)
        (raise-error err-list class-alloc-interface))
      (let* ((const (resolve-overloading methods 
                                         args 
                                         (lambda () (raise-error err-list ctor-arg-error))
                                         (lambda () (raise-error err-list ctor-conflict))
                                         (lambda () (raise-error err-list ctor-not-found))
                                         type-recs))
             (mods (method-record-modifiers const))
             (this (lookup-this type-recs env)))
        (when (and (memq 'private mods) (not (eq? class-record this)))
          (raise-error err-list class-alloc-access-private))
        (when (and (memq 'protected mods) (not (is-subclass? this type)))
          (raise-error err-list class-alloc-access-pro))      
        type)))
  
  
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

  ;check-access errors
  (define (variable-not-found var)
    (format "reference to undefined identifier ~a" var))
  
  ;field-lookup errors
  (define (field-not-found type field)
    (format "field ~a not found for object with type ~a" field type))
  (define (array-field type field)
    (format "array ~a does not have a field ~a, only length" type field))
  (define (prim-field-acc type)
    (format "attempted to access the field of a ~a, instead of a class" type))
  
  ;special-name errors
  (define (special-in-static a)
    "use of 'this' is not allowed in static methods")

  ;;Call errors
  (define (prim-call type level)
    (format "attempted to call a method on ~a, instead of a ~a type"
            (case level 
              ((beginner) "class")
              ((intermediate) "class or interface")
              (else "class, interface, or array"))))
  (define (meth-not-found class meth)
    (format "~a does not contain any method named ~a" class meth))
  (define (super-meth-not-found meth)
    (format "super class does not contain any method named ~a" meth))
  (define (local-meth-not-found meth)
    (format "this class does not contain any method named ~a" meth))
  (define (ctor-not-ctor ct)
    (format "calls to ~a may only occur in other constructors" ct))
  (define (abs-meth-called meth)
    (format "it is illegal to call abstract method ~a" meth))
  (define (pro-meth-called meth)
    (format "it is illegal to call protected method ~a in this class" meth))
  (define (pri-meth-called meth)
    (format "it is illegal to call private method ~a in this class" meth))  
  (define (call-arg-error meth num)
    (format "No definition of ~a with ~a argument(s) found" meth num))
  (define (call-conflict meth args)
    (format "No definition of ~a with ~a arguments found" meth (if (null? args) "no" args)))
  (define (full-meth-not-found meth)
    (format "Method ~a is not found with the types of arguments given" meth))
  
  ;;Class Alloc errors
  (define (class-alloc-abstract type)
    (format "abstract class ~a may not be constructed" type))
  (define (class-alloc-interface type)
    (format "interface ~a may not be constructed" type))
  (define (class-alloc-access-private class)
    (format "Access to the specified constructor for ~a is restricted to itself" class))
  (define (class-alloc-access-pro class)
    (format "Access to the specified constructor for ~a is restricted to itself and its subclasses" class))
  (define (ctor-arg-error type) 
    (format "No constructor found for ~a with given number of arguments" type))
  (define (ctor-conflict class) 
    (format "Arguments to constructor for ~a select multiple constructors to call" class))
  (define (ctor-not-found class) 
    (format "Constructor for ~a is not found with the types of arguments given" class))
  
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