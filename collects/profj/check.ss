#cs
(module check mzscheme
  
  (require "ast.ss"
           "types.ss"
           "parameters.ss"
           "error-messaging.ss"
           "restrictions.ss"
           "profj-pref.ss"
           "build-info.ss"
           (lib "class.ss")
           (lib "list.ss")
           (lib "string.ss"))
  (provide check-defs check-interactions-types)
  
  ;symbol-remove-last: symbol->symbol
  (define (symbol-remove-last s)
    (let ((str (symbol->string s)))
      (string->symbol (substring str 0 (sub1 (string-length str))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Environment functions

  ;env =>
  ;(make-environment (list var-type) (list type) (list string))
  (define-struct environment (types exns labels))
  
  ;Constant empty environment
  (define empty-env (make-environment null null null))

  ;; var-type => (make-var-type string type properties)
  (define-struct var-type (var type properties))
  
  ;;Environment variable properties
  ;;(make-properties bool bool bool bool bool bool)
  (define-struct properties (local? field? static? settable? final? usable?))
  (define parm (make-properties #t #f #f #t #f #t))
  (define final-parm (make-properties #t #f #f #f #t #t))
  (define obj-field (make-properties #f #t #f #t #f #t))
  (define (final-field settable)
    (make-properties #f #t #f settable #t #t))
  (define class-field (make-properties #f #t #t #f #t #t))
  (define (final-class-field settable)
    (make-properties #f #t #t settable #t #t))
  (define inherited-conflict (make-properties #f #t #f #f #f #f))
  
  ;; add-var-to-env: string type properties env -> env
  (define (add-var-to-env name type properties oldEnv)
    (make-environment (cons (make-var-type name type properties) (environment-types oldEnv))
                      (environment-exns oldEnv)
                      (environment-labels oldEnv)))
  
  ;; lookup-var-in-env: string env -> (U var-type boolean)
  (define (lookup-var-in-env name env)
    (letrec ((lookup
              (lambda (env)
                (and (not (null? env))
                     (if (string=? name (var-type-var (car env)))
                         (car env)
                         (lookup (cdr env)))))))
      (lookup (environment-types env))))
  
  ;remove-var-from-env string env -> env
  (define (remove-var-from-env name env)
    (letrec ((remove-from-env
              (lambda (env)
                (cond
                  ((null? env) null)
                  ((equal? name (var-type-var (car env)))
                   (remove-from-env (cdr env)))
                  (else (cons (car env) (remove-from-env (cdr env))))))))
      (make-environment (remove-from-env (environment-types env))
                        (environment-exns env)
                        (environment-labels env))))
  
  ;;add-exn-to-env: type env -> env
  (define (add-exn-to-env exn env)
    (make-environment (environment-types env)
                      (cons exn (environment-exns env))
                      (environment-labels env)))
  
  ;;add-exns-to-env: (list type) env -> env
  (define (add-exns-to-env exns env)
    (if (null? exns)
        env
        (add-exns-to-env (cdr exns)
                         (add-exn-to-env (car exns) env))))
  
  ;;lookup-exn: type env type-records symbol-> bool
  (define (lookup-exn type env type-recs level)
    (ormap (lambda (lookup)
             (assignment-conversion lookup type type-recs))
           (environment-exns env)))
  
  ;;add-label-to-env: string env -> env
  (define (add-label-to-env label env)
    (make-environment (environment-types env)
                      (environment-exns env)
                      (cons label (environment-labels env))))
  
  ;;lookup-label: string env -> bool
  (define (lookup-label label env)
    (member label (environment-labels env)))
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Generic helper functions
  
  ;; set-expr-type: expr type -> type
  (define (set-expr-type exp t)
    (set-expr-types! exp t) t)

  ;lookup-this type-records env -> class-record
  (define (lookup-this type-recs env)
    (let ((this (lookup-var-in-env "this" env)))
      (if this 
          (send type-recs get-class-record (var-type-type this))
          interactions-record)))

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
          (check-interface def package-name (def-level def) type-recs)
          (check-class def package-name (def-level def) type-recs empty-env)))
    (packages (cons def (packages)))
    (when (not (null? (check-list)))
      (check-defs (car (check-list)) level type-recs)))
  
  ;check-interactions-types: ast symbol location type-records -> void
  (define (check-interactions-types prog level loc type-recs)
    (check-location loc)
    (send type-recs set-location! 'interactions)
    (send type-recs set-class-reqs null)
    (let ((env (create-field-env (send type-recs get-interactions-fields) empty-env "scheme-interactions"))
          (c-class (list "scheme-interactions")))
      (cond
        ((pair? prog)
         (for-each (lambda (p)
                     (check-interactions-types p level loc type-recs)) prog))
        ((var-init? prog) 
         (let ((name (id-string (field-name prog))))
           (check-var-init (var-init-init prog)
                           (lambda (e) 
                             (check-expr e (remove-var-from-env name env) level type-recs c-class #f #t #t))
                           (type-spec-to-type (field-type prog) #f level type-recs)
                           (string->symbol name)
                           type-recs)))
        ((var-decl? prog) (void))
        ((statement? prog)
         (check-statement prog null env level type-recs c-class #f #t #f #f #t))
        ((expr? prog)
         (check-expr prog env level type-recs c-class #f #t #t))
        (else
         (error 'check-interactions "Internal error: check-interactions-types got ~a" prog)))))
  
  ;check-class: class-def (list string) symbol type-records env -> void
  (define (check-class class package-name level type-recs class-env)
    (let ((old-reqs (send type-recs get-class-reqs))
          (name (id-string (def-name class))))
      ;    (send type-recs set-location! (def-file class))
      (send type-recs set-class-reqs (def-uses class))
      (let ((this-ref (make-ref-type name package-name)))
        (check-members (def-members class)
                       (add-var-to-env "this" this-ref parm class-env)
                       level
                       type-recs 
                       (cons name package-name)
                       #f 
                       (memq 'abstract (map modifier-kind (header-modifiers (def-header class))))
                       (def-kind class)
                       (if (null? (header-extends (def-header class))) #f
                           (name-src (car (header-extends (def-header class)))))
                       ))
      (set-def-uses! class (send type-recs get-class-reqs))
      (send type-recs set-class-reqs old-reqs)))

  ;check-interface: interface-def (list string) symbol type-recs -> void
  (define (check-interface iface p-name level type-recs)
    (let ((old-reqs (send type-recs get-class-reqs)))
;    (send type-recs set-location! (def-file iface))
      (send type-recs set-class-reqs (def-uses iface))
      (check-members (def-members iface) empty-env level type-recs 
                     (cons (id-string (def-name iface)) p-name) #t #f (def-kind iface) #f)
      (set-def-uses! iface (send type-recs get-class-reqs))
      (send type-recs set-class-reqs old-reqs)))
  
  ;check-inner def symbol type-records (list string) env -> void
  (define (check-inner-def def level type-recs c-class env)
    (let ((p-name (cdr c-class)))
      (when (or (eq? (def-kind def) 'anon) (eq? (def-kind def) 'statement))
        (build-inner-info def p-name level type-recs (def-file def) #t))
      (if (interface-def? def)
        (check-interface def p-name level type-recs)
        (check-class def p-name level type-recs (add-var-to-env "encl-this-1" 
                                                                (var-type-type (lookup-this type-recs env))
                                                                final-parm env)))
      ;; Propagate uses in internal defn to enclosing defn:
      (for-each (lambda (use)
                  (add-required c-class (req-class use) (req-path use) type-recs))
                (def-uses def))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Member checking methods
  
  ;check-members: (list member) env symbol type-records (list string) bool bool src-> void
  (define (check-members members env level type-recs c-class iface? abst-class? class-kind extend-src)
    (let* ((class-record (lookup-this type-recs env))
           (fields (class-record-fields class-record))
           (field-env (create-field-env fields env (car c-class)))
           (ctor-throw-env (if iface? field-env
                               (consolidate-throws 
                                (get-constructors (class-record-methods class-record)) field-env)))
           (static-env (get-static-fields-env field-env))
           (setting-fields null)
           (inherited-fields null))
      (when (eq? level 'beginner)
        (let ((parent (send type-recs get-class-record (car (class-record-parents class-record)))))
          (when (memq 'abstract (class-record-modifiers parent))
            (set! inherited-fields (filter (lambda (f) (not (field-record-init? f)))
                                           (get-field-records parent))))))
      (let loop ((rest members) (statics empty-env) (fields env))
        (unless (null? rest)
          (let ((member (car rest)))
            (cond
              ((method? member)
               (if (memq 'static (map modifier-kind (method-modifiers member)))
                   (check-method member static-env level type-recs c-class #t iface?)
                   (check-method member field-env level type-recs c-class #f iface?))
               (loop (cdr rest) statics fields))
              ((initialize? member)
               (if (initialize-static member)
                   (check-statement (initialize-block member) 'void static-env
                                    level type-recs c-class #f #t #f #f #f)
                   (check-statement (initialize-block member) 'void ctor-throw-env level
                                    type-recs c-class #f #f #f #f #f))
               (loop (cdr rest) statics fields))
              ((field? member)
               (let ((static? (memq 'static (map modifier-kind (field-modifiers member))))
                     (name (id-string (field-name member)))
                     (type (type-spec-to-type (field-type member) c-class level type-recs)))
                 (if (var-init? member)
                     (check-var-init (var-init-init member)
                                     (lambda (e) 
                                       (check-expr e 
                                                   (if static? statics fields)
                                                   level type-recs c-class #f 
                                                   static? #f))
                                     type
                                     (string->symbol name)
                                     type-recs)
                     (when (field-needs-set? member level abst-class?)
                       (set! setting-fields (cons member setting-fields))))
                 (if static?
                     (loop (cdr rest) 
                           (add-var-to-env name type class-field statics) 
                           (add-var-to-env name type class-field fields))
                     (loop (cdr rest) statics 
                           (add-var-to-env name type obj-field fields)))))
              ((def? member)
               (check-inner-def member level type-recs c-class field-env)
               (loop (cdr rest) statics fields))
              ))))
      (let ((assigns (get-assigns members level (car c-class)))
            (static-assigns (get-static-assigns members level)))
        (when (eq? level 'beginner)
          (for-each (lambda (f)
                      (andmap (lambda (assign)
                                (inherited-field-set? f assign extend-src))
                              assigns))
                    inherited-fields))
        (for-each (lambda (field)
                    (if (memq 'static (map modifier-kind (field-modifiers field)))
                        (andmap
                         (lambda (assign)
                           (field-set? field assign (car c-class) level #t)) static-assigns)
                        (andmap
                         (lambda (assign)
                           (field-set? field assign (car c-class) level #f)) assigns)))
                  setting-fields))))
    
  ;create-field-env: (list field-record) env string -> env
  (define (create-field-env fields env class)
    (cond
      ((null? fields) env)
      (else
       (let* ((field (car fields))
              (name (field-record-name field))
              (in-env? (lookup-var-in-env name env))
              (static? (memq 'static (field-record-modifiers field)))
              (final? (memq 'final (field-record-modifiers field)))
              (current? (equal? class (car (field-record-class field)))))
         (add-var-to-env name
                         (field-record-type field)
                         (cond
                           ((and in-env? (not current?)) inherited-conflict)
                           ((and (not static?) (not final?)) obj-field)
                           ((and (not static?) final?) (final-field current?))
                           ((and static? (not final?)) class-field)
                           ((and static? final?) (final-class-field current?)))
                         (create-field-env (cdr fields) env class))))))
  
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
    (make-environment (filter (lambda (t) (properties-static? (var-type-properties t)))
                              (environment-types env))
                      (environment-exns env)
                      (environment-labels env)))

  ;field-needs-set?: field symbol bool-> bool
  (define (field-needs-set? field level abst-class?)
    (cond
      ((and (eq? level 'beginner) (not abst-class?) #t))
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
       (if (eq? (type-spec-name (method-type (car members))) 'ctor)
           (if (block? (method-body (car members)))
               (get-b-assigns (block-stmts (method-body (car members))) class)
               null)
           (get-beginner-assigns (cdr members) class)))))
  
  ;get-b-assigns: (list statement) string-> (list assignment)
  (define (get-b-assigns stmts class)
    (cond
      ((null? stmts) null)
      ((ifS? (car stmts)) 
       (beginner-ctor-error class (car stmts) (ifS-src (car stmts))))
      ((return? (car stmts)) 
       (beginner-ctor-error class (car stmts) (ifS-src (car stmts))))
      (else (append (get-b-assigns-expr (car stmts) class)
                    (get-b-assigns (cdr stmts) class)))))
  
  ;get-b-assigns-expr: Expression string -> assignment
  (define (get-b-assigns-expr body class)
    (cond
      ((assignment? body) 
       (unless (and (field-access? (access-name (assignment-left body)))
                    (special-name? (field-access-object (access-name (assignment-left body))))
                    (expr-src (field-access-object (access-name (assignment-left body)))))
         (beginner-assn-error 'not-left-this (expr-src (assignment-left body)))) 
       (when (and (access? (assignment-right body))
                  (field-access? (access-name (assignment-right body)))
                  (special-name? (field-access-object (access-name (assignment-right body)))))
         (beginner-assn-error 'right-this (expr-src (assignment-right body))))
       (list body))
      ((call? body) 
       (if (expr-src body)
           (beginner-ctor-error class body (expr-src body))
           null))
      (else 
       (beginner-ctor-error class body (expr-src body)))))
  
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
      ((array-alloc-init? exp) (get-init-assigns (array-init-vals (array-alloc-init-init exp))))
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
  
  ;get-init-assigns: (list (U Expression array-init)) -> (list assignment)
  (define (get-init-assigns inits)
    (cond
      ((null? inits) null)
      ((expr? (car inits))
       (apply append (map get-assigns-exp inits)))
      (else
       (apply append (map get-init-assigns (map array-init-vals inits))))))
  
          
  (define (get-static-assigns m l) null)
  
  ;field-set?: field (list assignment) string symbol bool -> bool
  (define (field-set? field assigns class level static?)
    (if (null? assigns) 
        (field-not-set-error (field-name field)
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
  
  ;inherited-field-set? field-record (list assignment) src -> bool
  (define (inherited-field-set? field assigns src)
    (if (null? assigns)
        (inherited-field-not-set-error (field-record-name field) src)
        (let* ((assign (car assigns))
               (left (assignment-left assign)))
          (or (cond
                ((local-access? left)
                 (equal? (id-string (local-access-name left))
                         (field-record-name field)))
                ((field-access? left)
                 (and (special-name? (field-access-object left))
                      (equal? "this" (special-name-name (field-access-object left)))
                      (equal? (id-string (field-access-field left)) (field-record-name field)))))
              (inherited-field-set? field (cdr assigns) src)))))
  
  (define (inherited-field-not-set-error name src)
    (raise-error (string->symbol name)
                 (format "Inherited field ~a must be set in the constructor for the current class" name)
                 (string->symbol name) src))
  
  ;check-method: method env type-records (list string) boolean boolean-> void
  (define (check-method method env level type-recs c-class static? iface?)
    (let* ((ctor? (eq? 'ctor (type-spec-name (method-type method))))
           (name (method-name method))
           (sym-name (string->symbol (id-string name)))
           (body (method-body method))
           (mods (map modifier-kind (method-modifiers method)))
           (return (if ctor? 
                       'void
                       (type-spec-to-type (method-type method) c-class level type-recs))))
      (when iface? (set! mods (cons 'abstract mods)))
      (when (memq 'native mods)
        (send type-recs add-req (make-req (string-append (car c-class) "-native-methods") (cdr c-class))))
      (if (or (memq 'abstract mods) (memq 'native mods))
          (when body
            (method-error (if (memq 'abstract mods) 'abstract 'native) sym-name (id-src name)))
          (begin
            (when (not body) (method-error 'no-body sym-name (id-src name)))
            (when (and (not (eq? return 'void))
                       (or (not (memq 'abstract mods))
                           (not (memq 'native mods)))
                       (not (reachable-return? body)))
              (method-error 'no-reachable sym-name (id-src name)))
            (check-statement body
                             return
                             (add-exns-to-env (map (lambda (n)
                                                     (name->type n c-class (name-src n) level type-recs))
                                                   (method-throws method))
                                              (build-method-env (method-parms method) env level c-class type-recs))
                             level
                             type-recs
                             c-class
                             ctor?
                             static?
                             #f
                             #f
                             #f)
            ))))
  
  ;build-method-env: (list field) env symbol (list string) type-records-> env
  (define (build-method-env parms env level c-class type-recs)
    (cond
      ((null? parms) env)
      (else
       (build-method-env (cdr parms)
                         (add-var-to-env (id-string (field-name (car parms)))
                                         (type-spec-to-type (field-type (car parms)) c-class level type-recs)
                                         (if (memq 'final (map modifier-kind (field-modifiers (car parms))))
                                             final-parm
                                             parm)
                                         env)
                         level
                         c-class
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
       (if (null? (block-stmts body))
           #f
           (reachable-return? (list-ref (block-stmts body)
                                        (sub1 (length (block-stmts body)))))))
      ((break? body) #f)
      ((continue? body) #f)
      ((label? body) (reachable-return? (label-stmt body)))
      ((synchronized? body) (reachable-return? (synchronized-stmt body)))
      (else #f)))

  ;check-var-init: expression (expression -> type) type symbol type-records -> type
  (define (check-var-init init check-e dec-type name type-recs)
    (if (array-init? init)
        (if (array-type? dec-type)
            (begin
              (send type-recs add-req (make-req 'array null))
              (check-array-init (array-init-vals init) check-e 
                                (array-type-type dec-type) type-recs))
            (var-init-error name dec-type (array-init-src init)))
        (check-e init)))
  
  ;check-array-init (U (list array-init) (list Expression)) (expression->type) type type-records -> type
  (define (check-array-init inits check-e dec-type type-recs)
    (cond
      ((null? inits) (make-array-type dec-type 1))
      ((array-init? (car inits))
       (let ((array-types (map (lambda (a) 
                                 (check-array-init (array-init-vals a) check-e dec-type type-recs))
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
                   ((no-reachable) (format "method ~a does not have a reachable return" method))
                   ((abstract)
                    (let ((line1 
                           (format "abstract method ~a has an implementation, abstract methods maynot have implementations"
                                   method))
                          (line2 "Either a ';'should come after the header, or the method should not be abstract"))
                      (format "~a~n~a" line1 line2)))
                   ((native) (format "native method ~a has an implementation which is not allowed" method))
                   ((no-body) (format "method ~a has no implementation and is not abstract" method)))
                 method src))
  
  ;var-init-error: symbol type src -> void
  (define (var-init-error name dec-type src)
    (raise-error name
                 (format "Expected ~a to be of declared type ~a, given an array" 
                         name (type->ext-name dec-type))
                 name src))

  ;array-init-error: type type src -> void
  (define (array-init-error dec-type given src)
    (let ((d (type->ext-name dec-type))
          (g (type->ext-name given)))
      (raise-error g
                   (format "Error initializing declared array of ~a, given element with incompatible type ~a"
                           d g)
                   d src)))

  ;field-not-set-error: id string symbol src
  (define (field-not-set-error name class kind src)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (format "Field ~a from ~a must be set in the ~a and is not"
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
  
  ;beginner-assn-error: sym src -> void
  (define (beginner-assn-error kind src)
    (raise-error 
     '=
     (case kind
       ((not-left-this)
        "Constructor must assign the class's fields.  This expression is not a field of this class and maynot be assigned")
       ((right-this)
        "The constructor maynot assign fields with other of its fields. Other values must be used"))
     '= src))          

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Statement checking functions
  
  ;;check-statement: statement type env symbol type-records (U #f string) bool bool bool bool bool-> type
  (define (check-statement statement return env level type-recs c-c ctor? static? in-loop? in-switch? interactions?)
    (let* ((check-s (lambda (stmt env in-l? in-s?)
                      (check-statement stmt return env level type-recs c-c ctor? static? in-l? in-s? interactions?)))
           (check-s-env-change (lambda (smt env) (check-s smt env in-loop? in-switch?)))
           (check-s-no-change (lambda (stmt) (check-s stmt env in-loop? in-switch?)))
           (check-e (lambda (exp env)
                      (check-expr exp env level type-recs c-c ctor? static? interactions?)))
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
                      interactions?
                      type-recs))
        ((return? statement)
         (check-return (return-expr statement)
                       return
                       check-e-no-change
                       (return-src statement)
                       interactions?
                       level
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
                    level
                    c-c
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
                      level
                      c-c
                      type-recs))
        ((break? statement)
         (check-break (break-label statement)
                      (break-src statement)
                      in-loop?
                      in-switch?
                      level
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

  ;check-throw: type src env bool type-records -> void
  (define (check-throw exp-type src env interact? type-recs)
    (cond
      ((or (not (ref-type? exp-type))
           (not (is-eq-subclass? exp-type throw-type type-recs)))
       (throw-error 'not-throwable exp-type src))
      ((not (is-eq-subclass? exp-type runtime-exn-type type-recs))
       (unless (or interact? (lookup-exn exp-type env type-recs 'full))
         (throw-error 'not-declared exp-type src)))
      (else
       (send type-recs add-req (make-req "Throwable" (list "java" "lang"))))))

  ;check-return: expression type (expression -> type) src bool symbol type-records -> void
  (define (check-return ret-expr return check src interact? level type-recs)
    (cond
      (interact? (void))
      ((and ret-expr (not (eq? 'void return)))
       (let ((ret-type (check ret-expr)))
         (unless (assignment-conversion return ret-type type-recs)
           (return-error 'not-equal ret-type return src))))
      ((and ret-expr (eq? 'void return) (not (eq? level 'full)))
       (return-error 'void #f return src))
      ((and (not ret-expr) (not (eq? 'void return)))
       (return-error 'val #f return src))))
  
  ;check-while: type src -> void
  (define check-while (check-cond 'while))
  
  ;check-do: type src void -> void
  (define check-do (check-cond 'do))
  
  ;check-for: forInit Expression (list Expression) Statement (Expression env -> type) 
  ;           (Statement env bool bool-> void) env symbol (list string) type-records bool -> void
  (define (check-for init cond incr loop check-e check-s env level c-class type-recs in-switch?)
    (let ((newEnv (if (and (not (null? init))
                           (field? (car init)))
                      (check-for-vars init env (lambda (e) (check-e e env)) level c-class type-recs)
                      (begin (for-each (lambda (e) (check-e e env)) init)
                             env))))
      ((check-cond 'for) (check-e cond newEnv) (expr-src cond))
      (map (lambda (e) (check-e e newEnv)) incr)
      (check-s loop newEnv #t in-switch?)))

  ;check-for-vars: (list field) env (expression -> type) symbol (list string) type-records -> env
  (define (check-for-vars vars env check-e level c-class types)
    (if (null? vars)
        env
        (check-for-vars (cdr vars) 
                        (check-local-var (car vars) env check-e level c-class types) check-e level c-class types)))
  
  ;check-local-var: field env (expression -> type) symbol (list string) type-records -> env
  (define (check-local-var local env check-e level c-class type-recs)
    (let* ((is-var-init? (var-init? local))
           (name (id-string (field-name local)))
           (in-env? (lookup-var-in-env name env))
           (sym-name (string->symbol name))
           (type (type-spec-to-type (field-type local) c-class level type-recs)))
      (when (and in-env? (not (properties-field? (var-type-properties in-env?))))
        (illegal-redefinition (field-name local) (field-src local)))
      (when is-var-init?
        (let ((new-type (check-var-init (var-init-init local) check-e type sym-name type-recs)))
          (unless (assignment-conversion type new-type type-recs)
            (variable-type-error (field-name local) new-type type (var-init-src local)))))
      (add-var-to-env name type parm env)))

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
                    (if (and in-env? (not (properties-field? (var-type-properties in-env?))))
                        (illegal-redefinition (field-name field) (field-src field))
                        (check-s (catch-body catch)
                                 (add-var-to-env name (field-type field) parm env)))))
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
  
  ;check-block: (list (U statement field)) env (statement env -> void) (expr -> type) symbol (list string) type-records -> void
  (define (check-block stmts env check-s check-e level c-class type-recs)
    (let loop ((stmts stmts) (block-env env))
      (cond 
        ((null? stmts) (void))
        ((field? (car stmts))
         (loop (cdr stmts) 
               (check-local-var (car stmts) block-env (lambda (e) (check-e e block-env)) level c-class type-recs)))
        (else
         (check-s (car stmts) block-env)
         (loop (cdr stmts) block-env )))))
      
  ;check-break: (U id #f) src bool bool symbol env-> void
  (define (check-break label src in-loop? in-switch? level env)
    (cond
      (label
       (unless (lookup-label (id-string label) env)
         (illegal-label 'break (id-string label) (id-src label))))
      ((or (not in-loop?) (not in-switch?))
       (break-error src level))))

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
                     ((not-declared)
                      (format "Thrown type ~a must be declared as thrown or caught" t)))
                   'throw src)))
  
  ;return-error: symbol type type src -> void
  (define (return-error kind given expected src)
    (let ((g (type->ext-name given))
          (e (type->ext-name expected)))
      (raise-error 
       'return
       (case kind
         ((not-equal)
          (let ((line1 
                 (format "The return expression's type must be equal to or a subclass of the method's return ~a." e))
                (line2
                 (format "The given expression has type ~a which is not equivalent to the declared return" g)))
            (format "~a~n~a" line1 line2)))
         ((void) "No value should be returned from void method, found a returned value")
         ((val)
          (format "Expected a return value assignable to ~a. No value was given" e)))
       'return src)))

  ;illegal-redefinition: id src -> void
  (define (illegal-redefinition field src)
    (let ((f (id->ext-name field)))
      (raise-error 
       f
       (format "Variable name ~a has already been used and may not be reused. Another name must be chosen" f)
       f src)))
  
  ;variable-type-error: id type type src -> void
  (define (variable-type-error field given expt src)
    (let ((f (id->ext-name field)))
      (raise-error 
       f
       (format "Variable ~a was declared to be ~a, which is incompatible with the initial value type of ~a"
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
  (define (break-error level src)
    (raise-error 'break (if (eq? level 'full) 
                            "break must be in either a loop or a switch"
                            "break must be in a loop")
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
  
  ;; check-expr: expression env symbol type-records (U string #f) bool bool bool-> type
  (define (check-expr exp env level type-recs current-class ctor? static? interactions?)
    (let ((check-sub-expr 
           (lambda (expr) (check-expr expr env level type-recs current-class ctor? static? interactions?))))
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
                        (check-access exp check-sub-expr env level type-recs current-class interactions?)))
        ((special-name? exp)
         (set-expr-type exp (check-special-name exp env static? interactions?)))
        ((call? exp)
         (set-expr-type exp (check-call exp
                                        (map check-sub-expr (call-args exp))
                                        check-sub-expr
                                        current-class
                                        level
                                        env
                                        type-recs
                                        ctor?
                                        static?
                                        interactions?)))
        ((class-alloc? exp)
         (set-expr-type exp
                        (check-class-alloc exp
                                           (class-alloc-name exp)
                                           (map check-sub-expr (class-alloc-args exp))
                                           (expr-src exp)
                                           type-recs
                                           current-class
                                           env
                                           level
                                           static?)))
        ((array-alloc? exp)
         (set-expr-type exp
                        (check-array-alloc (array-alloc-name exp)
                                           (array-alloc-size exp)
                                           (array-alloc-dim exp)
                                           (expr-src exp)
                                           check-sub-expr
                                           level
                                           current-class
                                           type-recs)))
        ((array-alloc-init? exp)
         (set-expr-type exp
                        (check-array-alloc-init (array-alloc-init-name exp)
                                                (array-alloc-init-dim exp)
                                                (array-alloc-init-init exp)
                                                (expr-src exp)
                                                check-sub-expr
                                                level
                                                current-class
                                                type-recs)))
        ((cond-expression? exp)
         (set-expr-type exp
                        (check-cond-expr (check-sub-expr (cond-expression-cond exp))
                                         (check-sub-expr (cond-expression-then exp))
                                         (check-sub-expr (cond-expression-else exp))
                                         (expr-src exp)
                                         (expr-src (cond-expression-cond exp))
                                         level
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
                                    level
                                    current-class 
                                    type-recs)))
        ((instanceof? exp)
         (set-expr-type exp
                        (check-instanceof (check-sub-expr (instanceof-expr exp))
                                          (instanceof-type exp)
                                          (expr-src exp)
                                          level
                                          current-class
                                          type-recs)))
        ((assignment? exp)
         (set-expr-type exp
                        (check-assignment (assignment-op exp)
                                          (assignment-left exp)
                                          (check-sub-expr (assignment-left exp))
                                          (check-sub-expr (assignment-right exp))
                                          (expr-src exp)
                                          ctor?
                                          #f ; static-init?
                                          current-class
                                          level
                                          type-recs
                                          env))))))

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
               ((or right-to-left left-to-right) 'boolean)
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

  ;;check-access: expression (expr -> type) env symbol type-records (list string) interactions? -> type
  (define (check-access exp check-sub-expr env level type-recs c-class interactions?)
    (let ((acc (access-name exp)))
      (cond
        ((field-access? acc)
         (let ((obj (field-access-object acc))
               (fname (id-string (field-access-field acc)))
               (src (id-src (field-access-field acc)))
               (record null))
           (if obj
               (set! record (field-lookup fname (check-sub-expr obj) obj src level type-recs))
               (set! record 
                     (let* ((name (var-access-class (field-access-access acc)))
                            (class-rec (get-record (send type-recs get-class-record 
                                                         (if (pair? name) name (list name))
                                                         #f
                                                         ((get-importer type-recs) name type-recs level src))
                                                   type-recs)))
                       (get-field-record fname class-rec
                                         (lambda () 
                                           (let* ((class? (member fname (send type-recs get-class-env)))
                                                  (method? (not (null? (get-method-records fname class-rec)))))
                                             (field-lookup-error (if class? 'class-name 
                                                                     (if method? 'method-name 'not-found))
                                                                 (string->symbol fname)
                                                                 (make-ref-type (if (pair? name)
                                                                                    (car name)
                                                                                    name) null)
                                                                 src)))))))

           (when (and (eq? level 'beginner)
                      (eq? (car c-class) (car (field-record-class record))))
             (when (or (not obj) (and (special-name? obj) (not (expr-src obj))))
               (beginner-field-access-error (string->symbol fname) src)))
           
           (set-field-access-access! acc (make-var-access 
                                          (memq 'static (field-record-modifiers record))
                                          (memq 'final (field-record-modifiers record))
                                          (field-record-init? record)
                                          (car (field-record-class record))))
           (add-required c-class 
                         (car (field-record-class record)) 
                         (if (null? (cdr (field-record-class record)))
                             (send type-recs lookup-path (car (field-record-class record))
                                   (lambda () null))
                             (cdr (field-record-class record)))
                         type-recs)
           (unless (eq? level 'full)
             (when (is-field-restricted? fname (field-record-class record))
               (restricted-field-access-err (field-access-field acc) 
                                        (field-record-class record) src)))
           (field-record-type record)))
        ((local-access? acc) 
         (let ((var (lookup-var-in-env (id-string (local-access-name acc)) env)))
           (if (properties-usable? (var-type-properties var))
               (var-type-type var)
               (unusable-var-error (string->symbol (var-type-var var)) (id-src (local-access-name acc))))))
        
        (else
         (let* ((first-acc (id-string (car acc)))
                (first-binding (lookup-var-in-env first-acc env))
                (new-acc
                 (cond
                   ((and (eq? level 'full) (not first-binding) (> (length acc) 1))
                    (let* ((static-class (find-static-class acc level type-recs))
                           (accs (cadr static-class)))
                      (build-field-accesses 
                       (make-access #f 
                                    (expr-src exp)
                                    (make-field-access 
                                     #f
                                     (car accs)
                                     (make-var-access #t #f #f (class-record-name (car static-class)))))
                       (cdr accs))))
                   ((and (memq level '(beginner intermediate advanced)) (not first-binding) (> (length acc) 1)
                         (with-handlers ((exn:syntax? (lambda (e) #f))) 
                           (type-exists? first-acc null c-class (id-src (car acc)) level type-recs)))
                    (build-field-accesses
                     (make-access #f
                                  (expr-src exp)
                                  (make-field-access #f
                                                     (cadr acc)
                                                     (make-var-access #t #f #f first-acc)))
                     (cddr acc)))
                   ((and first-binding (properties-local? (var-type-properties first-binding)))
                    (build-field-accesses
                     (make-access #f (expr-src exp) (make-local-access (car acc)))
                     (cdr acc)))
                   (first-binding
                    (if (properties-static? (var-type-properties first-binding))
                        (build-field-accesses
                         (make-access #f (expr-src exp)
                                      (make-field-access #f
                                                         (car acc)
                                                         (make-var-access #t #f #f c-class)))
                         (cdr acc))
                        (if interactions?
                            (build-field-accesses (make-access #f (expr-src exp) (make-local-access (car acc)))
                                                  (cdr acc))
                            (build-field-accesses
                             (make-access #f (expr-src exp)
                                          (make-field-access (make-special-name #f #f "this")
                                                             (car acc)
                                                             #f))
                             (cdr acc)))))
                   (else 
                    (let ((class? (member (id-string (car acc)) (send type-recs get-class-env)))
                          (method? (not (null? (get-method-records (id-string (car acc)) (lookup-this type-recs env))))))
                      (cond
                        ((or class? method?)
                         (variable-not-found-error (if class? 'class-name 'method-name) (car acc) (id-src (car acc))))
                        ((close-to-keyword? (id-string (car acc)))
                         (close-to-keyword-error 'field (car acc) (id-src (car acc))))
                        (else
                         (variable-not-found-error 'not-found (car acc) (id-src (car acc))))))))))
           (set-access-name! exp new-acc)
           (check-sub-expr exp))))))
  
  ;; field-lookup: string type expression src symbol type-records -> field-record
  (define (field-lookup fname obj-type obj src level type-recs)
    (let ((obj-src (expr-src obj))
          (name (string->symbol fname)))
      (cond
        ((reference-type? obj-type)
         (let ((obj-record (get-record (send type-recs get-class-record obj-type #f
                                             ((get-importer type-recs) obj-type type-recs level obj-src))
                                       type-recs)))
           (get-field-record fname obj-record
                             (lambda () 
                               (let* ((class? (member fname (send type-recs get-class-env)))
                                      (method? (not (null? (get-method-records fname obj-record)))))
                                 (field-lookup-error 
                                  (if class? 'class-name 
                                      (if method? 'method-name 'not-found)) name obj-type src))))))
        ((array-type? obj-type)
         (unless (equal? fname "length")
           (field-lookup-error 'array name obj-type src))
         (make-field-record "length" `() #f `(array) 'int))
        (else (field-lookup-error 'primitive name obj-type obj-src)))))
  
  ;; build-field-accesses: access (list id) -> field-access
  (define (build-field-accesses start accesses)
    (cond
      ((null? accesses) (access-name start))
      (else
       (build-field-accesses
        (make-access #f (expr-src start) 
                     (make-field-access start (car accesses) #f))
        (cdr accesses)))))
  
  ;;find-static-class: (list access) symbol type-recs -> (list class-record (list access))
  (define (find-static-class accs level type-recs)
    (let ((path (send type-recs lookup-path (id-string (car accs)) (lambda () #f))))
      (if path
          (list (let* ((name (cons (id-string (car accs)) path))
                       (record (get-record 
                                (send type-recs get-class-record name #f
                                      ((get-importer type-recs) name type-recs level (id-src (car accs))))
                                type-recs)))
                  record)
                (cdr accs))
          (let ((found? (find-static (list (car accs)) (cdr accs))))
            (if (car found?)
                (list (get-record (send type-recs get-class-record (car found?)) type-recs)
                      (cdr found?))
                (class-lookup-error (caadr found?) (id-src (car accs))))))))
    
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
  (define (check-call call args check-sub c-class level env type-recs ctor? static? interact?)
    (let* ((this (unless static? (lookup-this type-recs env)))
           (src (expr-src call))
           (name (call-method-name call))
           (expr (call-expr call))
           (exp-type #f)
           (handle-call-error 
            (lambda (exn)
              (when (not (access? expr)) (raise exn))
              (when (or (field-access? (access-name expr)) (local-access? expr)) (raise exn))
              (if (eq? level 'full)
                  (let ((record (car (find-static-class 
                                      (append (access-name expr) (list name))
                                      level
                                      type-recs))))
                    (set-call-expr! call #f)
                    (unless (equal? (class-record-name record) c-class)
                      (send type-recs add-req (make-req (car (class-record-name record))
                                                        (if (null? (cdr (class-record-name record)))
                                                            (send type-recs lookup-path 
                                                                  (car (class-record-name record))
                                                                  (lambda () null))
                                                            (cdr (class-record-name record))))))                                                                  
                    (get-method-records (id-string name) record))
                  (if (and (= (length (access-name expr)) 1)
                           (with-handlers ((exn:syntax? (lambda (exn) #f)))
                             (type-exists? (id-string (car (access-name expr)))
                                           null
                                           c-class
                                           (id-src (car (access-name expr)))
                                           level
                                           type-recs)))
                      (let ((record (send type-recs get-class-record (list (id-string (car (access-name expr)))))))
                        (set-call-expr! call #f)
                        (unless (equal? (class-record-name record) c-class)
                          (send type-recs add-req (make-req (car (class-record-name record))
                                                            (send type-recs lookup-path 
                                                                  (car (class-record-name record))
                                                                  (lambda () null)))))
                        (get-method-records (id-string name) record))
                      (raise exn)))))
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
                                            (send type-recs get-class-record call-exp #f
                                                  ((get-importer type-recs) call-exp type-recs level src))
                                            type-recs)))
                      (else (prim-call-error call-exp name src level)))))
                 (else 
                  (if (eq? level 'beginner)
                      (beginner-method-access-error name (id-src name))
                      (let ((rec (if static? (send type-recs get-class-record c-class) this)))
                        (if (null? rec) null
                            (get-method-records (id-string name) rec))))))))))

      
      (when (null? methods)
        (let* ((rec (if exp-type 
                        (send type-recs get-class-record exp-type)
                        (if static? (send type-recs get-class-record c-class) this)))
               (class? (member (id-string name) (send type-recs get-class-env)))
               (field? (cond
                         ((array-type? exp-type) (equal? (id-string name) "length"))
                         ((null? rec) 
                          (member (id-string name) (map field-record-name (send type-recs get-interactions-fields))))
                         (else (member (id-string name) (get-field-records rec)))))
               (sub-kind (if class? 'class-name (if field? 'field-name 'not-found))))
        (cond 
          ((eq? exp-type 'super) (no-method-error 'super sub-kind exp-type name src))
          (exp-type (no-method-error 'class sub-kind exp-type name src))
          (else 
           (cond
             ((close-to-keyword? (id-string name))
              (close-to-keyword-error 'method name src))
             (interact? (interaction-call-error name src level))
             (else
              (no-method-error 'this sub-kind exp-type name src)))))))
                  
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
                  (if (> (length methods) 1)
                      (let ((teaching-error
                             (lambda (kind)
                               (if (error-file-exists? (method-record-class (car methods)) type-recs)
                                   (call-provided-error (id-string name) args kind)
                                   (teaching-call-error kind name args exp-type src methods)))))
                        (resolve-overloading methods
                                             args
                                             (lambda () (teaching-error 'number))
                                             (lambda () (teaching-error 'type))
                                             (lambda () (teaching-error 'type))
                                             type-recs))
                      (when (check-method-args args (method-record-atypes (car methods)) name exp-type src type-recs)
                        (car methods)))))
             (mods (method-record-modifiers method-record)))
        
        (when (and static? 
                   (not (memq 'static mods))
                   (not expr))
          (non-static-called-error name c-class src level))
                
        (when (and (memq 'protected mods) (reference-type? exp-type) 
                   (not (is-eq-subclass? this exp-type)))
          (call-access-error 'pro name exp-type src))
        (when (and (memq 'private mods)
                   (reference-type? exp-type)
                   (if static?
                       (not (and (equal? (ref-type-class/iface exp-type) (car c-class))
                                 (equal? (ref-type-path exp-type) (cdr c-class))))
                       (not (eq? this (send type-recs get-class-record exp-type)))))
          (call-access-error 'pri name exp-type src))
        (when (eq? level 'full)
          (for-each (lambda (thrown)
                      (unless (lookup-exn thrown env type-recs level)
                        (thrown-error (ref-type-class/iface thrown) name exp-type src)))
                    (method-record-throws method-record)))
        (when (and (eq? level 'beginner)
                   (eq? 'void (method-record-rtype method-record)))
          (beginner-call-error name src))
        (unless (eq? level 'full)
          (when (and (id? name) (is-method-restricted? (id-string name) (method-record-class method-record)))
            (restricted-method-call name (method-record-class method-record) src)))
        (set-call-method-record! call method-record)
        (method-record-rtype method-record))))
    
  ;close-to-keyword: string -> bool
  (define (close-to-keyword? str)
    (let ((s (string-copy str)))
      (string-lowercase! s)
      (member s `("if" "return"))))
  
  (define (error-file-exists? class type-recs) #f)
  (define (call-provided-error) null)
  
  ;check-method-args: (list type) (list type) id type src type-records -> void
  (define (check-method-args args atypes name exp-type src type-recs)
    (unless (= (length args) (length atypes))
      (method-arg-error 'number args atypes name exp-type src))
    (for-each (lambda (arg atype)
                (unless (assignment-conversion atype arg type-recs)
                  (method-arg-error 'type (list arg) (cons atype atypes) name exp-type src)))
              args atypes))
  
      
  
  ;;Skip package access controls
  ;; 15.9
  ;;check-class-alloc: expr name (list type) src type-records (list string) env symbol bool-> type
  (define (check-class-alloc exp name/def args src type-recs c-class env level static?)
    (let* ((name (if (def? name/def)
                  (begin (check-inner-def name/def level type-recs c-class env)
                         (make-name (def-name name/def) null (id-src (def-name name/def))))
                  name/def))
           (type (name->type name/def c-class (name-src name) level type-recs))
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
                        (when (check-ctor-args args (method-record-atypes (car methods)) type src level type-recs)
                          (car methods))))
             (mods (method-record-modifiers const))
             (this (if static? 
                       class-record
                       (lookup-this type-recs env))))
        (when (eq? level 'full)
          (for-each (lambda (thrown)
                      (unless (lookup-exn thrown env type-recs level)
                        (ctor-throws-error (ref-type-class/iface thrown) type src)))
                    (method-record-throws const)))
        (when (and (memq 'private mods) (not (eq? class-record this)))
          (class-access-error 'pri type src))
        (when (and (memq 'protected mods) (not (is-eq-subclass? this type)))
          (class-access-error 'pro type src))
        (set-class-alloc-ctor-record! exp const)
        type)))

  ;check-ctor-args: (list type) (list type) type src symbol type-records -> void
  (define (check-ctor-args args atypes name src level type-recs)
    (unless (= (length args) (length atypes))
      (ctor-arg-error 'number args atypes name src))
    (for-each (lambda (arg atype)
                (unless (assignment-conversion atype arg type-recs)
                  (ctor-arg-error 'type (list arg) (cons atype atypes) name src)))
              args atypes))
  
  ;; 15.10
  ;;check-array-alloc type-spec (list expression) int src (expr->type) symbol (list string) type-records -> type
  (define (check-array-alloc elt-type exps dim src check-sub-exp level c-class type-recs)
    (send type-recs add-req (make-req 'array null))
    (let ((type (type-spec-to-type elt-type c-class level type-recs)))
      (when (ref-type? type)
        (add-required c-class (ref-type-class/iface type) (ref-type-path type) type-recs))
      (for-each (lambda (e)
                  (let ((t (check-sub-exp e)))
                    (unless (prim-integral-type? t)
                      (array-size-error type t (expr-src e)))))
                exps)
      (make-array-type type (+ (length exps) dim))))
  
  ;;15.10
  ;;check-array-alloc-init type-spec int array-init src (expr->type) symbol (list string) type-records -> type
  (define (check-array-alloc-init elt-type dim init src check-sub-exp level c-class type-recs)
    (send type-recs add-req (make-req 'array null))
    (let* ((type (type-spec-to-type elt-type c-class level type-recs))
           (a-type (check-array-init (array-init-vals init) check-sub-exp type type-recs)))
      (when (ref-type? type)
        (add-required c-class (ref-type-class/iface type) (ref-type-path type) type-recs))
      (unless (= (array-type-dim a-type) dim)
        (array-dim-error type dim (array-type-dim a-type) src))
      (make-array-type type dim)))
  
  
  ;; 15.25
  ;check-cond-expr: type type type src src symbol type-records -> type
  (define (check-cond-expr test then else-t src test-src level type-recs)
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
  ;check-cast: type type-spec src symbol (list string) type-records -> type
  (define (check-cast exp-type cast-type src level current-class type-recs)
    (let ((type (type-spec-to-type cast-type current-class level type-recs)))
      (when (reference-type? type)
        (unless (equal? (car current-class) (ref-type-class/iface type))
          (send type-recs add-req (make-req (ref-type-class/iface type) (ref-type-path type)))))
      (cond
        ((and (reference-type? exp-type) (reference-type? type)) type)
        ((and (not (reference-type? exp-type)) (not (reference-type? type))) type)
        ((reference-type? exp-type) (cast-error 'from-prim exp-type type src))
        (else (cast-error 'from-ref exp-type type src)))))

  ;; 15.20.2
  ;check-instanceof type type-spec src symbol (list string) type-records -> type
  (define (check-instanceof exp-type inst-type src level current-class type-recs)
    (let ((type (type-spec-to-type inst-type current-class level type-recs)))
      (unless (equal? (car current-class) (ref-type-class/iface type))
        (send type-recs add-req (make-req (ref-type-class/iface type) (ref-type-path type))))
      (cond 
        ((and (ref-type? exp-type) (ref-type? type) 
              (or (is-eq-subclass? exp-type type type-recs)
                  (is-eq-subclass? type exp-type type-recs))) 'boolean)
        ((and (ref-type? exp-type) (ref-type? type))
         (instanceof-error 'not-related-type type exp-type src))
        ((ref-type? exp-type)
         (instanceof-error 'not-class type exp-type src))
        (else
         (instanceof-error 'not-ref type exp-type src)))))
  
  ;; 15.26
  ;; SKIP - doing the check for compound assignment
  ;check-assignment: symbol expr type type src bool bool string symbol type-records env -> type
  (define (check-assignment op lexpr ltype rtype src c-tor? static-init? c-class level type-recs env)
    (when (and (eq? level 'beginner) (not c-tor?))
      (illegal-assignment src))
    (when (access? lexpr)
      (check-final lexpr c-tor? static-init? c-class env))
    (case op
      ((=)
       (if (assignment-conversion ltype rtype type-recs)
           ltype
           (assignment-error op ltype rtype src)))
      ((+= *= /= %= -= <<= >>= >>>= &= ^= or=)
       (check-bin-op op ltype rtype src level type-recs)
       ltype)))

  ;check-final: expression bool bool string -> void
  (define (check-final expr ctor? static-init? c-class env)
    (let ((access (access-name expr))
          (class (car c-class)))
      (cond
        ((local-access? access)
         (let* ((name (local-access-name access))
                (properties (var-type-properties (lookup-var-in-env (id-string name) env)))
                (settable? (properties-settable? properties))
                (static? (properties-static? properties)))
           (when (properties-final? properties)
             (when (properties-local? properties) (assign-final-error 'local name class))
             (cond
               ((and ctor? settable? (not static?)) (void))
               ((and ctor? settable? static?) (assign-final-error 'static-in-ctor name class))
               ((and ctor? (not settable?)) (assign-final-error 'cannot-set-ctor name class))
               ((and static-init? settable?) (void))
               ((and static-init? (not settable?)) (assign-final-error 'cannot-set-static name class))
               (else (assign-final-error (if static? 'static 'field) name class))))))
        ((field-access? access)
         (let* ((name (field-access-field access))
                (obj (field-access-object access))
                (v-acc (field-access-access access))
                (init? (var-access-init? v-acc))
                (static? (var-access-static? v-acc)))
           (when (var-access-final? v-acc)
             (if (and (or (this-expr? obj) (and static-init? (not obj))) 
                      (equal? (var-access-class v-acc) class))
                 (cond
                   ((and ctor? (not init?) (not static?)) (void))
                   ((and ctor? (not init?) static?) (assign-final-error 'static-in-ctor name class))
                   ((and ctor? init? static?) (assign-final-error 'static-ctor-already-set name class))
                   ((and ctor? init? (not static?)) (assign-final-error 'field-already-set name class))
                   ((and static-init? (not init?)) (void))
                   ((and static-init? init?) (assign-final-error 'static-already-set name class))
                   (else (assign-final-error (if static? 'static 'field) name class)))
                 (assign-final-error (if static? 'static 'field) name class))))))))
  
  ;this-expr: expr -> bool
  (define (this-expr? expr)
    (and (special-name? expr)
         (equal? "this" (special-name-name expr))))
  
      
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
         ((right) (format "Right hand side of ~a should be of type ~a, but given ~a" op ext-out rt))
         ((left) (format "Left hand side of ~a should be of type ~a, but given ~a" op ext-out lt))
         (else (format "~a expects arguments of type ~a, but given ~a and ~a" op ext-out lt rt)))
       op src)))
  
  ;bin-op-equality-error symbol symbol type type src -> void
  (define (bin-op-equality-error type op left right src)
    (let ((rt (type->ext-name right))
          (lt (type->ext-name left)))
      (raise-error 
       op
       (case type
         ((both) 
          (format "~a expects one argument to be assignable to the other, neither ~a nor ~a can be" op lt rt))
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
  
  ;variable-not-found-error: symbol id src -> void
  (define (variable-not-found-error kind var src)
    (let ((name (id->ext-name var)))
      (raise-error 
       name
       (case kind
         ((not-found) (format "reference to undefined identifier ~a" name))
         ((class-name) (format "class named ~a cannot be used as a variable, which is how it is used here" name))
         ((method-name) 
          (let ((line1
                 (format "method named ~a cannot be used as a variable, which is how it is used here." name))
                (line2 "A call to a method should be followed by () and any arguments to the method"))
            (format "~a~n~a" line1 line2))))
       name src)))
  
  ;field-lookup-error: symbol symbol type src -> void
  (define (field-lookup-error kind field exp src)
    (let ((t (type->ext-name exp)))
      (raise-error
       field
       (case kind
         ((not-found) (format "field ~a not found for object with type ~a" field t))
         ((class-name)
          (format "Class named ~a is being erroneously accessed as a field" field))
         ((method-name)
          (let ((line1
                 (format "Method ~a is being erroneously accessed as a field for class ~a." field t))
                (line2 "A call to a method chould be followed by () and any arguments to the method"))
            (format "~a~n~a" line1 line2)))
         ((array)
          (format "~a only has a length field, attempted to access ~a" t field))
         ((primitive)
          (format "attempted to access field ~a on ~a, this value does not have fields" field t)))
       field src)))

  ;unusable-var-error: symbol src -> void
  (define (unusable-var-error name src)
    (raise-error 
     name
     (format "field ~a cannot be used in this class, as two or more parents contain a field with this name" name)
     name src))
  
  ;beginner-field-access-error: symbol src -> void
  (define (beginner-field-access-error name src)
    (raise-error
     name
     (format "field ~a from the current class accessed as a variable. fields should be accessed with 'this'" name)
     name src))
  
  ;restricted-field-access: id (list string) src -> void
  (define (restricted-field-access-err field class src)
    (let ((n (id->ext-name field)))
      (raise-error n (format "field ~a from ~a may not be used" n (car class))
                   n src)))
  
  ;;special-name errors
  ;special-error: src bool -> void
  (define (special-error src interactions?)
    (raise-error 'this 
                 (format "use of 'this' is not allowed in ~a"
                         (if interactions? "the interactions window" "static code"))
                 'this src))
  
  ;;Call errors

  ;prim-call-error type id src symbol -> void
  (define (prim-call-error exp name src level)
    (let ((n (id->ext-name name))
          (t (type->ext-name exp)))
      (raise-error
       n
       (format "attempted to call method ~a on ~a which does not have methods. ~nOnly values with ~a types have methods"
               n t
               (case level 
                 ((beginner) "class")
                 ((intermediate) "class or interface")
                 (else "class, interface, or array")))
       n src)))
  
  ;no-method-error: symbol symbol type id src -> void
  (define (no-method-error kind sub-kind exp name src)
    (let ((t (type->ext-name exp))
          (n (id->ext-name name)))
      (raise-error 
       n
       (case sub-kind
         ((not-found) (format "~a does not contain a method named ~a"
                              (case kind
                                ((class) t)
                                ((super) "This class's super class")
                                ((this) "The current class"))
                              n))
         ((class-name)
          (let ((line1 
                 (format "Class ~a is inappropriately being used as a method." n))
                (line2
                 "Parenthesis typically follow the class name when creating an instance, perhaps 'new' was forgotten"))
          (format "~a~n~a" line1 line2)))
         ((field-name)
          (format 
           "Field ~a is being inappropriately used as a method, parentheses are not used in interacting with a field"
           n)))                     
       n src)))
  
  ;close-to-keyword-error: symbol id src -> void
  (define (close-to-keyword-error kind name src)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (case kind
                     ((method) 
                      (string-append
                       (format "this method call uses an unfound method ~a, which is similar to a reserved word~n"
                               n)
                       "Perhaps it is miscapitalized or misspelled"))
                     ((field)
                       (string-append
                        (format "this unknown variable, ~a, is similar to a reserved word.~n" n)
                        "Perhaps it is miscaptialzed or misspelled")))
                   n src)))

  ;beginner-method-access-error: id src -> void
  (define (beginner-method-access-error name src)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (format "method ~a from the current class must be call on 'this'" n)
                   n src)))

  
  ;restricted-method-call id (list string) src -> void
  (define (restricted-method-call name class src)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (format "method ~a from ~a may not be called" n (car class))
                   n src)))
  
  ;ctor-called-error: type id src -> void
  (define (ctor-called-error exp name src)
    (let ((t (type->ext-name exp))
          (n (id->ext-name name)))
      (raise-error n
                   (format "Constructor ~a from ~a cannot be used as a method" n t)
                   n src)))
  
  ;non-static-called-error: id (list string) src bool -> void
  (define (non-static-called-error name class src level)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (if (memq level '(advanced full))
                       (format "Non-static method ~a from ~a cannot be called directly from a static context"
                               n (car class))
                       (format "Method ~a from ~a cannot be called here" n (car class)))
                   n src)))
  
  ;interaction-call-error
  (define (interaction-call-error name src level)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (string-append (format "method ~a cannot be called in the interactions window.~n" n)
                                  (format "Only ~a methods or methods on objects may be called here" 
                                          (if (memq level '(beginner intermediate)) "certain library" "static")))
                   n src)))

  
  (define (illegal-ctor-call name src level)
    (let ((n (string->symbol name)))
      (raise-error n (format "calls to ~a may only occur in ~a"
                             n
                             (if (memq level `(full advanced)) "other constructors" "the super constructor"))
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
                              n e awitht (map type->ext-name (cdr atypes)) (car givens) (type->ext-name (car atypes)))))
                   n src)))

  ;teaching-call-error: symbol id (list type) type src (list method-record) -> void
  (define (teaching-call-error kind name args exp-type src methods)
    (let* ((method-args (map method-record-atypes methods))
           (predominant-number (get-most-occuring-length method-args))
           (type-lists (get-string-of-types (filter (lambda (a) (= (length a) predominant-number)) method-args))))
      (let ((n (id->ext-name name))
            (e (get-call-type exp-type))
            (givens (map type->ext-name args)))
        (raise-error n
                     (case kind
                       ((number)
                        (format "method ~a from ~a expects ~a arguments with types ~a. Given ~a"
                                n e predominant-number type-lists givens))
                       (else
                        (format "method ~a from ~a expects arguments with types ~a. Given ~a"
                                n e type-lists givens)))
                     n src))))
  
  ;get-most-occuring-lenght: (list (list type)) -> number
  (define (get-most-occuring-length args)
    (let* ((lengths (map length args))
           (max-length (apply max lengths))
           (vec (make-vector (add1 max-length) 0))
           (loc 0))
      (let loop ((l lengths))
        (unless (null? l)
          (vector-set! vec (car l) (add1 (vector-ref vec (car l))))
          (loop (cdr l))))
      (let loop ((i 0) (max-loc 0) (max 0))
        (if (= i (vector-length vec))
            (set! loc max-loc)
            (if (> (vector-ref vec i) max)
                (loop (add1 i) i (vector-ref vec i))
                (loop (add1 i) max-loc max))))
      loc))
        
  ;get-string-of-types: (list (list type)) -> string
  (define (get-string-of-types types)
    (let ((out (if (= 1 (length (car types)))
                   (apply string-append 
                          (map (lambda (a) (format "~a, or " (type->ext-name (car a)))) types))
                   (apply string-append
                          (map (lambda (a) (format "(~a), or "
                                                   (let ((internal 
                                                          (apply string-append 
                                                                 (map (lambda (aI) 
                                                                        (format "~a, " (type->ext-name aI))) a))))
                                                     (substring internal 0 (- (string-length internal) 2)))))
                               types)))))
      (substring out 0 (- (string-length out) 5))))                                                         
  
  ;call-access-error: symbol id type src -> void
  (define (call-access-error kind name exp src)
    (let ((n (id->ext-name name))
          (t (get-call-type exp)))
    (raise-error n
                 (case kind
                   ((abs) (format "Abstract methods may not be called. ~a from ~a is abstract"
                                  n t))
                   ((pro) (format "Protected method ~a from ~a may not be called here" n t))
                   ((pri) (format "Private method ~a from ~a may not be called here" n t)))
                 n src)))

  ;call-arg-error: symbol id (list type) type src -> void
  (define (call-arg-error kind name args exp src)
    (let* ((n (id->ext-name name))
           (t (get-call-type exp))
           (call-type (if (and (special-name? name)
                               (string=? "super" (special-name-name name)))
                          "super constructor for"
                          (format "method ~a from" n)))
           (as (map type->ext-name args)))
      (raise-error n
                   (case kind
                     ((number)
                      (format "~a ~a has no definition with ~a argument~a. Given ~a"
                              call-type t (length as) (if (> (length as) 1) "s" "") as))
                     ((no-match)
                      (format "~a ~a has no definition with compatible types as the given types: ~a"
                              call-type t as))
                     ((conflict)
                      (format "~a ~a has multiple compatible definitions with given arguments: ~a"
                              call-type t as)))
                   n src)))
  
  ;thrown-error: string id type src -> void
  (define (thrown-error thrown name exp src)
    (let ((n (id->ext-name name))
          (t (get-call-type exp)))
      (raise-error n
                   (format "called method ~a from ~a throws exception ~a, which is not caught or listed as thrown"
                           n t thrown)
                   n src)))
      
  ;beginner-call-error: id src -> void
  (define (beginner-call-error name src)
    (let ((n (id->ext-name name)))
      (raise-error n (format "method ~a cannot be called in Beginner Java" n) n src)))
  
  ;;Class Alloc errors

  ;class-alloc-error: symbol type src -> void
  (define (class-alloc-error kind type src)
    (let ((cl (type->ext-name type)))
      (raise-error cl
                   (case kind
                     ((abstract) (format "Instances cannot be made of abstract classes, class ~a is abstract" cl))
                     ((interface) (format "Instances cannot be made of interfaces, ~a is an interface" cl)))
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
                              n awitht (map type->ext-name (cdr atypes)) (car givens) (type->ext-name (car atypes)))))
                   n src)))

  ;ctor-overload-error: symbol type (list type) src -> void
  (define (ctor-overload-error kind name args src)
    (let ((n (type->ext-name name))
          (as (map type->ext-name args)))
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

  ;Array Alloc Init error
  ;array-dim-error: type int int src -> void
  (define (array-dim-error type dim g-dim src)
    (let ((t (type->ext-name (make-array-type type dim)))
          (given (type->ext-name (make-array-type type g-dim))))
      (raise-error t
                   (format "Expected an array of type ~a~a, found an array of type ~a~a" t given)
                   t src)))
  
  ;;Conditional Expression errors

  ;condition-error: type src -> void
  (define (condition-error type src)
    (let ((t (type->ext-name type)))
      (raise-error '?
                   (format "? requires that the first expression have type boolean. Given ~a" t)
                   '? src)))

  ;condition-mismatch-error: type type src -> void
  (define (condition-mismatch-error then else src)
    (raise-error 
     '?
     (format "? requires that the then and else branches have equivalent types: given ~a and ~a which are not equivalent"
             (type->ext-name then) (type->ext-name else))
     '? src))
    
  ;;Array Access errors
  ;illegal-array-access: type src -> void
  (define (illegal-array-access type src)
    (let ((n (type->ext-name type)))
      (raise-error 
       n
       (format "Expression of type ~a accessed as if it were an array, only arrays may be accessed with [N]" n)
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
    (raise-error 
     'cast
     (case kind
       ((from-prim)
        (let ((line1 (format "Illegal cast from primitive, ~a, to class or interface ~a."
                             (type->ext-name exp) (type->ext-name cast)))
              (line2 "Non-class or interface types may not be cast to class or interface types"))
          (format "~a~n~a" line1 line2)))
       ((from-ref)
        (let ((line1 (format "Illegal cast from class or interface ~a to primitive, ~a."
                             (type->ext-name exp) (type->ext-name cast)))
              (line2 "Class or interface types may not be cast to non-class or interface types"))
          (format "~a~n~a" line1 line2))))
     'cast src))
  
  ;;Instanceof errors
  ;instanceof-error: symbol type type src -> void
  (define (instanceof-error kind inst exp src)
    (let ((i (type->ext-name inst))
          (e (type->ext-name exp)))
      (raise-error 
       'instanceof
       (case kind
         ((not-related-type)
          (let ((line1 "instanceof requires that its expression be related to the given type")
                (line2 (format "~a is not a subtype of ~a, and ~a is not a subtype of ~a" e i i e)))
            (format "~a~n~a" line1 line2)))
         ((not-class)
          (format 
           "instanceof requires its expression to be compared to a class or interface: Given ~a which is neither"
           i))
         ((not-ref)
          (format "instanceof requires the expression, compared to ~a, to be a class or interface: Given ~a"
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
    "Assignment statements are only allowed in constructors")
  (define (assignment-convert-fail op d ltype rtype)
    (format "~a requires that the right hand type be equivalent to or a subtype of ~a: given ~a" 
            op ltype rtype))

  ;assign-final-error: symbol id string -> void
  (define (assign-final-error kind name class)
    (let* ((n (id->ext-name name))
           (already-set 
            (lambda (static?) (format "final~afield ~a has already been set" (if static? " static " " ") n)))
           (in-ctor
            (lambda (static?) (format "final~afield ~a may not be set in ~a's constructor" 
                                      (if static? " static " " ") n class))))
      (raise-error n
                   (case kind
                     ((local) (format "final parameter ~a may not be set" n))
                     ((static-in-ctor) (in-ctor #t))
                     ((cannot-set-ctor) (in-ctor #f))
                     ((cannot-set-static) (format "final field ~a may not be set in ~a's static initialization" n class))
                     ((static-ctor-already-set) 
                      (format "~a. Further, it may not be set in ~a's constructor" (already-set #t) class))
                     ((static-already-set) (already-set #t))
                     ((field-already-set) (already-set #f))
                     ((static) (format "final field ~a may only be set in the containing class's static initialization" n))
                     ((field) (format "final field ~a may only be set in the containing class's constructor" n)))                     
                   n (id-src name))))

  
  ;implicit import error
  ;class-lookup-error: string src -> void
  (define (class-lookup-error class src)
    (raise-error (string->symbol class)
                 (format "Implicit import of class ~a failed as this class does not exist at the specified location"
                         class)
                 (string->symbol class) src))
  
  (define check-location (make-parameter #f))
  
  (define raise-error (make-error-pass check-location))
      
  )