;Java To Scheme
;Kathy Gray
;May 2001
;Rewrite Dec 2001
;Rewrite June 2002

#cs
(module to-scheme mzscheme
  (require "ast.ss"
           "types.ss"
           "parameters.ss"
           (lib "class.ss")
           (lib "list.ss"))
  
  (provide translate-program translate-interactions (struct compilation-unit (contains code locations depends)))
  
  ;(make-compilation-unit (list string) (list syntax) (list location) (list (list string)))
  (define-struct compilation-unit (contains code locations depends))
  
  ;File takes java AST as defined by ast.ss and produces
  ;semantically (hopefully) equivalent scheme code
  
  ;NOTE! Abstract classes are treated no differently than any class.
  
  ;Parameters for information about each class
  (define class-name (make-parameter #f))
  (define loc (make-parameter #f))
  (define interactions? (make-parameter #f))
  (define class-override-table (make-parameter null))
  (define parent-name (make-parameter "Object"))
  (define module-name (make-parameter ""))
  (define module-require (make-parameter ""))
  
  (define stx-for-original-property (read-syntax #f (open-input-string "original")))
  (define create-syntax
    (lambda (oddness sexpression source)
      (datum->syntax-object (or oddness (syntax-location)) sexpression source stx-for-original-property)))
  (define make-syntax
    (lambda (oddness sexpression source)
      (datum->syntax-object (or oddness (syntax-location)) sexpression source)))
  
  ;-------------------------------------------------------------------------------------------------------------
  
  ;Type abbreviation
  
  ;The value of this will vary based on information I have at a given time, 
  ;as well as whether location is from source file or not
  ;SrcList  => boolean
  ;         |  (list symbol int)
  ;         |  (list symbol int int int)
  
  
  ;------------------------------------------------------------------------------------------------------------
  ;Helper functions
  ;Functions which are used throughout the transformation
  
  ;build-identifier: (U string symbol (list string)) -> symbol
  (define build-identifier
    (lambda (name)
      (cond
        ((symbol? name) name)
        ((string? name) (string->symbol name))
        ;PROBLEM might not be best method
        ((pair? name) (string->symbol (apply string-append (map (lambda (s) (string-append s ".")) name))))
        (else 
         (error 'build-identifier (format "Given ~s" name))
         name))))
  
  ;build-static-name: string symbol -> string
  (define build-static-name
    (lambda (name . args)
      (format "~a-~a" (if (null? args) (class-name) (car args)) name)))
  
  ;build-src: src -> SrcList
  (define build-src
    (lambda (src) 
      (if (not src)
          src
          (if (and (= (src-line src) 0)
                   (= (src-col src) 0)
                   (= (src-pos src) 0)
                   (= (src-span src) 0))
              #f
              (list (loc) (src-line src) (src-col src) (src-pos src) (src-span src))))))
  
  ;get-defualt-value: type-spec -> syntax
  (define get-default-value
    (lambda (type)
      (let ((name (type-spec-name type)))
        (if (> (type-spec-dim type) 0)
            (make-syntax #f 'null #f)
            (cond
              ((prim-numeric-type? name) (make-syntax #f 0 #f))
              ((eq? 'char name) (make-syntax #f `#\ #f))
              ((eq? 'boolean name) (make-syntax #f `#f #f))
              (else (make-syntax #f 'null #f)))))))
  
  ;create-get-name: string string? -> symbol
  (define create-get-name
    (lambda (name . args)
      (build-identifier (format "~a-~a-get" (if (null? args) (class-name) (car args))  name))))
  
  ;create-set-name: string string? -> symbol
  (define create-set-name
    (lambda (name . args)
      (build-identifier (format "~a-~a-set!" (if (null? args) (class-name) (car args)) name))))
  
  ;Methods to determine member restrictions
  ;make-mod-test: symbol -> (list -> bool)
  (define (make-mod-test acc) (lambda (m) (memq acc m)))
  (define public? (make-mod-test 'public))
  (define private? (make-mod-test 'private))
  (define protected? (make-mod-test 'protected))
  (define static? (make-mod-test 'static))
  (define abstract? (make-mod-test 'abstract))
  (define final? (make-mod-test 'final))
  
  ;retrieves static members from non-static
  ;get-statics: (list member) -> (list member)
  (define get-statics
    (lambda (l)
      (filter (lambda (m) (if (method? m)
                              (static? (map modifier-kind (method-modifiers m)))
                              (static? (map modifier-kind (field-modifiers m))))) l)))
  ;get-non-statics: (list member) -> (list member)
  (define get-non-statics
    (lambda (l)
      (filter (lambda (m) (not (if (method? m)
                                   (static? (map modifier-kind (method-modifiers m)))
                                   (static? (map modifier-kind (field-modifiers m)))))) l)))
  
  ;get-class-name: (U name type-spec) -> syntax
  (define get-class-name
    (lambda (name)
      (if (type-spec? name)
          (set! name (type-spec-name name)))
      (if (null? (name-path name))
          (translate-id (id-string (name-id name))
                        (id-src (name-id name)))
          (create-syntax #f
                         (build-identifier (get-class-string name))
                         (build-src (name-src name))))))
  
  ;get-class-string: name -> string
  (define get-class-string
    (lambda (name)
      (format "~a~a" (apply string-append (map (lambda (s)
                                                 (string-append s "."))
                                               (map id-string (name-path name))))
              (id-string (name-id name)))))
  
  ;build-id-name: string -> string
  (define (build-var-name id) (format "~a-f" id))
  
  ;build-method-name: string (list type) -> string
  (define build-method-name
    (lambda (id types)
      (letrec ((parm-name
                (lambda (t)
                  (format "-~a"
                          (cond
                            ((symbol? t) t)
                            ((ref-type? t) 
                             (string-append (apply string-append (map (lambda (p) (string-append p "."))
                                                                      (ref-type-path t)))
                                            (ref-type-class/iface t)))
                            ((array-type? t)
                             (string-append (let ((s (parm-name (array-type-type t))))
                                              (substring s 1 (string-length s)))
                                            (format "~a" (array-type-dim t))))
                            (else (error 'build-method-name (format "Internal Error: given unexptected type ~a" t))))))))
        (format "~a~a" id (apply string-append (map parm-name types))))))
  
  ;Converted, but don't know what generated constructor name will be
  ;constructor? string -> bool
  (define (constructor? name) 
    (equal? name (class-name)))
  
  ;build-constructor-name: string (list type) -> string
  (define build-constructor-name
    (lambda (class-name args)
      (build-method-name (format "~a-constructor" class-name) args)))
  
  
  ;-------------------------------------------------------------------------------------------------------------------------
  ;Translation
  
  ;translate-interactions: ast location type-records -> syntax
  (define (translate-interactions prog location type-recs)
    (loc location)
    (interactions? #t)
    (let ((reqs (send type-recs get-class-reqs))
          (syn (cond 
                 ((pair? prog)
                  (send type-recs set-class-reqs null)
                  (make-syntax #f 
                               `(begin ,@(map (lambda (f)
                                                (translate-interactions f location type-recs))
                                              prog))
                               #f))
                 ((field? prog) 
                  (translate-field `(private)
                                   (field-type prog)
                                   (field-name prog)
                                   (and (var-init? prog) prog)
                                   (if (var-init? prog)
                                       (var-init-src prog)
                                       (var-decl-src prog))
                                   #f))
                 ((statement? prog) (translate-statement prog type-recs))
                 ((expr? prog) (translate-expression prog))
                 (else 
                  (error 'translate-interactions "Internal Error: translate-interactions given ~a" prog)))))
      (if (null? reqs)
          syn
          (make-syntax #f
                       `(begin (require ,@(translate-interact-require reqs type-recs))
                               ,syn)
                       #f))))
  
  ;translate-program: package type-records -> (list compilation-unit) 
  (define (translate-program program type-recs)
    (interactions? #f)
    (let* ((package-path (if (package-name program)
                             (append (map id-string (name-path (package-name program)))
                                     (list (id-string (name-id (package-name program)))))
                             null))
           (full-defs (if (null? (packages)) (package-defs program) (append (packages) (package-defs program))))
           (dependent-defs (find-dependent-defs full-defs type-recs))
           (modules (map (lambda (defs)
                           (let-values (((translated-defs reqs) (translate-defs defs type-recs)))
                             (make-compilation-unit (map (lambda (def) (id-string (def-name def))) defs)
                                                    translated-defs
                                                    (map def-file defs)
                                                    reqs)))
                         dependent-defs)))
      modules))
  
  ;get-package: definition type-records -> (list string)
  (define (get-package def type-recs)
    (send type-recs set-location! (def-file def))
    (send type-recs lookup-path (id-string (def-name def)) (lambda () (error 'internal-error))))
  
  ;find-dependent-defs: (list defs) -> (list (list defs))
  (define (find-dependent-defs defs type-recs)
    (letrec ((not-found
              (lambda (def msg tbl)
                (lambda ()
                  (hash-table-put! tbl def msg) msg)))
             ;completed: maps def -> symbol
             (completed (make-hash-table))  
             ;completed? def -> bool
             (completed? 
              (lambda (def)
                (eq? 'completed (hash-table-get completed def (not-found def 'started completed)))))
             ;cycles: (list (list defs))
             (cycles null)  
             ;path maps def -> symbol
             (cycle (make-hash-table))
             ;in-cycle? def -> bool
             (in-cycle? 
              (lambda (def)
                (eq? 'in-cycle (hash-table-get cycle def (not-found def 'not-in-cycle cycle)))))
             
             ;find-cycle: def -> void
             (find-cycle 
              (lambda (def)
                (for-each (lambda (reqD)
                            (cond
                              ((or (completed? reqD) (in-cycle? reqD)) (void))
                              ((or (dependence-on-cycle reqD) (exists-path-to-cycle? reqD))
                               (hash-table-put! cycle reqD 'in-cycle)
                               (find-cycle def))))
                          (filter (lambda (x) x) (map find (def-uses def))))))
             
             ;exists-path-to-cycle: def -> bool
             (exists-path-to-cycle? 
              (lambda (def)
                (let ((reqs-in-cycle (filter (lambda (req)
                                               (or (not (completed? req))
                                                   (in-cycle? req)
                                                   (and (dependence-on-cycle req)
                                                        (hash-table-put! cycle req 'in-cycle))
                                                   (exists-path-to-cycle? req)))
                                             (filter (lambda (x) x) (map find (def-uses def))))))
                  (and (not (null? reqs-in-cycle))
                       (hash-table-put! cycle def 'in-cycle)))))
             
             ;dependence-on-cycle: req -> bool
             (dependence-on-cycle
              (lambda (reqD)
                (ormap (lambda (x) x)
                       (hash-table-map cycle (lambda (def v) (and (eq? v 'in-cycle)
                                                                  (dependence? reqD def)))))))
             
             ;to Determine if reqD directly depends on def
             (dependence? 
              (lambda (reqD def)
                (member (make-req (id-string (def-name def)) (get-package def type-recs))
                        (def-uses reqD))))
             
             ;find: req -> (U #f def)
             (find 
              (lambda (req)
                (letrec ((walker 
                          (lambda (defs)
                            (and (not (null? defs))
                                 (if (and (equal? (req-path req)
                                                  (get-package (car defs) type-recs))
                                          (equal? (req-class req)
                                                  (id-string (def-name (car defs)))))
                                     (car defs)
                                     (walker (cdr defs)))))))
                  (walker defs)))))
      
      (for-each (lambda (def)
                  (unless (completed? def)
                    (set! cycle (make-hash-table))
                    (hash-table-put! cycle def 'in-cycle)
                    (find-cycle def)
                    (let ((cyc (filter (lambda (d)
                                         (eq? (hash-table-get cycle d) 'in-cycle))
                                       (hash-table-map cycle (lambda (k v) k)))))
                      (for-each (lambda (c) (hash-table-put! completed c 'completed))
                                cyc)
                      (set! cycles (cons cyc cycles)))))
                defs)
      cycles))
  
  
  (define (make-composite-name d)
    (build-identifier (string-append (id-string (header-id (if (class-def? d)
                                                               (class-def-info d)
                                                               (interface-def-info d))))
                                     "-composite")))
  
  ;translate-defs: (list def) type-records -> (values (list syntax) (list reqs))
  (define (translate-defs defs type-recs)
    (module-name (make-composite-name (car defs)))
    (module-require (if (to-file) 
                        (let ((location (build-path (begin (send type-recs set-location! (def-file (car defs)))
                                                           (send type-recs get-compilation-location) "compiled"))))
                          `(file ,(build-path (string-append (symbol->string (module-name))) ".zo"))
                          (send type-recs set-composite-location (id-string (def-name (car defs))) location))
                        (module-name)))
    (let* ((translated-defs (map (lambda (d)
                                   (if (class-def? d)
                                       (translate-class d type-recs)
                                       (translate-interface d type-recs)))
                                 defs))
           (group-reqs (apply append (map (lambda (d) 
                                            (map (lambda (r) (list (def-file d) r)) (def-uses d)))
                                          defs)))
           (reqs (filter-reqs group-reqs defs type-recs)))
      (values (if (> (length translated-defs) 1)
                  (cons (make-syntax #f `(module ,(module-name) mzscheme
                                           (require (lib "class.ss")
                                                    (prefix javaRuntime: (lib "runtime.scm" "profj" "libs" "java"))
                                                    ,@(translate-require reqs type-recs))
                                           ,@(map car translated-defs))
                                     #f)
                        (map cadr translated-defs))
                  (list (make-syntax #f 
                                     `(module ,(build-identifier (regexp-replace "-composite" 
                                                                                 (symbol->string (module-name)) 
                                                                                 ""))
                                        mzscheme
                                        (require (lib "class.ss")
                                                 (prefix javaRuntime: (lib "runtime.scm" "profj" "libs" "java"))
                                                 ,@(translate-require (map (lambda (r) (list (def-file (car defs)) r))
                                                                           (def-uses (car defs)))
                                                                      type-recs))
                                        ,(car (car translated-defs)))
                                     #f)))
              (filter (lambda (req) (not (member req reqs)))
                      (map (lambda (r-pair) (cadr r-pair)) group-reqs)))))
  
  ;filter-reqs: (list (list location req)) (list def) type-records -> (list req)
  (define filter-reqs
    (lambda (reqs defs type-recs)
      (if (null? reqs)
          null
          (if (or (reference (car reqs) defs type-recs)
                  (req-member (car reqs) (cdr reqs)))
              (filter-reqs (cdr reqs) defs type-recs)
              (cons (car reqs) (filter-reqs (cdr reqs) defs type-recs))))))
  
  ;reference: (list req location) (list def) type-records -> bool
  (define reference 
    (lambda (req defs type-recs)
      (and (not (null? defs))
           (or (and (equal? (req-path (cadr req)) (get-package (car defs) type-recs))
                    (equal? (req-class (cadr req)) (id-string (def-name (car defs)))))
               (reference req (cdr defs) type-recs)))))
  
  ;req-member: (list location req) (list (list location req)) -> bool
  (define req-member
    (lambda (req reqs)
      (and (not (null? reqs))
           (or (equal? (cadr req) (cadr (car reqs)))
               (req-member req (cdr reqs))))))
  
  (define (translate-interact-require reqs type-recs)
    (if (null? reqs)
        null
        (let ((req (car reqs)))
          (cons (begin (send type-recs set-location! 'interactions)
                       (send type-recs get-require-syntax
                             (send type-recs require-prefix?
                                   (cons (req-class req) (req-path req))
                                   (lambda () #f))
                             (cons (req-class req) (req-path req))
                             (lambda () #f)))
                (translate-interact-require (cdr reqs) type-recs)))))
  
  ;translate-require: (list req) type-records -> (list syntax)
  (define (translate-require reqs type-recs)
    (if (null? reqs)
        null
        (let* ((req (cadr (car reqs)))
               (err (lambda () (error 'translate-require (format "Internal Error: ~a not found" req)))))
          (cons (begin (send type-recs set-location! (car (car reqs)))
                       (send type-recs get-require-syntax 
                             (send type-recs require-prefix? 
                                   (cons (req-class req) (req-path req)) 
                                   err)
                             (cons (req-class req) (req-path req))
                             err))
                (translate-require (cdr reqs) type-recs)))))
  
  ;translate-implements: (list name) -> (list syntax)
  (define (translate-implements imp)
    (map (lambda (i)
           (let* ((id (name-id i))
                  (st (id-string id))
                  (path (name-path i)))
             (if (null? path)
                 (translate-id st (id-src id))
                 (create-syntax #f (build-identifier (append (map id-string path) (list st)))
                                (build-src (name-src i))))))
         imp))
  
  
  ;Converted
  ;translate-class: class-def type-records -> (list syntax syntax)
  (define translate-class
    (lambda (class type-recs)
      (let*-values (((header) (class-def-info class))
                    ((parent parent-src) 
                     (if (null? (header-extends header))
                         (values "Object" (make-src 0 0 0 0))
                         (get-parent (header-extends header))))
                    ((class*) (create-syntax #f 'class* (build-src (class-def-key-src class))))
                    ((class-members) (separate-members (class-def-members class)))
                    ((methods) (separate-methods (members-method class-members) (make-accesses null null null null null)))
                    ((fields) (separate-fields (members-field class-members) (make-accesses null null null null null))))
        ;Set class-specific parameters (ok as only one class compiled at a time)
        (loc (class-def-file class))
        (class-name (id-string (header-id header)))
        (parent-name parent)
        (class-override-table (make-hash-table))
        
        (send type-recs set-location! (loc))
        
        (let* ((class (translate-id (class-name) 
                                    (id-src (header-id header))))
               (overridden-methods (get-overridden-methods (append (accesses-public methods) 
                                                                   (accesses-package methods) 
                                                                   (accesses-protected methods))
                                                           type-recs))
               (restricted-methods (make-method-names ;(append (accesses-package methods)
                                                              (accesses-protected methods);)
                                                      overridden-methods
                                                      type-recs))
               (static-method-names (make-static-method-names (accesses-static methods) type-recs))
               (static-field-names (make-static-field-names (accesses-static fields)))
               (static-field-setters (make-static-field-setters-names 
                                      (filter (lambda (f) (not (final? 
                                                                (map modifier-kind (field-modifiers f)))))
                                              (accesses-static fields))))
               (field-getters/setters (make-field-accessor-names (append (accesses-public fields)
                                                                         (accesses-package fields)
                                                                         (accesses-protected fields))))
               (provides `(provide ,(build-identifier (class-name))
;                                   ,@restricted-methods
                                   ,@(map build-identifier static-method-names)
                                   ,@(map build-identifier static-field-names)
                                   ,@static-field-setters
                                   ,@field-getters/setters)))
          
          (list `(begin ,provides
 ;                       ,(create-local-names (append (make-method-names (accesses-private methods)
 ;                                                                       (get-statics (accesses-private methods))
 ;                                                                       type-recs)
 ;                                                    restricted-methods))
                        (define ,class 
                          (,class* ,(translate-id parent parent-src) ,(translate-implements (header-implements header))
                           
                           ,@(map (lambda (f) (translate-field (map modifier-kind (field-modifiers f))
                                                               (field-type f)
                                                               (field-name f)
                                                               (and (var-init? f) f)
                                                               (if (var-init? f)
                                                                   (var-init-src f)
                                                                   (var-decl-src f))
                                                               #f))
                                  (append (accesses-public fields)
                                          (accesses-package fields)
                                          (accesses-protected fields)
                                          (get-non-statics (accesses-private fields))))
                           ,@(create-private-setters/getters (get-non-statics (accesses-private fields)))
                           
                           ,@(map (lambda (m) (translate-method (method-type m)
                                                                (map modifier-kind (method-modifiers m))
                                                                (method-name m)
                                                                (method-parms m)
                                                                (method-body m)
                                                                (method-src m)
                                                                type-recs))
                                  (append (accesses-public methods)
                                          (accesses-package methods)
                                          (accesses-protected methods)
                                          (get-non-statics (accesses-private methods))))
                           
                           (define/override (my-name) ,(class-name))
                           
                           (rename (super-field-names field-names))
                           (define/override (field-names)
                             (append (super-field-names)
                                     (list ,@(map (lambda (n) (id-string (field-name n)))
                                                  (append (accesses-public fields)
                                                          (accesses-package fields)
                                                          (accesses-protected fields)
                                                          (get-non-statics (accesses-private fields)))))))
                           (rename (super-field-values field-values))
                           (define/override (field-values)
                             (append (super-field-values)
                                     (list ,@(map (lambda (n) (build-identifier (build-var-name (id-string (field-name n)))))
                                                  (append (accesses-public fields)
                                                          (accesses-package fields)
                                                          (accesses-protected fields)
                                                          (get-non-statics (accesses-private fields)))))))
                           
                           ,@(map (lambda (i) (translate-initialize (initialize-static i)
                                                                    (initialize-block i)
                                                                    (initialize-src i)
                                                                    type-recs))
                                  (members-init class-members))
                           
                           (super-instantiate ())))

                        ,@(create-static-methods (append static-method-names
                                                         (make-static-method-names 
                                                          (get-statics (accesses-private methods)) 
                                                          type-recs))
                                                 (append (accesses-static methods)
                                                         (get-statics (accesses-private methods)))
                                                 type-recs)
                        ,@(create-static-fields (append static-field-names
                                                        (make-static-field-names (get-statics (accesses-private fields))))
                                                (append (accesses-static fields)
                                                        (get-statics (accesses-private fields))))
                        ,@(create-static-setters static-field-setters
                                                 (filter (lambda (f) (not (final? 
                                                                           (map modifier-kind (field-modifiers f)))))
                                                         (accesses-static fields)))
                        ,@(create-field-accessors field-getters/setters
                                                  (append (accesses-public fields)
                                                          (accesses-package fields)
                                                          (accesses-protected fields)))
                        
                        ,@(map (lambda (i) (translate-initialize (initialize-static i)
                                                                 (initialize-block i)
                                                                 (initialize-src i)
                                                                 type-recs))
                               (members-static-init class-members)))
                (make-syntax #f
                             `(module ,(build-identifier (class-name)) mzscheme (require ,(module-require)) ,provides)
                             #f))))))
  
  ;Code to separate different member types for easier access
  ;(make-accesses (list member) (list member) (list member) ...)
  (define-struct accesses (private protected static public package))
  ;(make-members (list method) (list field) (list init) (list init))
  (define-struct members (method field static-init init))
  
  ;update: ('a 'b -> void) 'a ('b -> (list 'a)) 'b) -> 'b 
  ;Allows a set! to be passed in and applied
  (define (update set add-on access struct)
    (set struct (cons add-on (access struct)))
    struct)
  
  ;separate-members: (list member) -> members
  (define (separate-members members)
    (letrec ((my-members (make-members null null null null))
             (separate
              (lambda (m h)
                (cond
                  ((null? m) h)
                  ((method? (car m))
                   (separate (cdr m) (update set-members-method! (car m) members-method h)))
                  ((field? (car m))
                   (separate (cdr m) (update set-members-field! (car m) members-field h)))
                  ((initialize? (car m))
                   (separate (cdr m)
                             (if (initialize-static (car m))
                                 (update set-members-static-init! (car m) members-static-init h)
                                 (update set-members-init! (car m) members-init h))))))))
      (separate members my-members)))
  
  ;make-access-separator: ('a -> (list symbol)) -> ((list 'a) accesses -> accesses)
  ;Note: private static members are placed with all private members
  ;      protected, package, and public static members are placed with all non-private static members
  (define (make-access-separator get-modifiers)
    (letrec ((separate
              (lambda (m h)
                (if (null? m) h
                    (separate (cdr m)
                              (let* ((current (car m))
                                     (modifiers (map modifier-kind (get-modifiers current))))
                                (cond
                                  ((private? modifiers) (update set-accesses-private! current accesses-private h))
                                  ((static? modifiers) (update set-accesses-static! current accesses-static h))
                                  ((protected? modifiers) (update set-accesses-protected! current accesses-protected h))
                                  ((public? modifiers) (update set-accesses-public! current accesses-public h))
                                  (else (update set-accesses-package! current accesses-package h)))))))))
      separate))
  
  ;separate-methods: (list method) accesses -> accesses
  (define separate-methods (make-access-separator method-modifiers))  
  ;separate-fields: (list field) accesses -> accesses
  (define separate-fields (make-access-separator field-modifiers))
  
  ;get-parent: name -> (values string src)
  (define get-parent
    (lambda (parent)
      (when (= (length parent) 1) (set! parent (car parent)))
      (when (null? parent) (set! parent (make-name (make-id "Object" #f) null #f)))
      (if (null? (name-path parent))
          (values (id-string (name-id parent)) (id-src (name-id parent)))
          (values (string-append (apply string-append (map (lambda (p)
                                                             (format "~s." p))
                                                           (map id-string (name-path parent))))
                                 (id-string (name-id parent)))
                  (name-src parent)))))
  
  ;create-local-names: (list symbol) -> syntax
  (define (create-local-names names)
    (make-syntax #f  `(define-local-member-name ,@names) #f))
  
  (define (translate-parents extends)
    (map (lambda (n)
           (if (null? (name-path n))
               (translate-id (id-string (name-id n))
                             (id-src (name-id n)))
               (create-syntax #f (build-identifier (append (map id-string (name-path n))
                                                           (list (id-string (name-id n)))))
                              (build-src (name-src n)))))
         extends))
  
  ;Converted
  ;translate-interface: interface-def type-records-> (list syntax)
  (define translate-interface
    (lambda (class type-recs)
      (let* ((header (interface-def-info class))
             (name (build-identifier (id-string (header-id header))))
             (syntax-name (translate-id (id-string (header-id header))
                                        (id-src (header-id header))))
             (source (build-src (interface-def-src class)))
             (interface (create-syntax #f 'interface (build-src (interface-def-key-src class))))
             (members (separate-members (interface-def-members class))))
        
        (loc (interface-def-file class))
        (class-name (id-string (header-id header)))
        (send type-recs set-location! (loc))
        
        (let* ((static-field-names (map build-identifier (make-static-field-names (members-field members))))
               (provides `(provide ,name ,@static-field-names)))
          
          (list `(begin ,provides
                        (define ,syntax-name (,interface ,(translate-parents (header-extends header))
                                              ,@(make-method-names (members-method members) null type-recs)))
                        ,@(create-static-fields static-field-names (members-field members)))
                (make-syntax #f `(module ,name mzscheme (requires ,(module-name)) ,provides) #f))))))
  
  ;override?: symbol type-records -> bool
  (define (override? method-name type-recs)
    (let* ((internal-error (lambda () (error 'override "Internal Error class or it's parent not in class record table")))
           (class-record (send type-recs get-class-record 
                               (make-ref-type (class-name)
                                              (send type-recs lookup-path (class-name) internal-error))
                               internal-error))
           (parent-record (send type-recs get-class-record  (car (class-record-parents class-record)) internal-error)))
      (memq method-name
            (map (lambda (m) (string->symbol (build-method-name (method-record-name m)
                                                                (method-record-atypes m))))
                 (class-record-methods parent-record)))))
  
  ;returns whether or not given method overrides an existing method
  ;overrider? symbol type-records -> bool
  (define (overrider? method-name type-recs)
    (hash-table-get (class-override-table) 
                    method-name 
                    (lambda () 
                      (let ((o? (override? method-name type-recs)))
                        (hash-table-put! (class-override-table) method-name o?)
                        o?))))
  
  ;get-overridden-names: (list method) type-records -> (list method)
  (define (get-overridden-methods methods type-recs) 
    (filter (lambda (m) 
              (let ((mname (id-string (method-name m))))
                (overrider? (build-identifier ((if (constructor? mname) build-constructor-name build-method-name)
                                               mname
                                               (map (lambda (t) (type-spec-to-type t 'full type-recs))
                                                    (map field-type (method-parms m)))))
                            type-recs)))
            methods))
  
  ;make-method-names: (list methods) (list methods) type-records -> (list symbol)
  (define (make-method-names methods minus-methods type-recs)
    (if (null? methods)
        null
        (if (memq (car methods) minus-methods)
            (make-method-names (cdr methods) minus-methods type-recs)
            (cons 
             (build-identifier ((if (constructor? (id-string (method-name (car methods))))
                                    build-constructor-name
                                    build-method-name)
                                (id-string (method-name (car methods)))
                                (map (lambda (t) (type-spec-to-type t 'full type-recs))
                                     (map field-type (method-parms (car methods))))))
             (make-method-names (cdr methods) minus-methods type-recs)))))
  
  ;translate-method: type-spec (list symbol) id (list parm) statement src type-records -> syntax
  (define (translate-method type modifiers id parms block src type-recs)
    (let* ((final (final? modifiers))
           (method-string ((if (constructor? (id-string id)) build-constructor-name build-method-name)
                           (id-string id)
                           (map (lambda (t) (type-spec-to-type t 'full type-recs))
                                (map field-type parms))))
           (method-name (translate-id method-string (id-src id)))
           (over? (overrider? (build-identifier method-string) type-recs))
           (definition (cond
                         ((and over? final) 'define/override-final)
                         (over? 'define/override)
                         (final 'define/public-final)
                         (else 'define/public))))
      (create-syntax #f
                     `(,definition ,method-name 
                       ,(translate-method-body method-string parms block modifiers type type-recs))
                     (build-src src))))
  
  ;make-static-method-names: (list method) type-recs -> (list string)
  (define (make-static-method-names methods type-recs)
    (map (lambda (m)
           (build-static-name (build-method-name (id-string (method-name m))
                                                 (map (lambda (t) (type-spec-to-type t 'full type-recs))
                                                      (map field-type (method-parms m))))))
         methods))
  
  ;create-static-methods: (list string) (list method) type-records -> (list syntax)
  (define (create-static-methods names methods type-recs)
    (if (null? names)
        null
        (let ((name (car names))
              (method (car methods)))
          (cons (create-syntax #f
                               `(define ,(translate-id name (id-src (method-name method)))
                                  ,(translate-method-body name
                                                          (method-parms method)
                                                          (method-body method)
                                                          (map modifier-kind (method-modifiers method))
                                                          (method-type method)
                                                          type-recs))
                               (build-src (method-src method)))
                (create-static-methods (cdr names) (cdr methods) type-recs)))))
  
  ;translate-method-body (list field) statement (list symbol) type-spec type-record -> syntax
  (define (translate-method-body method-name parms block modifiers rtype type-recs)
    (let ((parms (translate-parms parms))
          (void? (eq? (type-spec-name rtype) 'void))
          (native? (memq 'native modifiers))
          (static? (memq 'static modifiers)))
      (make-syntax #f
                   (cond
                     ((and block void?)
                      `(lambda ,parms 
                         (let/ec return-k
                           ,(translate-statement block type-recs)
                           (void))))
                     ((and block (not void?))
                      `(lambda ,parms
                         (let/ec return-k
                           ,(translate-statement block type-recs))))
                     ((and (not block) (memq 'abstract modifiers))
                      `(lambda ,parms (void)))
                     ((and (not block) native? void? (not static?))
                      `(lambda ,parms
                         (,(build-identifier (string-append method-name "-native")) this ,@parms)
                         (void)))
                     ((and (not block) native? (not static?))
                      `(lambda ,parms
                         (,(build-identifier (string-append method-name "-native")) this ,@parms)))
                     ((and (not block) native? void? static?)
                      `(lambda ,parms
                         (,(build-identifier (string-append method-name "-native")) ,@parms)
                         (void)))
                     ((and (not block) native? static?)
                      `(lambda ,parms
                         (,(build-identifier (string-append method-name "-native")) ,@parms))))
                   #f)))
  
  ;translate-parms: (list field) -> (list syntax)
  (define (translate-parms parms)
    (map (lambda (parm)
           (translate-id (build-var-name (id-string (field-name parm)))
                         (id-src (field-name parm))))
         parms))
  
  ;make-field-accessor-names: (list fields) -> (list symbol)
  (define (make-field-accessor-names fields)
    (if (null? fields)
        null
        (let ((name (id-string (field-name (car fields)))))
          (append (cons (create-get-name name)
                        (if (final? (map modifier-kind (field-modifiers (car fields))))
                            null
                            (list (create-set-name name))))
                  (make-field-accessor-names (cdr fields))))))
  
  (define (create-field-accessors names fields)
    (if (null? fields)
        null
        (let* ((field (car fields))
               (class (build-identifier (class-name)))
               (quote-name (build-identifier (build-var-name (id-string (field-name field)))))
               (getter (car names))
               (final (final? (map modifier-kind (field-modifiers field)))))
          (append (cons (make-syntax #f `(define ,getter
                                           (class-field-accessor ,class ,quote-name)) #f)
                        (if (not final)
                            (list (make-syntax #f `(define ,(cadr names)
                                                     (class-field-mutator ,class ,quote-name)) #f))
                            null))
                  (create-field-accessors (if final (cdr names) (cddr names)) (cdr fields))))))
  
  (define (make-static-field-setters-names fields) 
    (map (lambda (f) (create-set-name (id-string (field-name f)))) fields))
  
  (define (create-static-setters names fields)
    (if (null? names)
        null
        (let ((name (car names))
              (field (car fields)))
          (cons (make-syntax #f
                             `(define (,name my-val)
                                (set! ,(build-identifier (build-var-name (build-static-name (id-string (field-name field))))) 
                                      my-val))
                             #f)
                (create-static-setters (cdr names) (cdr fields))))))
  
  ;create-private-setters/getters fields -> (list syntax)
  (define (create-private-setters/getters fields)
    (if (null? fields)
        null
        (let* ((field (car fields))
               (s-name (id-string (field-name field)))
               (name (build-identifier (build-var-name s-name)))
               (getter (create-get-name s-name))
               (setter (create-set-name s-name)))
          (append (list (make-syntax #f `(define (,getter my-val) ,name) (build-src (id-src (field-name field))))
                        (make-syntax #f `(define (,setter m-obj my-val) (set! ,name my-val)) 
                                     (build-src (id-src (field-name field)))))
                  (create-private-setters/getters (cdr fields))))))
  
  
  
  (define (make-static-field-names fields)
    (map (lambda (f) (build-static-name (build-var-name (id-string (field-name f))))) fields))
  
  (define (create-static-fields names fields)
    (if (null? names)
        null
        (let ((name (car names))
              (f (car fields)))
          (cons (make-syntax #f
                             `(define ,(translate-id name (id-src (field-name f))) 
                                ,(translate-field-body (and (var-init? f) f) (field-type f)))
                             (build-src (if (var-init? f) (var-init-src f) (var-decl-src f))))
                (create-static-fields (cdr names) (cdr fields))))))
  
  ;translate-field: (list symbol) type-spec id (U #f var-init) src bool -> syntax
  (define translate-field
    (lambda (access type name init? src static?)
      (let ((value (translate-field-body init? type))
            (field-name (translate-id (build-var-name (if static? (build-static-name (id-string name)) (id-string name)))
                                      (id-src name))))
        (if (or static? (private? access))
            (make-syntax #f `(define ,field-name ,value) (build-src src))
            (make-syntax #f `(field (,field-name ,value)) (build-src src))))))
  
  ;translate-field-body (U bool var-init) type -> syntax
  (define (translate-field-body init? type)
    (if init?
        (if (array-init? (var-init-init init?))
            (initialize-array (array-init-vals (var-init-init init?))
                              type)
            (translate-expression (var-init-init init?)))
        (get-default-value type)))
  
  ;translate-initialize: bool block src string type-records -> syntax
  (define (translate-initialize static? body src type-recs)
    (translate-block (block-stmts body) (block-src body) type-recs))
  
  
  ;-------------------------------------------------------------------------------------------------------------------------
  ;translate-statement
  ;translates a Java statement into a Scheme expresion. 
  ;raises an error if it has no implementation for a statement type
  
  ;Converted
  ;translate-statement: statement string type-records -> syntax
  (define translate-statement
    (lambda (statement type-recs)
      (cond
        ((ifS? statement)
         (translate-if (translate-expression (ifS-cond statement))
                       (translate-statement (ifS-then statement) type-recs)
                       (if (ifS-else statement)
                           (translate-statement (ifS-else statement) type-recs)
                           'void)
                       (ifS-key-src statement)
                       (ifS-src statement)))
        ((throw? statement)
         (translate-throw (translate-expression (throw-expr statement))
                          (throw-key-src statement)
                          (throw-src statement)))
        ((return? statement) 
         (translate-return (if (return-expr statement)
                               (translate-expression (return-expr statement))
                               (datum->syntax-object #f '(void) #f))
                           (return-src statement)))
        ((while? statement)
         (translate-while (translate-expression (while-cond statement))
                          (translate-statement (while-loop statement) type-recs)
                          (while-src statement)))
        ((doS? statement)
         (translate-do (translate-statement (doS-loop statement) type-recs)
                       (translate-expression (doS-cond statement))
                       (doS-src statement)))
        ((for? statement)
         (translate-for (for-init statement)
                        (translate-expression (for-cond statement))
                        (map translate-expression (for-incr statement))
                        (translate-statement (for-loop statement) type-recs)
                        (for-src statement)
                        type-recs))
        ((try? statement)
         (translate-try (translate-statement (try-body statement) type-recs)
                        (try-catches statement)
                        (and (try-finally statement)
                             (translate-statement (try-finally statement) type-recs)) 
                        (try-key-src statement)
                        (try-src statement)
                        type-recs))
        ((switch? statement)
         (translate-switch (translate-expression (switch-expr statement))
                           (switch-cases statement)
                           (switch-src statement)
                           type-recs))
        ((block? statement)
         (translate-block (block-stmts statement) (block-src statement) type-recs))        
        ((break? statement)
         (translate-break (break-label statement)  (break-src statement)))
        ((continue? statement)
         (translate-continue (continue-label statement) (continue-src statement)))
        ((label? statement)
         (translate-label (label-label statement)
                          (translate-statement (label-stmt statement) type-recs)
                          (label-src statement)))
        ((synchronized? statement)
         (translate-synchronized (translate-expression (synchronized-expr statement))
                                 (translate-statement (synchronized-stmt statement) type-recs)
                                 (synchronized-src statement)))        
        ((statement-expression? statement)
         (translate-expression statement))
        (else
         (error 'translate-statement (format "translate-statement given unsupported: ~s" statement))))))
  
  
  ;Converted
  ;translate-if: syntax syntax syntax src src -> syntax
  (define translate-if
    (lambda (if? then else key src)
      (create-syntax #f `(,(create-syntax #f `if (build-src key)) ,if? ,then ,else) (build-src src))))
  
  ;Converted
  ;translate-throw: syntax src src -> syntax
  (define translate-throw
    (lambda (expr key src)
      (create-syntax #f `(let* ((obj ,expr)
                                (exn (make-java:exception (send obj |getMessage|) (current-continuation-marks) obj)))
                           (send obj set-exception! exn)
                           (,(create-syntax #f 'raise (build-src key)) exn)) 
                     (build-src src))))
  
  ;return -> call to a continuation 
  ;Presently a no-op in the interactions window, although this is incorrect for advanced and full
  ;translate-return: syntax src -> syntax
  (define (translate-return expr src)
    (if (interactions?)
        (make-syntax #f expr #f)
        (make-syntax #f `(return-k ,expr) (build-src src))))
  
  ;Converted
  ;translate-while: syntax syntax src -> syntax
  (define translate-while
    (lambda (cond body src)
      (make-syntax #f `(let/ec break-k
                         (let loop ()
                           (when ,cond
                             ,body
                             (loop))))
                   (build-src src))))
  
  ;Converted
  ;translate-do: syntax syntax src -> syntax
  (define translate-do
    (lambda (body cond src)
      (make-syntax #f `(begin ,body
                              (let/ec break-k
                                (let loop ()
                                  (when ,cond
                                    ,body
                                    (loop)))))
                   (build-src src))))
  
  ;Converted
  ;translate-for: (U (list statement) (list field)) syntax (list syntax) syntax src type-records-> syntax
  (define translate-for
    (lambda (init cond incr body src type-recs)
      (let ((loop `(let/ec break-k
                     (let loop ()
                       (when ,cond
                         ,body
                         ,@incr
                         (loop)))))
            (source (build-src src)))
        (if (and (pair? init) (field? (car init)))
            (make-syntax #f `(letrec (,@(map (lambda (var)
                                               `(,(translate-id (build-var-name (id-string (field-name var)))
                                                                (id-src (field-name var)))
                                                 ,(if (var-init? var)
                                                      (if (array-init? (var-init-init var))
                                                          (initialize-array (array-init-vals (var-init-init var)) 
                                                                            (field-type var))
                                                          (translate-expression (var-init-init var)))
                                                      (get-default-value (field-type var))))) 
                                             init))
                               ,loop) source)
            (make-syntax #f `(begin
                               ,@(map (lambda (s) (translate-statement s type-recs)) init)
                               ,loop)
                         source)))))
  
  ;Converted
  ;initialize-array: (list (U expression array-init)) type-spec string-> syntax
  (define initialize-array
    (lambda (inits type)
      (cond
        ((null? inits) (error 'initialize-array "Given empty list"))
        ;Note This has the wrong type for recursive cases! must fix PROBLEM! Still PROBLEM!
        ((array-init? (car inits))
         (make-syntax #f
                      `(make-java-array ,(translate-type-spec type)
                                        0
                                        (reverse (list ,@(map (lambda (a) (initialize-array (array-init-vals a) type))
                                                              inits))))
                      (build-src (array-init-src (car inits)))))
        (else
         (make-syntax #f
                      `(make-java-array ,(translate-type-spec type)
                                        0
                                        (reverse (list ,@(map translate-expression inits))))
                      (build-src (if (name? (car inits)) (name-src (car inits)) (expr-src (car inits)))))))))
  
  ;Converted
  ;translate-try: syntax (list catch) (U syntax boolean) src src type-records-> syntax
  (define translate-try
    (lambda (block catches finally key src type-recs)
      (let* ((handle (create-syntax #f 'with-handlers (build-src key)))
             (handlers (make-syntax #f `(,handle [ ,@(make-predicates catches type-recs) ]
                                         ,block)
                                    (build-src src))))
        (if finally
            (make-syntax #f
                         `(dynamic-wind void
                                        (lambda () ,handlers)
                                        (lambda () ,finally))
                         #f)
            handlers))))
  
  ;Converted
  ;make-predicates: (list catch) type-records-> (list syntax)
  (define make-predicates
    (lambda (catches type-recs)
      (map (lambda (catch)
             (let* ((catch-var (catch-cond catch))
                    (var-src (var-decl-src catch-var))
                    (class-name (get-class-name (field-type catch-var)))
                    (isRuntime? (descendent-Runtime? (field-type catch-var) type-recs))
                    (type 
                     (if isRuntime?
                         (make-syntax #f `exn? (build-src var-src))
                         (make-syntax #f 
                                      `(javaException:exception-is-a? ,class-name)
                                      (build-src var-src))))
                    (parm (translate-id (build-var-name (id-string (field-name catch-var)))
                                        (id-src (field-name catch-var))))
                    (block (make-syntax #f
                                        `(lambda (,parm)
                                           ,(translate-statement (catch-body catch) type-recs))
                                        (build-src (catch-src catch)))))
               (make-syntax #f `(,type                       
                                 ,(if isRuntime?
                                      `(lambda (exn)
                                         (if (javaException:supported-runtime-exception? exn)
                                             (,block (javaException:exception-to-class exn))
                                             (raise exn)))
                                      block))
                            (build-src (catch-src catch)))))
           catches)))
  
  ;Determines if the given type represents a class that is a descendent of the RuntimeException class
  ;descendent-Runtime?: type-spec type-records -> bool
  (define descendent-Runtime?
    (lambda (type type-recs)
      (let ((class-record (send type-recs get-class-record (type-spec-to-type type 'full type-recs) 
                                (lambda () (error 'descendent-Runtime "Internal Error: class record is not in table")))))
        (member `("java" "lang" "RuntimeException") (class-record-parents class-record)))))
  
  ;Converted
  ;translate-switch: syntax (list CaseStatements) src type-records -> syntax
  (define translate-switch
    (lambda (expr cases src type-recs)
      (make-syntax #f
                   `(case ,expr
                      ,@(map (lambda (case) 
                               (if (eq? (caseS-constant case) 'default)
                                   (if (null? (caseS-body case))
                                       `(else (void))
                                       `(else ,(translate-block (caseS-body case) (caseS-src case) type-recs)))
                                   `((,(translate-expression (caseS-constant case))
                                      ,(translate-block (caseS-body case) (caseS-src case) type-recs)))))
                             cases))
                   (build-src src))))
  
  ;Converted
  ;translate-block: (list (U Statement (U var-decl var-init))) src type-recs -> syntax
  (define translate-block
    (lambda (statements src type-recs)
      ;(list (U Statement (U var-decl var-init))) -> (list syntax)
      (letrec ((translate
                (lambda (statements)
                  (if (null? statements)
                      null
                      (let ((statement (car statements)))
                        (if (field? statement)
                            (translate-var (car statements) (cdr statements))
                            (cons (translate-statement statement type-recs)
                                  (translate (cdr statements))))))))
               (translate-var
                (lambda (var statements)
                  (let* ((is-var-init? (var-init? var))
                         (id (translate-id (build-var-name (id-string (field-name var))) (id-src (field-name var)))))
                    (list (make-syntax #f 
                                       `(letrec
                                            ((,id ,(if is-var-init?
                                                       (if (array-init? (var-init-init var))
                                                           (initialize-array (array-init-vals (var-init-init var))
                                                                             (field-type var))
                                                           (translate-expression (var-init-init var)))
                                                       (get-default-value (field-type var)))))
                                          ,@(if (null? statements)
                                                (list `(void))
                                                (translate statements)))
                                       (build-src (if is-var-init?
                                                      (var-init-src var)
                                                      (var-decl-src var)))))))))
        (if (null? statements)
            (make-syntax #f `void (build-src src))
            (make-syntax #f `(begin ,@(translate statements)) (build-src src))))))
  
  ;Converted
  ;translate-break: (U id #f) src -> syntax
  (define translate-break
    (lambda (id src)
      (if (not id)
          (make-syntax #f `(break-k void) (build-src src))
          (make-syntax #f `(,(translate-id (string-append (id-string id "-k")) (id-src id)) void) (build-src src)))))
  
  ;Converted
  ;translate-continue: (U string #f) src -> syntax
  (define translate-continue
    (lambda (id src)
      (if (not id)
          (make-syntax #f `(break-k (loop)) (build-src src))
          (make-syntax #f `(,(translate-id (string-append (id-string id) "-k") (id-src id)) 
                            (,(build-identifier (string-append (id-string id) "-continue"))))
                       (build-src src)))))
  
  ;translate-label: id syntax src -> syntax
  ;NOTE: probably does not have correct behavior
  (define translate-label
    (lambda (label stmt src)
      (make-syntax #f
                   `(let/ec ,(translate-id (string-append (id-string label) "-k") (id-src label))
                      (letrec ((,(build-identifier (string-append (id-string label) "-continue"))
                                (lambda () ,stmt)))
                        (,(build-identifier (string-append (id-string label) "-continue")))))
                   (build-src src))))
  
  ;translate-synchronized: syntax syntax src -> syntax
  ;PROBLEM! Does nothing
  (define translate-synchronized
    (lambda (expr stmt src)
      (make-syntax #f
                   `(begin ,expr ,stmt)
                   (build-src src))))
  
  ;------------------------------------------------------------------------------------------------------------------------
  ;translate-expression
  ;translates a Java expression into a Scheme expression.
  ;raises an error if it has no implementation for an expression type
  
  ;converted
  ;translate-expression: Expression -> syntax
  (define translate-expression
    (lambda (expr)
      (cond
        ((literal? expr) (translate-literal (expr-types expr)
                                            (literal-val expr)
                                            (expr-src expr)))
        ((bin-op? expr) (translate-bin-op (bin-op-op expr)
                                          (translate-expression (bin-op-left expr))
                                          (expr-types (bin-op-left expr))
                                          (translate-expression (bin-op-right expr))
                                          (expr-types (bin-op-right expr))
                                          (bin-op-key-src expr)
                                          (expr-src expr)
                                          (expr-types expr)))
        ((access? expr) (translate-access (access-name expr)
                                          (expr-src expr)))
        ((special-name? expr) (translate-special-name (special-name-name expr)
                                                      (expr-src expr)))
        ((call? expr) (translate-call (call-expr expr)
                                      (call-method-name expr)
                                      (map translate-expression (call-args expr))
                                      (call-method-record expr)
                                      (expr-src expr)))
        ((class-alloc? expr) (translate-class-alloc (class-alloc-name expr)
                                                    (map expr-types (class-alloc-args expr))
                                                    (map translate-expression (class-alloc-args expr))
                                                    (expr-src expr)
                                                    (class-alloc-ctor-record expr)))
        ((array-alloc? expr)(translate-array-alloc (array-alloc-name expr)
                                                   (map translate-expression (array-alloc-size expr))
                                                   (expr-src expr)))
        ((cond-expression? expr) (translate-cond (translate-expression (cond-expression-cond expr))
                                                 (translate-expression (cond-expression-then expr))
                                                 (translate-expression (cond-expression-else expr))
                                                 (expr-src expr)))
        ((array-access? expr) (translate-array-access (translate-expression (array-access-name expr))
                                                      (translate-expression (array-access-index expr))
                                                      (expr-src expr)))
        ((post-expr? expr) (translate-post-expr (translate-expression (post-expr-expr expr))
                                                (post-expr-op expr)
                                                (post-expr-key-src expr)
                                                (expr-src expr)))
        ((pre-expr? expr) (translate-pre-expr (pre-expr-op expr)
                                              (translate-expression (pre-expr-expr expr))
                                              (pre-expr-key-src expr)
                                              (expr-src expr)))
        ((unary? expr) (translate-unary (unary-op expr)
                                        (translate-expression (unary-expr expr))
                                        (unary-key-src expr)
                                        (expr-src expr)))
        ((cast? expr) (translate-cast (cast-type expr)
                                      (translate-expression (cast-expr expr))
                                      (expr-src expr)))
        ((instanceof? expr) (translate-instanceof (translate-expression (instanceof-expr expr))
                                                  (instanceof-type expr)
                                                  (expr-src expr)))
        ((assignment? expr) (translate-assignment (assignment-left expr)
                                                  (assignment-op expr)
                                                  (translate-expression (assignment-right expr))
                                                  (assignment-right expr)
                                                  (expr-types expr)
                                                  (assignment-key-src expr)
                                                  (expr-src expr)))
        (else
         (error 'translate-expression (format "Translate Expression given unrecognized expression ~s" expr))))))
  
  ;All of the following functions translate Java Expressions into syntax.
  ;Straightforward unless otherwise noted
  
  ;Converted
  ;translate-literal: symbol value src -> syntax
  (define translate-literal
    (lambda (type value src)
      (create-syntax #f 
                     (case type
                       ((char int long float double boolean) value)
                       ((String string) 
                        `(let ((temp-obj (make-object |String|)))
                           (send temp-obj make-mzscheme-string ,value)
                           temp-obj))
                       ((null) 'null)
                       (else
                        (error 'translate-literal (format "Translate literal given unknown type: ~s" type))))
                     (build-src src))))
  
  ;;is-string? type -> bool
  (define is-string?
    (lambda (type)
      (eq? type 'string)))
  ;;is-int? type -> bool
  (define is-int?
    (lambda (type)
      (eq? type 'int)))
  ;;is-char? type -> bool
  (define is-char?
    (lambda (type)
      (eq? type 'char)))
  
  ;Converted
  ;translate-bin-op: symbol syntax type syntax type src src type-> syntax
  (define (translate-bin-op op left left-type right right-type key src type)
    (let ((source (build-src src))
          (op-syntax (create-syntax #f op (build-src key)))
          (left (if (is-char? left-type) 
                    (make-syntax #f `(char->integer ,left) #f)
                    left))
          (right (if (is-char? right-type)
                     (make-syntax #f `(char->integer ,right) #f)
                     right)))
      (case op
        ;Mathematical operations
        ;PROBLEM! + and - do not take into account the possibility of overflow
        ((+)
         (cond 
           ((and (is-string-type? type) (is-string-type? left-type))
            (make-syntax #f `(send ,left concat-java.lang.String (javaRuntime:convert-to-string ,right)) source))
           ((and (is-string-type? type) (is-string-type? right-type))
            (make-syntax #f `(send (javaRuntime:convert-to-string ,left) concat-java.lang.String ,right) source))
           ((is-string-type? type)
            (make-syntax #f 
                         `(send (javaRuntime:convert-to-string ,left) concat-java.lang.String 
                                (javaRuntime:convert-to-string ,right)) 
                         source))
           (else
            (create-syntax #f `(,op-syntax ,left ,right) source))))
        ((- *) (make-syntax #f `(,op-syntax ,left ,right) source))
        ((/) (if (is-int? type)
                 (make-syntax #f `(,(create-syntax #f 'javaRuntime:divide-int (build-src key)) ,left ,right) source)
                 (make-syntax #f `(,(create-syntax #f 'javaRuntime:divide-float (build-src key)) ,left ,right) source)))
        ((%) (make-syntax #f `(,(create-syntax #f 'javaRuntime:mod (build-src key)) ,left ,right) source))
        ;Shift operations
        ((<< >> >>>) (make-syntax #f `(,(create-syntax #f 'javaRuntime:shift (build-src key)) ,op ,left ,right) source))
        ;comparisons
        ((< > <= >=) (make-syntax #f `(,op-syntax ,left ,right) source))
        ((==) (make-syntax #f `(,(create-syntax #f 'eq? (build-src key)) ,left ,right) source))
        ((!=) (make-syntax #f `(,(create-syntax #f 'javaRuntime:not-equal (build-src key)) ,left ,right) source))
        ;logicals
        ((& ^ or) (make-syntax #f `(,(create-syntax #f 'javaRuntime:bitwise (build-src key)) ,op ,left ,right) source))
        ;boolean
        ((&&) (make-syntax #f `(,(create-syntax #f 'javaRuntime:and (build-src key)) ,left ,right) source))
        ((oror) (make-syntax #f `(,(create-syntax #f 'javaRuntime:or (build-src key)) ,left ,right) source))
        (else
         (error 'translate-op (format "Translate op given unknown operation ~s" op))))))

  ;translate-access: (U field-access local-access) src -> syntax
  (define (translate-access name src)
    (cond
      ((local-access? name)
       (translate-id (build-var-name (id-string (local-access-name name)))
                     (id-src (local-access-name name))))
      ((field-access? name)
       (let* ((field-string (id-string (field-access-field name)))
              (field-src (id-src (field-access-field name)))
              (access (field-access-access name))
              (obj (field-access-object name))
              (cant-be-null? (never-null? obj))
              (expr (if obj (translate-expression obj))))
         (cond
           ((var-access-static? access)
            (translate-id (build-var-name (build-static-name field-string (var-access-class access)))
                          field-src))
           ((eq? 'array (var-access-class access))
            (if cant-be-null?
                (make-syntax #f `(send ,expr ,(translate-id field-string field-src)) (build-src src))
                (make-syntax #f
                             `(if (null? ,expr)
                                  (javaRuntime:nullError 'field)
                                  (send ,expr ,(translate-id field-string field-src)))
                             (build-src src))))
           (else
              (let ((id (create-get-name field-string (var-access-class access))))
                (if cant-be-null?
                    (make-syntax #f `(,id ,expr) (build-src src))
                    (make-syntax #f
                                 `(if (null? ,expr)
                                      (javaRuntime:nullError 'field)
                                      (,id ,expr))
                                 (build-src src))))))))))
  
  ;Converted
  ;translate-special-name: string src -> syntax
  ;I might need to change to always return this
  (define translate-special-name
    (lambda (name src)
      (let ((id (build-identifier name)))
        (make-syntax #f id (build-src src)))))
  
  ;Converted 
  ;translate-call: (U expression #f) (U special-name id) (list syntax) method-record src-> syntax
  (define (translate-call expr method-name args method-record src)
    (let ((cant-be-null? (never-null? expr))
          (expression (if expr (translate-expression expr) #f)))
      (cond
        ;Constructor case
        ((special-name? method-name)
         (let ((c-name (build-identifier (build-constructor-name 
                                          (if (equal? (special-name-name method-name) "super")
                                              (parent-name)
                                              (class-name)) 
                                          (method-record-atypes method-record)))))
           (if cant-be-null?
               (create-syntax #f `(send ,(if expr expression 'this) ,c-name ,@args) (build-src src))
               (create-syntax #f 
                              `(if (null? ,expression)
                                   (javaRuntime:nullError 'method)
                                   (send ,expression ,c-name ,@args))
                              (build-src src)))))
          
        ;Normal case
        ((id? method-name)
         (let* ((static? (memq 'static (method-record-modifiers method-record)))
                (temp (build-method-name (method-record-name method-record)
                                         (method-record-atypes method-record)))
                (m-name (if static?
                            (build-static-name temp (car (method-record-class method-record)))
                            temp)))
           (cond 
             ((special-name? expr)
              (let ((name (translate-id (if (and (equal? (special-name-name expr) "super") 
                                                 (overridden? m-name))
                                            (format "super.~a" m-name)
                                            m-name)
                                        (id-src method-name))))
                (if static?
                    (create-syntax #f `(,name ,@args) (build-src src))
                    (create-syntax #f `(send this ,name ,@args) (build-src src)))))
             ((not expr)
              (if (or static? (memq 'private (method-record-modifiers method-record)))
                  (create-syntax #f `(,(translate-id m-name (id-src method-name)) ,@args) (build-src src))
                  (create-syntax #f `(send this ,(translate-id m-name (id-src method-name)) ,@args) (build-src src))))
             (else
              (let ((name (translate-id m-name (id-src method-name))))
                (cond
                  ((and cant-be-null? (not static?))          
                   (create-syntax #f `(send ,expression ,name ,@args) (build-src src)))
                  (static? (create-syntax #f `(,name ,@args) (build-src src)))
                  (else
                   (create-syntax #f
                                  `(if (null? ,expression)
                                       (javaRuntime:nullError 'method)
                                       (send ,expression ,name ,@args))
                                  (build-src src)))))))))
        
        (else (error 'translate-call (format "Translate call given ~s as method-name" method-name))))))

  ;Add more checks perhaps to see in other cases if it can be null
  ;never-null? expression -> bool
  (define (never-null? expr)
    (cond
      ((not expr) #t)
      ((special-name? expr) #t)
      (else #f)))
  
  (define (overridden? name)
    (hash-table-get (class-override-table) name (lambda () #f)))
  
  ;translate-class-alloc: java-name (list type) (list syntax) src method-record-> syntax
  (define (translate-class-alloc class-type arg-types args src ctor-record)
    (if (name? class-type)
        (let ((class-string (get-class-string class-type))
              (class-id (name-id class-type)))
          (make-syntax #f `(let ((new-o (make-object ,(translate-id class-string
                                                                    (id-src class-id)))))
                             (send new-o ,(translate-id (build-constructor-name 
                                                         (if (null? (name-path class-type)) 
                                                             class-string 
                                                             (id-string (name-id class-type)))
                                                         (method-record-atypes ctor-record))
                                                        (id-src class-id))
                                   ,@args)
                             new-o) 
                       (build-src src)))
        (error 'translate-class-alloc "does not have gj-name implemented")))
  
  ;converted
  ;translate-array-alloc: type-spec (list syntax) src -> syntax
  (define translate-array-alloc
    (lambda (type sizes src)
      (create-array sizes (translate-type-spec type) src)))
  
  ;converted
  ;create-array: (list syntax) type src -> syntax
  (define create-array
    (lambda (sizes type src)
      (cond
        ((null? sizes) 
         (error 'create-array "Internal Error: create array given a null list")) 
        ((null? (cdr sizes))
         (make-syntax #f `(make-java-array ,type ,(car sizes) null) (build-src src)))
        (else
         (make-syntax #f `(make-java-array ,type (list ,@sizes) null) (build-src src))))))
  
  ;converted
  ;translate-type-spec: type-spec -> syntax
  (define translate-type-spec
    (lambda (type)
      (make-syntax #f
                   `(make-runtime-type ,(if (symbol? (type-spec-name type))
                                            `(quote ,(type-spec-name type))
                                            ;Come Back : losses src locations
                                            (build-identifier 
                                             (if (null? (name-path (type-spec-name type)))
                                                 (id-string (name-id (type-spec-name type)))
                                                 (append (map id-string (name-path (type-spec-name type)))
                                                         (list (id-string (name-id (type-spec-name type))))))))
                                       ,(type-spec-dim type))
                   (build-src (type-spec-src type)))))
  
  ;converted
  ;translate-array-access: syntax syntax src -> syntax
  (define translate-array-access
    (lambda (array index src)
      (make-syntax #f `(send ,array access ,index) 
                   (build-src src))))
  
  ;converted
  ;translate-cond: syntax syntax syntax src -> syntax
  (define translate-cond
    (lambda (if? then else src)
      (make-syntax #f `(if ,if? ,then ,else) (build-src src))))
  
  ;converted
  ;translate-post-expr: syntax symbol src src -> syntax
  (define translate-post-expr
    (lambda (expr op key src)
      (make-syntax #f `(begin0
                         ,expr
                         (set! ,expr ( ,(create-syntax #f (if (eq? op '++) 'add1 'sub1) (build-src key))
                                       ,expr)))
                   (build-src src))))
  
  ;converted
  ;translate-pre-expr: symbol syntax src src -> syntax
  (define translate-pre-expr
    (lambda (op expr key src)
      (make-syntax #f
                   `(begin
                      (set! ,expr (,(create-syntax #f (if (eq? op '++) 'add1 'sub1) (build-src key))
                                   ,expr))
                      ,expr)
                   (build-src src))))
  
  ;converted
  ;translate-unary: symbol syntax src src -> syntax
  (define translate-unary
    (lambda (op expr key src)
      (make-syntax #f  (case op
                         ((-) `(,(create-syntax #f '- (build-src key)) ,expr))
                         ((!) `(,(create-syntax #f 'not (build-src key)) ,expr))
                         ((~) `(,(create-syntax #f '- (build-src key)) (- ,expr) 1))
                         ((+) expr))
                   (build-src src))))
  
  ;converted
  ;translate-cast: type-spec syntax src
  (define (translate-cast type expr src)
    (if (symbol? (type-spec-name type))
        (make-syntax #f `(javaRuntime:cast-primitive ,expr (quote ,(type-spec-name type))) (build-src src))
        (make-syntax #f `(javaRuntime:cast-reference ,expr ,(get-class-name type) (quote ,(get-class-name type)))
                     (build-src src))))
  
  ;converted
  ;translate-instanceof: syntax type-spec src -> syntax
  (define translate-instanceof
    (lambda (expr type src)
      (make-syntax #f `(is-a? ,expr ,(get-class-name type)) (build-src src))))
  
  ;converted
  ;translate-assignment: (U access array-access) symbol syntax expression ?? src src -> syntax
  (define translate-assignment
    (lambda (name op expr assign-to type key src)
      (let ((expression (lambda (name) (case op
                                         ((=) expr)
                                         ((*=) `(* ,name ,expr))
                                         ((/=) `(/ ,name ,expr))
                                         ((+=) `(+ ,name ,expr))
                                         ((-=) `(- ,name ,expr))
                                         ((%= <<= >>= >>>= &= ^= or=) 
                                          (error 'translate-assignment "Only supports =, +=, -=, *=, & /= at this time"))))))
        (cond 
          ((array-access? name)
           (translate-array-mutation name expression assign-to src))
          ((access? name)
           (let* ((access (access-name name))
                  (src-h (build-src src))
                  (set-h 
                   (lambda (id)
                     (make-syntax #f `(begin (,(create-syntax #f 'set! (build-src key))
                                              ,id ,(expression id)) ,id) src-h))))
             (cond
               ((local-access? access)
                (set-h (translate-id (build-var-name (id-string (local-access-name access)))
                                     (id-src (local-access-name access)))))
               ((field-access? access)
                ;Come Back : loses source information
                (let* ((field (id-string (field-access-field access)))
                       (field-src (id-src (field-access-field access)))
                       (vaccess (field-access-access access))
                       (obj (field-access-object access))
                       (expr (if obj (translate-expression obj))))
                  (cond
                    ((var-access-static? vaccess)
                     (set-h (build-identifier (build-static-name (build-var-name field)
                                                                 (build-identifier (var-access-class vaccess))))))
                    ((not obj) (set-h (translate-id (build-var-name field) field-src)))
                    (else
                     (let ((setter (create-set-name field (var-access-class vaccess)))
                           (getter (create-get-name field (var-access-class vaccess))))
                       (make-syntax #f `(begin 
                                          (,setter ,expr ,(expression `(,getter ,expr)))
                                          (,getter ,expr))
                                    src-h)))))))))))))
  
  ;converted
  ;translate-array-mutation: array-access (syntax -> (list symbol syntax syntax)) expression src -> syntax
  (define translate-array-mutation
    (lambda (array expression expr src)
      (let ((array-name (translate-expression (array-access-name array)))
            (array-index (translate-expression (array-access-index array))))
        (make-syntax #f
                     `(begin 
                        (send ,array-name set ,array-index ,(expression `(send ,array-name access ,array-index)))
                        (send ,array-name access ,array-index))
                     (build-src src)))))
  
  ;translate-id: string src -> syntax
  (define translate-id
    (lambda (id src)
      (create-syntax #f (build-identifier id) (build-src src))))
  
  )