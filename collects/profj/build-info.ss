(module build-info mzscheme
  
  (require (lib "class.ss") (lib "file.ss") (lib "list.ss")
           "ast.ss" "types.ss" "error-messaging.ss" "parameters.ss" "parser.ss")

  (provide build-info build-interactions-info find-implicit-import load-lang)
  
  ;-------------------------------------------------------------------------------
  ;General helper functions for building information
  
  ;; name->list: name -> (list string)
  (define (name->list n)
    (cons (id-string (name-id n)) (map id-string (name-path n))))
  
  ;build-require-syntax: string (list string) (list string) bool -> (list syntax)
  (define build-require-syntax
    (lambda (name path dir local?)
      (let* ((syn (lambda (acc) (datum->syntax-object #f acc #f)))
             (lib? (member (car dir) 
                           (map (lambda (p) (build-path p "drj" "libs"))
                                (current-library-collection-paths))))
             (access (lambda (name)
                       (cond
                         (lib? `(lib ,name "drj" "libs" ,@path))
                         ((and local? (not (to-file))) name)
                         (else `(file ,(build-path (apply build-path dir) name))))))
             (make-name (lambda ()
                          (if (or (not local?) lib? (to-file))
                              (string-append name ".ss")
                              (string->symbol name)))))
        (list (datum->syntax-object #f
                                    `(prefix ,(string->symbol (apply string-append
                                                                    (map (lambda (s) (string-append s "."))
                                                                         path)))
                                             ,(syn (access (make-name))))
                                    #f)
              (syn (access (make-name)))))))

  ;-------------------------------------------------------------------------------
  ;Main functions

  ;; build-info: package symbol type-records (opt symbol)-> void
  (define (build-info prog level type-recs . args)
    (let* ((pname (if (package-name prog)
                      (append (map id-string (name-path (package-name prog)))
                              (list (id-string (name-id (package-name prog)))))
                      null))
           (lang-pack `("java" "lang"))
           (lang (send type-recs get-package-contents lang-pack 
                       (lambda () (error 'type-recs "Internal error: Type record not set with lang"))))
           (current-loc (unless (null? (package-defs prog)) (def-file (car (package-defs prog))))))
      
      ;Add lang to local environment
      (for-each (lambda (class) (send type-recs add-to-env class lang-pack current-loc)) lang)
      (for-each (lambda (class) (send type-recs add-class-req (cons class lang-pack) #f current-loc)) lang)
      (send type-recs add-class-req (list 'array) #f current-loc)
      
      ;Set location for type error messages
      (build-info-location current-loc)
      
      ;Add all defs in this file to environment
      (for-each (lambda (def)
                  (let ((defname (cons (id-string (def-name def)) pname)))
                    (send type-recs add-to-env (car defname) pname current-loc)
                    (when (execution?)
                      (send type-recs add-to-env (car defname) pname 'interactions))
                    (send type-recs add-class-req defname #f current-loc)
                    (send type-recs add-require-syntax defname
                          (build-require-syntax (car defname) pname 
                                                (find-directory pname (lambda () #f))
                                                #t))
                    (send type-recs add-to-records def-name
                          (lambda () (process-class/iface def pname type-recs (null? args) level)))))
                  (package-defs prog))
      (execution? #f)
      
      ;Add package information to environment
      (add-my-package type-recs pname (package-defs prog) current-loc level)
      
      ;Add import information
      (for-each (lambda (imp) (process-import type-recs imp level)) (package-imports prog))

      ;Build jinfo information for each def in this file
      (for-each (lambda (def) (process-class/iface def pname type-recs (null? args) level))
                (package-defs prog))

      ;Add these to the list for type checking
      (add-to-queue (package-defs prog))))

  ;build-interactions-info: ast location type-records -> void
  (define (build-interactions-info prog level loc type-recs)
    (build-info-location loc)
    (if (list? prog)
        (for-each (lambda (f) (build-interactions-info f level loc type-recs))
                  prog)
        (when (field? prog)
          (send type-recs add-interactions-field 
                (process-field prog '("scheme-interactions") type-recs level)))))
  
  ;add-to-queue: (list definition) -> void
  (define (add-to-queue defs)
    (check-list (append defs (check-list))))  
  
  ;-----------------------------------------------------------------------------------
  ;Import processing/General loading

  ;;process-import: type-records import symbol -> void
  (define (process-import type-recs imp level) 
    (let* ((star? (import-star imp))
           (file (import-file imp))
           (name (id-string (name-id (import-name imp))))
           (name-path (map id-string (name-path (import-name imp))))
           (path (if star? (append name-path (list name)) name-path))
           (err (lambda () (import-error (import-name imp) (import-src path)))))
      (if star?
          (let ((classes (send type-recs get-package-contents path (lambda () #f))))
            (if classes
                (for-each (lambda (class) (send type-recs add-to-env class path file))
                          classes)
                (let* ((dir (find-directory path err))
                       (classes (get-class-list dir)))
                  (for-each (lambda (class) (import-class class path dir file type-recs level #t))
                            classes)
                  (send type-recs add-package-contents path classes))))
          (import-class name path (find-directory path err) file type-recs level #t))))
  
  ;import-class: string (list string) (list string) location type-records symbol bool-> void
  (define (import-class class path dir loc type-recs level add-to-env)
    (cond
      ((send type-recs get-class-record (cons class path) (lambda () #f)) void)
      ((file-exists? (string-append (build-path (apply build-path dir) "compiled" class) ".jinfo"))
       (send type-recs add-class-record (read-record (string-append (build-path (apply build-path dir) "compiled" class) ".jinfo")))
       (send type-recs add-require-syntax (cons class path) (build-require-syntax class path dir #f)))
      ((file-exists? (string-append (build-path (apply build-path dir) class) ".java"))
       (send type-recs add-to-records 
             (cons class path)
             (lambda () 
               (let* ((location (string-append class ".java"))
                      (ast (call-with-input-file (string-append (build-path (apply build-path dir) class) ".java")
                             (lambda (p) (parse p location level)))))
                 (send type-recs set-compilation-location location (build-path (apply build-path dir) "compiled"))
                 (build-info ast level type-recs 'not_look_up)
                 (send type-recs get-class-record (cons class path) (lambda () 'internal-error "Failed to add record"))
                 )))
       (send type-recs add-require-syntax (cons class path) (build-require-syntax class path dir #t)))
      (else (file-error 'file (cons class path))))
    (when add-to-env (send type-recs add-to-env class path loc))
    (send type-recs add-class-req (cons class path) (not add-to-env) loc))
  
  ;add-my-package: type-records (list string) (list defs) loc symbol-> void
  (define (add-my-package type-recs package defs loc level)
    (let* ((dir (find-directory package (lambda () #f)))
           (classes (if dir (get-class-list dir) null)))
      (for-each (lambda (c) (import-class c package dir loc type-recs level #t)) 
                (filter (lambda (c) (not (contained-in? defs c))) classes))
      (send type-recs add-package-contents package classes)))
      
  ;contained-in? (list definition) definition -> bool
  (define (contained-in? defs class)
    (and (not (null? defs))
         (or (equal? class (id-string (def-name (car defs))))
             (contained-in? (cdr defs) class))))
  
  ;find-implicit-import: (list string) type-records symbol-> ( -> record )
  (define (find-implicit-import name type-recs level)
    (lambda ()
      (let ((original-loc (send type-recs get-location))
            (dir (find-directory (cdr name) (lambda () (file-error 'dir name)))))
        (import-class (car name) (cdr name) dir original-loc type-recs level #f)
        (begin0
          (get-record (send type-recs get-class-record name (lambda () #f) type-recs))
          (send type-recs set-location! original-loc)))))
  
  ;find-directory: (list string) ( -> void) -> (list string)
  (define (find-directory path fail)
    (if (null? path) 
        (list (build-path 'same))
        (let ((class-path (cons (build-path 'same)
                                (get-preference 'classpath
                                                (lambda () 
                                                  (let ((libs (map (lambda (p) (build-path p "drj" "libs"))
                                                                   (current-library-collection-paths))))
                                                    (put-preferences `(classpath) (list libs))
                                                    libs))))))
          (let loop ((paths class-path))
            (cond
              ((null? paths) (fail))
              ((and (directory-exists? (build-path (car paths) 
                                                   (apply build-path path))))
               (cons (car paths) path))
              (else (loop (cdr paths))))))))

  ;get-class-list: (list string) -> (list string)
  (define (get-class-list dir)
    (filter (lambda (c-name) (not (equal? c-name "")))
            (map (lambda (fn) (substring fn 0 (- (string-length fn) 5)))
                 (filter (lambda (f) (equal? (filename-extension f) "java"))
                         (directory-list (apply build-path dir))))))
  
  ;load-lang: type-records -> void (adds lang to type-recs)
  (define (load-lang type-recs)
    (let* ((lang `("java" "lang"))
           (dir (find-directory lang (lambda () (error 'load-lang "Internal-error: Lang not accessible"))))
           (class-list (map (lambda (fn) (substring fn 0 (- (string-length fn) 6)))
                            (filter (lambda (f) (equal? (filename-extension f) "jinfo"))
                                    (directory-list (build-path (apply build-path dir) "compiled")))))
           (array (datum->syntax-object #f `(lib "array.ss" "drj" "libs" "java" "lang") #f)))
      (send type-recs add-package-contents lang class-list)
      (for-each (lambda (c) (import-class c lang dir #f type-recs 'full #f)) class-list)
      (send type-recs add-require-syntax (list 'array) (list array array))
      
      ;Add lang to interactions environment
      (for-each (lambda (class) (send type-recs add-to-env class lang 'interactions)) class-list)
      (send type-recs set-location! 'interactions)
      (for-each (lambda (class) (send type-recs add-class-req (cons class lang) #f 'interactions)) class-list)
      (send type-recs add-class-req (list 'array) #f 'interactions)
      ))
        
  ;------------------------------------------------------------------------------------
  ;Functions for processing classes and interfaces
  
  ;; process-class/iface: (U class-def interface-def) (list string) type-records bool symbol -> class-record
  (define (process-class/iface ci package-name type-recs look-in-table level)
    (cond
      ((interface-def? ci) (process-interface ci package-name type-recs look-in-table level))
      ((class-def? ci) (process-class ci package-name type-recs look-in-table level))))
  
  ;;get-parent-record: (list string) name (list string) type-records (list string) -> record
  (define (get-parent-record name n child-name level type-recs)
    (when (equal? name child-name)
      (dependence-error 'immediate (name-id n) (name-src n)))
    (let ((record (send type-recs get-class-record name (lambda () #f))))
      (cond
        ((class-record? record) record)
        ((procedure? record) (get-record record type-recs))
        ((eq? record 'in-progress)
         (dependence-error 'cycle (name-id n) (name-src n)))
        (else (get-record (find-implicit-import name type-recs level) type-recs)))))
  
  (define (class-specific-field? field)
    (not (memq 'private 
               (field-record-modifiers field))))
  
  (define (class-specific-method? method new-methods)
    (not (or (memq 'static (method-record-modifiers method))
             (memq 'private (method-record-modifiers method))
             (eq? 'ctor (method-record-rtype method))
             (over-riden? method new-methods))))
    
  (define (over-riden? m listm)
    (and (not (null? listm))
         (let ((m2 (car listm)))
           (or (and (equal? (method-record-name m)
                            (method-record-name m2))
                    (type=? (method-record-rtype m)
                            (method-record-rtype m2))
                    (and (= (length (method-record-atypes m))
                            (length (method-record-atypes m2)))
                         (andmap type=? (method-record-atypes m)
                                 (method-record-atypes m2)))
                    (and (= (length (method-record-modifiers m))
                            (length (method-record-modifiers m2)))
                         (andmap eq? (method-record-modifiers m)
                                     (method-record-modifiers m2))))
               (over-riden? m (cdr listm))))))
  
  ;; Won't get the implemented interfaces' parents as implemented interfaces
  ;; process-class: class-def (list string) type-records bool symbol -> class-record
  (define (process-class class package-name type-recs look-in-table? level)
    (let* ((info (class-def-info class))
           (cname (cons (id-string (header-id info)) package-name)))
      (send type-recs set-location! (def-file class))
      (let ((build-record
             (lambda ()
               (send type-recs add-to-records cname 'in-progress)
               (let* ((super (if (null? (header-extends info)) null (car (header-extends info))))
                      (super-name (if (null? super) '("Object" "java" "lang") (name->list super)))
                      (super-record (get-parent-record super-name super cname level type-recs))
                      (iface-records (map (lambda (i)
                                            (get-parent-record (name->list i) i #f level type-recs))
                                          (header-implements info)))
                      (members (class-def-members class))
                      (modifiers (header-modifiers info))
                      (test-mods (map modifier-kind modifiers))
                      (reqs (map (lambda (name-list)
                                   (if (= (length name-list) 1)
                                       (make-req (car name-list) (send type-recs lookup-path (car name-list) (lambda () null)))
                                       (make-req (car name-list) (cdr name-list))))
                                 (cons super-name (map name->list (header-implements info))))))
                 
                 (send type-recs set-location! (class-def-file class))
                 (set-class-def-uses! class reqs)
                 (unless (andmap (lambda (req) (type-exists? (req-class req) (req-path req) type-recs))
                                 reqs)
                   (error 'process-class "Internal error: not all of imports and classes exist"))
                 
                 (when (memq 'final (class-record-modifiers super-record))
                   (extension-error 'final (header-id info) super (id-src super)))
                 
                 (unless (class-record-class? super-record)
                   (extension-error 'class-iface (header-id info) super (id-src super)))
                 
                 (when (ormap class-record-class? iface-records)
                   (letrec ((find-class
                             (lambda (recs names)
                               (if (class-record-class? (car recs))
                                   (car names)
                                   (find-class (cdr recs) (cdr names)))))
                            (name (find-class iface-records (header-implements info))))
                     (extension-error 'implement-class (header-id info) name (id-src name))))
                 
                 (valid-iface-implement? iface-records (header-implements info))
                                  
                 (let*-values (((old-methods) (class-record-methods super-record))
                               ((f m) 
                                (if (memq 'strictfp test-mods)
                                    (process-members members old-methods cname type-recs level (find-strictfp modifiers))
                                    (process-members members old-methods cname type-recs level))))
                   (valid-field-names? f members type-recs)
                   (valid-method-sigs? m members level type-recs)

                   (when (not (memq 'abstract test-mods))
                     (and (class-fully-implemented? super-record super 
                                                    iface-records (header-implements info)
                                                    m level)
                          (no-abstract-methods m members type-recs)))
                   
                   (valid-inherited-methods? (cons super-record iface-records)
                                             (cons (if (null? super)
                                                       (make-name (make-id "Object" #f) null #f)
                                                       super)
                                                   (header-implements info))
                                             level)
                   
                   (check-current-methods (cons super-record iface-records)
                                          m
                                          members
                                          level
                                          type-recs)
                   
                   (let ((record
                          (make-class-record 
                           cname
                           (check-class-modifiers 'full modifiers)
                           #t
                           (append f (filter class-specific-field? (class-record-fields super-record)))
                           (append m (filter (lambda (meth)
                                               (class-specific-method? meth m))
                                             (class-record-methods super-record)))
                           (cons super-name (class-record-parents super-record))
                           (append (map name->list (header-implements info))
                                   (class-record-ifaces super-record)))))
                     (send type-recs add-class-record record)
                     record))))))
        (if look-in-table?
            (get-record (send type-recs get-class-record cname build-record) type-recs)
            (build-record)))))
  
  (define (find-strictfp mods)
    (if (null? mods)
        null
        (if (eq? 'strictfp (modifier-kind (car mods)))
            (car mods)
            (find-strictfp (cdr mods)))))
  
  ;; process-interface: interface-def (list string) type-records bool symbol -> class-record
  (define (process-interface iface package-name type-recs look-in-table? level)
    (let* ((info (interface-def-info iface))
           (iname (cons (id-string (header-id info)) package-name)))
      (send type-recs set-location! (def-file iface))
      (let ((build-record 
             (lambda ()
               (send type-recs add-to-records iname 'in-progress)
               (let* ((super-names (map name->list (header-extends info)))
                      (super-records (map (lambda (n sc) (get-parent-record n sc iname level type-recs))
                                          super-names
                                          (header-extends info)))
                      (members (interface-def-members iface))
                      (reqs (map (lambda (name-list) (make-req (car name-list) (cdr name-list)))
                                 super-names)))
                 (send type-recs set-location! (interface-def-file iface))
                 (set-interface-def-uses! iface reqs)                 
                 (unless (andmap (lambda (req) (type-exists? (req-class req) (req-path req) type-recs))
                                 reqs)
                   (error 'process-interface "Internal error: Not all of extends exist"))
                 
                 (when (ormap class-record-class? super-records)
                   (letrec ((find-class
                             (lambda (recs names)
                               (if (class-record-class? (car recs))
                                   (car names)
                                   (find-class (cdr recs) (cdr names)))))
                            (name (find-class super-records (header-extends info))))
                     (extension-error 'iface-class (header-id info) name (id-src name))))
                 
                 (valid-iface-extend? super-records (header-extends info))
                 
                 (let-values (((f m) (process-members members null iname type-recs level)))
                   
                   (valid-field-names? f members type-recs)
                   (valid-method-sigs? m members level type-recs)
                   (valid-inherited-methods? super-records (header-extends info) level)
                   (check-current-methods super-records m members level type-recs)
                   
                   (let ((record
                          (make-class-record 
                           iname
                           (check-interface-modifiers level (header-modifiers info))
                           #f
                           (apply append (cons f (map class-record-fields super-records)))
                           (apply append (cons m (map class-record-methods super-records)))
                           (apply append (cons super-names 
                                               (map class-record-parents super-records)))
                           null)))
                     (send type-recs add-class-record record)
                     record))))))
        (if look-in-table?
            (get-record (send type-recs get-class-record iname build-record) type-recs)
            (build-record)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Code to check for conflicts in method/field/class naming (including types)
  
  (define (valid-iface-extend? records extends)
    (or (null? records)
        (and (memq (car records) (cdr records))
             (extension-error 'ifaces #f (car extends) (id-src (car extends))))
        (valid-iface-extend? (cdr records) (cdr extends))))
  
  (define (valid-iface-implement? records implements)
    (or (null? records)
        (and (memq (car records) (cdr records))
             (extension-error 'implement #f (car implements) (id-src (car implements))))
        (valid-iface-implement? (cdr records) (cdr implements))))
  
  ;valid-field-names? (list field-record) (list member) type-records -> bool
  (define (valid-field-names? fields members type-recs)
    (or (null? fields)
        (and (field-member? (car fields) (cdr fields))
             (let ((f (find-member (car fields) members type-recs)))
               (field-error (field-name f) (field-src f))))
        (valid-field-names? (cdr fields) members type-recs)))
  
  ;field-member: field-record (list field-record) -> bool
  (define (field-member? field fields)
    (and (not (null? fields))
         (or (equal? (field-record-name field)
                     (field-record-name (car fields)))
             (field-member? field (cdr fields)))))
  
  (define (find-member member-record members type-recs)
    (when (null? members)
      (error 'internal-error "Find-member given a member that is not contained in the member list"))
    (cond
      ((and (field-record? member-record)
            (field? (car members)))
       (if (equal? (id-string (field-name (car members)))
                   (field-record-name member-record))
           (car members)
           (find-member member-record (cdr members) type-recs)))
      ((and (method-record? member-record)
            (method? (car members)))
       (if (and (equal? (id-string (method-name (car members)))
                        (method-record-name member-record))
                (= (length (method-record-atypes member-record))
                   (length (method-parms (car members))))
                (andmap type=?
                        (method-record-atypes member-record)
                        (map (lambda (t)
                               (type-spec-to-type t type-recs))
                             (map field-type (method-parms (car members))))))
           (car members)
           (find-member member-record (cdr members) type-recs)))
      (else
       (find-member member-record (cdr members) type-recs))))
  
  ;valid-method-sigs? (list method-record) (list member) symbol type-records -> bool
  (define (valid-method-sigs? methods members level type-recs)
    (or (null? methods)
        (and (method-member? (car methods) (cdr methods) level)
             (let ((m (find-member (car methods) members type-recs)))
               (method-error 'repeated 
                             (method-name m)
                             (method-parms m)
                             (method-record-class (car methods))
                             (method-src m))))
        (valid-method-sigs? (cdr methods) members level type-recs)))
  
  (define (method-member? method methods level)
    (and (not (null? methods))
         (or (and (equal? (method-record-name method)
                          (method-record-name (car methods)))
                  (or (or (eq? level 'beginner) (eq? level 'intermediate))
                      (and (= (length (method-record-atypes method))
                              (length (method-record-atypes (car methods))))
                           (andmap type=? (method-record-atypes method) 
                                   (method-record-atypes (car methods))))))
             (method-member? method (cdr methods) level))))                              
  
  (define (valid-inherited-methods? records extends level)
    (or (null? records)
        (and (check-inherited-method (class-record-methods (car records))
                                     (cdr records)
                                     (car extends)
                                     level)
             (valid-inherited-methods? (cdr records) (cdr extends) level))))
  
  (define (check-inherited-method methods records from level)
    (or (null? methods)
        (and (method-conflicts? (car methods) 
                                (apply append (map class-record-methods records))
                                level)
             (method-error 'inherit-conflict 
                           (method-name (car methods))
                           (method-parms (car methods))
                           from
                           (method-src (car methods))))
        (check-inherited-method (cdr methods) records from level)))
  
  (define (method-conflicts? method methods level)
    (and (not (null? methods))
         (or (and (equal? (method-record-name method)
                          (method-record-name (car methods)))
                  (or (or (eq? level 'beginner) (eq? level 'intermediate))
                      (andmap type=? (method-record-atypes method) (method-record-atypes (car methods))))
                  (not (type=? (method-record-rtype method) (method-record-rtype (car methods)))))
             (method-conflicts? method (cdr methods) level))))                              

  (define (check-current-methods records methods members level type-recs)
    (or (null? records)
        (and (check-for-conflicts methods (car records) members level type-recs)
             (check-current-methods methods (cdr records) members level type-recs))))
  
  (define (check-for-conflicts methods record members level type-recs)
    (or (null? methods)
        (and (method-conflicts? (car methods)
                                (class-record-methods record)
                                level)
             (let ((method (find-member (car methods) members type-recs)))
               (method-error 'conflicts 
                             (method-name method)
                             (method-parms method)
                             (car (class-record-name record))
                             (method-src method))))
        (check-for-conflicts (cdr methods) record members level type-recs)))
  
  ;class-fully-implemented? class-record id (list class-record) (list id) (list method) symbol -> bool
  (define (class-fully-implemented? super super-name ifaces ifaces-name methods level) 
    (when (memq 'abstract (class-record-modifiers super))
      (implements-all? (filter (lambda (method)
                                 (memq 'abstract (method-record-modifiers method)))
                               (class-record-methods super))
                       methods super-name level))
    (andmap (lambda (iface iface-name)
              (implements-all? (class-record-methods iface) methods iface-name level))
            ifaces
            ifaces-name
            ))
  
  ;implements-all? (list method-record) (list method) id symbol -> bool
  (define (implements-all? inherit-methods methods name level)
    (or (null? inherit-methods)
        (and (not (method-member? (car inherit-methods) methods level))
             (method-error 'not-implement 
                           (make-id (method-record-name (car inherit-methods)) #f)
                           (map (lambda (parm)
                                  (make-var-decl (make-id (type->ext-name parm #f) null #f #f)))
                                (method-record-atypes (car inherit-methods)))
                           (id-string name)
                           (id-src name)))
        (implements-all? (cdr inherit-methods) methods name level)))
  
  (define (no-abstract-methods methods members type-recs)
    (or (null? methods)
        (and (memq 'abstract (method-record-modifiers (car methods)))
             (let ((method (find-member (car methods) members type-recs)))
               (method-error 'illegal-abstract 
                             (method-name method) 
                             (method-parms method) 
                             (method-record-class (car methods))
                             (method-src method))))
        (no-abstract-methods (cdr methods) members type-recs)))
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Methods to process fields and methods
  
  ;; process-members: (list members) (list method-record) (list string) type-records symbol -> 
  ;;                     (values (list field-record) (list method-record))
  (define (process-members members inherited-methods cname type-recs level . args)
    (let loop ((members members)
               (fields null)
               (methods null))
      (cond
        ((null? members) (values fields methods))
        ((var-decl? (car members))
         (loop (cdr members)
               (cons (process-field (car members) cname type-recs level) fields)
               methods))
        ((var-init? (car members))
         (loop (cdr members)
               (cons (process-field (var-init-var-decl (car members)) cname type-recs level) fields)
               methods))
        ((method? (car members))
         (loop (cdr members)
               fields
               (cons (if (null? args)
                         (process-method (car members) inherited-methods cname type-recs level)
                         (process-method (car members) inherited-methods cname type-recs level (car args)))
                         methods)))
        (else
         (loop (cdr members)
               fields
               methods)))))
  
  ;; process-field: var-decl (string list) type-records symbol -> field-record
  (define (process-field field cname type-recs level)
    (make-field-record (id-string (field-name field)) 
                       (check-field-modifiers level (field-modifiers field)) 
                       cname 
                       (type-spec-to-type (field-type field) type-recs)))

  ;; process-method: method (list method-record) (list string) type-records symbol -> method-record  
  (define (process-method method inherited-methods cname type-recs level . args)
    
    (make-method-record (id-string (method-name method))
                        (if (null? args)
                            (check-method-modifiers level (method-modifiers method))
                            (check-method-modifiers level (cons (car args) (method-modifiers method))))
                        (type-spec-to-type (method-type method) type-recs)
                        (map (lambda (ts)
                               (type-spec-to-type ts type-recs))
                             (map var-decl-type
                                  (method-parms method)))
                        (filter (lambda (n)
                                  (not (or (is-subclass? n runtime-exn-type type-recs))))
;                                           (is-subclass? n error-type type-recs))))
                                (map (lambda (t)
                                       (let ((n (make-ref-type (id-string (name-id t))
                                                               (map id-string (name-path t)))))
                                         (if (is-subclass? n throw-type type-recs)
                                             n
                                             (throws-error (name-id t) (name-src t)))))
                                     (method-throws method)))
                        ""
                        cname))
    
  ;-----------------------------------------------------------------------------------
  ;Code to check modifiers
  
  ;check-class-modifiers: (list modifier) symbol -> (list symbol)
  (define (check-class-modifiers level mods)
    (when (and (valid-class-mods? level mods)
               (not (final-and-abstract? mods))
               (not (duplicate-mods? mods)))
      (map modifier-kind mods)))
  
  ;check-interface-modifiers: (list modifier) symbol -> (list symbol)
  (define (check-interface-modifiers level mods)
    (when (and (valid-interface-mods? level mods)
               (not (duplicate-mods? mods)))
      (map modifier-kind mods)))

  ;check-method-modifiers: symbol (list modifier) -> (list symbol)
  (define (check-method-modifiers level mods)
    (when (and (not (duplicate-mods? mods))
               (one-of-access? mods)
               (valid-method-mods? level mods)
               (not (native-and-fp? mods))
               (or (not (memq 'abstract (map modifier-kind mods)))
                   (valid-method-mods? 'abstract mods)))
      (map modifier-kind mods)))
  
  ;check-field-modifiers: symbol (list modifier) -> (list symbol)
  (define (check-field-modifiers level mods)
    (when (and (not (duplicate-mods? mods))
               (one-of-access? mods)
               (valid-field-mods? level mods)
               (not (volatile-and-final? mods)))
      (map modifier-kind mods)))
  
  ;make-valid-mods: (symbol -> (list symbol)) (symbol -> symbol) -> (symbol (list modifier) -> bool)
  (define (make-valid-mods valids-choice error-type)
    (letrec ((tester
              (lambda (level mods)
                (or (null? mods)
                    (and (not (memq (modifier-kind (car mods)) (valids-choice level)))
                         (modifier-error (error-type level) (car mods)))
                    (tester level (cdr mods))))))
      tester))

  ;valid-*-mods?: symbol (list modifier) -> bool
  (define valid-class-mods?
    (make-valid-mods (lambda (x) '(public abstract final strictfp))
                     (lambda (x) 'invalid-class)))
  (define valid-interface-mods?
    (make-valid-mods (lambda (x) '(public abstract strictfp))
                     (lambda (x) 'invalid-iface)))
  (define valid-field-mods?
    (make-valid-mods
     (lambda (level)
       (case level
         ((beginner intermediate) '(private final))
         ((advanced) '(public protected private static))
         ((full) `(public protected private static final transient volatile))))
     (lambda (x) 'invalid-field)))  
  (define valid-method-mods?
    (make-valid-mods
     (lambda (level)
       (case level
         ((beginner intermediate) '(public abstract))
         ((advanced) `(public protected private abstract static final))
         ((full) '(public protected private abstract static final synchronized native strictfp))
         ((abstract) '(public protected))))
     (lambda (level)
       (if (eq? level 'abstract) 'invalid-abstract 'invalid-method))))
  
  ;one-access: symbol symbol symbol (list modifiers) -> bool
  (define (one-access is check1 check2 mods)
    (and (eq? is (modifier-kind (car mods)))
         (or (memq check1 (map modifier-kind (cdr mods)))
             (memq check2 (map modifier-kind (cdr mods))))
         (modifier-error 'access (car mods))))

  ;one-of-access?: (list modifier) -> bool
  (define (one-of-access? mods)
    (or (null? mods)
        (one-access 'public 'private 'protected mods)
        (one-access 'private 'public 'protected mods)
        (one-access 'protected 'private 'public mods)
        (one-of-access? (cdr mods))))

  ;make-not-two: symbol symbol symbol -> ((list modifier) -> bool)
  (define (make-not-two first second error)
    (letrec ((tester
              (lambda (mods)
                (and (not (null? mods))
                     (or (and (eq? first (modifier-kind (car mods)))
                              (memq second (map modifier-kind (cdr mods)))
                              (modifier-error error (car mods)))
                         (and (eq? second (modifier-kind (car mods)))
                              (memq first (map modifier-kind (cdr mods)))
                              (modifier-error error (car mods)))
                         (tester (cdr mods)))))))
      tester))
  
  (define final-and-abstract? (make-not-two 'final 'abstract 'final-abstract))
  (define volatile-and-final? (make-not-two 'volatile 'final 'final-volatile))
  (define native-and-fp? (make-not-two 'native 'strictfp 'native-strictfp))
    
  ;duplicate-mods?: (list modifier) -> bool
  (define (duplicate-mods? mods)
    (and (not (null? mods))
         (or (and (memq (modifier-kind (car mods))
                        (map modifier-kind (cdr mods)))
                  (modifier-error 'dups (car mods)))
             (duplicate-mods? (cdr mods)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Error raising code: code that takes information about the error message and throws the error
  
  ;modifier-error: symbol modifier -> void
  (define (modifier-error kind mod)
    (let ((m (modifier-kind mod))
          (src (modifier-src mod)))
      (raise-error m
                   (case kind
                     ((dups) 
                      (format "Modifier ~a appears multiple times in one declaration" m))
                     ((access)
                      "Declaration can only be one of public, private, or protected")
                     ((invalid-iface)
                      (format "Modifier ~a is not valid for interfaces" m))
                     ((invalid-class)
                      (format "Modifier ~a is not valid for classes" m))
                     ((invalid-field)
                      (format "Modifier ~a is not valid for fields" m))
                     ((invalid-method)
                      (format "Modifier ~a is not valid for methods" m))
                     ((invalid-abstract)
                      (format "Modifier ~a is not valid for an abstract method" m))
                     ((final-abstract) "Class cannot be final and abstract")
                     ((final-volatile) "Field cannot be final and volatile")
                     ((native-strictfp) "Method cannot be native and strictfp"))
                   m src)))

  ;dependence-error: symbol id src -> void
  (define (dependence-error kind name src)
    (let ((n (id->ext-name name)))
      (raise-error n
                   (case kind
                     ((immediate) (format "~a may not extend itself" n))
                     ((cycle) 
                      (format "~a is illegally dependent on itself, potentially through other definitions" n)))
                   n src)))

  ;extension-error: symbol id id src -> void
  (define (extension-error kind name super src) 
    (let ((n (if name (id->ext-name name) name))
          (s (id->ext-name super)))
      (raise-error s
                   (case kind
                     ((final) (format "Final class ~a may not be extended by ~a" s n))
                     ((implement) (format "Interface ~a may not be implemented twice by this class" s))
                     ((ifaces) (format "Interface ~a cannot be extended twice by this interface" s))
                     ((iface-class) (format "Interface ~a may not extend class ~a" n s))
                     ((class-iface) (format "Class ~a may not extend interface ~a" n s))
                     ((implement-class) (format "Class ~a cannot implement class ~a" n s)))
                   s src)))

  ;method-error: symbol id (list field) string src -> void
  (define (method-error kind name parms class src)
    (let ((m-name (method-name->ext-name (id-string name) parms)))
      (raise-error m-name
                   (case kind
                     ((illegal-abstract)
                      (format "Abstract method ~a is not allowed in non-abstract class ~a" m-name class))
                     ((repeated)
                      (format "Method ~a has already been written in this class (~a)" m-name class))
                     ((inherit-conflict)
                      (format "Inherited method ~a from ~a conflicts with another method of the same name"
                              m-name class))
                     ((conflict)
                      (format "Method ~a conflicts with a method inherited from ~a" m-name class))
                     ((not-implement)
                      (format "Method ~a from ~a is not implemented" m-name class)))
                   m-name src)))

  ;field-error: id src -> void
  (define (field-error name src)
    (let ((n (id->ext-name name)))
      (raise-error n (format "Multiple fields are declared with the name ~a" n) n src)))

  ;import-error: name src -> void
  (define (import-error imp src)
    (raise-error 'import
                 (format "Import ~a not found" (path->ext (name->path imp)))
                 'import src))

  ;file-error: symbol (list string) -> void
  (define (file-error kind path)
    (let ((k (if (eq? kind 'file) 'file-not-found 'directory-not-found)))
      (raise-error k
                   (case kind
                     ((file) (format "Required file ~a not found" (path->ext path)))
                     ((dir) (format "Required directory ~a not found" (path->ext path))))
                   k #f)))
  
  ;throws-error id src -> void
  (define (throws-error t src)
    (raise-error 'throws
                 (format "Thrown class must be a subtype of Throwable: Given ~a" 
                         (id->ext-name t))
                 'throws src))
        
  (define build-info-location (make-parameter #f))
  (define raise-error (make-error-pass build-info-location))
  
  )
  