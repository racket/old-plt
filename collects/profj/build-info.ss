(module build-info mzscheme
  
  (require (lib "class.ss") (lib "file.ss") (lib "list.ss")
           "ast.ss" "types.ss"
           "parameters.ss" "parser.ss")

  (provide build-info find-implicit-import load-lang build-interactions-info)
  
  ;-------------------------------------------------------------------------------
  ;General helper functions for building information
  
  (define (build-src-list src)
    (if (not src)
        src
        (if (and (= (src-line src) 0)
                 (= (src-col src) 0)
                 (= (src-pos src) 0)
                 (= (src-span src) 0))
            #f
            (list (build-info-location) (src-line src) (src-col src) (src-pos src) (src-span src)))))        

  (define get-error-message
    (let ((dup-mods (lambda (a) 
                      (format "Modifier ~a cannot appear multiple times in one declaration" a)))
          (one-of-access (lambda (a)
                           "Only one of public, private, or protected permitted"))
          (invalid-mods 
           (lambda (for)
             (lambda (a) (format "Modifier ~a is not a legal modifier for ~a" a for))))
          (fin-and-abs 
           (lambda (a) "Class cannot be both abstract and final"))
          (fin-and-vol 
           (lambda (a) "Field cannot be both final and volatile"))
          (nat-and-fp 
           (lambda (a) "Method cannot be both native and strictfp"))
          (extend-self 
           (lambda (a) (format "~a may not extend itself" a)))
          (cycle 
           (lambda (a)
             (format "~a is illegally dependent on itself, potentially through other definitions" a)))
          (iface-repeat-ext 
           (lambda (a)
             (format "Interface ~a cannot be extended twice by one interface" a)))
          (field-repeat 
           (lambda (a)
             (format "More than one field is declared with name ~a" a)))
          (method-repeat 
           (lambda (a)
             (format "More than one method is declared as ~a" a)))
          (method-conflict 
           (lambda (a meth)
             (format "Inherited method ~a from ~a has a conflict with another method of the same name" 
                     meth a))))                                       
      (lambda (type)
        (case type
          ((duplicate-mods) dup-mods)
          ((one-of-access) one-of-access)
          ((interface-incorrect-modifiers) (invalid-mods "interfaces"))
          ((class-incorrect-modifiers) (invalid-mods "class"))
          ((invalid-field-mod) (invalid-mods "fields"))
          ((invalid-method-mod) (invalid-mods "methods"))
          ((abstract-restrict) (invalid-mods "an abstract method"))
          ((final-and-abstract) fin-and-abs)
          ((final-and-volatile) fin-and-vol)
          ((native-and-fp) nat-and-fp)
          ((extends-self) extend-self)
          ((cyclic-depends) cycle)
          ((repeatint-inherited-iface) iface-repeat-ext)
          ((repeated-field-name) field-repeat)
          ((repeated-method) method-repeat)
          ((inherited-method-conflict) method-conflict)))))
  
  ;raise-error ast symbol -> void
  ;raises syntax error to indicate erroneous types
  (define (raise-error wrong-code type)
    (if (symbol? type)
        (case type
          ((duplicate-mods one-of-access interface-incorrect-modifiers class-incorrect-modifiers 
                           invalid-field-mod invalid-method-mod abstract-restrict final-and-abstract
                           final-and-volatile native-and-fp) 
           ;wrong-code : modifier
           (raise-syntax-error (modifier-kind wrong-code)
                               ((get-error-message type) (modifier-kind wrong-code))
                               (datum->syntax-object #f 
                                                     (modifier-kind wrong-code) 
                                                     (build-src-list (modifier-src wrong-code)))))
          ((extends-self cyclic-depends repeating-inherited-iface)
           ;wrong-code : name
           (let ((name (string->symbol (id-string (name-id wrong-code)))))
             (raise-syntax-error name 
                                 ((get-error-message type) name)
                                 (datum->syntax-object #f
                                                       name
                                                       (build-src-list (name-src wrong-code))))))
          ((repeated-field-name)
           ;wrong-code : field
           (let ((field (string->symbol (id-string (field-name wrong-code)))))
             (raise-syntax-error field
                                 ((get-error-message type) field)
                                 (datum->syntax-object #f field (build-src-list (id-src (field-name wrong-code)))))))
          ((repeated-method)
           ;wrong-code : method
           (let ((m-name (id-string (method-name wrong-code)))
                 (parms (apply string-append
                               (map (lambda (p) (string-append (id-string (field-name p)) " "))
                                    (method-parms wrong-code)))))
             (raise-syntax-error (string->symbol m-name)
                                 ((get-error-message type) (format "~a(~a)" m-name 
                                                                   (substring parms 0 (sub1 (string-length parms)))))
                                 (datum->syntax-object #f 
                                                       (string->symbol m-name)
                                                       (build-src-list (method-src wrong-code))))))
          ((inherited-method-conflict)
           ;wrong-code : (list name method-record)
           (let ((name (car wrong-code))
                 (method (cadr wrong-code)))
             (raise-syntax-error (string->symbol (id-string (name-id name)))
                                 ((get-error-message type) (string->symbol (id-string (name-id name)))
                                  (method-record-name method)))))
          (else
           (error 'type-error "This file has a type error with uncaught type ~a" type)))
        (error 'internal-error "raise-error given ~a and ~a" wrong-code type)))
        
  (define build-info-location (make-parameter #f))
    
  
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
  ;Main function

  ;; build-info: package type-records (opt symbol)-> void
  (define (build-info prog type-recs . args)
    (let* ((pname (if (package-name prog)
                      (append (map id-string (name-path (package-name prog)))
                              (list (id-string (name-id (package-name prog)))))
                      null))
           (lang (send type-recs get-package-contents `("java" "lang") 
                                                       (lambda () (error 'type-recs 
                                                                         "Internal error: Type record not set with lang")))))
      
      (for-each (lambda (class) (send type-recs add-to-env class  `("java" "lang") (get-location prog))) lang)
      (for-each (lambda (class) (send type-recs add-class-req (cons class (list "java" "lang")) #f (get-location prog))) lang)
      (send type-recs add-class-req (list 'array) #f (get-location prog))

      
      (build-info-location (get-location prog))
      
      (for-each (lambda (class)
                  (send type-recs add-to-env (id-string (def-name class))
                        pname (get-location prog)))
                (package-defs prog))
      
      (add-my-package type-recs pname (package-defs prog) (get-location prog))
      
      (for-each (lambda (imp) (process-import type-recs imp))
                (package-imports prog))
      
      (for-each (lambda (def)
                  (let ((def-name (cons (id-string (header-id (def-header def))) pname)))
                    (send type-recs add-class-req def-name #f (get-location prog))
                    (send type-recs add-require-syntax def-name
                          (build-require-syntax (car def-name) 
                                                pname 
                                                (find-directory pname (lambda () (raise-error)))
                                                #t))
                    (send type-recs add-to-records def-name
                          (lambda () (process-class/iface def pname type-recs (null? args) 'full)))))
                  (package-defs prog))
                        
      (for-each (lambda (def) (process-class/iface def pname type-recs (null? args) 'full))
                (package-defs prog))
      
      (add-to-queue (package-defs prog))))

  (define (get-location p)
    (def-file (car (package-defs p))))
  
  (define (add-to-queue defs)
    (check-list (append defs (check-list))))

  (define (build-interactions-info prog type-recs)
    (when (field? prog)
      (send type-recs add-interaction-field 
            (process-field prog '("scheme-interactions") null type-recs 'full))))

  
  ;-----------------------------------------------------------------------------------
  ;Import processing

  ;;process-import: type-records import -> void
  (define (process-import type-recs imp) 
    (let* ((star? (import-star imp))
           (path (if star? 
                     (append (map id-string (name-path (import-name imp)))
                             (list (id-string (name-id (import-name imp)))))
                     (map id-string (name-path (import-name imp))))))
      (if star?
          (let ((classes (send type-recs get-package-contents path (lambda () #f))))
            (if classes
                (for-each (lambda (class) (send type-recs add-to-env class path (import-file imp))) classes)
                (let* ((dir (find-directory path (lambda () (raise-error #f #f))))
                       (classes (get-class-list dir)))
                  (for-each (lambda (class) (import-class class path dir (import-file imp) type-recs #t)) classes)
                  (send type-recs add-package-contents path classes))))
          (import-class (id-string (name-id (import-name imp)))
                        path
                        (find-directory path (lambda () (raise-error #f #f)))
                        (import-file imp)
                        type-recs
                        #t))))
  
  ;import-class: string (list string) (list string) location type-records bool-> void
  (define (import-class class path dir loc type-recs add-to-env)
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
                             (lambda (p) (parse p location 'full)))))
                 (send type-recs set-compilation-location location (build-path (apply build-path dir) "compiled"))
                 (build-info ast type-recs 'not_look_up)
                 (send type-recs get-class-record (cons class path) (lambda () 'internal-error "Failed to add record")))))
       (send type-recs add-require-syntax (cons class path) (build-require-syntax class path dir #t)))
      (else (raise-error)))
    (when add-to-env (send type-recs add-to-env class path loc))
    (send type-recs add-class-req (cons class path) (not add-to-env) loc))
  
  (define (add-my-package type-recs package defs loc)
    (let* ((dir (find-directory package (lambda () (raise-error))))
           (classes (get-class-list dir)))
      (for-each (lambda (c) (import-class c package dir loc type-recs #t)) 
                (filter (lambda (c) (not (contained-in? defs c))) classes))
      (send type-recs add-package-contents package classes)))
      
  (define (contained-in? defs class)
    (and (not (null? defs))
         (or (equal? class (id-string (def-name (car defs))))
             (contained-in? (cdr defs) class))))
  
  ;find-implicit-import: name type-records -> ( -> record )
  (define (find-implicit-import name type-recs)
    (lambda ()
      (let ((original-loc (send type-recs get-location))
            (dir (find-directory (cdr name) (lambda () (raise-error #f #f)))))
        (import-class (car name) (cdr name) dir original-loc type-recs #f)
        (begin0
          (get-record (send type-recs get-class-record name 
                            (lambda () (error 'internal-error (format "find-implicit-import: addition of ~a failed" name))))
                      type-recs)
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

  (define (get-class-list dir)
    (filter (lambda (c-name) (not (equal? c-name "")))
            (map (lambda (fn) (substring fn 0 (- (string-length fn) (string-length ".java"))))
                 (filter (lambda (f) (equal? (filename-extension f) "java"))
                         (directory-list (apply build-path dir))))))
  
  (define (load-lang type-recs)
    (let* ((dir (find-directory `("java" "lang") (lambda () 'load-lang "Internal-error: Lang not accessible")))
           (class-list (map (lambda (fn) (substring fn 0 (- (string-length fn) (string-length ".jinfo"))))
                            (filter (lambda (f) (equal? (filename-extension f) "jinfo"))
                                    (directory-list (build-path (apply build-path dir) "compiled"))))))
      (send type-recs add-package-contents `("java" "lang") class-list)
      (for-each (lambda (c) (import-class c `("java" "lang") dir #f type-recs #f)) class-list)
      (send type-recs add-require-syntax (list 'array) (list (datum->syntax-object #f `(lib "array.ss" "drj" "libs" "java" "lang") #f)
                                                             (datum->syntax-object #f `(lib "array.ss" "drj" "libs" "java" "lang") #f)))))
      
  
  ;------------------------------------------------------------------------------------
  ;Functions for processing classes and interfaces
  
  ;; process-class/iface: (U class-def interface-def) (list string) type-records bool symbol -> class-record
  (define (process-class/iface ci package-name type-recs look-in-table level)
    (cond
      ((interface-def? ci) (process-interface ci package-name type-recs look-in-table level))
      ((class-def? ci) (process-class ci package-name type-recs look-in-table level))))
       
  
  ;;get-parent-record: (list string) name (list string) type-records (list string) -> record
  (define (get-parent-record name name-src child-name type-recs)
    (when (equal? name child-name)
      (raise-error name-src 'extends-self))
    (let ((record (send type-recs get-class-record name (lambda () #f))))
      (cond
        ((class-record? record) record)
        ((procedure? record) (get-record record type-recs))
        ((eq? record 'in-progress)
         (raise-error name-src 'cyclic-depends))
        (else (get-record (find-implicit-import name type-recs) type-recs)))))
  
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
               (let* ((super-name (if (null? (header-extends info))
                                      '("Object" "java" "lang")
                                      (name->list (car (header-extends info)))))
                      (super-record (get-parent-record super-name (if (null? (header-extends info))
                                                                      null
                                                                      (car (header-extends info)))
                                                       cname type-recs))
                      (members (class-def-members class))
                      (reqs (map (lambda (name-list) 
                                   (if (= (length name-list) 1)
                                       (make-req (car name-list) (send type-recs lookup-path (car name-list) (lambda () (raise-error #f #f))))
                                       (make-req (car name-list) (cdr name-list))))
                                 (cons super-name (map name->list (header-implements info))))))
                 (send type-recs set-location! (class-def-file class))
                 (set-class-def-uses! class reqs)
                 (unless (andmap (lambda (req) (type-exists? (req-class req) (req-path req) type-recs))
                                 reqs)
                   (raise-error #f #f))
                 (let-values (((f m) (process-members members cname type-recs level)))
                   (valid-field-names? f members type-recs)
                   (valid-method-sigs? m members level type-recs)
                   (let ((record
                          (make-class-record 
                           cname
                           (check-class-modifiers 'full (header-modifiers info))
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
  
  ;; process-interface: interface-def (list string) type-records bool symbol -> class-record
  (define (process-interface iface package-name type-recs look-in-table? level)
    (let* ((info (interface-def-info iface))
           (iname (cons (id-string (header-id info)) package-name)))
      (send type-recs set-location! (def-file iface))
      (let ((build-record 
             (lambda ()
               (send type-recs add-to-records iname 'in-progress)
               (let* ((super-names (map name->list (header-extends info)))
                      (super-records (map (lambda (n sc) (get-parent-record n sc iname type-recs))
                                          super-names
                                          (header-extends info)))
                      (members (interface-def-members iface))
                      (reqs (map (lambda (name-list) (make-req (car name-list) (cdr name-list)))
                                 super-names)))
                 (send type-recs set-location! (interface-def-file iface))
                 (set-interface-def-uses! iface reqs)                 
                 (unless (andmap (lambda (req) (type-exists? (req-class req) (req-path req) type-recs))
                                 reqs)
                   (raise-error #f #f))
                 
                 (valid-iface-extend? super-records (header-extends info))
                 
                 (let-values (((f m) (process-members members null type-recs level)))
                   
                   (valid-field-names? f members type-recs)
                   (valid-method-sigs? m members level type-recs)
                   (valid-inherited-methods? super-records (header-extends info))
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
             (raise-error (car extends) 'repeating-inherited-iface))
        (valid-iface-extend? (cdr records) (cdr extends))))
  
  (define (valid-field-names? fields members type-recs)
    (or (null? fields)
        (and (field-member? (car fields) (cdr fields))
             (raise-error (find-member (car fields) members type-recs) 'repeated-field-name))
        (valid-field-names? (cdr fields) members type-recs)))
  
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
  
  (define (valid-method-sigs? methods members level type-recs)
    (or (null? methods)
        (and (method-member? (car methods) (cdr methods) level)
             (raise-error (find-member (car methods) members type-recs) 'repeated-method))
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
  
  (define (valid-inherited-methods? records extends)
    (or (null? records)
        (and (check-inherited-method (class-record-methods (car records))
                                     (cdr records)
                                     (car extends))
             (valid-inherited-methods? (cdr records) (cdr extends)))))
  
  (define (check-inherited-method methods records from level)
    (or (null? methods)
        (and (method-conflicts? (car methods) 
                                (apply append (map class-record-methods records)))
             (raise-error (list from (car methods)) 'inherited-method-conflict))
        (check-inherited-method (cdr methods) records from)))
  
  (define (method-conflicts? method methods level)
    (and (not (null? methods))
         (or (and (equal? (method-record-name method)
                          (method-record-name (car methods)))
                  (or (or (eq? level 'beginner) (eq? level 'intermediate))
                      (andmap type=? (method-record-atypes method) (method-record-atypes (car methods))))
                  (type=? (method-record-rtype method) (method-record-rtype (car methods))))
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
             (raise-error (list (find-member (car methods) members type-recs) record) 'conflicting-method))
        (check-for-conflicts methods record members level type-recs)))
  
      
  ;; process-members: (list members) (list string) type-records symbol -> 
  ;;                     (values (list field-record) (list method-record))
  (define (process-members members cname type-recs level)
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
               (cons (process-method (car members) cname type-recs level) methods)))
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

  ;; process-method: method (list string) type-records symbol -> method-record  
  (define (process-method method cname type-recs level)
    (make-method-record (id-string (method-name method))
                        (check-method-modifiers level (method-modifiers method))
                        (type-spec-to-type (method-type method) type-recs)
                        (map (lambda (x)
                               (type-spec-to-type  x type-recs))
                             (map var-decl-type
                                  (method-parms method)))
                        (map (lambda (t)
                               (cons (id-string (name-id t))
                                     (map id-string (name-path t))))
                             (method-throws method))
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
      mods))
  
  ;make-valid-mods: (symbol -> (list symbol)) (symbol -> symbol) -> (symbol (list modifier) -> bool)
  (define (make-valid-mods valids-choice error-type)
    (letrec ((tester
              (lambda (level mods)
                (or (null? mods)
                    (and (not (memq (modifier-kind (car mods)) (valids-choice level)))
                         (raise-error (car mods) (error-type level)))
                    (tester level (cdr mods))))))
      tester))

  ;valid-*-mods?: symbol (list modifier) -> bool
  (define valid-class-mods?
    (make-valid-mods (lambda (x) '(public abstract final strictfp))
                     (lambda (x) 'class-incorrect-modifiers)))
  (define valid-interface-mods?
    (make-valid-mods (lambda (x) '(public abstract strictfp))
                     (lambda (x) 'interface-incorrect-modifiers)))  
  (define valid-field-mods?
    (make-valid-mods
     (lambda (level)
       (case level
         ((beginner intermediate) '(private))
         ((advanced) '(public protected private static))
         ((full) `(public protected private static final transient volatile))))
     (lambda (x) 'invalid-field-mod)))  
  (define valid-method-mods?
    (make-valid-mods
     (lambda (level)
       (case level
         ((beginner intermediate) '(public abstract))
         ((advanced) `(public protected private abstract static final))
         ((full) '(public protected private abstract static final synchronized native strictfp))
         ((abstract) '(public protected))))
     (lambda (level)
       (if (eq? level 'abstract) 'abstract-restrict 'invalid-method-mod))))
  
  ;one-access: symbol symbol symbol (list modifiers) -> bool
  (define (one-access is check1 check2 mods)
    (and (eq? is (modifier-kind (car mods)))
         (or (memq check1 (map modifier-kind (cdr mods)))
             (memq check2 (map modifier-kind (cdr mods))))
         (raise-error (car mods) 'one-of-access)))

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
                              (raise-error (car mods) error))
                         (and (eq? second (modifier-kind (car mods)))
                              (memq first (map modifier-kind (cdr mods)))
                              (raise-error (car mods) error))
                         (tester (cdr mods)))))))
      tester))
  
  (define final-and-abstract? (make-not-two 'final 'abstract 'final-and-abstract))
  (define volatile-and-final? (make-not-two 'volatile 'final 'final-and-volatile))  
  (define native-and-fp? (make-not-two 'native 'strictfp 'native-and-fp))
    
  ;duplicate-mods?: (list modifier) -> bool
  (define (duplicate-mods? mods)
    (and (not (null? mods))
         (or (and (memq (modifier-kind (car mods))
                        (map modifier-kind (cdr mods)))
                  (raise-error (car mods) 'duplicate-mods))
             (duplicate-mods? (cdr mods)))))
  
  )
  