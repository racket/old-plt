(module compile mzscheme
  (require "parameters.ss" "ast.ss" "types.ss" "parser.ss" "build-info.ss" "check.ss" "to-scheme.ss")
  (require (lib "list.ss")
           (lib "file.ss")
           (lib "class.ss"))

  (provide compile-java compile-interactions compile-files
           compilation-unit-code compilation-unit-contains set-compilation-unit-code!
           read-record write-record
           set-syntax-location create-type-record
           )
  
  (define (set-syntax-location so) (syntax-location so))
  
  (define (create-type-record)
    (let ((t (make-object type-records)))
      (set-importer! t find-implicit-import)
      (load-lang t)
      t))
  
  ;kind = 'file | 'port
  ;level = 'beginner | 'intermediate | 'advanced | 'full
  
  ;compile: kind kind level (U #f string) (U #f port) (U #f location) -> (U (list compilation-unit) void)
  (define (compile-java src dest level name port loc . type-recs)
    (when (and (eq? src 'file)
               (not (file-exists? name)))
      (error 'compile-java "Compile-java given file that does not exist: ~a" name))
    (let ((type-recs (if (null? type-recs)
                         (make-object type-records)
                         (car type-recs))))
      (cond
        ((and (eq? src 'file) (eq? dest 'file))
         (call-with-input-file name (lambda (port) (compile-to-file port name level))))
        ((eq? dest 'file)
         (compile-to-file port loc level))
        ((eq? src 'file)
         (call-with-input-file 
             name
             (lambda (port) (compile-java-internal port name type-recs #f level))))
        (else
         (compile-java-internal port loc type-recs #f level)))))
    
  ;compile-to-file: port location level -> void
  ;Should have side-effect of writing to file all files needed for compilation
  (define (compile-to-file port location level)
    (let ((type-recs (make-object type-records)))
      (send type-recs set-compilation-location
            location 
            (if (equal? (file-name-from-path location) location)
                (build-path "compiled")
                (build-path (substring location 0 (- (string-length location)
                                                     (string-length (file-name-from-path location)))) "compiled")))
      (for-each (lambda (dependents)
                  (let ((names (compilation-unit-contains dependents))
                        (syntaxes (compilation-unit-code dependents))
                        (locations (compilation-unit-locations dependents)))
                    (unless (= (length names) (length syntaxes))
                      (call-with-output-file* (send type-recs get-composite-location (car names))
                                              (lambda (port) (write (compile (car syntaxes)) port)))
                      (set! syntaxes (cdr syntaxes)))
                    (unless (= (length names) (length syntaxes) (length locations))
                      (error 'compile-to-file "Internal error: compilation unit not represented as expected"))
                    (for-each (lambda (name code location)
                                (send type-recs set-location! location)
                                (let ((directory (send type-recs get-compilation-location)))
                                  (unless (directory-exists? directory) (make-directory directory))
                                  (call-with-output-file* (build-path directory (string-append name ".zo"))
                                                          (lambda (port) (write (compile code) port))
                                                          'truncate/replace)
                                  (call-with-output-file* (build-path directory (string-append name ".jinfo"))
                                                          (lambda (port) (write-record (send type-recs get-class-record 
                                                                                             (list name) 
                                                                                             class-record-error)
                                                                                       port))
                                                          'truncate/replace)))
                              names syntaxes locations)))
                (compile-java-internal port location type-recs #t level))))
  
  (define (class-record-error) (error 'compile-to-file "Internal error: class record not found"))
  
  ;package: (list string)
  
  ;compile-files: (list (list (list path) package)) boolean symbol ->
  ;               (list (list package (list (list compiliation-unit)) (list class-record)))
  (define (compile-files files to-file? level)
    (let ((type-recs (make-object type-records))
          (get-class-names 
           (lambda (files)
             (map (lambda (f)
                    (regexp-replace ".java" (file-name-from-path f) ""))
                  files))))
      (map (lambda (package-files)
             (let* ((files (car package-files))
                    (package-name (cadr package-files))
                    (class-names (get-class-names files)))
               (list package-name
                     (filter (lambda (t) t)
                             (map (lambda (file class)
                                    (let ((existing-record (send type-recs get-class-record (cons class package-name) 
                                                                 (lambda () #f))))
                                      (and (or (not existing-record) 
                                               (procedure? existing-record))
                                           (call-with-input-file file 
                                             (lambda (port) (compile-java-internal port file type-recs to-file? level))))))
                                  files class-names))
                     (map (lambda (class)
                            (send type-recs get-class-record (cons class package-name) (lambda () (error 'internal-error))))
                          class-names))))
           files)))
  
  ;compile-java-internal: port location type-records bool level-> (list compilation-unit)
  (define (compile-java-internal port location type-recs file? level)
    (packages null)
    (check-list null)
    (to-file file?)
    (let ((ast (parse port location level)))
      (remember-main ast)
      (load-lang type-recs)
      (set-importer! type-recs find-implicit-import)
      (build-info ast level type-recs #f)
      (check-defs (car (check-list)) level type-recs)
      (remove-from-packages ast type-recs)
      (order-compilation-units (translate-program ast type-recs) type-recs)))
  
  ;compile-interactions: port location type-records level -> syntax
  (define (compile-interactions port location type-recs level)
    (to-file #f)
    (let ((ast (parse-interactions port location level)))
      (build-interactions-info ast level location type-recs)
      (check-interactions-types ast level location type-recs)
      (translate-interactions ast type-recs)))
      
  (define-struct elt (prev val next))
  
  (define fifo
    (class* object% ()
      (define head null)
      (define tail null)
      
      (define/public (empty?)
        (and (null? head) (null? tail)))
      
      (define/public (pop)
        (let ((old-head head))
          (set! head (elt-next head))
          (when (null? head) 
            (set! tail null))
          (unless (null? head)
            (set-elt-prev! head null))
          (elt-val old-head)))
      (define/public (push e)
        (let ((new-elt (make-elt tail e null)))
          (if (empty?)
              (begin (set! head new-elt)
                     (set! tail head))
              (begin
                (set-elt-next! tail new-elt)
                (set! tail new-elt)))))
      (super-instantiate ())))
  
  (define (make-queue) (make-object fifo))
  (define (empty-queue? q) (send q empty?))
  (define (add-to-work-queue q elts) (for-each (lambda (e) (send q push e)) elts))
  (define (queue-head q) (send q pop))
          

  (define (order-compilation-units units type-recs)
    (letrec ((ordered-cu null)
             (work-queue (make-queue))
             ;compilation-unit to bool
             (completed (make-hash-table))
             
             (completed? 
              (lambda (elt)
                (hash-table-get completed elt (lambda () #f))))
             
             (sort-from-element
              (lambda (elt) 
                (unless (completed? elt)
                  (hash-table-put! completed elt #t)
                  (set! ordered-cu (append ordered-cu (list elt)))
                  (add-to-work-queue work-queue (get-local-depends (compilation-unit-depends elt)))
                  (process-queue))))
             
             (process-queue
              (lambda ()
                (unless (empty-queue? work-queue)
                  (let ((elt (queue-head work-queue)))
                    (unless (completed? elt)
                      (hash-table-put! completed elt #t)
                      (set! ordered-cu (cons elt ordered-cu))
                      (add-to-work-queue work-queue (get-local-depends (compilation-unit-depends elt)))
                      (process-queue))))))
             
             (get-local-depends 
              (lambda (reqs)
                (if (null? reqs)
                    null
                    (let ((found? (find (car reqs))))
                      (if found?
                          (cons found? (get-local-depends (cdr reqs)))
                          (get-local-depends (cdr reqs)))))))
             
             (find 
              (lambda (req)
                (letrec ((walker 
                          (lambda (cus)
                            (and (not (null? cus))
                                 (if (contained-in? (car cus))
                                     (car cus)
                                     (walker (cdr cus))))))
                         (class (req-class req))
                         (contained-in? 
                          (lambda (cu)
                            (and (member class (compilation-unit-contains cu))
                                 (equal? (req-path req)
                                         (begin
                                           (send type-recs set-location! (list-ref (compilation-unit-locations cu)
                                                                                   (get-position class (compilation-unit-contains cu) 0)))
                                           (send type-recs lookup-path class (lambda () (error 'internal-error)))))))))
                  (walker units))))
             
             (get-position
              (lambda (name names pos)
                (if (or (null? (cdr names))
                        (equal? name (car names)))
                    pos
                    (get-position name (cdr names) (add1 pos))))))
      
      (for-each sort-from-element units)
      ordered-cu))
                
  
  (define (remove-from-packages ast type-recs) 
    (packages (filter (lambda (def) (not (contained-in? def (package-defs ast))))
                      (packages))))

  (define (contained-in? def defs)
    (and (not (null? defs))
         (or (eq? def (car defs))
             (contained-in? def (cdr defs)))))
  
  (define (contains-main? members)
    (and (not (null? members))
         (or (and (method? (car members))
                  (equal? "main" (id-string (method-name (car members)))))
             (contains-main? (cdr members)))))
  
  (define (remember-main ast)
    (let ((main-class (filter (lambda (def)
                                (memq 'public 
                                      (map modifier-kind (header-modifiers (class-def-info def)))))
                              (filter class-def?
                                      (package-defs ast)))))
      (if (null? main-class)
          (main (list #f null))
          (main (list (contains-main? (class-def-members (car main-class)))
                      (id-string (header-id (class-def-info (car main-class)))))))))
    
  )

