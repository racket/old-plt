(module teachpack mzscheme
  (require (lib "unitsig.ss")
	   (lib "list.ss")
           (lib "file.ss")
	   (lib "framework.ss" "framework")
	   (lib "mred.ss" "mred")
	   (lib "string-constant.ss" "string-constants")
	   "reload.ss"
           "drsig.ss")

  (provide teachpack@)

  (define teachpack@
    (unit/sig drscheme:teachpack^
      (import [drscheme:init : drscheme:init^])

      ;; type teachpack-cache = (make-teachpack-cache (listof cache-entry) number)
      ;; the timestamp indicates the last time this teachpack was loaded
      (define-struct teachpack-cache (tps timestamp))
      
      ;; type cache-entry = (make-cache-entry string (union #f bindings))
      ;; type bindings = (union (listof (list symbol TST)) #f)
      ;; bindings represents the invoked teachpack unit. It is #f when the
      ;; teachpack doesn't apply to this language level
      (define-struct cache-entry (filename bindings))
      
      ;; new-teachpack-cache : -> teachpack-cache
      (define (new-teachpack-cache) (make-teachpack-cache null 0))
      
      ;; set-teachpack-filenames : teachpack-cache (listof string) -> void
      ;; updates the teachpack cache with the new filenames, preserving
      ;; any cached teachpacks already in there
      (define (set-teachpack-cache-filenames! teachpack-cache filenames)
        (let ([saved-cache-entries (filter (lambda (x) 
                                             (ormap 
                                              (lambda (y)
                                                (filename=? (cache-entry-filename x) y))
                                              filenames))
                                           (teachpack-cache-tps teachpack-cache))]
              [missing-files (filter (lambda (x) 
                                       (andmap
                                        (lambda (y) (not (filename=? x (cache-entry-filename y))))
                                        (teachpack-cache-tps teachpack-cache)))
                                     filenames)])
          (set-teachpack-cache-tps!
           teachpack-cache
           (append saved-cache-entries
                   (map (lambda (filename) (make-cache-entry filename #f))
                        missing-files)))))
      
      ;; load-teachpacks : namespace teachpack-cache -> void
      ;; loads the teachpacks from the disk
      ;; initializes teachpack-units, reloading the teachpacks from the disk if they have changed.
      ;; ensures that none of the cache-entry structs have #f's.
      ;; re-invokes all of the teachpacks.
      (define (load-teachpacks user-namespace cache)
	(let ([new-timestamp (current-seconds)]
              [new-tps (map (lambda (x) (load-teachpack 
                                         user-namespace
                                         (cache-entry-filename x)
                                         (teachpack-cache-timestamp cache)))
                            (teachpack-cache-tps cache))])
          (set-teachpack-cache-timestamp! cache new-timestamp)
	  (set-teachpack-cache-tps! cache (filter (lambda (x) x) new-tps))))
      
      ;; load-teachpack : namespace string[filename] number[timestamp] -> (union #f cache-entry)
      ;; loads the techpack file and invokes the teachpack unit
      ;; returns #f  and display error to user if the teachpack doesn't load properly
      (define (load-teachpack user-namespace tp-filename timestamp)
	(let ([teachpack-assoc
               (with-handlers ([not-break-exn?
                                (lambda (x)
                                  (show-teachpack-error tp-filename x)
                                  #f)])
                 (reload `(file ,tp-filename) timestamp)
                 (let* ([teachpack-unit@ (dynamic-require `(file ,tp-filename) 'teachpack-unit@)]
                        [export-signature
                         (exploded->flattened-signature
                          (unit/sig-exports
                           teachpack-unit@))]
                        [imports (get-teachpack-imports tp-filename user-namespace teachpack-unit@)]
                        [unitsig-name ((current-module-name-resolver)
                                       '(lib "unitsig.ss") #f #f)]
                        [orig-namespace (current-namespace)])
                   (and imports
                        (parameterize ([current-namespace (make-namespace)])
                          (namespace-attach-module orig-namespace unitsig-name)
                          (namespace-require '(lib "unitsig.ss"))
                          (copy-to-current-namespace user-namespace imports)
                          (eval `(let ()
                                   (define-values/invoke-unit/sig ,export-signature 
                                                                  ,teachpack-unit@
                                                                  #f
                                                                  ,imports)
                                   (list
                                    ,@(map (lambda (ent) `(list ',ent ,ent))
                                           export-signature))))))))])
          (and teachpack-assoc
               (make-cache-entry tp-filename teachpack-assoc))))

      ;; get-teachpack-imports : string namespace unit/sig -> (union #f (listof symbol))
      ;; extracts the imports from the teachpack.
      ;; if there is more than one unit imports, an error is signaled
      ;; returns #f if any of the names in the import are not defined
      ;; in the namespace.
      (define (get-teachpack-imports tp-filename user-namespace teachpack-unit@)
        (let ([imports (unit/sig-imports teachpack-unit@)])
          (unless (= 1 (length imports))
            (error
             'teachpack
             (string-constant teachpack-not-only-one-import)
             tp-filename))
          (let ([names (exploded->flattened-signature (cdr (car imports)))])
            (and (parameterize ([current-namespace user-namespace])
                   (andmap (lambda (x)
                             (with-handlers ([not-break-exn? (lambda (x) #f)])
                               (namespace-variable-binding x)
                               #t))
                           names))
                 names))))

      ;; copy-to-current-namespace : namespace (listof symbol) -> void
      ;; copies the values bound by `names' from `namespace' to the 
      ;; current namespace.
      (define (copy-to-current-namespace namespace names)
        (for-each
         (lambda (name)
           (let ([other-binding (parameterize ([current-namespace namespace])
                               (namespace-variable-binding name))])
             (namespace-variable-binding name other-binding)))
         names))
      
      ;; install-teachpacks : teqachpack-cache -> void
      ;; =User=
      ;; installs the loaded teachpacks
      ;; updates the cache, removing those teachpacks that failed to run
      ;; requires that none of the cache-entries have #fs in them.
      (define (install-teachpacks cache)
        (for-each install-teachpack (teachpack-cache-tps cache)))
      
      ;; install-teachpack : cache-entry -> void
      ;; =User=
      (define (install-teachpack cache-entry)
        (let ([bindings (cache-entry-bindings cache-entry)])
          (when bindings
            (for-each (lambda (ent) (namespace-variable-binding (car ent) (cadr ent)))
                      bindings))))
      
      ;; marshall-teachpack-cache : teachpack-cache -> writable
      (define (marshall-teachpack-cache cache)
        (map cache-entry-filename (teachpack-cache-tps cache)))
      
      ;; unmarshall-teachpack-cache : writable -> teachpack-cache
      (define (unmarshall-teachpack-cache lof)
        (make-teachpack-cache
         (if (and (list? lof)
                  (andmap string? lof))
             (map (lambda (x) (make-cache-entry x #f)) lof)
             null)
         0))
      
      ;; teachpack-cache-filenames : teachpack-cache -> (listof string)
      (define (teachpack-cache-filenames cache)
        (map cache-entry-filename (teachpack-cache-tps cache)))
      
      ;; teachpack-cache-applies : teachpack-cache -> (listof boolean)
      (define (teachpack-cache-applies cache)
        (map (lambda (x) (not (not (cache-entry-bindings x)))) (teachpack-cache-tps cache)))
      
      ;; filename=? : string string -> boolean
      ;; compares two strings as filenames (depends on current-directory parameter)
      (define (filename=? f1 f2)
        (string=? (normal-case-path (normalize-path f1))
                  (normal-case-path (normalize-path f2))))
      
      ;; exploded->flattened-signature : exploded-signature -> (listof symbol)
      ;; flattens a signature according to the way mz does it (so we hope).
      ;;    (see mzscheme docs for exploded signature data spec)
      (define (exploded->flattened-signature signature)
        
        ;; handle-exploded : exploded (listof symbol) string -> (listof symbol)
        ;; translates an exploded signature to a flattened signature
        ;; `sofar' is an accumulator: the list of symbols flattened out sofar
        ;; `prefix' is an accumulator: the current prefix for symbols at this point in the tree
        (define (handle-exploded signature sofar prefix)
          (let loop ([elements (vector->list signature)]
                     [sofar sofar])
            (cond
              [(null? elements) sofar]
              [else (loop (cdr elements)
                          (handle-element (car elements) sofar prefix))])))
        
        ;; handle-element : signature-element -> (listof symbol)
        ;; translates a signature element from an exploded signature to a flattened signature
        ;;    (see mzscheme docs for signature element data spec)
        ;; `sofar' is an accumulator: the list of symbols flattened out sofar
        ;; `prefix' is an accumulator: the current prefix for symbols at this point in the tree
        (define (handle-element signature-element sofar prefix)
          (cond
            [(symbol? signature-element) (cons (add-prefix prefix signature-element) sofar)]
            [(pair? signature-element)
             (handle-exploded (cdr signature-element)
                              sofar
                              (extend-prefix (car signature-element) prefix))]))
        
        ;; add-prefix : string symbol -> symbol
        ;; prefxixes prefix-str onto sym
        (define (add-prefix prefix-str sym)
          (string->symbol (string-append prefix-str (symbol->string sym))))
        
        ;; extend-prefix : string string -> string
        ;; extends the current prefix
        (define (extend-prefix current-prefix new-bit)
          (string-append current-prefix new-bit ":"))
        
        ;; start the helper functions
        (handle-exploded signature null ""))

       ;; show-teachpack-error : TST -> void
      ;; shows an error message for a bad teachpack.
      (define (show-teachpack-error tp-filename exn)
        (message-box 
         (string-constant teachpack-error-label)
         (string-append
          (format (string-constant teachpack-didnt-load)
                  tp-filename)
          (string #\newline)
          
          ;; should check for error trace and use that here (somehow)
          (if (exn? exn)
              (exn-message exn)
              (format "uncaught exception: ~s" exn))))))))
