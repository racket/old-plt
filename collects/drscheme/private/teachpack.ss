(module teachpack mzscheme
  (require (lib "unitsig.ss")
	   (lib "list.ss")
           (lib "file.ss")
	   (lib "framework.ss" "framework")
	   (lib "mred.ss" "mred")
	   "string-constant.ss"
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
      ;; type bindings = (listof (list symbol TST))
      ;; bindings represents the invoked teachpack unit.
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
      
      ;; load-teachpacks : teachpack-cache -> void
      ;; loads the teachpacks from the disk
      ;; initializes teachpack-units, reloading the teachpacks from the disk if they have changed.
      ;; ensures that none of the cache-entry structs have #f's
      (define (load-teachpacks cache)
	(let ([new-timestamp (current-seconds)]
              [new-tps (map (lambda (x) (load-teachpack 
                                         (cache-entry-filename x)
                                         (teachpack-cache-timestamp cache)))
                            (teachpack-cache-tps cache))])
          (set-teachpack-cache-timestamp! cache new-timestamp)
	  (set-teachpack-cache-tps! cache (filter (lambda (x) x) new-tps))))
      
      ;; load-teachpack : string[filename] number[timestamp] -> (union #f cache-entry)
      ;; loads the techpack file and invokes the teachpack unit
      ;; returns #f  and display error to user if the teachpack doesn't load properly
      (define (load-teachpack tp-filename timestamp)
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
                        [unitsig-name ((current-module-name-resolver)
                                       '(lib "unitsig.ss") #f #f)]
                        [orig-namespace (current-namespace)])
                   (parameterize ([current-namespace (make-namespace)])
                     (namespace-attach-module orig-namespace unitsig-name)
                     (namespace-require '(lib "unitsig.ss"))
                     (eval `(let ()
                              (define-values/invoke-unit/sig ,export-signature ,teachpack-unit@)
                              (list
                               ,@(map (lambda (ent) `(list ',ent ,ent))
                                      export-signature)))))))])
          (and teachpack-assoc
               (make-cache-entry tp-filename teachpack-assoc))))

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
        (for-each (lambda (ent) (namespace-variable-binding (car ent) (cadr ent)))
                  (cache-entry-bindings cache-entry)))
      
      
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
        
        ;; start the helper function
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
