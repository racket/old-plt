
(module teachpack mzscheme
  (require (lib "unitsig.ss")
	   (lib "list.ss")
           (lib "file.ss")
           (lib "etc.ss")
	   (lib "framework.ss" "framework")
	   (lib "mred.ss" "mred")
	   (lib "string-constant.ss" "string-constants")
           "drsig.ss")

  (provide teachpack@)

  (define o (current-output-port))
  (define (oprintf . args) (apply fprintf o args))
  
  (define teachpack@
    (unit/sig drscheme:teachpack^
      (import)

      ;; type teachpack-cache = (make-teachpack-cache (listof cache-entry))
      ;; the timestamp indicates the last time this teachpack was loaded
      (define-struct teachpack-cache (tps))
      
      ;; type cache-entry = (make-cache-entry string[filename])
      (define-struct cache-entry (filename))
      
      ;; new-teachpack-cache : -> teachpack-cache
      (define new-teachpack-cache
        (opt-lambda ([filenames '[]])
          (make-teachpack-cache (map make-cache-entry filenames))))
      
      ;; set-teachpack-cache-filenames! : teachpack-cache (listof string) -> void
      ;; this shouldn't remove all of the #fs.
      (define (set-teachpack-cache-filenames! teachpack-cache filenames)
        (set-teachpack-cache-tps!
         teachpack-cache
         (map (lambda (filename) (make-cache-entry filename))
              filenames)))
      
      ;; install-teachpacks : teqachpack-cache -> void
      ;; =User=
      ;; installs the loaded teachpacks
      ;; updates the cache, removing those teachpacks that failed to run
      ;; requires that none of the cache-entries have #fs in them.
      (define (install-teachpacks cache)
        (for-each install-teachpack (teachpack-cache-tps cache))
        (set-teachpack-cache-tps!
         cache
         (filter cache-entry-filename (teachpack-cache-tps cache))))
      
      ;; install-teachpack : cache-entry -> void
      ;; =User=
      ;; updates the cache-entry's filename to #f if the teachpack fails
      ;; to run properly
      (define (install-teachpack cache-entry)
        (let ([filename (cache-entry-filename cache-entry)])
          (with-handlers ([not-break-exn?
                           (lambda (exn)
                             (set-cache-entry-filename! cache-entry #f)
                             (show-teachpack-error filename exn))])
            (verify-no-new-exports filename)
            (namespace-require `(file ,filename)))))

      ;; verify-no-new-exports : string -> void
      ;; ensures that the teachpack wouldn't override any thing in the user's namespace
      (define (verify-no-new-exports filename)
        (let ([exports (extract-provided-variables-from-module filename)]
              [ns-contents (namespace-mapped-symbols)]
              [ht (make-hash-table)])
          (for-each (lambda (ns-sym) (hash-table-put! ht ns-sym #t)) ns-contents)
          (for-each (lambda (expt)
                      (when (hash-table-get ht expt (lambda () #f))
                        (error 'teachpack "export of ~a from ~s conflicts with already existing definitions"
                               expt filename)))
                    exports)))
      
      ;; extract-provided-variables-from-module : string -> ()listof symbol)
      (define (extract-provided-variables-from-module filename)
        (let* ([module-stx 
                (parameterize ([current-namespace (make-namespace)])
                  (expand (parameterize [(read-case-sensitive #f)
                                         (read-square-bracket-as-paren #t)
                                         (read-curly-brace-as-paren #t)
                                         (read-accept-box #t)
                                         (read-accept-compiled #t)
                                         (read-accept-bar-quote #t)
                                         (read-accept-graph #t)
                                         (read-decimal-as-inexact #t)
                                         (read-accept-dot #t)
                                         (read-accept-quasiquote #t)]
                            (call-with-input-file filename (lambda (port) (read-syntax filename port))))))]
               [var-prop (get-exported-names (syntax-property module-stx 'module-variable-provides))]
               [mac-prop (get-exported-names (syntax-property module-stx 'module-syntax-provides))])
          (append var-prop mac-prop)))
      
      ;; get-exported-names : module-variable-provides / module-syntax-provides info (see mz manual) -> (listof symbol)
      (define (get-exported-names names)
        (if names
            (map (lambda (x)
                   (cond
                     [(symbol? x) x]
                     [(symbol? (cdr x)) (car x)]
                     [else (cadr x)]))
                 names)
            '()))
        
      ;; marshall-teachpack-cache : teachpack-cache -> writable
      (define (marshall-teachpack-cache cache)
        (map cache-entry-filename (teachpack-cache-tps cache)))
      
      ;; unmarshall-teachpack-cache : writable -> teachpack-cache
      (define (unmarshall-teachpack-cache lof)
        (make-teachpack-cache
         (if (and (list? lof)
                  (andmap string? lof))
             (map (lambda (x) (make-cache-entry x)) lof)
             null)))
      
      ;; teachpack-cache-filenames : teachpack-cache -> (listof string)
      (define (teachpack-cache-filenames cache)
        (map cache-entry-filename (teachpack-cache-tps cache)))
      
      ;; launcher-init-code : teachpack-cache -> sexp
      ;; constructs code to be put in  a module that loads the teachpacks.
      ;; used with launchers
      (define (launcher-init-code cache)
        `(begin
           (void)
           ,@(map (lambda (ce)
                    `(namespace-require '(file ,(cache-entry-filename ce))))
                  (teachpack-cache-tps cache))))
      
      ;; launcher-modules-to-embed : teachpack-cache -> (listof module-spec)
      ;; the modules to embed in a stand-alone executable.
      (define (launcher-modules-to-embed cache)
        (map (lambda (ce) `(file ,(cache-entry-filename ce)))
             (teachpack-cache-tps cache)))

      ;; show-teachpack-error : string TST -> void
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
