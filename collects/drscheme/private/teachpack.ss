
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

      ;; type teachpack-cache = (make-teachpack-cache (listof cache-entry))
      ;; the timestamp indicates the last time this teachpack was loaded
      (define-struct teachpack-cache (tps))
      
      ;; type cache-entry = (make-cache-entry string[filename] 
      ;;                                      (union #f (listof symbol)))
      (define-struct cache-entry (filename provides))
      
      ;; new-teachpack-cache : -> teachpack-cache
      (define (new-teachpack-cache) (make-teachpack-cache null))
      
      ;; set-teachpack-cache-filenames! : teachpack-cache (listof string) -> void
      ;; this shouldn't remove all of the #fs.
      (define (set-teachpack-cache-filenames! teachpack-cache filenames)
        (set-teachpack-cache-tps!
         teachpack-cache
         (map (lambda (filename) (make-cache-entry filename #f))
              filenames)))
      
      ;; load-teachpacks : namespace teachpack-cache -> void
      ;; loads the teachpacks from the disk and extracts their provided
      ;; variables. If any teachpack signaled an error
      (define (load-teachpacks user-namespace tp)
        (for-each load-teachpack (teachpack-cache-tps tp))
        (set-teachpack-cache-tps!
         tp
         (filter cache-entry-filename (teachpack-cache-tps tp))))
         
      ;; load-teachpack : cache-entry -> void
      ;; updates the cache-entry's filename to #f if the teachpack
      ;; failed to load properly.
      (define (load-teachpack cache-entry)
        (unless (cache-entry-provides cache-entry)
          (let ([filename (cache-entry-filename cache-entry)])
            (with-handlers ([not-break-exn?
                             (lambda (exn) 
                               (set-cache-entry-filename! cache-entry #f)
                               (show-teachpack-error filename exn))])
              (dynamic-require `(file ,filename) #f)
              (let* ([stx (expand (call-with-input-file filename 
                                    (lambda (x) (read-syntax filename x))))]
                     [provided-variables
                      (map translate-variable-provide-output
                           (syntax-property stx 'module-variable-provides))])
                (set-cache-entry-provides! cache-entry provided-variables))))))

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
            
            ;; run this to clear out an errors (hopefully)
            ;; before starting to bang in the user's namespace
            (dynamic-require `(file ,filename) #f)
            
            (for-each (lambda (provided-var)
                        (namespace-variable-binding 
                         provided-var
                         (dynamic-require `(file ,filename) provided-var)))
                      (cache-entry-provides cache-entry)))))

      ;; translate-variable-provide-output : variable-provide-output -> symbol
      (define (translate-variable-provide-output s)
        (cond
          [(symbol? s) s]
          [(and (cons? s) (cons? (cdr s)))
           (cdar s)]
          [(cons? s) (car s)]))
      
      ;; marshall-teachpack-cache : teachpack-cache -> writable
      (define (marshall-teachpack-cache cache)
        (map cache-entry-filename (teachpack-cache-tps cache)))
      
      ;; unmarshall-teachpack-cache : writable -> teachpack-cache
      (define (unmarshall-teachpack-cache lof)
        (make-teachpack-cache
         (if (and (list? lof)
                  (andmap string? lof))
             (map (lambda (x) (make-cache-entry x #f)) lof)
             null)))
      
      ;; teachpack-cache-filenames : teachpack-cache -> (listof string)
      (define (teachpack-cache-filenames cache)
        (map cache-entry-filename (teachpack-cache-tps cache)))
      
      ;; teachpack-cache-applies : teachpack-cache -> (listof boolean)
      (define (teachpack-cache-applies cache)
        (map (lambda (x) #t) (teachpack-cache-tps cache)))
      
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
