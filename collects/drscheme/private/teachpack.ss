
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
      
      ;; type cache-entry = (make-cache-entry string[filename])
      (define-struct cache-entry (filename))
      
      ;; new-teachpack-cache : -> teachpack-cache
      (define (new-teachpack-cache) (make-teachpack-cache null))
      
      ;; set-teachpack-cache-filenames! : teachpack-cache (listof string) -> void
      ;; this shouldn't remove all of the #fs.
      (define (set-teachpack-cache-filenames! teachpack-cache filenames)
        (set-teachpack-cache-tps!
         teachpack-cache
         (map (lambda (filename) (make-cache-entry filename))
              filenames)))
      
      ;; load-teachpacks : namespace teachpack-cache -> void
      ;; in this iteration of teachpacks, there is no preloading to do.
      (define (load-teachpacks user-namespace tp)
        (void))

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
            (namespace-require `(file ,filename)))))

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
