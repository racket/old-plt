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
      
      ;; type cache-entry = (make-cache-entry string (union #f number) (union #f unit))
      (define-struct cache-entry (filename unit))
      
      ;; new-teachpack-cache : -> teachpack-cache
      (define (new-teachpack-cache) (make-teachpack-cache null 0))

      ;; original-namespace : namespace
      ;; all teachpacks are loaded into this namespace and then copied into
      ;; the user's namespace
      (define original-namespace (current-namespace))
      
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
      
      (define (filename=? f1 f2)
        (string=? (normal-case-path (normalize-path f1))
                  (normal-case-path (normalize-path f2))))
      
      ;; load-teachpacks : teachpack-cache -> void
      ;; =Handler=, =Kernel=
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
      ;; =Handler=, =Kernel=
      ;; loads the file and returns #f if the teachpack doesn't load properly.
      (define (load-teachpack tp-filename timestamp)
	(let/ec escape
	  (let ([teachpack-unit
		 (with-handlers ([not-break-exn?
				  (lambda (x)
				    (message-box 
				     (string-constant teachpack-error-label)
				     (string-append
				      (format (string-constant teachpack-didnt-load)
					      tp-filename)
				      (string #\newline)
                                      (format-error-message
                                       exn
                                       (if (exn? x)
                                           (exn-message x)
                                           (format "uncaught exception: ~s" x)))))
				    (escape #f))])
		   (printf "reloading~n")
                   (reload `(file ,tp-filename) timestamp)
		   (printf "reloaded~n")
                   (dynamic-require `(file ,tp-filename) 'teachpack-unit@))])
	    (make-cache-entry tp-filename teachpack-unit))))

      ;; install-teachpacks : teqachpack-cache -> void
      ;; =Handler=, =User=
      ;; installs the loaded teachpacks
      ;; updates the cache, removing those teachpacks that failed to run
      ;; requires that none of the cache-entries have #fs in them.
      (define (install-teachpacks cache)
        (let ([new-ents (map invoke-teachpack (teachpack-cache-tps cache))])
	  (set-teachpack-cache-tps! cache (filter (lambda (x) x) new-ents))))
      
      ;; invoke-teachpack : cache-entry -> (union cache-entry #f)
      ;; =Handler=, =User=
      (define (invoke-teachpack cache-entry)
        (with-handlers ([not-break-exn?
                         (lambda (x)
                           (parameterize ([current-eventspace
                                           drscheme:init:system-eventspace])
                             (queue-callback
                              (lambda ()
                                (message-box
                                 (string-constant teachpack-error-label)
                                 (string-append
                                  (format (string-constant teachpack-error-invoke)
                                          (cache-entry-filename cache-entry))
                                  (string #\newline)
                                  (if (exn? x)
                                      (exn-message x)
                                      (format "uncaught exception: ~s" x))))))))])
	  (let ([export-signature
		 (exploded->flattened-signature
		  (unit/sig-exports
                   (cache-entry-unit cache-entry)))])
	    (eval `(global-define-value/invoke-unit/sig
		    ,export-signature
		    ,(cache-entry-unit cache-entry))))))
      
      ;; exploded->flattened-signature : exploded-signature -> (listof symbol)
      ;; flattens a signature according to the way mz does it (so we hope).
      (define (exploded->flattened-signature signature)
	(cond
	  [(vector? signature)
	   (apply append (map exploded->flattened-signature (vector->list signature)))]
	  [(pair? signature)
	   (map (lambda (x) (string->symbol
			     (string-append
			      (symbol->string (car signature))
			      ":"
			      x)))
		(cdr signature))]))
      
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
      
      
      ;; format-error-message : exn msg -> string
      ;; prints the error message into a string for displaying in a dialog.
      (define (format-error-message exn msg)
        (let ([p (open-output-string)])
          (parameterize ([current-output-port p]
                         [current-error-port p])
            ((error-display-handler) msg exn))
          (get-output-string p))))))
