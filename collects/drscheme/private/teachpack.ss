(module teachpack mzscheme
  (require (lib "unitsig.ss")
           "drsig.ss")

  (provide teachpack@)

  (define teachpack@
    (unit/sig drscheme:teachpack^
      (import)

      ;; type struct-teachpack-cache = (make-teachpack-cache (listof cache-entry))
      (define-struct teachpack-cache (tps))
      
      ;; type cache-entry = (make-cache-entry string number symbol)
      (define-struct cache-entry (filename timestamp module-name))
      
      ;; new-teachpack-cache : -> teachpack-cache
      (define (new-teachpack-cache) (make-teachpack-cache null))

      ;; original-namespace : namespace
      ;; all teachpacks are loaded into this namespace and then copied into
      ;; the user's namespace
      (define original-namespace (current-namespace))
      
      ;; load-teachpacks : -> void
      ;; =Handler=, =Kernel=
      ;; loads the teachpacks from the disk
      ;; initializes teachpack-units, reloading the teachpacks from the disk if they have changed.
      (define (load-teachpacks cache)
	(for-each load-teachpack/cache cache))
      
      ;; load-teachpack : string[filename] number[timestamp] -> (union #f cache-entry)
      ;; =Handler=, =Kernel=
      ;; loads the file and returns #f if the teachpack doesn't load properly.
      (define (load-teachpack tp-filename time-stamp)
	(let/ec escape
	  (let ([teachpack-module-name
		 (with-handlers ([not-break-exn?
				  (lambda (x)
				    (preferences:set 
				     'drscheme:teachpack-file
				     (remove
				      tp-filename
				      (preferences:get 'drscheme:teachpack-file)))
				    (message-box 
				     (string-constant teachpack-error-label)
				     (string-append
				      (format (string-constant teachpack-didnt-load)
					      tp-filename)
				      (string #\newline)
				      (if (exn? x)
					  (exn-message x)
					  (format "uncaught exception: ~s" x))))
				    (escape #f))])
                   ((current-module-name-resolver `(file ,tp-filename) #f #f)))])
	    (make-cache-entry filename time-stamp teachpack-unit))))

      ;; load-teachpack/cache : teachpack-cache string[filename] -> void
      ;; =Handler=, =Kernel=
      ;; may load a new teahpack file, if the cached value is out of date.
      ;; updates teachpack-units if the file needed to be reloaded.
      ;; shows an error message to the user if the teachpack file doesn't exist.
      (define (load-teachpack/cache teachpack-cache tp-filename)
	(let/ec escape
	  (let ([file-on-disk-stamp
		 (with-handlers ([not-break-exn?
				  (lambda (x)
				    (preferences:set 
				     'drscheme:teachpack-file
				     (remove
				      tp-filename
				      (preferences:get 'drscheme:teachpack-file)))
				    (message-box 
				     (string-constant teachpack-error-label)
				     (format (string-constant teachpack-dne/cant-read) tp-filename))
				    (escape (void)))])
		   (file-modify-seconds tp-filename))])
	    (let ([cache-value (assoc tp-filename teachpack-units)])
	      (when (and cache-value
			 (file-on-disk-stamp . > . (second cache-value)))
		(let ([new-one (load-teachpack tp-filename cache-value)]
                      [removed (remove cache-value (teachpack-cache-tps teachpack-cache))])
                  (set-teachpack-cache-tps!
                   teachpack-cache
                   (if new-one
                       (cons new-one removed)
                       removed))))))))

      ;; install-teachpacks : teqachpack-cache -> void
      ;; =Handler=, =User=
      ;; installs the loaded teachpacks
      ;; expects teachpack-units to be initialized
      (define (install-teachpacks cache)
        (for-each install-teachpack (teachpack-cache-tps cache)))
      
      ;; install-teachpack : cache-entry -> void
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
                                          tp-filename)
                                  (string #\newline)
                                  (if (exn? x)
                                      (exn-message x)
                                      (format "uncaught exception: ~s" x))))))))])
          (namespace-attach-module original-namespace (cache-entry-module-name cache-entry))))
      
      
      )))
