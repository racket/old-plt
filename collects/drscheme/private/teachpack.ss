(module teachpack mzscheme
  (require (lib "unitsig.ss")
	   (lib "list.ss")
	   (lib "framework.ss" "framework")
	   (lib "mred.ss" "mred")
	   "string-constant.ss"
	   "reload.ss"
           "drsig.ss")

  (provide teachpack@)

  (define teachpack@
    (unit/sig drscheme:teachpack^
      (import)

      ;; type teachpack-cache = (make-teachpack-cache (listof cache-entry))
      (define-struct teachpack-cache (tps))
      
      ;; type cache-entry = (make-cache-entry string number unit)
      (define-struct cache-entry (filename timestamp unit))
      
      ;; new-teachpack-cache : -> teachpack-cache
      (define (new-teachpack-cache) (make-teachpack-cache null))

      ;; original-namespace : namespace
      ;; all teachpacks are loaded into this namespace and then copied into
      ;; the user's namespace
      (define original-namespace (current-namespace))
      
      ;; load-teachpacks : teachpack-cache -> void
      ;; =Handler=, =Kernel=
      ;; loads the teachpacks from the disk
      ;; initializes teachpack-units, reloading the teachpacks from the disk if they have changed.
      (define (load-teachpacks cache)
	(let ([new-tps (map load-teachpack (teachpack-cache-tps cache))])
	  (set-teachpack-cache-tps! cache (filter (lambda (x) x) new-tps))))
      
      ;; load-teachpack : string[filename] number[timestamp] -> (union #f cache-entry)
      ;; =Handler=, =Kernel=
      ;; loads the file and returns #f if the teachpack doesn't load properly.
      (define (load-teachpack tp-filename time-stamp)
	(let/ec escape
	  (let ([teachpack-unit
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
		   (reload `(file ,tp-filename))
                   (dynamic-require `(file ,tp-filename) 'teachpack-unit@))])
	    (make-cache-entry filename time-stamp teachpack-unit))))

      ;; install-teachpacks : teqachpack-cache -> void
      ;; =Handler=, =User=
      ;; installs the loaded teachpacks
      ;; updates the cache, removing those teachpacks that failed to run
      (define (install-teachpacks cache)
        (let ([new-ents (map invoke-teachpack (teachpack-cache-tps cache))])
	  (set-teachpack-cahe-tps! cache (filter (lambda (x) x) new-ents))))
      
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
                                          tp-filename)
                                  (string #\newline)
                                  (if (exn? x)
                                      (exn-message x)
                                      (format "uncaught exception: ~s" x))))))))])
	  (let ([export-signature
		 (exploded->flattened-signature
		  (unit-with-signature-exports (cache-entry-unit cache-entry)))])
	    (eval `(global-define-value/invoke-unit/sig
		    ,export-signature
		    ,unit)))))
      
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
		(cdr signature))])))))
