(module teachpack mzscheme
  (require (lib "unitsig.ss"))

  (provide teachpack@)

  (define teachpack@
    (unit/sig drscheme:teachpack^
      (import)

      ;; load-teachpacks : -> void
      ;; =Handler=, =Kernel=
      ;; loads the teachpacks from the disk
      ;; initializes teachpack-units, reloading the teachpacks from the disk if they have changed.
      (define (load-teachpacks)

	;; load-teachpack : string[filename] number[timestamp] ->
	;;                  (union #f (list string[filename] number[timestamp] unit))
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
		     (load/cd tp-filename))])
	      (list filename time-stamp teachpack-unit))))

	;; load-teachpack/cache : string[filename] -> void
	;; may load a new teahpack file, if the cached value is out of date.
	;; updates teachpack-units if the file needed to be reloaded.
	;; shows an error message to the user if the teachpack file doesn't exist.
	(define (load-teachpack/cache tp-filename)
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
		  (let ([new-one (load-teachpack tp-filename cache-value)])
		    (if new-one
			(set! teachpack-units (cons new-one (remove cache-value teachpack-units)))
			(set! teachpack-units (remove cache-value teachpack-units)))))))))
	
	(for-each load-teachpack/cache (preferences:get 'drscheme:teachpack-file)))
      
      ;; install-teachpacks : -> void
      ;; =Handler=, =User=
      ;; installs the loaded teachpacks
      ;; expects teachpack-units to be initialized
      (define (install-teachpacks)
	(define (invoke-teachpack  tp-unit)
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
	    (invoke-unit/sig tp-unit #f ...)))
	(for-each invoke-teachpack teachpack-units)))))
