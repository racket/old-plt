(module refresh-util mzscheme

  (require (lib "launcher.ss" "launcher")
	   (lib "url.ss" "net")
	   (lib "process.ss")
	   (lib "etc.ss"))

  (provide find/create-temporary-docs-dir
	   refresh-docs-dir-base
	   delete-directory/r
	   doc-url-format
	   get-progress-input-port
	   get-progress-output-port
	   set-progress-input-port!
	   set-progress-output-port!
	   progress-semaphore
	   refresh-semaphore
	   reset-progress-semaphore!
	   reset-refresh-semaphore!
	   make-local-doc-filename
	   download-known-doc
	   delete-known-doc
	   run-setup-plt)

  (define refresh-docs-dir-base
	"help-refresh-docs")

  (define refresh-docs-dir-fmt
	(string-append refresh-docs-dir-base "~a"))

  ;; find/create-temporary-docs-dir : -> string
  ;; if cannot find a suitable directory, an exn is raised.
  (define (find/create-temporary-docs-dir)
    (let ([temp-dir (find-system-path 'temp-dir)])
      (let loop ([n 0])
	(if (= n 15)
	    (raise 'no-docs-dir)
	    (let ([candidate (build-path 
			      temp-dir 
			      (format refresh-docs-dir-fmt n))])
	      (if (directory-exists? candidate)
		  (loop (+ n 1))
		  (begin
		    (make-directory candidate)
		    candidate)))))))

  ;; delete-directory/r : string -> void
  ;; deletes the entire subtree underneath this directory
  ;; (including the dir itself)
  (define (delete-directory/r dir)
    (when (directory-exists? dir)
          (let loop ([dir dir])
            (let ([children (directory-list dir)])
              (for-each (lambda (f) 
			  (let ([fullname (build-path dir f)])
			    (when (file-exists? fullname)
				(with-handlers
				 ([void 
				   (lambda _
				     (printf "Warning: file ~a not deleted"
					     fullname))])
				 (delete-file fullname)))))
                        children)
              (for-each (lambda (d) 
			  (when (directory-exists? (build-path dir d))
				(loop (build-path dir d))))
                        children)
              (delete-directory dir)))))

  (define doc-url-format "http://download.plt-scheme.org/doc/pre-release/bundles/~a-doc.plt")
  (define (make-local-doc-filename tmp-dir stub)
    (build-path tmp-dir (format "~a-doc.plt" stub)))
      
  ;; download-known-doc : string string string -> void
  ;; stub is the `drscheme' portion of `drscheme-doc.plt'.
  (define (download-known-doc tmp-dir stub)
    (let ([url (format doc-url-format stub)]
	  [doc-name (make-local-doc-filename tmp-dir stub)])
      (call-with-output-file doc-name
	(lambda (out-port)
	  (call/input-url (string->url url) get-pure-port 
			  (lambda (in-port)
			    (let loop ()
			      (let ([s (read-string 1024 in-port)])
				(unless (eof-object? s)
					(display s out-port)
                                      (loop))))))))
    (void)))

  (define (delete-known-doc tmp-dir doc-name)
    (let ([doc-dir (build-path (collection-path "doc") doc-name)])
      (delete-directory/r doc-dir)))
      
  (define setup-plt (mzscheme-program-launcher-path "Setup PLT"))

  (define (run-setup-plt tmp-dir doc-name)
    (parameterize
     ([current-output-port progress-output-port]
      [current-error-port progress-output-port])
     (system* setup-plt (make-local-doc-filename tmp-dir doc-name))))

  (define progress-input-port #f)
  (define progress-output-port #f)

  (define (set-progress-input-port! p)
    (unless (input-port? p)
	    (error 'set-progress-input-port! 
		   "Argument not an input port"))
    (set! progress-input-port p))

  (define (set-progress-output-port! p)
    (unless (output-port? p)
	    (error 'set-progress-output-port! 
		   "Argument not an output port"))
    (set! progress-output-port p))

  (define progress-semaphore (make-semaphore 0))
  (define refresh-semaphore (make-semaphore 0))

  (define (reset-refresh-semaphore!)
    (set! refresh-semaphore (make-semaphore 0)))

  (define (reset-progress-semaphore!)
    (set! progress-semaphore (make-semaphore 0)))

  (define (get-progress-input-port)
    (or progress-input-port
	(error 'get-progress-input-port "Not initialized")))

  (define (get-progress-output-port)
    (or progress-output-port
	(error 'get-progress-output-port "Not initialized"))))











