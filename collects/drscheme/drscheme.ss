(require-library "refer.ss")
(define file-stack null)
(define file-ht (make-hash-table))
(define value-ht (make-hash-table))
(define mods-ht (make-hash-table))

(current-load
 (let ([ol (current-load)])
   (lambda (fn)
     (unless (file-exists? fn)
       (error 'load-handler "file ~a does not exist" fn))
     (let ([sym (string->symbol fn)])
       (dynamic-wind
	(lambda ()
	  (hash-table-put! file-ht sym null)
	  (for-each (lambda (stack-fn)
		      (hash-table-put! file-ht stack-fn (cons fn (hash-table-get file-ht stack-fn))))
		    file-stack)
	  (hash-table-put! mods-ht sym (file-or-directory-modify-seconds fn))
	  (set! file-stack (cons sym file-stack)))
	(lambda ()
	  (let ([res (ol fn)])
	    (hash-table-put! value-ht sym res)
	    res))
	(lambda ()
	  (set! file-stack (cdr file-stack))))))))

(define check-require/proc
  (lambda (filename)
    (unless (file-exists? filename)
      (error 'check-require/proc "file does not exist: ~a~n" filename))
    (let* ([sym (string->symbol filename)]
	   [load/save
	    (lambda (filename reason)
	      (printf "loading: ~a because ~a~n" filename reason)
	      (let ([ans (load filename)])
		(hash-table-put! value-ht sym ans)
		ans))]
	   [hash-table-maps?
	    (lambda (ht value)
	      (let/ec k
		(hash-table-get ht value (lambda () (k #f)))
		#t))])
      (if (hash-table-maps? value-ht sym)
	  (let ([secs (hash-table-get mods-ht sym)])
	    (if (ormap (lambda (fn) (< secs (file-or-directory-modify-seconds fn)))
		       (cons filename (hash-table-get file-ht sym)))
		(load/save filename "cached version expired")
		(hash-table-get value-ht sym)))
	  (load/save filename "never before loaded")))))

(define-macro require-relative-library
  (lambda (filename)
    `(let ([require-relative-collection (current-require-relative-collection)])
       (unless require-relative-collection
	 (error 'require-relative-library "no collection~n"))
       ((global-defined-value 'check-require/proc)
	(build-path
	 (apply collection-path require-relative-collection)
	 ,filename)))))

(define-macro require-library
  (lambda (filename . collections)
    `(let ([g (list ,@collections)]
	   [f ,filename])
       (let ([h (if (null? g)
		    (list "mzlib")
		    g)])
	 (parameterize ([current-require-relative-collection h])
	   ((global-defined-value 'check-require/proc)
	    (build-path
	     (apply collection-path h)
	     f)))))))

(define debug? #t)

(define start-drscheme-expression
  '(let-values ([(change-splash-message shutdown-splash close-splash)
		 ((require-library "splash.ss" "framework")
		  (build-path (collection-path "icons") "plt.gif")
		  "DrScheme"
		  123
		  5)])
     (require-relative-library "drsig.ss")
     (let ([unit (require-relative-library "link.ss")])
       (shutdown-splash)
       (invoke-open-unit/sig unit
			     #f
			     (program argv)))))

(if debug?
    (begin
      (thread
       (lambda ()
	 (let* ([f (let loop ([n 10])
		     (cond
		      [(get-top-level-focus-window) => (lambda (x) x)]
		      [(zero? n) (error 'drscheme.ss "didn't find frame after 5 seconds")]
		      [else
		       (sleep/yield 1/2)
		       (loop (- n 1))]))]
		[canvas
		 (let loop ([f f])
		   (cond
		    [(is-a? f editor-canvas%) f]
		    [(is-a? f area-container<%>) (ormap loop (send f get-children))]
		    [else (error 'drscheme.ss "couldn't find editor")]))]
		[text (send canvas get-editor)]
		[port (open-output-string)]
		[event (make-object key-event%)])
	   (write start-drscheme-expression port)
	   (send text insert (get-output-string port))
	   (send event set-key-code #\return)
	   (send text on-char event))))

      (graphical-read-eval-print-loop))
    (eval start-drscheme-expression))

