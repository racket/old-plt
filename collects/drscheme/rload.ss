(current-load
 (local [(define ol (current-load))
	 (define file-stack null)

	 (define (make-get-ht sym)
	   (lambda ()
	     (unless (defined? sym)
	       (global-defined-value sym (make-hash-table)))
	     (global-defined-value sym)))

	 (define get-file-ht (make-get-ht (gensym "file-ht")))
	 (define get-value-ht (make-get-ht (gensym "value-ht")))
	 (define get-mods-ht (make-get-ht (gensym "mods-ht")))
	 
	 (define-values (display-width display-height) (get-display-size))
	 (define loading-frame (make-object frame% "Loading message" #f (- display-width 80) #f))
	 (define loading-messages null)
	 
	 (define exception-raised? #f)

	 (define (check-cache/force filename)
	   (let* ([sym (string->symbol filename)]
		  [normal-termination? #f]
		  [load/save
		   (lambda (filename reason)
		     
		     (unless (<= (length file-stack)
				 (length loading-messages))
		       (let ([new-msg (make-object message% "" loading-frame)])
			 (printf "width: ~a height: ~a~n" 
				 (send new-msg min-width)
				 (send new-msg min-height))
			 (send new-msg stretchable-width #t)
			 (yield) (sleep) (yield) (sleep) (yield) (sleep)
			 (yield) (sleep) (yield) (sleep) (yield) (sleep)
			 (yield) (sleep) (yield) (sleep) (yield) (sleep)
			 (yield) (sleep) (yield) (sleep) (yield) (sleep)
			 (yield) (sleep) (yield) (sleep) (yield) (sleep)
			 (set! loading-messages
			       (append loading-messages (list new-msg)))))

		     (hash-table-put! (get-mods-ht) (string->symbol filename) (file-or-directory-modify-seconds filename))

		     (dynamic-wind
		      (lambda () 
			(set! normal-termination? #f))
		      (lambda ()
		       (let* ([index #f]
			      [old-message #f])

			 (when exception-raised?
			   (set! exception-raised? #f)
			   (for-each (lambda (x) (send x set-label "")) loading-messages))
			 
			 (set! index (- (length file-stack) 1))
			 (set! old-message (send (list-ref loading-messages index) get-label))
			 (send (list-ref loading-messages index) set-label
			       (format "loading: ~a because ~a" filename reason))
			 (printf "~a: ~s~n" index (format "loading: ~a because ~a" filename reason))
			 (yield) (sleep) (yield) (sleep) (yield) (sleep)
			 (yield) (sleep) (yield) (sleep) (yield) (sleep)
			 (yield) (sleep) (yield) (sleep) (yield) (sleep)
			 (yield) (sleep) (yield) (sleep) (yield) (sleep)
			 (yield) (sleep) (yield) (sleep) (yield) (sleep)
			 
			 (let ([anss (call-with-values (lambda () (ol filename)) list)])
			   (hash-table-put! (get-value-ht) sym anss)
			   (send (list-ref loading-messages index) set-label old-message)
			   (set! normal-termination? #t)
			   (apply values anss))))
		      (lambda ()
			(unless normal-termination?
			  (set! exception-raised? #t)))))]

		  [hash-table-maps?
		   (lambda (ht value)
		     (let/ec k
		       (hash-table-get ht value (lambda () (k #f)))
		       #t))])
	     (begin0
	      (if (hash-table-maps? (get-value-ht) sym)
		  (let* ([secs (hash-table-get (get-mods-ht) sym (lambda () (error 'rload "mods-ht doesn't map: ~a" sym)))]
			 [reason (ormap (lambda (fn)
					  (if (< (hash-table-get
						  (get-mods-ht)
						  (string->symbol fn)
						  (lambda ()
						    (error 'rload "mods-ht doesn't map ~a (started with ~a)"
							   fn sym)))
						 (file-or-directory-modify-seconds fn))
					      fn
					      #f))
					(cons filename (hash-table-get (get-file-ht) sym (lambda () null))))])
		    (if reason
			(load/save filename (format "~a was modified" reason))
			(apply values (hash-table-get (get-value-ht) sym))))
		  (load/save filename "never before loaded")))))]

   (global-defined-value 'f loading-frame)

   (send loading-frame set-alignment 'left 'center)
   (send loading-frame show #t)

   (lambda (fn)
     (unless (file-exists? fn)
       (error 'load-handler "file ~a does not exist" fn))
     (let ([sym (string->symbol fn)])
       (dynamic-wind
	(lambda ()
	  (for-each (lambda (stack-fn)
		      (let ([old (hash-table-get (get-file-ht) stack-fn (lambda () null))])
			(unless (member fn old)
			  (hash-table-put! (get-file-ht) stack-fn (cons fn old)))))
		    file-stack)
	  (set! file-stack (cons sym file-stack)))
	(lambda () (check-cache/force fn))
	(lambda ()
	  (set! file-stack (cdr file-stack))))))))

(define-macro require-relative-library
  (lambda (filename)
    `(let ([require-relative-collection (current-require-relative-collection)])
       (unless require-relative-collection
	 (error 'require-relative-library "no collection~n"))
       (#%load
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
	   (#%load
	    (build-path
	     (apply collection-path h)
	     f)))))))
