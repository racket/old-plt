(require-library "errortrace.ss" "errortrace")
(error-print-width 80)

(require-library "refer.ss")
(define file-stack null)
(define file-ht (make-hash-table))
(define value-ht (make-hash-table))
(define mods-ht (make-hash-table))

(define loading-frame (make-object frame% "Loading message"))
(define loading-message (make-object message% 
			  (apply string-append
				 (let loop ([n (if (eq? (system-type) 'macos) 6 4)])
				   (cond
				     [(zero? n) null]
				     [else (cons "abcdefghijklmnopqrstuvwxyz" (loop (- n 1)))])))
			  loading-frame))
(send loading-frame set-alignment 'left 'center)
(send loading-frame show #t)

(current-load
 (let ([ol (current-load)])
   (lambda (fn)
     (unless (file-exists? fn)
       (error 'load-handler "file ~a does not exist" fn))
     (let ([sym (string->symbol fn)])
       (dynamic-wind
	(lambda ()
	  (for-each (lambda (stack-fn)
		      (let ([old (hash-table-get file-ht stack-fn (lambda () null))])
			(unless (member fn old)
			  (hash-table-put! file-ht stack-fn (cons fn old)))))
		    file-stack)
	  (hash-table-put! mods-ht sym (file-or-directory-modify-seconds fn))
	  (set! file-stack (cons sym file-stack)))
	(lambda () (ol fn))
	(lambda ()
	  (set! file-stack (cdr file-stack))))))))

(define check-require/proc
  (let ([loading-message loading-message])
    (lambda (filename)
      (unless (file-exists? filename)
	(error 'check-require/proc "file does not exist: ~a~n" filename))
      
      (let* ([sym (string->symbol filename)]
	     [load/save
	      (lambda (filename reason)
		(send loading-message set-label (format "loading: ~a because ~a" filename reason))
		(let ([anss (call-with-values (lambda () (load filename)) list)])
		  (hash-table-put! value-ht sym anss)
		  (apply values anss)))]
	     [hash-table-maps?
	      (lambda (ht value)
		(let/ec k
		  (hash-table-get ht value (lambda () (k #f)))
		  #t))])
	(begin0
	  (if (hash-table-maps? value-ht sym)
	      (let* ([secs (hash-table-get mods-ht sym)]
		     [reason (ormap (lambda (fn)
				      (if (< (hash-table-get mods-ht (string->symbol fn))
					     (file-or-directory-modify-seconds fn))
					  fn
					  #f))
				    (cons filename (hash-table-get file-ht sym (lambda () null))))])
		(if reason
		    (load/save filename (format "~a was modified" reason))
		    (apply values (hash-table-get value-ht sym))))
	      (load/save filename "never before loaded")))))))

(define-macro require-relative-library
  (lambda (filename)
    `(let ([require-relative-collection (current-require-relative-collection)])
       (unless require-relative-collection
	 (error 'require-relative-library "no collection~n"))
       (,check-require/proc
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
	   (,check-require/proc
	    (build-path
	     (apply collection-path h)
	     f)))))))

(define graphical-debug? #t)

(define drscheme-custodian #f)
(define drscheme-eventspace #f)

(load-relative "start-drs.ss")

(define (T)
  (when drscheme-custodian (custodian-shutdown-all drscheme-custodian))
  (set! drscheme-custodian (make-custodian))
  (parameterize ([current-custodian drscheme-custodian])
    (set! drscheme-eventspace (make-eventspace))
    (parameterize ([current-eventspace drscheme-eventspace])
      (start-drscheme)))
  (send loading-message set-label ""))

(define start-drscheme-expression '(T))

(cond
  [graphical-debug?
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
	       [send-sexp
		(lambda (sexp)
		  (let ([port (open-output-string)]
			[event (make-object key-event%)])
		    (write sexp port)
		    (send text insert (get-output-string port))
		    (send event set-key-code #\return)
		    (send text on-char event)
		    (sleep 1/2)))])
	  (send-sexp start-drscheme-expression))))

     (graphical-read-eval-print-loop))]
  [else
   (require-library "rep.ss" "readline")
   (read-eval-print-loop)])