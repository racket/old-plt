
;; Utilities for creating a .plt package

(require-library "deflate.ss")
(require-library "base64.ss" "net")

(define pack
  (case-lambda
   [(dest name paths collections)
    (pack dest name paths collections std-filter #t 'file)]
   [(dest name paths collections filter)
    (pack dest name paths collections filter #t 'file)]
   [(dest name paths collections filter encode?)
    (pack dest name paths collections filter encode? 'file)]
   [(dest name paths collections filter encode? file-mode)
    (pack dest name paths collections filter encode? file-mode #f)]
   [(dest name paths collections filter encode? file-mode unpack-unit)
    (let*-values ([(file) (open-output-file dest 'truncate/replace)]
		  [(fileout thd)
		   (if encode?
		       (let-values ([(b64-out b64-in) (make-pipe 4096)]
				    [(gz-out gz-in) (make-pipe 4096)])
			 (thread
			  (lambda ()
			    (let ([normal (lambda ()
					    (gzip-through-ports gz-out b64-in #f 0)
					    (close-output-port b64-in))]
				  [gzip (find-executable-path "gzip" #f)])
			      (if gzip
				  (let ([p (process* gzip "-c")])
				    (if (eq? 'running ((list-ref p 4) 'status))
					(begin
					  ;; Use gzip process.
					  ;; Errors to error port:
					  (thread 
					   (lambda ()
					     (dynamic-wind
					      void
					      (lambda ()
						(let loop ()
						  (let ([r (read-line (cadddr p))])
						    (unless (eof-object? r)
						      (fprintf (current-error-port)
							       "~a~n" r)
						      (loop)))))
					      (lambda ()
						(close-input-port (cadddr p))))))
					  ;; Copy input to gzip:
					  (thread
					   (lambda ()
					     (let ([s (make-string 4096)])
					       (dynamic-wind
						void
						(lambda ()
						  (let loop ()
						    (let ([n (read-string-avail! s gz-out)])
						      (unless (eof-object? n)
							(display (substring s 0 n) (cadr p))
							(loop)))))
						(lambda ()
						  (close-input-port gz-out)
						  (close-output-port (cadr p)))))))
					  ;; Copy input to b64:
					  (dynamic-wind
					   void
					   (lambda ()
					     (let ([s (make-string 4096)])
					       (let loop ()
						 (let ([n (read-string-avail! s (car p))])
						   (unless (eof-object? n)
						     (display (substring s 0 n) b64-in)
						     (loop))))))
					   (lambda ()
					     (close-input-port (car p))
					     (close-output-port b64-in))))
					(normal)))
				  (normal)))))
			 (values
			  gz-in
			  (thread
			   (lambda ()
			     (base64-encode-stream b64-out file)
			     (close-output-port file)))))
		       (values file (thread void)))])
      (fprintf fileout "PLT~n")
      (write
       `(lambda (request failure)
	  (case request
	    [(name) ,name]
	    [(unpacker) 'mzscheme]))
       fileout)
      (newline fileout)
      (write
       (or unpack-unit
	   `(unit 
	      (import plthome mzuntar)
	      (export)
	      (mzuntar void)
	      (quote ,collections)))
       fileout)
      (newline fileout)
      (for-each
       (lambda (path)
	 (mztar path fileout filter file-mode))
       paths)
      (close-output-port fileout)
      (thread-wait thd))]))

(define (mztar path output filter file-mode)
  (define (path->list p)
    (let-values ([(base name dir?) (split-path p)])
	(if (string? base)
	    (append (path->list base) (list name))
	    (list name))))
  (define-values (init-dir init-files)
    (if (file-exists? path)
	(let-values ([(base name dir?) (split-path path)])
	  (values base (list name)))
	(values path #f)))

  (let loop ([dir init-dir][dpath (path->list init-dir)][files init-files])
    (printf "MzTarring ~a~a...~n" dir
	    (if files (car files) ""))
    (fprintf output "~s~n~s~n" 'dir dpath)
    (for-each
     (lambda (f)
       (let* ([p (build-path dir f)]
	      [filter-val (filter p)])
	 (when filter-val
	   (if (directory-exists? p)
	       (loop p (append dpath (list f)) #f)
	       (let ([len (file-size p)])
		 ; (printf "MzTarring ~a~n" p)
		 (fprintf output "~s~n~s~n~s~n*"
			  (case filter-val
			    [(file) 'file]
			    [(file-replace) 'file-replace]
			    [else file-mode])
			  (append dpath (list f))
			  len)
		 (with-input-from-file p
		   (lambda ()
		     (let ([s (make-string 4096)])
		       (let loop ()
			 (let ([n (read-string-avail! s)])
			   (unless (eof-object? n)
			     (if (= n 4096)
				 (display s output)
				 (display (substring s 0 n) output))
			     (loop))))))))))))
     (or files (directory-list dir)))))

(define (std-filter path)
  (not (or (regexp-match "CVS$" path)
	   (regexp-match "compiled$" path)
	   (regexp-match "~$" path)
	   (regexp-match "^#.*#$" path))))

