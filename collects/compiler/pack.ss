
;; Utilities for creating a .plt package, relies on gzip and mmencode

(define pack
  (case-lambda
   [(dest name paths collections)
    (pack dest name paths collections std-filter #t)]
   [(dest name paths collections filter)
    (pack dest name paths collections filter #t)]
   [(dest name paths collections filter encode?)
    (let* ([p (if encode?
		  (process (format "gzip -c | mmencode > ~s" dest))
		  #f)]
	   [stdin (if p 
		      (cadr p)
		      (open-output-file dest 'truncate/replace))]
	   [echo (lambda (p)
		   (thread
		    (lambda ()
		      (let loop ()
			(let ([l (read-line p)])
			  (unless (eof-object? l)
			    (printf "~a~n" l)
			    (loop)))))))]
	   [t1 (and p (echo (car p)))]
	   [t2 (and p (echo (list-ref p 3)))])
      (fprintf stdin "PLT~n")
      (write
       `(lambda (request failure)
	  (case request
	    [(name) ,name]
	    [(unpacker) 'mzscheme]))
       stdin)
      (newline stdin)
      (write
       `(unit 
	 (import plthome mzuntar)
	 (export)
	 (mzuntar void)
	 (quote ,collections))
       stdin)
      (newline stdin)
      (for-each
       (lambda (path)
	 (mztar path stdin filter))
       paths)
      (close-output-port stdin)
      (when p
	(thread-wait t1)
	(thread-wait t2)))]))

(define (mztar path output filter)
  (define (path->list p)
    (let-values ([(base name dir?) (split-path p)])
	(if (string? base)
	    (append (path->list base) (list name))
	    (list name))))
  (let loop ([dir path][dpath (path->list path)])
    (printf "MzTarring ~a...~n" dir)
    (fprintf output "~s~n~s~n" 'dir dpath)
    (for-each
     (lambda (f)
       (let ([p (build-path dir f)])
	 (when (filter p)
	       (if (directory-exists? p)
		   (loop p (append dpath (list f)))
		   (let ([len (file-size p)])
		     ; (printf "MzTarring ~a~n" p)
		     (fprintf output "~s~n~s~n~s~n*"
			      'file
			      (append dpath (list f))
			      len)
		     (with-input-from-file p
		       (lambda ()
			 (let loop ()
			   (let ([c (read-char)])
			     (unless (eof-object? c)
				(write-char c output)
				(loop)))))))))))
     (directory-list dir))))

(define (std-filter path)
  (not (or (regexp-match "CVS$" path)
	   (regexp-match "compiled$" path))))

