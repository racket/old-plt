
;; Utilities for creating a .plt package, relies on gzip and mmencode

(define (pack dest name dirs collections encode?)
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
    (write
     `(lambda (request failure)
	(case request
	  [(name) "MrSpidey"]
	  [(unpacker) 'mzscheme]))
     stdin)
    (write
     `(unit 
       (import plthome mzuntar)
       (export)
       (mzuntar void)
       (quote ,collections))
     stdin)
    (for-each
     (lambda (dir)
       (mztar dir stdin std-filter))
     dirs)
    (close-output-port stdin)
    (when p
      (thread-wait t1)
      (thread-wait t2))))

(define (mztar dir output filter)
  (let loop ([dir dir])
    (printf "MzTarring ~a...~n" dir)
    (fprintf output "~s~n~s~n" 'dir dir)
    (for-each
     (lambda (f)
       (let ([p (build-path dir f)])
	 (when (filter p)
	       (if (directory-exists? p)
		   (loop p)
		   (let ([len (file-size p)])
		     ; (printf "MzTarring ~a~n" p)
		     (fprintf output "~s~n~s~n~s~n*"
			      'file
			      p
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

