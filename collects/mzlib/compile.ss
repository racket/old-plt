
(module compile mzscheme
  (import "file.ss")
  (export compile-file)
  
  (define -re:suffix (regexp "\\..?.?.?$"))

  (define compile-file
    (case-lambda
     [(src)
      (let-values ([(base name dir?) (split-path src)])
	(let ([cdir (build-path
		     (if (symbol? base)
			 'same
			 base)
		     "compiled")])
	  (unless (directory-exists? cdir)
	    (make-directory cdir))
	  (compile-file src (build-path cdir
					(regexp-replace 
					 -re:suffix src
					 ".zo")))))]
     [(src dest)
      (with-input-from-file* 
       src
       (lambda ()
	 (with-handlers ([(lambda (x) (not (exn:i/o:filesystem? x)))
			  (lambda (exn)
			    (delete-file dest)
			    (raise exn))])
	   (with-output-to-file*
	    dest
	    (lambda ()
	      (let loop ([r (read-syntax src)])
		(unless (eof-object? r)
		  (write (compile r) dest))
		(loop)))
	    'truncate/replace))))])))

