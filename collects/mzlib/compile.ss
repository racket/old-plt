
(module compile mzscheme
  (require "file.ss")
  (provide compile-file)
  
  (define -re:suffix (regexp "\\...?.?$"))

  ;; (require (lib "src2src.ss" "compiler"))

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
					 -re:suffix name
					 ".zo")))))]
     [(src dest)
      (with-input-from-file* 
       src
       (lambda ()
	 (port-count-lines! (current-input-port))
	 (with-handlers ([not-break-exn?
			  (lambda (exn)
			    (with-handlers ([void void])
			      (delete-file dest))
			    (raise exn))])
	   (with-output-to-file*
	    dest
	    (lambda ()
	      (let loop ()
		(let ([r (read-syntax src)])
		  (unless (eof-object? r)
		    (write (compile r))
		    (loop)))))
	    'truncate/replace))))])))

