(define mred:autoload@
  (unit/s mred:autoload^
    (import [mred:debug mred:debug^]
	    [mred:preferences mred:preferences^]
	    [mzlib:file mzlib:file^])

   (define autoload-paths (mred:preferences:get-preference 'autoload-paths))

   (define make-autoload
     (lambda (name file)
       (lambda args
	 (if (defined? name)
	     (apply (eval name) args)
	     (catch escape
		    (let ([path (mzlib:file:path-only file)]
			  [try-it
			   (lambda (file)
			     (load/cd file)
			     (if (defined? name)
				 (escape (apply (eval name) args))
				 (error 'autoload "still undefined: ~s" name)))])
		      (if (not path)
			  (begin
			    (for-each
			     (lambda (dir)
			       (let ([file (string-append dir file)])
				 (when (file-exists? file)
				       (try-it file))))
			     autoload-paths)
			    (error 'autoload "unable to find file: ~s" file))
			  (try-it file))))))))))
