(unit/sig framework:autoload^
  (import [preferences : framework:preferences^]
	  [mzlib:file : mzlib:file^])

  (define make-autoload
    (lambda (name file)
      (lambda args
	(if (defined? name)
            (apply (eval name) args)
            (call/ec
	     (lambda (escape)
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
                        (preferences:get 'framework:autoload-paths))
		       (error 'autoload "unable to find file: ~s" file))
		     (try-it file))))))))))

