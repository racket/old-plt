
  (unit/sig mred:autoload^
    (import [wx : wx^]
      [mred:constants : mred:constants^]
      [mred:preferences : mred:preferences^]
      [mzlib:file : mzlib:file^])

    (mred:debug:printf 'invoke "mred:autoload@")
	    
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
                        (mred:preferences:get-preference 'mred:autoload-paths))
                      (error 'autoload "unable to find file: ~s" file))
                    (try-it file))))))))))

