
(module getinfo mzscheme

  (require (lib "file.ss"))
  (require (lib "match.ss"))

  (provide get-info)

  (define (get-info coll-path)
    (let* ([coll-path (map (lambda (x) (if (path? x) (path->string x) x)) coll-path)]
	   [dir (apply collection-path coll-path)]
	   [file (build-path dir "info.ss")])
      (if (file-exists? file)
	  (begin
	    (with-input-from-file file
	      (lambda ()
		(let ([r (read)])
		  (unless (eof-object? (read))
		    (error "info.ss file has multiple expressions in ~a" dir))
		  (match r
		    [('module 'info '(lib "infotab.ss" "setup")
		       expr ...)
		     'ok]
		    [else (error 
			   'get-info
			   "info file does not contain a module of the right shape: \"~a\""
			   file)]))))
	    (dynamic-require `(lib "info.ss" ,@coll-path) '#%info-lookup))
	  #f))))


