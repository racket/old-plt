
  (unit/sig mred:path-utils^
    (import mred:wx^
	    [mred:constants : mred:constants^])
	    
    (mred:debug:printf 'invoke "mred:path-utils@")

    (define generate-autosave-name 
      (lambda (name)
	(let-values ([(base name dir?)
		      (if (null? name)
			  (values (current-directory) "mredauto" #f)
			  (split-path name))])
	  (let* ([base (if (string? base) 
			   base
			   (current-directory))]
		 [path (if (relative-path? base)
			   (build-path (current-directory) base)
			   base)]
		 [without-ext
		  (if (eq? wx:platform 'windows)
		      (list->string
		       (let loop ([list (string->list name)])
			 (if (or (null? list)
				 (char=? (car list) #\.))
			     ()
			     (cons (car list)
				   (loop (cdr list))))))
		      name)])
	    (let loop ([n 1])
	      (let ([new-name
		     (build-path path
				 (if (eq? wx:platform 'windows)
				     (string-append without-ext
						    "."
						    (number->string n))
				     (string-append "#"
						    name
						    "#"
						    (number->string n)
						    "#")))])
		(if (file-exists? new-name)
		    (loop (add1 n))
		    new-name)))))))
    (define generate-backup-name
      (lambda (name)
	(if (eq? wx:platform 'windows)
	    (list->string
	     (let loop ([list (string->list name)])
	       (if (or (null? list)
		       (char=? (car list) #\.))
		   '(#\. #\b #\a #\k)
		   (cons (car list)
			 (loop (cdr list))))))
	    (string-append name "~")))))

