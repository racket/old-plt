(module mkindex mzscheme

  (provide create-index-file)

  (require (lib "pregexp.ss")
	   (lib "file.ss")
	   (lib "list.ss"))

  (require "servlets/private/util.ss")

  (define servlet-dir (normalize-path 
		       (build-path (collection-path "help") "servlets")))
  (define exploded-servlet-dir-len (length (explode-path servlet-dir)))
  (define dest-dir (build-path (collection-path "help") 'up "doc" "help"))

  (define old-curr-dir (current-directory))
  (unless (directory-exists? dest-dir)
	  (make-directory* dest-dir))
  (current-directory dest-dir)

  (define index-file "hdindex")

  (define anchor-regexp ; (A ((NAME "foo") (VALUE "bar")))
    (pregexp "\\(A\\s+\\(\\(NAME\\s+\".*\"\\)\\s+\\(VALUE\\s+\".*\"\\)\\)\\)"))
  (define (make-attr-regexp attr)
    (pregexp (string-append "\\(" attr "\\s+\"[^\"]*\"\\)")))
  (define name-regexp 
    (make-attr-regexp "NAME"))
  (define value-regexp 
    (make-attr-regexp "VALUE"))
  (define quoted-regexp
    (pregexp "\".*\""))
  (define title-regexp
    (pregexp "\\(TITLE .*\\)"))

  (define (get-servlet-files dir)
    (let* ([all-files 
	    (map (lambda (f) (build-path dir f))
		 (directory-list dir))]
	   [servlet-files 
	    (filter 
	     (lambda (s)
	       (let ([len (string-length s)])
		 (string=? 
		  ".ss"
		  (substring s (- len 3) len))))
	     all-files)]
	   [dirs 
	    (filter directory-exists? all-files)])
      (apply append servlet-files
	     (map get-servlet-files dirs))))

  (define sys-type (system-type))

  ; path is absolute, and has the servlet dir as a prefix 
  (define (relativize-and-slashify path)
    (let* ([exp-path (explode-path path)]
	   [prefix-len (sub1 exploded-servlet-dir-len)]
	   [relative-exp-path
	    (let loop ([p exp-path]
		       [n 0])
	      ; leave off prefix up to servlet dir
	      (if (>= n prefix-len)
		  p
		  (loop (cdr p) (add1 n))))])
      (fold-into-web-path relative-exp-path)))

  (define index '())

  (define (add-index-entry! val file name title)
    (set! index 
	  (cons
	   (list val
		 (format "\"/~a\"" 
			 (relativize-and-slashify file))
		 name 
		 title)
	   index)))
  
  (define (gen-index dir)
    (let* ([all-files (directory-list)]
	   [servlet-files (get-servlet-files dir)])
      (for-each
       (lambda (file)
	 (let ([port (open-input-file file 'text)]
	       [title-value file]
	       [real-title #f])
	   (let loop ()
	     (let ([ln (read-line port)])
	       (if (eof-object? ln) 
		   (close-input-port port)
		   (begin
		     (when (not real-title)
			   (let ([title (pregexp-match title-regexp ln)])
			     (when title
				   (set! title-value 
					 (format "~a" 
						 (car 
						  (pregexp-match 
						   quoted-regexp 
						   (car title)))))
				   (set! real-title title-value))))
		     (let ([anchors (pregexp-match anchor-regexp ln)])
		       (when anchors
			   (for-each 
			    (lambda (a)
			      (let* ([name 
				      (car (pregexp-match name-regexp a))]
				     [name-value				
				      (format "~a"
					      (car 
					       (pregexp-match 
						quoted-regexp name)))]
				     [value 
				      (car
				       (pregexp-match value-regexp a))]
				     [value-value
				      (format "~a" (car (pregexp-match quoted-regexp value)))])
				(add-index-entry! value-value file name-value title-value)))
			    anchors)))
		     (loop)))))))
       servlet-files)))

  (define (create-index-file)
    (when (file-exists? index-file)
	  (delete-file index-file))
    (gen-index servlet-dir)
    (let ([output-port (open-output-file (build-path dest-dir index-file))])
      (fprintf output-port "(~n")
      (let loop ([index index])
	(if (null? index)
	    (begin
	      (fprintf output-port ")~n")
	      (close-output-port output-port))
	    (begin 
	      (fprintf output-port "~a~n" (car index))
	      (loop (cdr index))))))))


