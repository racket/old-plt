
(unit/sig (make-embedding-executable)
  (import)

  ;; Find executable via (find-system-path 'exec-file), then
  ;;  fixup name to be MrEd or MzScheme
  (define (find-exe mred?)
    (let* ([sp (find-system-path 'exec-file)]
	   [exe (find-executable-path sp #f)]
	   [fail
	    (lambda ()
	      (error 'make-embedding-executable
		     "can't find ~a executable"
		     (if mred? "MrEd" "MzScheme")))])
      (unless exe (fail))
      (let-values ([(base name dir?) (split-path exe)])
	(let* ([mr (regexp-match
		    "^(.*)([Mm][Rr][Ee][Dd])(.*)$"
		    name)]
	       [mz (regexp-match
		    "^(.*)([Mm][Zz][Ss][Cc][Hh][Ee][Mm][Ee])(.*)$"
		    name)]
	       [r (or mr mz)])
	  (unless r (fail))
	  (let ([exe
		 (build-path base
			     (string-append (cadr r)
					    (if mred?
						"mred"
						"mzscheme")
					    (cadddr r)))])
	    (unless (file-exists? exe)
	      (fail))
	    exe)))))

  (define re:compiled "^[Cc][Oo][Mm][Pp][Ii][Ll][Ee][Dd]$")
  (define re:cvs "^[Cc][Vv][Ss]$")

  ;; Find all files to embed for the collection,
  ;;  and add a wrapper around each to provide it
  ;;  as a library.
  (define (find-collect-files collects verbose?)
    (define plain-namespace (make-namespace))
    (define (make-provide-expr file collect str)
      (parameterize ([current-namespace plain-namespace])
	(compile
	 `(#%provide-library (#%lambda ()
			      (#%let ([p (#%open-input-string ,str)])
			       (#%let loop ([v (#%list (#%void))])
			         (#%let ([e (#%parameterize ([#%read-accept-compiled #t])
					      (#%read p))])
				  (#%if (#%eof-object? e)
				        (#%apply #%values v)
				        (loop (#%call-with-values 
					      (#%lambda () (#%eval e))
					      #%list)))))))
			     ,file ,@collect))))
  
    (apply
     append
     (map (lambda (collect)
	    (when verbose?
	      (fprintf (current-error-port)
		       "Reading collection ~s~n" collect))
	    (let collect-loop ([collect collect])
	      (let ([dir (apply collection-path collect)])
		(let loop ([files (directory-list dir)])
		  (if (null? files)
		      null
		      (let ([full (build-path dir (car files))])
			;; Is a file?
			(cond
			 [(or (regexp-match re:compiled (car files))
			      (regexp-match re:cvs (car files)))
			  ;; Skip it
			  (loop (cdr files))]
			 [(file-exists? full)
			  ;; Has a .zo form?
			  (let ([src (let* ([zo (build-path dir "compiled"
							    (regexp-replace (#%regexp "\\..?.?.?$") 
									    (car files)
									    ".zo"))])
				       (if (file-exists? zo)
					   zo
					   full))])
			    (when verbose?
			      (fprintf (current-error-port) "  source: ~s~n" src))
			    (cons
			     (open-input-string (format "~s" (make-provide-expr
							      (car files) collect
							      (with-input-from-file src
								(lambda ()
								  (read-string (file-size src)))))))
			     (loop (cdr files))))]
			 [(directory-exists? full)
			  (append (collect-loop (append collect (list (car files))))
				  (loop (cdr files)))]
			 [else
			  ;; Broken soft link? Skip it
			  (loop (cdr files))])))))))
	  collects)))
  
  (define (add-files files dest)
    (let ([out (open-output-file dest 'append)]
	  [s (make-string 1024)])
	(let ([count
	       (let floop ([c 0][files files])
		 (if (null? files)
		     c
		     (let ([in (cond
				[(string? (car files)) (open-input-file (car files))]
				[else (car files)])])
		       (let loop ([c c])
			 (let ([n (read-string-avail! s in)])
			   (if (eof-object? n)
			       (begin
				 (close-input-port in)
				 (floop c (cdr files)))
			       (begin
				 (display (if (= n (string-length s)) s (substring s 0 n))
					  out)
				 (loop (+ c n)))))))))])
	  (close-output-port out)
	  count)))
  
  ;; Find the magic point in the binary:
  (define (find-cmdline)
    (define magic (string->list "[Replace me for EXE hack"))
    (let loop ([pos 0][l magic])
      (cond
       [(null? l) (- pos (length magic))]
       [else (let ([c (read-char)])
	       (when (eof-object? c)
		 (error 
		  'make-embedding-executable
		  (format
		   "can't find cmdline position in executable")))
	       (if (eq? c (car l))
		   (loop (add1 pos) (cdr l))
		   (loop (add1 pos) magic)))])))


  ;; The main function (see doc.txt):
  (define (make-embedding-executable dest mred? verbose? files collects cmdline)
    (unless (< (apply + (length cmdline) (map string-length cmdline)) 50)
      (error 'make-embedding-executable "command line too long"))
    (for-each (lambda (f)
		(unless (file-exists? f)
		  (error 'make-embedding-executable "can't find file: ~s" f)))
	      files)
    (let ([exe (find-exe mred?)]
	  [collects (find-collect-files collects verbose?)])
      (when verbose?
	(fprintf (current-error-port) "Copying to ~s~n" dest))
      (when (file-exists? dest)
	(delete-file dest))
      (copy-file exe dest)
      (with-handlers ([void
		       (lambda (x)
			 (when (file-exists? dest)
			   (delete-file dest))
			 (raise x))])
	(let ([start (file-size dest)]
	      [count (add-files (append collects
					(if (pair? files)
					    files
					    (list files)))
				dest)]
	      [cmdpos (with-input-from-file dest find-cmdline)])
	  (when verbose?
	    (fprintf (current-error-port) "Setting command line~n"))
	  (let ([out (open-output-file dest 'update)]
		[start-s (number->string start)]
		[end-s (number->string (+ start count))])
	    (file-position out cmdpos)
	    (display "!" out)
	    (for-each
	     (lambda (s)
	       (fprintf out "~c~a~c"
			(integer->char (add1 (string-length s))) s #\000))
	     (list* "-k" start-s end-s cmdline))
	    (display #\000 out)
	    (close-output-port out)))))))


      
      
    