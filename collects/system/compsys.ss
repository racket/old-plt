(require-library "compile.ss")

(letrec* ([old-handler (current-load)]
	  [offset-string "  "]
	  [error-offset "## "]
	  [non-error-offset "   "]
	  [indent-string ""]
	  [iprintf
	   (lambda (fmt . args)
	     (apply printf
		    (string-append non-error-offset indent-string fmt)
		    args))])
  (current-load (lambda (f)
		  (let* ([file (if (relative-path? f)
				   (build-path (current-directory) f)
				   f)]
			 [link?
			  (lambda (x)
			    (let* ([len (string-length x)]
				   [ans (or (and (> len 4)
						 (string=? (substring x (- len 4) len) "link"))
					    (and (> len 6)
						 (string=? (substring x (- len 6) len) "macros")))])
			      (when ans
				(iprintf "Skip; filename: ~a~n" x))
			      ans))]
			 [mzlib?
			  (let ([mzlib-path (collection-path "mzlib")])
			    (lambda (input)
			      (let*-values ([(base1 fn1 dir1) (split-path input)]
					    [(base2 fn2 dir2) (split-path base1)])
				(and (string=? "mzlib" fn2)
				     (iprintf "Skip; mzlib: ~a~n" input)))))])
		    (iprintf "Loading ~a...~n" file)
		    (let* ([indent
			    (lambda ()
			      (set! indent-string (string-append offset-string indent-string)))]
			   [outdent
			    (lambda ()
			      (set! indent-string
				    (substring indent-string
					       0
					       (max (- (string-length indent-string)
						       (string-length offset-string))
						    0))))]
			   [answer
			    (dynamic-wind
			     indent
			     (lambda () (call-with-values
					 (lambda () (old-handler f))
					 list))
			     outdent)]
			   [len (string-length file)]
			   [basename (substring file 0 (- len 3))]
			   [suffix (substring file (- len 3) len)]
			   [zo (let-values ([(base file dir?)
					     (split-path basename)])
				   (let ([dir (build-path base "compiled")])
				     (unless (or (directory-exists? dir)
						 (not (string=? ".ss" suffix)))
					     (make-directory dir))
				     (build-path dir
						 (string-append file ".zo"))))]
			   [error-handler
			    (lambda (e) 
			      (delete-file zo)
			      ((error-display-handler)
			       (string-append error-offset
					      indent-string
					      (exn-message e)))
			      #f)])
		      (when (and (not (link? basename))
				 (string=? ".ss" suffix)
				 (or (not (file-exists? zo))
				     (<= (file-modify-seconds zo)
					 (file-modify-seconds file)))
				 (not (mzlib? file)))
			(iprintf "Compiling ~a...~n" file)
			(indent)
			(with-handlers ([(lambda (x) #t) error-handler])
			  (compile-file file zo '(preserve-constructions)))
			(outdent)
			(iprintf "Compiled ~a.~n" file))
		      (iprintf "Loaded ~a.~n" file)
		      (apply values answer))))))
