(letrec* ([old-handler (current-load)]
	  [offset-string "  "]
	  [indent-string ""])
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
				(printf "~afilname; skip: ~a~n" indent-string x))
			      ans))]
			 [explode-path
			  (lambda (filename)
			    (let loop ([filename filename])
			      (let-values ([(base ele _) (split-path filename)])
				(if (string? base)
				    (cons ele (loop base))
				    (list ele)))))]
			 [mzlib?
			  (lambda (x)
			    (let ([explode (explode-path x)])
			      (and (>= (length explode) 4)
				   (string=? (cadr explode) "standard")
				   (string=? (caddr explode) "collects")
				   (string=? (cadddr explode) "mzscheme")
				   (printf "~amzlib; skip: ~a~n" indent-string x))))])
		    (printf "~aLoading ~a...~n" indent-string file)
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
			   [zo (string-append basename ".zo")]
			   [error-handler
			    (lambda (e) 
			      (delete-file zo)
			      ((error-display-handler)
			       (string-append indent-string
					      (exn-message e)))
			      #f)])
		      (when (and (not (link? basename))
				 (string=? ".ss" suffix)
				 (or (not (file-exists? zo))
				     (<= (file-modify-seconds zo)
					 (file-modify-seconds file)))
				 (not (mzlib? file)))
			(printf "~aCompiling ~a...~n" indent-string file)
			(indent)
			(with-handlers ((void error-handler))
				       (compile-file file zo '(preserve-constructions))
				       #t)
			(outdent)
			(printf "~aCompiled ~a.~n" indent-string file))
		      (printf "~aLoaded ~a.~n" indent-string file)
		      (apply values answer))))))
