(module file mzscheme
  (provide find-relative-path
	  explode-path
	  normalize-path
	  build-absolute-path
	  build-relative-path
	  filename-extension
	  file-name-from-path
	  path-only
	  delete-directory/files
	  make-directory*
	  make-temporary-file
	  find-library

	  get-preference
	  put-preference

	  call-with-input-file*
	  call-with-output-file*)

  (require "list.ss")

  (define build-relative-path
    (lambda (p . args)
      (if (relative-path? p)
	  (apply build-path p args)
	  (error 'build-relative-path "base path ~s is absolute" p))))

  (define build-absolute-path
    (lambda (p . args)
      (if (relative-path? p)
	  (error 'build-absolute-path "base path ~s is relative" p)
	  (apply build-path p args))))

  ;; Note that normalize-path does not normalize the case
  (define normalize-path
    (letrec ([resolve-all
	      (lambda (path wrt)
		(let ([orig-path (if (and wrt (not (complete-path? path)))
				     (path->complete-path path wrt)
				     path)])
		  (let loop ([full-path orig-path][seen-paths (list orig-path)])
		    (let ([resolved (resolve-path full-path)])
		      (if (string=? resolved full-path)
			  (do-normalize-path resolved #f)
			  (let ([path (if (relative-path? resolved)
					  (build-path
					   (let-values ([(base name dir?) (split-path full-path)])
					     base)
					   resolved)
					  resolved)])
			    (if (member path seen-paths)
				(error 'normalize-path "circular reference at ~s" path)
				(let ([spath
				       ;; Use simplify-path to get rid of ..s, which can
				       ;;  allow the path to grow indefinitely in a cycle.
				       ;; An exception must mean a cycle of links.
				       (with-handlers ([not-break-exn?
							(lambda (x)
							  (error 'normalize-path "circular reference at ~s" path))])
					 (simplify-path path))])
				  (loop spath (cons path seen-paths))))))))))]
	     [resolve
	      (lambda (path)
		(if (string=? path (resolve-path path))
		    path
		    (resolve-all path #f)))]
	     [normalize-path
	      (case-lambda 
	       [(orig-path) (do-normalize-path orig-path (current-directory))]
	       [(orig-path wrt) 
		(unless (complete-path? wrt)
		  (raise-type-error 'normalize-path "complete path" wrt))
		(do-normalize-path orig-path wrt)])]
	     [error-not-a-dir
	      (lambda (path)
		(error 'normalize-path 
		       "~s (within the input path) is not a directory or does not exist"
		       path))]
	     [do-normalize-path
	      (lambda (orig-path wrt)
		(let normalize ([path (expand-path orig-path)])
		  (let-values ([(base name dir?) (split-path path)])
		    (cond
		     [(eq? name 'up)
		      (let up ([base (if (eq? base 'relative)
					 wrt
					 (resolve-all base wrt))])
			(if (directory-exists? base)
			    (let-values ([(prev name dir?) (split-path base)])
			      (cond
			       [(not prev) 
				(error 'normalize-path
				       "root has no parent directory: ~s"
				       orig-path)]
			       [else
				(let ([prev
				       (if (eq? prev 'relative)
					   wrt
					   (normalize prev))])
				  (cond
				   [(eq? name 'same) (up prev)]
				   [(eq? name 'up) (up (up prev))]
				   [else prev]))]))
			    (error-not-a-dir base)))]
		     [(eq? name 'same)
		      (cond
		       [(eq? base 'relative) wrt]
		       [else (let ([n (normalize base)])
			       (if (directory-exists? n)
				   n
				   (error-not-a-dir n)))])]
		     [else
		      (cond
		       [(not base) (path->complete-path path)]
		       [else (let* ([base (if (eq? base 'relative)
					      (normalize wrt)
					      (normalize base))]
				    [path (if (directory-exists? base)
					      (build-path base name)
					      (error-not-a-dir base))]
				    [resolved (expand-path (resolve path))])
			       (cond
				[(relative-path? resolved)
				 (normalize (build-path base resolved))]
				[(complete-path? resolved)
				 resolved]
				[else (path->complete-path resolved base)]))])]))))])
      normalize-path))

					; Argument must be in normal form
  (define explode-path
    (lambda (orig-path)
      (let loop ([path orig-path][rest '()])
	(let-values ([(base name dir?) (split-path path)])
	  (if (or (and base
		       (not (string? base)))
		  (not (string? name)))
	      (error 'explode-path "input was not in normal form: ~s" orig-path))
	  (if base
	      (loop base (cons name rest))
	      (cons name rest))))))

					; Arguments must be in normal form
  (define find-relative-path
    (lambda (directory filename)
      (let ([dir (explode-path directory)]
	    [file (explode-path filename)])
	(if (string=? (normal-case-path (car dir))
		      (normal-case-path (car file)))
	    (let loop ([dir (cdr dir)]
		       [file (cdr file)])
	      (cond
	       [(null? dir) (if (null? file) filename (apply build-path file))]
	       [(null? file) (apply build-path (map (lambda (x) 'up) dir))]
	       [(string=? (normal-case-path (car dir))
			  (normal-case-path (car file)))
		(loop (cdr dir) (cdr file))]
	       [else
		(apply build-path 
		       (append (map (lambda (x) 'up) dir)
			       file))]))
	    filename))))

  (define file-name-from-path
    (lambda (name)
      (let-values ([(base file dir?) (split-path name)])
	(if (and (not dir?) (string? file))
	    file
	    #f))))

  (define path-only
    (lambda (name)
      (let-values ([(base file dir?) (split-path name)])
	(cond
	 [dir? name]
	 [(string? base) base]
	 [else #f]))))

  ;; name can be any string; we just look for a dot
  (define filename-extension
    (lambda (name)
      (let* ([len (string-length name)]
	     [extension
	      (let loop ([p (sub1 len)])
		(cond
		 [(negative? p) #f]
		 [(char=? (string-ref name p) #\.)
		  (substring name (add1 p) len)]
		 [else (loop (sub1 p))]))])
	extension)))

  (define (delete-directory/files path)
    (cond
     [(or (link-exists? path) (file-exists? path))
      (unless (delete-file path)
	(error 'delete-directory/files
	       "error deleting file or link: ~a" path))]
     [(directory-exists? path)
      (for-each (lambda (e) (delete-directory/files (build-path path e)))
		(directory-list path))
      (unless (delete-directory path)
	(error 'delete-directory/files
	       "error deleting a directory: ~a" path))]
     [else (error 'delete-directory/files
		  "encountered ~a, neither a file nor a directory"
		  path)]))

  (define (make-directory* dir)
    (let-values ([(base name dir?) (split-path dir)])
      (when (and (string? base)
		 (not (directory-exists? base)))
	(make-directory* base))
      (make-directory dir)))

  (define make-temporary-file
    (case-lambda
     [(template copy-from)
      (with-handlers ([not-break-exn?
		       (lambda (x)
			 (raise-type-error 'make-temporary-file
					   "format string for 1 argument"
					   template))])
	(format template void))
      (let ([tmpdir (find-system-path 'temp-dir)])
	(let loop ([s (current-seconds)][ms (current-milliseconds)])
	  (let ([name (build-path tmpdir (format template (format "~a~a" s ms)))])
	    (with-handlers ([exn:i/o:filesystem? (lambda (x) 
						   (if (eq? (exn:i/o:filesystem-detail x)
							    'already-exists)
						       ;; try again with a new name
						       (loop (- s (random 10))
							     (+ ms (random 10)))
						       ;; It's something else; give up
						       (raise x)))])
	      (close-output-port (open-output-file name))
	      name))))]
     [(template) (make-temporary-file template #f)]
     [() (make-temporary-file "mztmp~a" #f)]))
  
  (define find-library
    (case-lambda 
     [(name) (find-library name "mzlib")]
     [(name collection . cp)
      (let ([dir (with-handlers ([not-break-exn? (lambda (exn) #f)])
		   (apply collection-path collection cp))])
	(if dir
	    (let ([file (build-path dir name)])
	      (if (file-exists? file)
		  file
		  #f))
	    #f))]))

  (define pref-box (make-weak-box #f)) ; non-weak box => need to save
  (define (get-prefs)
    (let ([f (if (weak-box? pref-box)
		 (weak-box-value pref-box)
		 (unbox pref-box))])
      (or f
	  (let ([f (let ([v (with-handlers ([not-break-exn? (lambda (x) null)])
			      (with-input-from-file (find-system-path 'pref-file) 
				read))])
		     (if (and (list? v) 
			      (andmap (lambda (x) 
					(and (pair? x) 
					     (pair? (cdr x))
					     (null? (cddr x))))
				      v))
			 v
			 null))])
	    (set! pref-box (make-weak-box f))
	    f))))

  (define get-preference
    (lambda (name fail-thunk)
      (unless (symbol? name)
	(raise-type-error
	 'get-preference
	 "symbol"
	 name))
      (unless (and (procedure? fail-thunk)
		   (procedure-arity-includes? fail-thunk 0))
	(raise-type-error
	 'get-preference
	 "procedure (arity 0)"
	 fail-thunk))
      (let ([f (get-prefs)])
	(let ([m (assq name f)])
	  (if m
	      (cadr m)
	      (fail-thunk))))))

  (define put-preference
    (lambda (name val save?)
      (unless (symbol? name)
	(raise-type-error
	 'put-preference
	 "symbol"
	 name))
      (let ([f (get-prefs)])
	(let ([m (assq name f)])
	  (if m
	      (set-car! (cdr m) val)
	      (set! f (cons (list name val) f))))
	(set! pref-box (box f))
	(when save?
	  (with-output-to-file (find-system-path 'pref-file) 
	    (lambda ()
	      (parameterize ([read-case-sensitive #f]
			     [print-struct #f])
		(printf "(~n")
		(for-each (lambda (a) (printf " ~s~n" a)) f)
		(printf ")~n")))
	    'truncate/replace)
	  (set! pref-box (make-weak-box f))))))

  (define call-with-input-file*
    (lambda (file thunk . flags)
      (let ([p (apply open-input-file file flags)])
	(dynamic-wind
	    void
	    (lambda () (thunk p))
	    (lambda () (close-input-port p))))))	  

  (define call-with-output-file*
    (lambda (file thunk . flags)
      (let ([p (apply open-output-file file flags)])
	(dynamic-wind
	    void
	    (lambda () (thunk p))
	    (lambda () (close-output-port p)))))))
