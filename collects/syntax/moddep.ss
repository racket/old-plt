
(module moddep mzscheme

  (define (check-module-form exp expected-module filename)
    (cond
     [(compiled-module-expression? (syntax-e exp))
      (if (eq? (module-compiled-name (syntax-e exp))
	       (string->symbol
		(format "~a~a"
			(or (current-module-name-prefix) "")
			expected-module)))
	  ;; It's fine:
	  exp
	  ;; Wrong name:
	  (and filename
	       (error 'load-handler
		      "file ~s declares wrong module name: ~a"
		      filename (module-compiled-name (syntax-e exp)))))]
     [(and (syntax? exp)
	   (syntax-case exp ()
	     [(mod nm . _)
	      (and (eq? (syntax-e (syntax mod)) 'module)
		   (eq? (syntax-e (syntax nm)) expected-module))]
	     [_else #f]))
      ;; It's ok; need to install a specific `module' binding:
      (with-syntax ([(mod nm . _) exp])
	(syntax (module nm . _)))]
     [else
      (and filename
	   (error 'load-handler 
		  "file ~s does not contain a module declaration as expected"
		  filename))]))
  
  (define re:suffix (regexp "\\..?.?.?$"))
	  
  (define (resolve s)
    (if (complete-path? s)
	s
	(let ([d (current-load-relative-directory)])
	  (if d (path->complete-path s d) s))))
  
  (define (date>=? a bm)
    (and a
	 (let ([am (with-handlers ([not-break-exn? (lambda (x) #f)])
		     (file-or-directory-modify-seconds a))])
	   (or (and (not bm) am) (and am bm (>= am bm))))))

  (define (read-one path src?)
    (let ([p (open-input-file path)])
      (when src? (port-count-lines! p))
      (dynamic-wind
       void
       (lambda ()
	 (parameterize ([read-accept-compiled
			 (or (read-accept-compiled) (not src?))])
	   (let ([v (read p)])
	     (when (eof-object? v)
	       (error 'read-one "empty file; expected a module declaration in: ~a" path))
	     (when src?
	       (unless (and (pair? v) (eq? 'module (car v)))
		 (error 'read-one "expected a module declaration, found: ~e in: ~a" v path))
	       (let ([name (let-values ([(base name dir?) (split-path path)])
			     (string->symbol (regexp-replace re:suffix name "")))])
		 (unless (and (pair? (cdr v))
			      (eq? (cadr v) name))
		   (error 'read-one "expected a module declaration named ~a, found: ~e in: ~a" 
			  name v path))))
	     (unless (eof-object? (read p))
	       (error 
		'read-one 
		"file has more than one expression; expected a module declaration only in: ~a"
		path))
	     v)))
       (lambda () (close-input-port p)))))
  
  (define (get-module-code path)
    (unless (and (string? path) (or (relative-path? path) (absolute-path? path)))
      (raise-type-error 'get-module-code "pathname string" path))
    (let*-values ([(path) (resolve path)]
		  [(base file dir?) (split-path path)]
		  [(base) (if (eq? base 'relative) 'same base)]
		  [(mode) (use-compiled-file-kinds)]
		  [(comp?) (not (eq? mode 'none))])
      (let* ([get-so (lambda (file)
		       (if comp?
			   (build-path base
				       "compiled"
				       "native"
				       (system-library-subpath)
				       (regexp-replace 
					re:suffix file
					(case (system-type)
					  [(windows) ".dll"]
					  [else ".so"])))
			   #f))]
	     [ok-kind? (lambda (file) (eq? mode 'all))]
	     [zo (and comp?
		      (build-path base
				  "compiled"
				  (regexp-replace re:suffix file ".zo")))]
	     [so (get-so file)]
	     [_loader-so (get-so "_loader.ss")]
	     [path-d (with-handlers ([not-break-exn? (lambda (x) #f)])
		       (file-or-directory-modify-seconds path))]
	     [with-dir (lambda (t) 
			 (parameterize ([current-load-relative-directory 
					 (if (string? base) base (current-directory))])
			   (t)))])
	(cond
	 [(and (date>=? _loader-so path-d)
	       (let ([getter (load-extension _loader-so)])
		 (getter (string->symbol (regexp-replace re:suffix file "")))))
	  => (lambda (loader)
	       (error 'get-module-code "cannot use _loader file: ~e" _loader-so))]
	 [(date>=? so path-d)
	  (with-dir (lambda () (error 'get-module-code "cannot use extension file; ~e" so)))]
	 [(date>=? zo path-d)
	  (read-one zo #f)]
	 [(not path-d)
	  (error 'get-module-code "no such file: ~e" path)]
	 [else (with-dir (lambda () (compile (read-one path #t))))]))))

  (define re:dir (regexp "(.+?)/+(.*)"))

  (define resolve-module-path
    ;; relto should be a complete path or #f
    (lambda (s relto)
      (let ([get-dir (lambda ()
		       (if relto
			   (let-values ([(base n d?) (split-path relto)])
			     base)
			   (current-directory)))])
	(cond
	 [(string? s)
	  ;; Parse Unix-style relative path string
	  (let loop ([path (get-dir)][s s])
	    (let ([prefix (regexp-match re:dir s)])
	      (if prefix
		  (loop (build-path path 
				    (let ([p (cadr prefix)])
				      (cond
				       [(string=? p ".") 'same]
				       [(string=? p "..") 'up]
				       [else p])))
			(caddr prefix))
		  (build-path path s))))]
	 [(or (not (pair? s))
	      (not (list? s)))
	  #f]
	 [(eq? (car s) 'lib)
	  (let ([cols (let ([len (length s)])
			(if (= len 2)
			    (list "mzlib")
			    (cddr s)))])
	    (let ([p (apply collection-path cols)])
	      (build-path p (cadr s))))]
	 [(eq? (car s) 'file)
	  (let ([p (cadr s)])
	    (path->complete-path p (get-dir)))]
	 [else #f]))))

  (define (resolve-module-path-index mpi relto)
    ;; relto must be a complete path
    (let-values ([(path base) (module-path-index-split mpi)])
      (if path
	  (resolve-module-path path (resolve-module-path-index base relto))
	  relto)))

  (define re:path-only (regexp "^(.*)/[^/]*$"))

  (define collapse-module-path
    ;; relto-mp should be a relative-path, '(lib relative-path collection), or '(file path)
    (lambda (s relto-mp)
      (let ([combine-relative-elements
	     (lambda (elements)
	       (cond
		[(string? relto-mp)
		 (apply
		  string-append
		  (let ([m (regexp-match re:path-only relto-mp)])
		    (if m
			(cadr m)
			"."))
		  (map (lambda (e)
			 (cond
			  [(eq? e 'same) "/."]
			  [(eq? e 'up) "/.."]
			  [else (string-append "/" e)]))
		       elements))]
		[else (let ([path (apply build-path
					 (let-values ([(base n d?) (split-path (cadr relto-mp))])
					   (if (eq? base 'relative)
					       'same
					       base))
					 elements)])
			(if (eq? (car relto-mp) 'lib)
			    `(lib ,path ,(caddr relto-mp))
			    `(file ,path)))]))])
	(cond
	 [(string? s)
	  ;; Parse Unix-style relative path string
	  (let loop ([elements null][s s])
	    (let ([prefix (regexp-match re:dir s)])
	      (if prefix
		  (loop (cons (let ([p (cadr prefix)])
				(cond
				 [(string=? p ".") 'same]
				 [(string=? p "..") 'up]
				 [else p]))
			      elements)
			(caddr prefix))
		  (combine-relative-elements 
		   (reverse (cons s elements))))))]
	 [(or (not (pair? s))
	      (not (list? s)))
	  #f]
	 [(eq? (car s) 'lib)
	  (let ([cols (let ([len (length s)])
			(if (= len 2)
			    (list "mzlib")
			    (cddr s)))])
	    `(lib ,(build-path (apply build-path 'same (cdr cols)) (cadr s))
		  ,(car cols)))]
	 [(eq? (car s) 'file)
	  (let ([p (cadr s)])
	    (if (absolute-path? p)
		s
		(let loop ([p p][elements null])
		  (let-values ([(base name dir?) (split-path p)])
		    (cond
		     [(eq? base 'relative)
		      (combine-relative-elements 
		       (cons name elements))]
		     [else (loop base (cons name elements))])))))]
	 [else #f]))))

  (define (collapse-module-path-index mpi relto-mp)
    (let-values ([(path base) (module-path-index-split mpi)])
      (if path
	  (collapse-module-path path (resolve-module-path-index base relto-mp))
	  relto-mp)))

  (define (show-import-tree module-path)
    (let loop ([path (resolve-module-path module-path #f)][indent ""][fs? #f])
      (printf "~a~a~a~n" indent path (if fs? " [for-syntax]" ""))
      (let ([code (get-module-code path)])
	(let-values ([(imports fs-imports) (module-compiled-imports code)]
		     [(mk-loop) (lambda (fs?)
				  (lambda (i)
				    (unless (symbol? i)
				      (loop (resolve-module-path-index i path)
					    (format " ~a" indent)
					    fs?))))])
	  (for-each (mk-loop #f) imports)
	  (for-each (mk-loop #t) fs-imports)))))

  (provide check-module-form

	   get-module-code

	   resolve-module-path
	   resolve-module-path-index

	   collapse-module-path
	   collapse-module-path-index

	   show-import-tree))
