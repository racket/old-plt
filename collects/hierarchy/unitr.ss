(require-library "match.ss")

(unit/sig hierachy:client^
  (import hierachy^
	  [core : mzlib:core^])
  
   (define path-only
     (lambda (name)
       (let-values ([(base file dir?) (split-path name)])
                   (cond
                    [dir? name]
                    [(string? base) base]
                    [else #f]))))

  (define name-only
    (lambda (x) 
      (let*-values ([(pa a _2) (split-path x)]
		    [(pb b _2) (split-path pa)]
		    [(pc c _2) (split-path pb)])
	(build-path b a))))
  
  (define get-two-paths
    (lambda (x) 
      (let*-values ([(pa a _2) (split-path x)]
		    [(pb b _2) (split-path pa)]
		    [(pc c _2) (split-path pb)])
	(build-path b a))))
  
  (define space "")
  
  (define add-relationT 
    (lambda (x y)
      (printf "~aadd-relation: ~s ~s~n" space x y)
      (add-relation x y)))
  
  (define drscheme:tool-directories
    (directory-list (build-path (collection-path "drscheme")
					   "tools")))
  
  (define find-references
    (let ([badones (list "drscheme/basis.ss"
			 "mzlib/compiler.ss"
			 "mzlib/referr.ss"
			 "drscheme/zodiac.ss")])
      (lambda (t-filename)
	(unless (member (get-two-paths t-filename) badones)
	  (let loop ([sexp (call-with-input-file t-filename
			     (lambda (p)
			       (let loop ()
				 (let ([x (read p)])
				   (if (eof-object? x)
				       null
				       (cons x (loop)))))))])
	    (match sexp
	      [(or ('begin-elaboration-time . x)
		   ('begin-construction-time . x))
	       (when (and (list? x)
			  (not (null? x)))
		 (loop (eval `(begin ,@x))))]
	      [('reference-unit/sig filename)
	       (let ([path (core:file@:normalize-path (eval filename))])
		 (add-relationT (string->symbol (name-only path))
				(string->symbol (name-only t-filename)))
		 (traverse-file path))]
	      [('reference-library-unit/sig filename location . rest)
	       (let ([path (core:file@:normalize-path (build-path (apply collection-path location rest)
						       filename))])
		 (add-relationT (string->symbol (name-only path))
				(string->symbol (name-only t-filename)))
		 (traverse-file path))]
	      [('reference-library-unit/sig filename)
	       (let* ([path (core:file@:normalize-path (build-path (collection-path "mzlib")
							filename))])
		 (add-relationT (string->symbol (name-only path))
				(string->symbol (name-only t-filename)))
		 (traverse-file path))]
	      [(x . y)
	       (loop x)
	       (loop y)
	       '(for-each loop l)]
	      [else (void)]))))))
  
  (define traverse-file
    (lambda (filename)
      (set! space (string-append "  " space))
      (printf "~atraversing file: ~a~n" space filename)
      (let ([old-dir (current-directory)])
	(current-directory (path-only filename))
	(find-references filename)
	(current-directory old-dir))
      (set! space (substring space 2 (string-length space)))
      (printf "~afinished file: ~a~n" space filename)))
  
  (define mred:plt-home-directory (or (getenv "PLTHOME")
				      "/usr/local/lib/plt"))
  (define plt:home-directory mred:plt-home-directory)
  
  (define drs-roots
    (map core:file@:normalize-path
	 (list (build-path (collection-path "drscheme") "link.ss")
	       ;(build-path (collection-path "gusrspce") "gusrspcr.ss")
	       )))
  (define process-root 
    (lambda (root)
      (add-relationT (string->symbol (name-only root)) #f)
      (traverse-file root)))
  
  (define (build-tree)
    (for-each process-root drs-roots)))