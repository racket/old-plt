
(unit/sig
 make:collection^
 (import make:make^
	 mzlib:function^
	 dynext:file^
	 (compiler:option : compiler:option^)
	 compiler^)

 (define (make-collection
	  collection-name
	  header-expr
	  collection-files
	  argv)
  (printf "building collection ~a: ~a~n" collection-name collection-files)
  (let* ([zo-compiler #f]
	 [ext-compiler #f]
	 [dest-dir (build-path "compiled" "native" (system-library-subpath))]
	 [src-dir (current-directory)]
	 [sses (quicksort collection-files string<?)]
	 [bases (map (lambda (src)
		       (extract-base-filename/ss src 'make-collection-extension))
		     sses)]
	 [cs (map 
	      (lambda (base)
		(build-path
		 "compiled"
		 "native"
		 (append-c-suffix base)))
	      bases)]
	 [zos (map 
	       (lambda (base)
		 (build-path
		  "compiled"
		  (append-zo-suffix base)))
	       bases)]
	 [kps (map 
	       (lambda (base)
		 (build-path
		  "compiled"
		  "native"
		  (append-constant-pool-suffix base)))
	       bases)]
	 [objs (map
		(lambda (base)
		  (build-path
		   dest-dir
		   (append-object-suffix base)))
		bases)]
	 [ss->zo-list
	  (map (lambda (ss zo)
		 `(,zo (,ss)
		       ,(lambda ()
			  (unless zo-compiler
			      (set! zo-compiler (compile-zos header-expr)))
			  (zo-compiler (list ss) "compiled"))))
	       sses zos)]
	 [ss->c-list
	  (map (lambda (ss c)
		 `(,c (,ss)
		      ,(lambda ()
			 (unless ext-compiler
			   (set! ext-compiler (compile-extension-parts-to-c header-expr)))
			 (parameterize ([compiler:option:setup-prefix 
					 (string-append "_" collection-name)])
			   (ext-compiler (list ss) (build-path "compiled" "native"))))))
	       sses cs)]
	 [c->o-list
	  (map (lambda (c obj)
		 `(,obj (,c)
			,(lambda ()
			   (parameterize ([compiler:option:setup-prefix 
					   (string-append "_" collection-name)])
			       (compile-c-extension-parts (list c) dest-dir)))))
	       cs objs)]
	 [o->so
	  `(,(build-path dest-dir (append-extension-suffix "_loader"))
	    ,objs
	    ,(lambda ()
	       (parameterize ([compiler:option:setup-prefix (string-append "_" collection-name)])
		 (link-extension-parts
		  (append objs kps)
		  dest-dir))))])
    (unless (directory-exists? "compiled") (make-directory* "compiled"))
    (unless (or (equal? argv #("zo"))
		(directory-exists? dest-dir))
       (make-directory* dest-dir))
    (make*
     (append
      (list o->so)
      `(("zo" ,zos))
      ss->zo-list
      ss->c-list
      c->o-list)
     argv))))
