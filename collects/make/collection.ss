
(require-library "make.ss" "make")
(require-library "file.ss" "mzscheme" "dynext")
(require-library "functio.ss")

(define (make-collection-extension
	 collection-name
	 header-expr
	 collection-files
	 argv)
  (printf "building collection ~a: ~a~n" collection-name collection-files)
  (let* ([prefixed? #f]
	 [namespace #f]
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
			  (require-library "zo.ss" "compiler")
			  (unless namespace
			      (set! namespace (make-namespace))
			      (eval header-expr namespace))
			  (compile-to-zo ss zo namespace))))
	       sses zos)]
	 [ss->c-list
	  (map (lambda (ss c)
		 `(,c (,ss)
		      ,(lambda ()
			 (require-library "load.ss" "compiler")
			 (unless prefixed?
			   (compiler:load-prefixes (list header-expr))
			   (set! prefixed? #t))
			 (parameterize ([compiler:option:c-only #t]
					[compiler:option:multi-o #t]
					[compiler:option:setup-prefix 
					 (string-append "_" collection-name)])
			   (s:compile ss src-dir (build-path "compiled" "native"))))))
	       sses cs)]
	 [c->o-list
	  (map (lambda (c obj)
		 `(,obj (,c)
			,(lambda ()
			   (require-library "load.ss" "compiler")
			   (parameterize ([compiler:option:multi-o #t]
					  [compiler:option:setup-prefix 
					   (string-append "_" collection-name)])
			       (s:compile c src-dir dest-dir)))))
	       cs objs)]
	 [o->so
	  `(,(build-path dest-dir (append-extension-suffix "_loader"))
	    ,objs
	    ,(lambda ()
	       (require-library "ld.ss" "compiler")
	       (parameterize ([compiler:option:setup-prefix (string-append "_" collection-name)])
		 (link-multi-file-extension
		  (append objs kps)
		  dest-dir))))])
    (unless (directory-exists? dest-dir)
       (make-directory* dest-dir))
    (make*
     (append
      (list o->so)
      `(("zo" ,zos ,void))
      ss->zo-list
      ss->c-list
      c->o-list)
     argv)))
