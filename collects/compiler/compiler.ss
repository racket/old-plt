
(unit/sig
 compiler^
 (import compiler:option^
	 mzlib:function^
	 mzlib:pretty-print^
	 mzlib:file^
	 mzlib:compile^
	 dynext:compile^
	 dynext:link^
	 dynext:file^)

 (define load-namespace (make-namespace))

 (define (make-extension-compiler mode prefix)
   (let ([u (parameterize ([current-namespace load-namespace])
	       (require-library "refer.ss")
	       (require-library "sigload.ss" "compiler")
	       (require-library "loadr.ss" "compiler"))]
	 [init (unit/sig 
		()
		(import compiler:inner^)
		(eval-compile-prefix prefix)
		(case mode
		  [(compile-extension) compile-extension]
		  [(compile-extension-to-c) compile-extension-to-c]
		  [(compile-c-extension) compile-c-extension]
		  [(compile-extension-part) compile-extension-part]
		  [(compile-extension-part-to-c) compile-extension-part-to-c]
		  [(compile-c-extension-part) compile-c-extension-part]))])
     (invoke-unit/sig
      (compound-unit/sig
       (import (FUNCTION : mzlib:function^)
	       (PRETTY-PRINT : mzlib:pretty-print^)
	       (FILE : mzlib:file^)
	       (COMPILE : dynext:compile^)
	       (LINK : dynext:link^)
	       (DFILE : dynext:file^)
	       (OPTIONS : compiler:option^))
       (link [COMPILER : compiler:inner^ (u FUNCTION
					    PRETTY-PRINT
					    FILE
					    COMPILE
					    LINK
					    DFILE
					    OPTIONS)]
	     [INIT : () (init COMPILER)])
       (export))
      mzlib:function^
      mzlib:pretty-print^
      mzlib:file^
      dynext:compile^
      dynext:link^
      dynext:file^
      compiler:option^)))

 (define (make-compiler mode) 
   (lambda (prefix)
     (let ([c (make-extension-compiler mode prefix)])
       (lambda (source-files destination-directory)
	 (map
	  (lambda (source-file)
	    (c source-file (or destination-directory 'same)))
	  source-files)))))

 (define (make-unprefixed-compiler mode)
   (let ([f #f])
     (lambda (source-files destination-directory)
       (unless f
	  (set! f ((make-compiler mode) '(void))))
       (f source-files destination-directory))))

 (define compile-extensions
   (make-compiler 'compile-extension))
 (define compile-extensions-to-c
   (make-compiler 'compile-extension-to-c))
 (define compile-c-extensions
   (make-unprefixed-compiler 'compile-c-extension))

 (define compile-extension-parts
   (make-compiler 'compile-extension-part))
 (define compile-extension-parts-to-c
   (make-compiler 'compile-extension-part-to-c))
 (define compile-c-extension-parts
   (make-unprefixed-compiler 'compile-c-extension-part))

 (define (link-extension-parts source-files destination-directory)
   (let ([u (parameterize ([current-namespace load-namespace])
	       (require-library "refer.ss")
	       (require-library "sigload.ss" "compiler")
	       (require-library "ldr.ss" "compiler"))]
	 [init (unit/sig
		()
		(import compiler:linker^)
		link-extension)])
     (let ([f (invoke-unit/sig
	       (compound-unit/sig
		(import (COMPILE : dynext:compile^)
			(LINK : dynext:link^)
			(DFILE : dynext:file^)
			(FUNCTION : mzlib:function^)
			(OPTIONS : compiler:option^))
		(link [LINKER : compiler:linker^ (u COMPILE 
						    LINK
						    DFILE
						    FUNCTION
						    OPTIONS)]
		      [INIT : () (init LINKER)])
		(export))
	       dynext:compile^
	       dynext:link^
	       dynext:file^
	       mzlib:function^
	       compiler:option^)])
       (f source-files destination-directory))))

 (define (compile-to-zo src dest namespace)
   (let ([cwd (current-directory)])
     (parameterize ([current-namespace namespace]) 
      (with-handlers ([void (lambda (exn)
			      (delete-file (path->complete-path dest cwd))
			      (raise exn))])
        (compile-file src dest
		      '(use-current-namespace
			ignore-macro-definitions
			ignore-require-library))
	(printf " [output to \"~a\"]~n" dest)))))

 (define (compile-zos prefix)
   (let ([n (make-namespace)])
     (parameterize ([current-namespace n]) 
		   (eval prefix))
     (lambda (source-files destination-directory)
       (let ([file-bases (map
			  (lambda (file)
			    (let ([f (extract-base-filename/ss file 'mzc)])
			      (if destination-directory
				  (let-values ([(base file dir?) (split-path f)])
				    (build-path destination-directory file))
				  f)))
			  source-files)])
	   (for-each
	    (lambda (f b)
	      (let ([zo (append-zo-suffix b)])
		(compile-to-zo f zo n)))
	    source-files file-bases)))))

 (define (compile-collection cp zos?)
   (let ([coll (require-library "collectionr.ss" "make")]
	 [make (require-library "maker.ss" "make")]
	 [init (unit/sig
		()
		(import make:collection^)
		make-collection)])
     (let ([f (invoke-unit/sig
	       (compound-unit/sig
		(import (FUNCTION : mzlib:function^)
			(DFILE : dynext:file^)
			(OPTIONS : compiler:option^)
			(COMPILER : compiler^))
		(link [MAKE : make:make^ (make)]
		      [COLL : make:collection^ (coll MAKE
						     FUNCTION
						     DFILE
						     OPTIONS
						     COMPILER)]
		      [INIT : () (init COLL)])
		(export))
	       mzlib:function^
	       dynext:file^
	       compiler:option^
	       compiler^)])
       (let ([dir (apply collection-path cp)]
	     [orig (current-directory)])
	 (dynamic-wind
	  (lambda () (current-directory dir))
	  (lambda ()
	    (parameterize ([current-load-relative-directory dir])
	       (let ([info (apply require-library "info.ss" cp)])
		 (let ([sses (filter
			      (lambda (s)
				(regexp-match "\\.(ss|scm)$" s))
			      (directory-list))])
		   (f (info 'name)
		      (info 'compile-prefix)
		      (remove*
		       (info 'compile-omit-files)
		       sses)
		      (if zos? #("zo") #()))))))
	  (lambda () (current-directory orig)))))))

 (define (compile-collection-extension collection . cp)
  (compile-collection (cons collection cp) #f))

 (define (compile-collection-zos collection . cp)
  (compile-collection (cons collection cp) #t)))


