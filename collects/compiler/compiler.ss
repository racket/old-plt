;; Main compilation procedures
;; (c) 1997-8 PLT, Rice University

;; The various procedures provided by this library are implemented
;;  by dynamically linking to code supplied by the MzLib, dynext, and
;;  compiler collections.

;; The Scheme->C compiler is loaded as either sploadr.ss (link in
;;  real MrSpidey) or loadr.ss (link in trivial MrSpidey stubs).

(unit/sig
 compiler^
 (import compiler:option^
	 mzlib:function^
	 mzlib:pretty-print^
	 mzlib:file^
	 mzlib:string^
	 mzlib:compile^
	 dynext:compile^
	 dynext:link^
	 dynext:file^)

 (define load-namespace (make-namespace))

 (define (make-extension-compiler mode prefix)
   (let ([u (parameterize ([current-namespace load-namespace])
	      (require-library "refer.ss")
	      (if (or (use-mrspidey) (use-mrspidey-for-units))
		  (begin
		    (require-library "spsigload.ss" "compiler")
		    (require-library "sploadr.ss" "compiler"))
		  (begin
		    (require-library "sigload.ss" "compiler")
		    (require-library "loadr.ss" "compiler"))))]
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
	       (STRING : mzlib:string^)
	       (COMPILE : dynext:compile^)
	       (LINK : dynext:link^)
	       (DFILE : dynext:file^)
	       (OPTIONS : compiler:option^))
       (link [COMPILER : compiler:inner^ (u FUNCTION
					    PRETTY-PRINT
					    FILE
					    STRING
					    COMPILE
					    LINK
					    DFILE
					    OPTIONS)]
	     [INIT : () (init COMPILER)])
       (export))
      mzlib:function^
      mzlib:pretty-print^
      mzlib:file^
      mzlib:string^
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

 (define (link/glue-extension-parts link? source-files destination-directory)
   (let ([u (parameterize ([current-namespace load-namespace])
	       (require-library "refer.ss")
	       (require-library "sigload.ss" "compiler")
	       (require-library "ldr.ss" "compiler"))]
	 [init (unit/sig
		()
		(import compiler:linker^)
		(if link?
		    link-extension
		    glue-extension))])
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

 (define (link-extension-parts source-files destination-directory)
   (link/glue-extension-parts #t source-files destination-directory))

 (define (glue-extension-parts source-files destination-directory)
   (link/glue-extension-parts #f source-files destination-directory))

 (define (compile-to-zo src dest namespace)
   (let ([cwd (current-directory)])
     (parameterize ([current-namespace namespace]) 
      (with-handlers ([void (lambda (exn)
			      (delete-file (path->complete-path dest cwd))
			      (raise exn))])
        (compile-file src dest
		      '(use-current-namespace
			strip-macro-definitions
			ignore-require-library))
	(printf " [output to \"~a\"]~n" dest)))))

 (define (compile-zos prefix)
   (let ([n (make-namespace)])
     (parameterize ([current-namespace n]) 
		   (eval '(require-library "refer.ss"))
		   (eval '(require-library "macrox.ss"))
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
     (let ([make-collection
	    (invoke-unit/sig
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
	     [orig (current-directory)]
	     [info (apply require-library/proc "info.ss" cp)])
	 (dynamic-wind
	  (lambda () (current-directory dir))
	  (lambda ()
	    (parameterize ([current-load-relative-directory dir]
			   [current-require-relative-collection cp])
	      ;; Compile the collection files via make-collection
	      (let ([sses (filter
			   extract-base-filename/ss
			   (map
			    normal-case-path
			    (directory-list)))])
		(make-collection
		 (info 'name (lambda () (error 'compile-collection "info.ss did not provide a name")))
		 (info 'compile-prefix (lambda () '(void)))
		 (remove*
		  (map normal-case-path
		  	(info 
			 (if zos? 
			     'compile-zo-omit-files 
			     'compile-extension-omit-files)
			 (lambda () null)))
		  (remove*
		   (map normal-case-path 
			(info 'compile-omit-files (lambda () null)))
		   sses))
		 (if zos? #("zo") #())))
	      ;; compile-elaboration-zos
	      (when zos?
		(let ([n (make-namespace)]
		      [need-prefix? #t]
		      [need-load null])
		  (parameterize ([current-namespace n])
		   (for-each
		     (lambda (ss)
		       (let* ([base (extract-base-filename/ss ss 'compile-collection)]
			      [zo (build-path "compiled" (append-zo-suffix base))]
			      [ss-date (file-or-directory-modify-seconds ss)]
			      [zo-date (and (file-exists? zo) (file-or-directory-modify-seconds zo))])
			 (if (or (not ss-date) (not zo-date) (> ss-date zo-date))
			     (with-handlers ([void (lambda (exn)
						     (delete-file zo)
						     (raise exn))])
			       (when need-prefix?
				  (set! need-prefix? #f)
				  (eval (info 'compile-elaboration-zos-prefix
					      (lambda () '(void)))))
			       (for-each
				(lambda (f)
				  (printf "loading ~a~n" f)
				  (require-relative-library/proc f))
				(reverse need-load))
			       (set! need-load null)
			       (printf "compiling ~a~n" ss)
			       (compile-file
				ss
				zo
				'(also-preserve-elaborations use-current-namespace)))
			     (set! need-load (cons ss need-load)))))
		     (info 'compile-elaboration-zos
			   (lambda () null))))))))
	  (lambda () (current-directory orig)))
	 (when (compile-subcollections)
	   (for-each
	    (lambda (s)
	      (unless (and (pair? s) (list? s) (andmap string? s))
	        (error 'compile-collection "bad sub-collection path: ~a" s))
	      (compile-collection s zos?))
	    (info 'compile-subcollections (lambda () null))))))))

 (define (compile-collection-extension collection . cp)
  (compile-collection (cons collection cp) #f))

 (define (compile-collection-zos collection . cp)
  (compile-collection (cons collection cp) #t)))


