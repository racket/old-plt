;; Main compilation procedures
;; (c) 1997-2001 PLT

;; The various procedures provided by this library are implemented
;;  by dynamically linking to code supplied by the MzLib, dynext, and
;;  compiler collections.

;; The Scheme->C compiler is loaded as either sploadr.ss (link in
;;  real MrSpidey) or loadr.ss (link in trivial MrSpidey stubs).

(module compiler-unit mzscheme 
  (require (lib "unitsig.ss"))

  (require "sig.ss")
  (require (lib "file-sig.ss" "dynext")
	   (lib "link-sig.ss" "dynext")
	   (lib "compile-sig.ss" "dynext")
	   
	   (lib "make-sig.ss" "make")
	   (lib "collection-sig.ss" "make")

	   (lib "toplevel.ss" "syntax")
	   (lib "moddep.ss" "syntax"))

  (require (lib "list.ss")
	   (lib "file.ss")
	   (lib "compile.ss") ; gets compile-file
	   (lib "cm.ss")
	   (lib "getinfo.ss" "setup"))

  (provide compiler@)

  (define orig-namespace (current-namespace))

  ;; ;;;;;;;; ----- The main compiler unit ------ ;;;;;;;;;;
  (define compiler@
    (unit/sig compiler^
      (import compiler:option^
	      dynext:compile^
	      dynext:link^
	      dynext:file^)

      (define current-compiler-dynamic-require-namespace 
	(make-parameter orig-namespace))
      (define (c-dynamic-require path id)
	(if (current-compiler-dynamic-require-namespace)
	    (parameterize ([current-namespace (current-compiler-dynamic-require-namespace)])
	      (dynamic-require path id))
	    (dynamic-require path id)))

      (define (make-extension-compiler mode prefix)
	(let ([u (c-dynamic-require `(lib "base.ss" "compiler" "private")
				    'base@)]
	      [init (unit/sig ()
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
	    (import (COMPILE : dynext:compile^)
		    (LINK : dynext:link^)
		    (DFILE : dynext:file^)
		    (OPTION : compiler:option^))
	    (link [COMPILER : compiler:inner^ (u COMPILE
						 LINK
						 DFILE
						 OPTION)] 
		  [INIT : () (init COMPILER)])
	    (export))
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
	(let ([u (c-dynamic-require '(lib "ld-unit.ss" "compiler") 'ld@)]
	      [init (unit/sig ()
		      (import compiler:linker^)
		      (if link?
			  link-extension
			  glue-extension))])
	  (let ([f (invoke-unit/sig
		    (compound-unit/sig
		     (import (COMPILE : dynext:compile^)
			     (LINK : dynext:link^)
			     (DFILE : dynext:file^)
			     (OPTION : compiler:option^))
		     (link [LINKER : compiler:linker^ (u COMPILE 
							 LINK
							 DFILE
							 OPTION)]
			   [INIT : () (init LINKER)])
		     (export))
		    dynext:compile^
		    dynext:link^
		    dynext:file^
		    compiler:option^)])
	    (f source-files destination-directory))))

      (define (link-extension-parts source-files destination-directory)
	(link/glue-extension-parts #t source-files destination-directory))

      (define (glue-extension-parts source-files destination-directory)
	(link/glue-extension-parts #f source-files destination-directory))

      (define (compile-to-zo src dest namespace eval?)
	((if eval? (lambda (t) (t)) with-module-reading-parameterization)
	 (lambda ()
	   (parameterize ([current-namespace namespace])
	     (compile-file src dest
			   (if eval?
			       (lambda (expr)
				 (expand-syntax-top-level-with-compile-time-evals expr))
			       values)))))
	(printf " [output to \"~a\"]~n" dest))

      (define (compile-zos prefix)
	(let ([n (if prefix (make-namespace) (current-namespace))])
	  (when prefix
	    (eval prefix n))
	  (lambda (source-files destination-directory)
	    (let ([file-bases (map
			       (lambda (file)
				 (let ([f (extract-base-filename/ss file 'mzc)])
				   (if destination-directory
				       (let-values ([(base file dir?) (split-path f)])
					 (build-path (if (eq? destination-directory 'auto)
							 (let ([d (build-path (if (eq? base 'relative)
										  'same
										  base)
									      "compiled")])
							   (unless (directory-exists? d)
							     (make-directory* d))
							   d)
							 destination-directory)
						     file))
				       f)))
			       source-files)])
	      (for-each
	       (lambda (f b)
		 (let ([zo (append-zo-suffix b)])
		   (compile-to-zo f zo n prefix)))
	       source-files file-bases)))))

      (define (compile-collection cp zos?)
	(let ([make (c-dynamic-require '(lib "make-unit.ss" "make") 'make@)]
	      [coll (c-dynamic-require '(lib "collection-unit.ss" "make") 'make:collection@)]
	      [init (unit/sig ()
		      (import make:collection^)
		      make-collection)])
	  (let ([make-collection
		 (invoke-unit/sig
		  (compound-unit/sig
		   (import (DFILE : dynext:file^)
			   (OPTION : compiler:option^)
			   (COMPILER : compiler^))
		   (link [MAKE : make^ (make)]
			 [COLL : make:collection^ (coll MAKE
							DFILE
							OPTION
							COMPILER)]
			 [INIT : () (init COLL)])
		   (export))
		  dynext:file^
		  compiler:option^
		  compiler^)])
	    (let ([dir (apply collection-path cp)]
		  [orig (current-directory)]
		  [info (get-info cp)])
	      (dynamic-wind
		  (lambda () (current-directory dir))
		  (lambda ()
		    (parameterize ([current-load-relative-directory dir])
		      ;; Compile the collection files via make-collection
		      (let ([sses (filter
				   extract-base-filename/ss
				   (directory-list))])
			(let ([filtered-sses
			       (remove*
				(info 
				 (if zos? 
				     'compile-zo-omit-files 
				     'compile-extension-omit-files)
				 (lambda () null))
				(remove*
				 (info 'compile-omit-files (lambda () null))
				 sses))])
			  (if zos?
			      ;; Verbose compilation manager:
			      (parameterize ([manager-trace-handler (lambda (s) (printf "~a~n" s))])
			        (map (make-caching-managed-compile-zo) filtered-sses))
			      ;; Old collection compiler:
			      (make-collection
			       ((or info (lambda (a f) (f)))
				'name 
				(lambda () (error 'compile-collection "info did not provide a name for collection: ~e" cp)))
			       filtered-sses
			       (if zos? #("zo") #())))))))
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
	(compile-collection (cons collection cp) #t)))))
