
(module setup-extension mzscheme
  (require (lib "make.ss" "make")
	   (lib "link.ss" "dynext")
	   (lib "compile.ss" "dynext")
	   (lib "file.ss" "dynext")
	   (lib "file.ss")
	   (lib "list.ss"))

  (provide pre-install
	   with-new-flags)

  ;; Syntax used to add a command-line flag:
  (define-syntax with-new-flags
    (syntax-rules ()
      [(_ param flags body0 body ...)
       (parameterize ([param (append
			      (param)
			      flags)])
	 body0 body ...)]))
  
  (define (pre-install plthome
		       collection-dir
		       file.c
		       default-lib-dir
		       include-subdirs
		       find-unix-libs
		       find-windows-libs
		       unix-libs
		       windows-libs
		       extra-depends
		       last-chance-k)
    (parameterize ([current-directory collection-dir])
      (define mach-id (string->symbol (system-library-subpath)))
      (define is-win? (eq? mach-id 'win32\\i386))

      ;; We look for libraries and includes in the 
      ;;  following places:
      (define search-path
	(append
	 (let ([v (getenv "PLT_EXTENSION_LIB_PATHS")])
	   (if v 
	       (path-list-string->path-list v (list default-lib-dir))
	       (list default-lib-dir)))
	 (list "/usr"
	       "/usr/local"
	       "/usr/local/gnu"
	       ;; OS X fink location:
	       "/sw"
	       ;; Hack for NU PLT's convenience:
	       "/arch/gnu/packages/readline-4.2")))
      
      (define sys-path
	(ormap (lambda (x)
		 (and (andmap
		       (lambda (sub)
			 (directory-exists? (build-path x "include" sub)))
		       include-subdirs)
		      (andmap (lambda (lib)
				(ormap (lambda (suffix)
					 (file-exists? 
					  (build-path x 
						      "lib"
						      (format "~a~a.~a" 
							      (if is-win? 
								  ""
								  "lib")
							      lib 
							      suffix))))
				       '("a" "so" "dylib" "lib")))
			      (if is-win?
				  find-windows-libs
				  find-unix-libs))
		      x))
	       search-path))

      (unless sys-path
	(error 'extension-installer
	       "can't find needed include files and/or library; try setting the environment variable PLT_EXTENSION_LIB_PATHS"))

      (parameterize ([make-print-checking #f])

	;; Used as make dependencies:
	(define mz-inc-dir (build-path (collection-path "mzscheme") 'up 'up "include"))
	(define headers (map (lambda (name)
			       (build-path mz-inc-dir name))
			     '("scheme.h" "schvers.h" "schemef.h" "sconfig.h" "stypes.h")))
	
	(define dir (let ([std (build-path "compiled" "native" (system-library-subpath))])
		      (case (link-variant)
			[(3m) (build-path std "3m")]
			[else std])))
	(define base-file (let-values ([(base name dir?) (split-path (extract-base-filename/c file.c 'pre-install))])
			    name))
	(define file.so (build-path dir (append-extension-suffix base-file)))
	(define file.o (build-path dir (append-object-suffix base-file)))

	(with-new-flags 
	 current-extension-compiler-flags
	 (list (format "-I~a/include" sys-path))
	 
	 ;; If we don't have a .so file, we need to make the linker
	 ;;   use the whole archive:
	 (with-new-flags
	  current-extension-linker-flags
	  (if (not (ormap (lambda (lib)
			    (or (file-exists? (build-path sys-path "lib" (format "lib~a.so" lib)))
				(file-exists? (build-path sys-path "lib" (format "lib~a.dylib" lib)))))
			  find-unix-libs))
	      (case mach-id
		[(sparc-solaris i386-solaris) (list "-u" "rl_readline_name")]
		[(i386-linux i386-freebsd sparc-linux) (list "--whole-archive")]
		[(win32\\i386) null]
		[else (fprintf (current-error-port)
			       "~a~a~a~n"
			       "Warning: trying to use .a library, "
			       "but don't know how to force inclusion; "
			       "result may have undefined references")
		      null])
	      null)
	  
	  ;; Add -L and -l for Unix:
	  (with-new-flags
	   current-extension-linker-flags 
	   (if is-win?
	       null
	       (cons (format "-L~a/lib" sys-path)
		     (map (lambda (l)
			    (string-append "-l" l))
			  (append find-unix-libs unix-libs))))
	   
	   ;; Add libs for Windows:
	   (with-new-flags
	    current-standard-link-libraries
	    (if is-win?
		(append (map 
			 (lambda (l)
			   (build-path sys-path "lib" (format "~a.lib" l)))
			 find-windows-libs)
			windows-libs)
		null)
	    
	    ;; Extra stuff:
	    (with-new-flags
	     current-extension-linker-flags 
	     (case mach-id
	       [(rs6k-aix) (list "-lc")]
	       [else null])
	     
	     (define (delete/continue x)
	       (with-handlers ([(lambda (x) #t) void])
		 (delete-file x)))

	     (make-directory* dir)

	     (last-chance-k
	      (lambda ()
		(make/proc
		 (list (list file.so 
			     (list file.o)
			     (lambda ()
			       (link-extension #f (list file.o) file.so)))
		       
		       (list file.o 
			     (append (list file.c)
				     headers
				     extra-depends)
			     (lambda ()
			       (compile-extension #f file.c file.o ()))))
		 #()))))))))))))
