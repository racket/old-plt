
(module compile-unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "include.ss")
	   (lib "process.ss")
	   (lib "sendevent.ss")
	   "private/dirs.ss")

  (require "compile-sig.ss")

  (provide dynext:compile@)

  (define dynext:compile@
    (unit/sig dynext:compile^ 
      (import)
      
      (define (get-unix-compile)
	(or (getenv "MZSCHEME_DYNEXT_COMPILER")
	    (find-executable-path "gcc" #f)
	    (find-executable-path "cc" #f)))
      
      (define (get-windows-compile)
	(or (find-executable-path "cl.exe" #f)
	    (find-executable-path "gcc.exe" #f)
	    (find-executable-path "bcc32.exe" #f)))
      
      (define current-extension-compiler 
	(make-parameter 
	 (case (system-type) 
	   [(unix beos macosx) (get-unix-compile)]
	   [(windows) (get-windows-compile)]
	   [else #f])
	 (lambda (v)
	   (when v 
	     (if (and (string? v) (or (relative-path? v) (absolute-path? v)))
		 (unless (and (file-exists? v)
			      (memq 'execute (file-or-directory-permissions v)))
		   (error 'current-extension-compiler 
			  "compiler not found or not executable: ~s" v))
		 (raise-type-error 'current-extension-compiler "pathname string or #f" v)))
	   v)))

      (define win-gcc?
	(let ([c (current-extension-compiler)])
	  (and c (regexp-match "gcc.exe$" c))))
      (define win-borland?
	(let ([c (current-extension-compiler)])
	  (and c (regexp-match "bcc32.exe$" c))))
      (define unix-cc?
	(let ([c (current-extension-compiler)])
	  (and c (regexp-match "[^g]cc$" c))))

      (define gcc-compile-flags (append '("-c" "-O2")
					(case (string->symbol (system-library-subpath))
					  [(parisc-hpux) '("-D_HPUX_SOURCE" "-fpic")]
					  [(ppc-macosx) '("-fno-common" "-DOS_X" )]
					  [(ppc-macosxonx) '("-fno-common" "-DOS_X" "-DXONX" )]
					  [else null])))
      (define unix-compile-flags (case (string->symbol (system-library-subpath))
				   [(parisc-hpux) '("-c" "-O2" "-Aa" "-D_HPUX_SOURCE" "+z")]
				   [else gcc-compile-flags]))
      (define msvc-compile-flags '("/c" "/MT" "/O2"))

      (define current-extension-compiler-flags
	(make-parameter
	 (case (system-type)
	   [(unix beos macosx) (if unix-cc?
				   unix-compile-flags
				   gcc-compile-flags)]
	   [(windows) (if (or win-gcc? win-borland?)
			  gcc-compile-flags
			  msvc-compile-flags)]
	   [(macos) '()])
	 (lambda (l)
	   (unless (and (list? l) (andmap string? l))
	     (raise-type-error 'current-extension-compiler-flags "list of strings" l))
	   l)))
      
      (define unix-compile-include-strings (lambda (s) (list (string-append "-I" s))))
      (define msvc-compile-include-strings (lambda (s) (list (string-append "/I" s))))

      (define current-make-compile-include-strings
	(make-parameter
	 (case (system-type)
	   [(unix beos macosx) unix-compile-include-strings]
	   [(windows) (if (or win-gcc? win-borland?)
			  unix-compile-include-strings
			  msvc-compile-include-strings)]
	   [(macos) unix-compile-include-strings])
	 (lambda (p)
	   (unless (procedure-arity-includes? p 1)
	     (raise-type-error 'current-make-compile-include-strings "procedure of arity 1" p))
	   p)))
      
      (define current-make-compile-input-strings
	(make-parameter
	 (lambda (s) (list s))
	 (lambda (p)
	   (unless (procedure-arity-includes? p 1)
	     (raise-type-error 'current-make-compile-input-strings "procedure of arity 1" p))
	   p)))
      
      (define unix-compile-output-strings (lambda (s) (list "-o" s)))
      (define msvc-compile-output-strings (lambda (s) (list (string-append "/Fo" s))))

      (define current-make-compile-output-strings
	(make-parameter
	 (case (system-type)
	   [(unix beos macosx) unix-compile-output-strings]
	   [(windows) (if (or win-gcc? win-borland?)
			  unix-compile-output-strings
			  msvc-compile-output-strings)]
	   [(macos) unix-compile-output-strings])
	 (lambda (p)
	   (unless (procedure-arity-includes? p 1)
	     (raise-type-error 'current-make-compile-output-strings "procedure of arity 1" p))
	   p)))
      
      (define (get-standard-compilers)
	(case (system-type)
	  [(unix beos macosx) '(gcc cc)]
	  [(windows) '(gcc msvc borland)]
	  [(macos) '(cw)]))

      (define (use-standard-compiler name)
	(define (bad-name name)
	  (error 'use-standard-compiler "unknown compiler: ~a" name))
	(case (system-type)
	  [(unix beos macosx) 
	   (case name
	     [(cc gcc) (let* ([n (if (eq? name 'gcc) "gcc" "cc")]
			      [f (find-executable-path n n)])
			 (unless f
			   (error 'use-standard-linker "cannot find ~a" n))
			 (current-extension-compiler f))
	      (current-extension-compiler-flags (if (eq? name 'gcc)
						    gcc-compile-flags
						    unix-compile-flags))
	      (current-make-compile-include-strings unix-compile-include-strings)
	      (current-make-compile-input-strings (lambda (s) (list s)))
	      (current-make-compile-output-strings unix-compile-output-strings)]
	     [else (bad-name name)])]
	  [(windows) 
	   (case name
	     [(gcc) (let ([f (find-executable-path "gcc.exe" #f)])
		      (unless f
			(error 'use-standard-linker "cannot find gcc.exe"))
		      (current-extension-compiler f))
	      (current-extension-compiler-flags gcc-compile-flags)
	      (current-make-compile-include-strings unix-compile-include-strings)
	      (current-make-compile-input-strings (lambda (s) (list s)))
	      (current-make-compile-output-strings unix-compile-output-strings)]
	     [(borland) (let ([f (find-executable-path "bcc32.exe" #f)])
			  (unless f
			    (error 'use-standard-linker "cannot find bcc32.exe"))
			  (current-extension-compiler f))
	      (current-extension-compiler-flags gcc-compile-flags)
	      (current-make-compile-include-strings unix-compile-include-strings)
	      (current-make-compile-input-strings (lambda (s) (list s)))
	      (current-make-compile-output-strings unix-compile-output-strings)]
	     [(msvc) (let ([f (find-executable-path "cl.exe" #f)])
		       (unless f
			 (error 'use-standard-linker "cannot find MSVC's cl.exe"))
		       (current-extension-compiler f))
	      (current-extension-compiler-flags msvc-compile-flags)
	      (current-make-compile-include-strings msvc-compile-include-strings)
	      (current-make-compile-input-strings (lambda (s) (list s)))
	      (current-make-compile-output-strings msvc-compile-output-strings)]
	     [else (bad-name name)])]
	  [(macos) 
	   (case name
	     [(cw) (current-extension-compiler #f)
	      (current-extension-compiler-flags unix-compile-flags)
	      (current-make-compile-include-strings unix-compile-include-strings)
	      (current-make-compile-input-strings (lambda (s) (list s)))
	      (current-make-compile-output-strings unix-compile-output-strings)]
	     [else (bad-name name)])]))
      
      (define-values (my-process* stdio-compile)
	(let-values ([(p* do-stdio) (include (build-path "private" "stdio.ss"))])
	  (values
	   p*
	   (lambda (start-process quiet?)
	     (do-stdio start-process quiet? (lambda (s) (error 'compile-extension "~a" s)))))))
      
      (define unix/windows-compile
	(lambda (quiet? in out includes)
	  (let ([c (current-extension-compiler)])
	    (if c
		(stdio-compile (lambda (quiet?) 
				 (let ([command (append 
						 (list c)
						 (current-extension-compiler-flags)
						 (apply append 
							(map 
							 (lambda (s) 
							   ((current-make-compile-include-strings) s)) 
							 includes))
						 ((current-make-compile-include-strings) include-dir)
						 ((current-make-compile-input-strings) in)
						 ((current-make-compile-output-strings) out))])
				   (unless quiet? 
				     (printf "compile-extension: ~a~n" command))
				   (apply my-process* command)))
			       quiet?)
		(error 'compile-extension "can't find compiler")))))
      
      (include (build-path "private" "macinc.ss"))
      
      (define (macos-compile quiet? input-file output-file includes)
	(macos-make 'compile-extension "extension-project" "lib" quiet? 
		    (list input-file) output-file includes))
      
      (define compile-extension
	(case (system-type)
	  [(unix beos windows macosx) unix/windows-compile]
	  [(macos) macos-compile])))))
