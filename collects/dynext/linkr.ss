(unit/sig dynext:link^ (import)
  
  (define include-dir (collection-path "mzscheme" "include"))

  (define (get-windows-linker)
    (or (find-executable-path "cl.exe" "cl.exe")
	(find-executable-path "ld.exe" "ld.exe")))

  (define (get-unix-linker)
    (let ([s (case (string->symbol (system-library-subpath))
	       [(rs6k-aix) "cc"]
	       [else "ld"])])
      (find-executable-path s s)))
  
  (define current-extension-linker 
    (make-parameter 
     (case (system-type) 
       [(unix beos) (get-unix-linker)]
       [(windows) (get-windows-linker)]
       [else #f])
     (lambda (v)
       (when v 
	 (if (and (string? v) (or (relative-path? v) (absolute-path? v)))
	     (unless (and (file-exists? v)
			  (memq 'execute (file-or-directory-permissions v)))
	       (error 'current-extension-linker 
		      "linker not found or not executable: ~s" v))
	     (raise-type-error 'current-extension-linker "pathname string or #f" v)))
       v)))
  
  (define win-gcc?
    (let ([c (current-extension-linker)])
      (and c (regexp-match "ld.exe$" c))))
  
  (define (get-unix-link-flags)
    (case (string->symbol (system-library-subpath))
      [(sparc-solaris i386-solaris) (list "-G")]
      [(sparc-sunos4) (list "-Bdynamic")]
      [(i386-freebsd) (list "-Bshareable")]
      [(rs6k-aix) (let ([version (read (car (process* "/usr/bin/uname" "-v")))])
		    (list "-bM:SRE"
			  (format "-bI:~a/mzscheme.exp" include-dir)
			  (format "-bE:~a/ext.exp" include-dir)
			  (if (= 3 version)
			      "-e _nostart"
			      "-bnoentry")))]
      [(parisc-hpux) (list "-b")]
      [else (list "-shared")]))

  (define msvc-linker-flags (list "/LD"))
  (define win-gcc-linker-flags (list "--dll"))
  
  (define current-extension-linker-flags
    (make-parameter
     (case (system-type)
       [(unix beos) (get-unix-link-flags)]
       [(windows) (if win-gcc?
		      win-gcc-linker-flags
		      msvc-linker-flags)]
       [(macos) null])
     (lambda (l)
       (unless (and (list? l) (andmap string? l))
	 (raise-type-error 'current-extension-link-flags "list of strings" l))
       l)))
  
  (define std-library-dir (build-path (collection-path "mzscheme" "lib") (system-library-subpath)))
  
  (define-values (my-process* stdio-link)
    (let-values ([(p* do-stdio) (require-library "stdio.ss" "dynext")])
      (values
       p*
       (lambda (start-process quiet?)
	 (do-stdio start-process quiet? (lambda (s) (error 'link-extension "~a" s)))))))
  
  (define current-make-link-input-strings
    (make-parameter
     (lambda (s) (list s))
     (lambda (p)
       (unless (procedure-arity-includes? p 1)
	 (raise-type-error 'current-make-link-input-strings "procedure of arity 1" p))
       p)))
  
  (define win-gcc-link-output-strings (lambda (s) (list "-e" "_dll_entry@12" "-o" s)))
  (define msvc-link-output-strings (lambda (s) (list (string-append "/Fe" s))))

  (define current-make-link-output-strings
    (make-parameter
     (case (system-type)
       [(unix beos) (lambda (s) (list "-o" s))]
       [(windows) (if win-gcc?
		      win-gcc-link-output-strings
		      msvc-link-output-strings)]
       [(macos) (lambda (s) (list "-o" s))])
     (lambda (p)
       (unless (procedure-arity-includes? p 1)
	 (raise-type-error 'current-make-link-output-strings "procedure of arity 1" p))
       p)))

  (define (make-win-link-libraries win-gcc?)
    (let ([file (lambda (f)
		  (build-path std-library-dir 
			      (if win-gcc?
				  "gcc"
				  "msvc")
			      f))])
      (if win-gcc?
	  (map file (list "mzdyn.exp"
			  "mzdyn.o"
			  "init.o"
			  "fixup.o"))
	  (map file (list "mzdyn.exp"
			  "mzdyn.obj")))))
  
  (define (get-unix/macos-link-libraries)
    (list (build-path std-library-dir "mzdyn.o")))

  (define current-standard-link-libraries
    (make-parameter
     (case (system-type)
       [(unix beos macos) (get-unix/macos-link-libraries)]
       [(windows) (make-win-link-libraries win-gcc?)])
     (lambda (l)
       (unless (and (list? l) (andmap string? l))
	 (raise-type-error 'current-standard-link-libraries "list of strings" l))
       l)))
  
  (define unix/windows-link
    (lambda (quiet? in out)
      (let ([c (current-extension-linker)])
	(if c
	    (stdio-link (lambda (quiet?) 
			  (let ([command (append (list c)
						 (current-extension-linker-flags)
						 (apply append (map (lambda (s) ((current-make-link-input-strings) s)) in))
						 (current-standard-link-libraries)
						 ((current-make-link-output-strings) out))])
			    (unless quiet? 
			      (printf "link-extension: ~a~n" command))
			    (apply my-process* command)))
			quiet?)
	    (error 'link-extension "can't find linker")))))
  
  (define (use-standard-linker name)
    (define (bad-name name)
      (error 'use-standard-linker "unknown linker: ~a" name))
    (case (system-type)
      [(unix beos) 
       (case name
	 [(cc gcc) (current-extension-linker (get-unix-linker))
		   (current-extension-linker-flags (get-unix-link-flags))
		   (current-make-link-input-strings (lambda (s) (list s)))
		   (current-make-link-output-strings (lambda (s) (list "-o" s)))
		   (current-standard-link-libraries (get-unix/macos-link-libraries))]
	 [else (bad-name name)])]
      [(windows)
       (case name
	 [(gcc) (let ([f (find-executable-path "ld.exe" "ld.exe")])
		  (unless f
		    (error 'use-standard-linker "cannot find gcc's ld.exe"))
		  (current-extension-linker f)
		  (current-extension-linker-flags win-gcc-linker-flags)
		  (current-make-link-input-strings (lambda (s) (list s)))
		  (current-make-link-output-strings win-gcc-link-output-strings)
		  (current-standard-link-libraries (make-win-link-libraries #t)))]
	 [(msvc) (let ([f (find-executable-path "cl.exe" "cl.exe")])
		   (unless f
		     (error 'use-standard-linker "cannot find MSVC's cl.exe"))
		   (current-extension-linker f)
		   (current-extension-linker-flags msvc-linker-flags)
		   (current-make-link-input-strings (lambda (s) (list s)))
		   (current-make-link-output-strings msvc-link-output-strings)
		   (current-standard-link-libraries (make-win-link-libraries #f)))]
	 [else (bad-name name)])]
      [(macos)
       (case name
	 [(cw) (current-extension-linker #f)
	       (current-extension-linker-flags null)
	       (current-make-link-input-strings (lambda (s) (list s)))
	       (current-make-link-output-strings (lambda (s) (list "-o" s)))
	       (current-standard-link-libraries (get-unix/macos-link-libraries))]
	 [else (bad-name name)])]))
  
  (include "macinc.ss")
  
  (define (macos-link quiet? input-files output-file)
    (macos-make 'link-extension "linking-project" "so" quiet? 
                input-files output-file null))
  
  (define link-extension
    (case (system-type)
      [(unix beos windows) unix/windows-link]
      [(macos) macos-link])))

