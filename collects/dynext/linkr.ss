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
       [(unix) (get-unix-linker)]
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
      [(sparc-solaris) (list "-G")]
      [(sparc-sunos4) (list "-Bdynamic")]
      [(i386-freebsd) (list "-Bshareable")]
      [(rs6k-aix) (let ([version (read (car (process* "/usr/bin/uname" "-v")))])
		    (list "-bM:SRE"
			  (format "-bI:~a/mzscheme.exp" include-dir)
			  (format "-bE:~a/ext.exp" include-dir)
			  (if (= 3 version)
			      "-e _nostart"
			      "-bnoentry")))]
      [(parisc-hpux) "-b"]
      [else (list "-shared")]))
  
  (define current-extension-linker-flags
    (make-parameter
     (case (system-type)
       [(unix) (get-unix-link-flags)]
       [(windows) (if win-gcc?
		      (list "--dll")
		      (list "/LD"))]
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
  
  (define current-make-link-output-strings
    (make-parameter
     (case (system-type)
       [(unix) (lambda (s) (list "-o" s))]
       [(windows) (lambda (s) (if win-gcc?
				  (list "-e" "_dll_entry@12" "-o" s)
				  (list (string-append "/Fe" s))))]
       [(macos) (lambda (s) (list "-o" s))])
     (lambda (p)
       (unless (procedure-arity-includes? p 1)
	 (raise-type-error 'current-make-link-output-strings "procedure of arity 1" p))
       p)))
  
  (define current-standard-link-libraries
    (make-parameter
     (case (system-type)
       [(unix macos) (list (build-path std-library-dir "mzdyn.o"))]
       [(windows) (let ([file (lambda (f)
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
					"mzdyn.obj"))))])
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
  
  (include "macinc.ss")
  
  (define (macos-link quiet? input-files output-file)
    (macos-make 'link-extension "linking-project" "so" quiet? 
                input-files output-file null))
  
  (define link-extension
    (case (system-type)
      [(unix windows) unix/windows-link]
      [(macos) macos-link])))

