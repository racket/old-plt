(unit/sig dynext:link^ (import)
  
  (define include-dir (collection-path "mzscheme" "include"))

  (define (get-windows-linker)
    (or (find-executable-path "cl.exe" "cl.exe")
	(find-executable-path "ld.exe" "ld.exe")))

  (define (get-unix-linker)
    (or (getenv "MZSCHEME_DYNEXT_LINKER")
	(let ([s (case (string->symbol (system-library-subpath))
		   [(rs6k-aix) "cc"]
		   [else "ld"])])
	  (find-executable-path s s))))
  
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
  
  (define (still-win-gcc?)
    (and (eq? 'windows (system-type))
	 (let ([c (current-extension-linker)])
	   (and c (regexp-match "ld.exe$" c)))))

  (define win-gcc? (still-win-gcc?))
  
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
  
  (define std-library-dir (build-path (collection-path "mzscheme" "lib") 
				      (system-library-subpath)))
  
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
  
  (define (make-win-gcc-temp suffix)
    (let ([d (find-system-path 'temp-dir)])
      (let loop ([n 1])
	(let ([f (build-path d (format "tmp~a.~a" n suffix))])
	  (if (file-exists? f)
	      (loop (add1 n))
	      f)))))

  (define win-gcc-link-output-strings (lambda (s) (list "--base-file"
							(make-win-gcc-temp "base")
							"-e" "_dll_entry@12" 
							"-o" s)))
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
	    (let* ([output-strings
		    ((current-make-link-output-strings) out)]
		   [libs (current-standard-link-libraries)]
		   [command 
		    (append 
		     (list c)
		     (current-extension-linker-flags)
		     (apply append (map (lambda (s) ((current-make-link-input-strings) s))
					in))
		     libs
		     output-strings)])
	      (unless quiet? 
		(printf "link-extension: ~a~n" command))
	      (stdio-link (lambda (quiet?)
			    (apply my-process* command))
			  quiet?)

	      ;; Stange Cygwin system for relocatable DLLs: we run dlltool twice and
	      ;;  ld three times total
	      (when (still-win-gcc?)
		(let ([dlltool (find-executable-path "dlltool.exe" "dlltool.exe")]
		      ;; Find base-file name we already made up:
		      [basefile (let ([m (member "--base-file" output-strings)])
				  (and m (cadr m)))]
		      ;; Make new exp file name:
		      [expfile (make-win-gcc-temp "exp")])
		  (when (and dlltool basefile)
		    (let* ([dll-command
			    ;; Generate DLL link information
			    `("--dllname" ,out 
					  "--def" ,(build-path std-library-dir "gcc" "mzdyn.def")
					  "--base-file" ,basefile
					  "--output-exp" ,expfile)]
			   ;; Command to link with new .exp, re-create .base:
			   [command1
			    (map (lambda (s)
				   (if (regexp-match "[.]exp$" s)
				       expfile
				       s))
				 command)]
			   ;; Command to link with new .exp file, no .base needed:
			   [command2
			    (let loop ([l command1])
			      (cond
			       [(null? l) null]
			       [(string=? (car l) "--base-file")
				(cddr l)]
			       [else (cons (car l) (loop (cdr l)))]))])
		      (unless quiet?
			(printf "link-extension, dlltool phase: ~a~n" 
				(cons dlltool dll-command)))
		      (stdio-link (lambda (quiet?) 
				    (apply my-process* dlltool dll-command))
				  quiet?)
		      (unless quiet?
			(printf "link-extension, re-link phase: ~a~n" 
				command1))
		      (stdio-link (lambda (quiet?) 
				    (apply my-process* command1))
				  quiet?)
		      (unless quiet?
			(printf "link-extension, re-dlltool phase: ~a~n" 
				(cons dlltool dll-command)))
		      (stdio-link (lambda (quiet?)
				    (apply my-process* dlltool dll-command))
				  quiet?)
		      (unless quiet?
			(printf "link-extension, last re-link phase: ~a~n" 
				command2))
		      (stdio-link (lambda (quiet?)
				    (apply my-process* command2))
				  quiet?)
		      (delete-file basefile)
		      (delete-file expfile))))))
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

