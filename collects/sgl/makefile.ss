(module makefile mzscheme
  (require (lib "make.ss" "make")
           (prefix dynext: (lib "compile.ss" "dynext"))
           (all-except (lib "file.ss" "dynext") append-c-suffix)
           (prefix dynext: (lib "link.ss" "dynext"))
           (lib "file.ss")
	   (lib "pretty.ss")
	   (lib "restart.ss")
	   (lib "launcher.ss" "launcher")
           (lib "13.ss" "srfi")
           "make-gl-info.ss")

  (provide pre-installer)
    
  (define dir (build-path "compiled" "native" (system-library-subpath #f)))
  (define 3mdir (build-path dir "3m"))

  (define use-3m?
    (and (memq (system-type) '(unix macosx windows))
	 (memq '3m (available-mzscheme-variants))
	 (directory-exists? (build-path (collection-path "sgl")
					'up 'up "src" "mzscheme" "gc2"))))
  
  (define (append-h-suffix s)
    (string-append s ".h"))
  
  (define (append-c-suffix s)
    (string-append s ".c"))
  
  (define (delete/continue x)
    (with-handlers ([(lambda (x) #t) void])
      (delete-file x)))

  (define (get-precompiled-path file.so)
    (let loop ([path-in file.so])
      (let*-values (((path name _) (split-path path-in)))
        (if (string=? (path->string name) "compiled")
	    (build-path (cond
			 ((eq? 'relative path) 'same)
			 (else path))
			"precompiled")
	    (build-path (loop path) name)))))
  
  (define (do-copy file.so)
    (let ([pre-compiled (get-precompiled-path file.so)])
      (and (file-exists? pre-compiled)
           (begin
             (printf "  Copying ~a~n       to ~a~n" pre-compiled file.so)
             (when (file-exists? file.so) 
               (delete-file file.so))
             (copy-file pre-compiled file.so)))))

  (define (parse-includes s)
    (map (lambda (s)
           (substring s 2 (string-length s)))
         (string-tokenize s)))
    
  (define (get-args which-arg home)
    (let ((fp (build-path home "lib" "buildinfo")))
      (cond
        ((file-exists? fp)
         (call-with-input-file fp
           (lambda (i)
             (let loop ((l (read-line i)))
               (cond
                 ((eof-object? l) "")
                 (else
                  (let ((m (regexp-match (format "^~a=(.*)$" which-arg) l)))
                    (if m
                        (cadr m)
                        (loop (read-line i))))))))))
        (else ""))))
       
        
               
  (define (compile-c-to-so file file.c file.so home variant)
    (unless (do-copy file.so)
      (parameterize ((dynext:current-extension-compiler-flags
                      (append
                       (dynext:current-extension-compiler-flags)
                       (case (system-type)
                         ((windows) (if (eq? variant 'normal)
					'("/FIwindows.h")
					null))
                         (else '()))))
                     (dynext:current-standard-link-libraries
                      (append
                       (dynext:current-standard-link-libraries)
                       (case (system-type)
                         ((windows) (list "opengl32.lib" "glu32.lib"))
                         (else '())))))
        (let ((file.o (append-object-suffix file)))
	  (parameterize ([dynext:compile-variant variant]
			 [dynext:link-variant variant])
	    (dynext:compile-extension #f 
				      file.c
				      file.o
				      `(,@(parse-includes (get-args "X_CFLAGS" home))
                                          ,(build-path home "collects" "compiler")))
	    (dynext:link-extension #f 
				   (list file.o)
				   file.so)
	    (delete/continue file.o))))))

  (define (xform home src dest use-precomp precomp?)
    (let-values ([(base name dir?) (split-path dest)])
      (make-directory* base))
    (parameterize ([current-directory (collection-path "sgl")])
      (when (and use-precomp
		 (not precomp?)
		 (eq? 'windows (system-type))
		 (not (file-exists? use-precomp)))
	(let ([precomp.c (let-values ([(base name dir?) (split-path use-precomp)])
			   (build-path base "precomp.c"))])
	  (make-directory* 3mdir)
	  (with-output-to-file precomp.c
	    (lambda () (newline))
	    'truncate/replace)
	  (xform home precomp.c use-precomp #f #t)))
      (restart-mzscheme #() 
			(lambda (x) x)
			(list->vector 
			 (append
			  (list
			   "-qr"
			   (path->string
			    (build-path 'up 'up "src" "mzscheme" "gc2" "xform.ss")))
			  (if (eq? 'windows (system-type))
			      (cond
			       [precomp? (list "--precompile")]
			       [use-precomp
				(list "--precompiled" (path->string use-precomp))]
			       [else null])
			      null)
			  (list
			   (let* ([fix-path (lambda (s)
					      (regexp-replace* " " (path->string s) "\" \""))]
				  [inc (build-path 'up 'up "include")]
				  [extras (apply string-append
						 (format " ~a~a" 
							 (if (eq? 'windows (system-type))
							     " /I"
							     " -I")
							 (fix-path inc))
						 (map (lambda (p)
							(format 
							 " ~a~s"
							 (if (eq? 'windows (system-type))
							     " /I"
							     " -I")
							 (fix-path
							  (build-path p "include"))))
						      (parse-includes (get-args "X_CFLAGS" home))))])
			     (if (eq? 'windows (system-type))
				 (format "cl.exe /MT /E /FIwindows.h ~a" 
					 extras)
				 (format "gcc -E ~a~a" 
					 (if (eq? 'macosx (system-type))
					     "-DOS_X "
					     "")
					 extras)))
			   (if (path? src) (path->string src) src)
			   (if (path? dest) (path->string dest) dest))))
			void)))

  (define (build-names str)
    (list str
          (build-path dir (append-extension-suffix str))
          (append-c-suffix str)
          (append-h-suffix str)
          (build-path 3mdir (append-c-suffix str))
          (build-path 3mdir (append-extension-suffix str))))

  (define mz-headers
    (let ([inc-dir (build-path (collection-path "mzlib") 'up 'up "include")])
      (list (build-path inc-dir "scheme.h")
	    (build-path inc-dir "schvers.h"))))
  

  (define (make-gl-info-rules file-name home)
    (let* ((names (build-names file-name))
           (file (car names))
           (file.so (cadr names))
           (file.c (caddr names))
           (file3m.c (list-ref names 4))
           (file3m.so (list-ref names 5)))
      `((,file.so (,@(let ((p (get-precompiled-path file.so)))
                       (cond
                         ((file-exists? p) (list p))
                         (else `(,file.c ,@mz-headers)))))
         ,(lambda ()
	    (compile-c-to-so file file.c file.so home 'normal)))
	(,file3m.so (,@(let ((p (get-precompiled-path file3m.so)))
			 (cond
			  ((file-exists? p) (list p))
			  (else `(,file3m.c ,@mz-headers)))))
	 ,(lambda ()
	    (compile-c-to-so file file3m.c file3m.so home '3m)))
	(,file3m.c (,file.c ,@mz-headers)
	 ,(lambda ()
	    (delete/continue file3m.c)
	    (xform home file.c file3m.c #f #f))))))
               
  (define (use-names names)
    ;; Extract all .so names to build
    (if use-3m?
	(list (cadr names) (list-ref names 5))
	(list (cadr names))))

  (define (pre-installer home)
    (parameterize ((current-directory (collection-path "sgl"))
		   (make-print-reasons #f)
		   (make-print-checking #f))
      (make/proc
       `((,dir () ,(lambda () (make-directory* dir)))
         ("gl-info.c" ("make-gl-info.ss") ,(lambda () (make-gl-info "gl-info.c")))
         ,@(make-gl-info-rules "gl-info" home))
       (list->vector
        `(,dir "gl-info.c" ,@(use-names (build-names "gl-info")))))))
         
  )
