(module makefile mzscheme
  (require (lib "make.ss" "make")
           (prefix dynext: (lib "compile.ss" "dynext"))
           (all-except (lib "file.ss" "dynext") append-c-suffix)
           (prefix dynext: (lib "link.ss" "dynext"))
           (lib "file.ss")
	   (lib "pretty.ss")
	   (lib "restart.ss")
	   (lib "launcher.ss" "launcher"))

  (provide pre-installer)
  
  (define X11-include 
    (if (eq? (system-type) 'unix)
        '("/usr/X11R6/include" "/usr/X/include")
        '()))
  
  (define dir (build-path "compiled" "native" (system-library-subpath #f)))
  (define 3mdir (build-path dir "3m"))

  (define use-3m?
    (and (memq (system-type) '(unix macosx windows))
	 (memq '3m (available-mzscheme-variants))
	 (directory-exists? (build-path 'up 'up "src" "mzscheme" "gc2"))))
  
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

  (define (compile-c-to-so file file.c file.so home variant)
    (unless (do-copy file.so)
      (parameterize ((dynext:current-extension-compiler-flags
                      (append
                       (dynext:current-extension-compiler-flags)
                       (case (system-type)
                         ((windows) '("/FIwindows.h"))
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
				      `(,@X11-include ,(build-path home "collects" "compiler")))
	    (dynext:link-extension #f 
				   (list file.o)
				   file.so)
	    (delete/continue file.o))))))

  (define (xform src dest)
    (make-directory* 3mdir)
    (parameterize ([current-directory (collection-path "sgl")])
      (restart-mzscheme #() 
			(lambda (x) x)
			(list->vector 
			 (list
			  "-qr"
			  (path->string
			   (build-path 'up 'up "src" "mzscheme" "gc2" "xform.ss"))
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
						     X11-include))])
			    (if (eq? 'windows (system-type))
				(format "cl.exe /MT /E ~a" 
					extras)
				(format "gcc -E ~a~a" 
					(if (eq? 'macosx (system-type))
					    "-DOS_X "
					    "")
					extras)))
			  (if (path? src) (path->string src) src)
			  (if (path? dest) (path->string dest) dest)))
			void)))

  (define (build-names str)
    (list str
          (build-path dir (append-extension-suffix str))
          (append-c-suffix str)
          (append-h-suffix str)
          (build-path 3mdir (append-c-suffix str))
          (build-path 3mdir (append-extension-suffix str))))
  (define (generate input-file output-file trans)
    (let ((in-str (call-with-input-file input-file
                    (lambda (in)
                      (read-string (expt 2 20) in)))))
      (let* ((out-str (regexp-replace* "<type>" in-str (car trans)))
             (out-str (regexp-replace* "<type-name>" out-str (cadr trans)))
             (out-str (regexp-replace* "<sreal-to-type>" out-str (caddr trans)))
             (out-str (regexp-replace* "<type-to-scheme>" out-str (cadddr trans))))
        (call-with-output-file output-file (lambda (x) (display out-str x)) 'replace))))

  (define mz-headers
    (let ([inc-dir (build-path (collection-path "mzlib") 'up 'up "include")])
      (list (build-path inc-dir "scheme.h")
	    (build-path inc-dir "schvers.h"))))
  
  (define (make-gl-vector file-name+replacements home)
    (let* ((file-name (car file-name+replacements))
           (replacements (cadr file-name+replacements))
           (names (map (lambda (n) (build-path "gl-vectors" n))
                       (build-names file-name)))
           (file (car names))
           (file.so (cadr names))
           (file.c (caddr names))
           (file.h (cadddr names))
           (file3m.c (list-ref names 4))
           (file3m.so (list-ref names 5)))
      (let ([so-rule (lambda (file.so file.c variant)
		       `(,file.so ,(let ((p (get-precompiled-path file.so)))
				     (cond
				      ((file-exists? p) (list p))
				      (else (list file.c file.h))))
			 ,(lambda () (compile-c-to-so file file.c file.so home variant))))])
	`(,(so-rule file.so file.c 'normal)
	  ,(so-rule file3m.so file3m.c '3m)
	  (,file.c ("gl-vectors/gl-vector.c" ,@mz-headers)
           ,(lambda ()
	      (delete/continue file.c)
	      (generate "gl-vectors/gl-vector.c" file.c replacements)))
	  (,file.h ("gl-vectors/gl-vector.h")
           ,(lambda ()
	      (delete/continue file.h)
	      (generate "gl-vectors/gl-vector.h" file.h replacements)))
	  (,file3m.c (,file.c)
	   ,(lambda ()
	      (delete/continue file3m.c)
	      (xform file.c file3m.c)))))))
    
  (define (make-gl-prims file-name vecs home)
    (let* ((names (build-names file-name))
           (file (car names))
           (file.so (cadr names))
           (file.c (caddr names))
           (file3m.c (list-ref names 4))
           (file3m.so (list-ref names 5))
           (vec-sos (map (lambda (v)
                           (build-path "gl-vectors"
                                       (cadr (build-names v))))
                         vecs)))
      `((,file.so (,@(let ((p (get-precompiled-path file.so)))
                       (cond
                         ((file-exists? p) (list p))
                         (else `(,file.c "gl-prims.h" ,@vec-sos ,@mz-headers)))))
         ,(lambda ()
	    (compile-c-to-so file file.c file.so home 'normal)))
	(,file3m.so (,@(let ((p (get-precompiled-path file3m.so)))
			 (cond
			  ((file-exists? p) (list p))
			  (else `(,file3m.c ,@mz-headers)))))
	 ,(lambda ()
	    (compile-c-to-so file file3m.c file3m.so home '3m)))
	(,file3m.c (,file.c  "gl-prims.h" ,@mz-headers)
	 ,(lambda ()
	    (delete/continue file3m.c)
	    (xform file.c file3m.c))))))
  
  (define vec-names
    '(("gl-double-vector"
       ("GLdouble" "double" "scheme_real_to_double" "scheme_make_double"))
      ("gl-float-vector"
       ("GLfloat" "float" "scheme_real_to_double" "scheme_make_double"))
      ("gl-uint-vector"
       ("GLuint" "uint" "scheme_get_uint" "scheme_make_integer_value_from_unsigned"))
      ("gl-ushort-vector"
       ("GLushort" "ushort" "scheme_get_uint" "scheme_make_integer_value_from_unsigned"))
      ("gl-ubyte-vector"
       ("GLubyte" "ubyte" "scheme_get_uint" "scheme_make_integer_value_from_unsigned"))
      ("gl-int-vector"
       ("GLint" "int" "scheme_get_int" "scheme_make_integer_value"))
      ("gl-short-vector"
       ("GLshort" "short" "scheme_get_int" "scheme_make_integer_value"))
      ("gl-byte-vector"
       ("GLbyte" "byte" "scheme_get_int" "scheme_make_integer_value"))
      ("gl-boolean-vector"
       ("GLboolean" "boolean" "scheme_get_boolean" "scheme_make_boolean"))))
             
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
         (,(build-path "gl-vectors" dir) ()
          ,(lambda ()
             (make-directory* (build-path "gl-vectors" dir))))
         ,@(apply append (map (lambda (x) (make-gl-vector x home)) vec-names))
         ,@(make-gl-prims "gl-prims" (map car vec-names) home)
         ,@(make-gl-prims "gl-prims-unsafe" (map car vec-names) home))
       (list->vector
        `(,dir
          ,(build-path "gl-vectors" dir)
          ,@(apply
	     append
	     (map (lambda (x)
		    (map (lambda (name)
			   (build-path "gl-vectors" name))
			 (use-names (build-names (car x)))))
		  vec-names))
          ,@(use-names (build-names "gl-prims"))
          ,@(use-names (build-names "gl-prims-unsafe")))))))

  )
