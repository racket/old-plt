(module makefile mzscheme
  (require (lib "make.ss" "make")
           (prefix dynext: (lib "compile.ss" "dynext"))
           (all-except (lib "file.ss" "dynext") append-c-suffix)
           (prefix dynext: (lib "link.ss" "dynext"))
           (lib "file.ss")
	   (lib "pretty.ss"))

  (provide pre-installer)
  
  (define X11-include 
    (if (eq? (system-type) 'unix)
        '("/usr/X11R6/include" "/usr/X/include")
        '()))
  
  (define dir (build-path "compiled" "native" (system-library-subpath)))
  
  (define (append-h-suffix s)
    (string-append s ".h"))
  
  (define (append-c-suffix s)
    (string-append s ".c"))
  
  (define (delete/continue x)
    (with-handlers ([(lambda (x) #t) void])
      (delete-file x)))

  (define (get-precompiled-path file.so)
    (printf "~a~n" file.so)
    (let*-values (((path name _) (split-path file.so))
                  ((path d1 _) (split-path path))
                  ((path d2 _) (split-path path))
                  ((path c _) (split-path path)))
      (build-path (cond
                    ((eq? 'relative path) 'same)
                    (else path))
                  "precompiled" d2 d1 name)))
  
  (define (do-copy file.so)
    (let ([pre-compiled (get-precompiled-path file.so)])
      (and (file-exists? pre-compiled)
           (begin
             (printf "  Copying ~a~n       to ~a~n" pre-compiled file.so)
             (when (file-exists? file.so) 
               (delete-file file.so))
             (copy-file pre-compiled file.so)))))

  (define (compile-c-to-so file file.c file.so home)
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
          (dynext:compile-extension #f 
                                    file.c
                                    file.o
                                    `(,@X11-include ,(build-path home "collects" "compiler")))
          (dynext:link-extension #f 
                                 (list file.o)
                                 file.so)
          (delete/continue file.o)))))
    
  (define (build-names str)
    (list str
          (build-path dir (append-extension-suffix str))
          (append-c-suffix str)
          (append-h-suffix str)))
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
           (file.h (cadddr names)))
      `((,file.so (,file.c ,file.h
                   ,@(let ((p (get-precompiled-path file.so)))
                       (cond
                         ((file-exists? p) (list p))
                         (else null))))
         ,(lambda () (compile-c-to-so file file.c file.so home)))
        (,file.c ("gl-vectors/gl-vector.c" ,@mz-headers)
         ,(lambda ()
            (delete/continue file.c)
            (generate "gl-vectors/gl-vector.c" file.c replacements)))
        (,file.h ("gl-vectors/gl-vector.h")
         ,(lambda ()
            (delete/continue file.h)
            (generate "gl-vectors/gl-vector.h" file.h replacements))))))
    
  (define (make-gl-prims file-name vecs home)
    (let* ((names (build-names file-name))
           (file (car names))
           (file.so (cadr names))
           (file.c (caddr names))
           (vec-sos (map (lambda (v)
                           (build-path "gl-vectors"
                                       (cadr (build-names v))))
                         vecs)))
      `((,file.so (,file.c "gl-prims.h" ,@vec-sos ,@mz-headers
                   ,@(let ((p (get-precompiled-path file.so)))
                       (cond
                         ((file-exists? p) (list p))
                         (else null))))
                  ,(lambda ()
                     (compile-c-to-so file file.c file.so home))))))
      
  
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
          ,@(map (lambda (x)
                   (build-path "gl-vectors" (cadr (build-names (car x)))))
                 vec-names)
          ,(cadr (build-names "gl-prims"))
          ,(cadr (build-names "gl-prims-unsafe")))))))

  )
