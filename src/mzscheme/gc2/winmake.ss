
(require-library "restart.ss")

(define (system- s)
  (fprintf (current-error-port) "~a~n" s)
  (system s))

(define srcs
  '("salloc"
    "bignum"
    "bool"
    "builtin"
    "char"
    "complex"
    "dynext"
    "env"
    "error"
    "eval"
    "file"
    "fun"
    "hash"
    "image"
    "list"
    "network"
    "numarith"
    "number"
    "numcomp"
    "numstr"
    "objclass"
    "object"
    "port"
    "portfun"
    "print"
    "process"
    "promise"
    "rational"
    "read"
    "regexp"
    "sema"
    "setjmpup"
    "string"
    "struct"
    "symbol"
    "syntax"
    "type"
    "unit"
    "unitsig"
    "vector"))

(define (try src deps dest objdest includes)
  (unless (and (file-exists? dest)
	       (let ([t (file-or-directory-modify-seconds dest)])
		 (andmap
		  (lambda (dep)
		    (> t (file-or-directory-modify-seconds dep)))
		  deps)))
    (unless (restart-mzscheme #() (lambda (x) x)
			      (vector "-r"
				      "xform.ss"
				      "ctok.ss"
				      (format "cl.exe /MT /E ~a" includes)
				      src
				      dest)
			      void)
      (when (file-exists? dest)
	(delete-file dest))
      (error "error xforming")))
  (compile dest objdest null ""))

(define (compile c o deps flags)
  (unless (and (file-exists? o)
	       (let ([t (file-or-directory-modify-seconds o)])
		 (and (>= t (file-or-directory-modify-seconds c))
		      (andmap
		       (lambda (f)
			 (>= t (file-or-directory-modify-seconds f)))
		       deps))))
    (unless (system- (format "cl.exe ~a /MT /Zi /O2 /c ~a /Fdxsrc/ /Fo~a" flags c o))
      (error "failed compile"))))

(define common-deps (list "xform.ss" "ctok.ss"))
(define (find-obj f d) (format "../../worksp/~a/release/~a.obj" d f))

(for-each
 (lambda (x)
   (try (format "../src/~a.c" x)
	(list* (find-obj x "mzsrc")
	       (format "../src/~a.c" x)
	       common-deps)
	(format "xsrc/~a.c" x)
	(format "xsrc/~a.obj" x)
	"/I ../include"))
 srcs)

(try "../main.c"
     (list* (find-obj "main" "mzscheme")
	    "../main.c"
	    common-deps)
     "xsrc/main.c"
     "xsrc/main.obj"
     "/I ../include")

(compile "gc2.c" "xsrc/gc2.obj" '("compact.c") "")
(compile "../src/mzsj86.c" "xsrc/mzsj86.obj" '() "/I ../include")

(define exe "mzpgc.exe")

(define libs "kernel32.lib user32.lib wsock32.lib")

(let ([objs (list*
	     "xsrc/main.obj"
	     "xsrc/gc2.obj"
	     "xsrc/mzsj86.obj"
	     (map
	      (lambda (n)
		(format "xsrc/~a.obj" n))
	      srcs))])
  (let ([ms (if (file-exists? exe)
		(file-or-directory-modify-seconds exe)
		0)])
    (when (ormap
	   (lambda (f)
	     (> (file-or-directory-modify-seconds f)
		ms))
	   objs)
      (unless (system- (format "cl.exe /MT /Zi /Fe~a ~a ~a"
			       exe
			       (let loop ([objs objs])
				 (if (null? objs)
				     ""
				     (string-append
				      (car objs)
				      " "
				      (loop (cdr objs)))))
			       libs))
	(error "link failed")))))

				 