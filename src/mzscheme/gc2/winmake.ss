
(require-library "restart.ss")

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
				      (format "cl.exe /E ~a" includes)
				      src
				      dest)
			      void)
      (when (file-exists? dest)
	(delete-file dest))
      (error "error xforming")))
  (compile dest objdest))

(define (compile c o)
  (unless (and (file-exists? o)
	       (>= (file-or-directory-modify-seconds o)
		   (file-or-directory-modify-seconds c)))
    (unless (system (format "cl.exe /c ~a /Fo~a" c o))
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

(compile "gc2.c" "xsrc/cgc2.obj")
