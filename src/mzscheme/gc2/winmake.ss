
(require (lib "restart.ss")
	 (lib "process.ss"))

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
    "module"
    "network"
    "numarith"
    "number"
    "numcomp"
    "numstr"
    "port"
    "portfun"
    "print"
    "rational"
    "read"
    "regexp"
    "sema"
    "setjmpup"
    "string"
    "struct"
    "symbol"
    "syntax"
    "stxobj"
    "thread"
    "type"
    "vector"))

(define (try src deps dest objdest includes use-precomp extra-compile-flags)
  (unless (and (file-exists? dest)
	       (let ([t (file-or-directory-modify-seconds dest)])
		 (andmap
		  (lambda (dep)
		    (> t (file-or-directory-modify-seconds dep)))
		  deps)))
    (unless (restart-mzscheme #() (lambda (x) x)
			      (list->vector 
			       (append
				(list "-r"
				      "xform.ss")
				(if objdest
				    (if use-precomp
					(list "--precompiled" use-precomp)
					null)
				    (list "--precompile"))
				(list
				 (format "cl.exe /MT /DSCHEME_EMBEDDED_NO_DLL /E ~a" includes)
				 src
				 dest)))
			      void)
      (when (file-exists? dest)
	(delete-file dest))
      (error "error xforming")))
  (when objdest
    (compile dest objdest null extra-compile-flags)))

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

(define common-deps (list "xform.ss"))
(define (find-obj f d) (format "../../worksp/~a/release/~a.obj" d f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define wx-inc (string-append "/I ../include "
			      "/I ../gc2 "
			      "/I ../../wxwindow/include/msw "
			      "/I ../../wxwindow/include/base "
			      "/I ../../wxwindow/contrib/wxxpm/libxpm.34b/lib "
			      "/I ../../wxWindow/contrib/fafa"))
(try "wxprecomp.cxx" common-deps "xsrc/wxprecomp.h" #f wx-inc #f "")

(define (wx-try base x)
  (let ([cxx-file (format "../../~a/~a.cxx" base x)])
    (try cxx-file
	 (list* (find-obj x "wxwin")
		cxx-file
		common-deps)
	 (format "xsrc/~a.cxx" x)
	 (format "xsrc/~a.obj" x)
	 wx-inc
	 "xsrc/wxprecomp.h"
	 "-DGC2_JUST_MACROS /FI../gc2.h")))

(map (lambda (x)
       (wx-try "wxwindow/src/msw" x))
     '("wx_buttn"
       "wx_canvs"
       "wx_check"
       "wx_choic"
       "wx_clipb"
       "wx_cmdlg"
       "wx_dc"
       "wx_dialg"
       "wx_frame"
       "wx_gauge"
       "wx_gbox"
       "wx_gdi"
       "wx_item"
       "wx_lbox"
       "wx_main"
       "wx_menu"
       "wx_messg"
       "wx_panel"
       "wx_pdf"
       "wx_rbox"
       "wx_slidr"
       "wx_tabc"
       "wx_timer"
       "wx_utils"
       "wx_win"
       "wximgfil"
       "wximgxbm"))

(exit)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(try "precomp.c" common-deps "xsrc/precomp.h" #f "/I ../include /I ../src" #f "")

(for-each
 (lambda (x)
   (try (format "../src/~a.c" x)
	(list* (find-obj x "libmzsch")
	       (format "../src/~a.c" x)
	       common-deps)
	(format "xsrc/~a.c" x)
	(format "xsrc/~a.obj" x)
	"/I ../include"
	"xsrc/precomp.h"
	""))
 srcs)

(try "../main.c"
     (list* (find-obj "main" "mzscheme")
	    "../main.c"
	    common-deps)
     "xsrc/main.c"
     "xsrc/main.obj"
     "/I ../include"
     #f
     "")

(compile "gc2.c" "xsrc/gc2.obj" '("compact.c") "")
(compile "../src/mzsj86.c" "xsrc/mzsj86.obj" '() "/I ../include")

(define exe "mzscheme3m.exe")

(define libs "kernel32.lib user32.lib wsock32.lib shell32.lib")

(let ([objs (list*
	     "xsrc/main.obj"
	     "xsrc/gc2.obj"
	     "xsrc/mzsj86.obj"
	     (find-obj "gmp" "libmzsch")
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
