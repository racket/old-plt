#!/bin/sh -f
string=? ; if [ "$PLTHOME" = "" ] ; then
string=? ;  echo Please define PLTHOME 
string=? ;  exit -1
string=? ; fi
string=? ; exec ${PLTHOME}/bin/mzscheme -qr $0 "$@"

;; I have no idea whether the above is important, a rewrite, or whatever,
;; but the readline stuff has it so I left it in.
(define mach-id (string->symbol (system-library-subpath)))

(define search-path
  (list "/usr" "/usr/local/gnu" "/sw"))

(define mzssl-path
  (ormap (lambda (x)
	   (and (directory-exists? (build-path x "include" "openssl"))
		(or ;(file-exists? (build-path x "lib" "libssl.a"))
		    (file-exists? (build-path x "lib" "libssl.so"))
		    (file-exists? (build-path x "lib" "libssl.dylib")))
		(or ;(file-exists (build-path x "lib" "libcrypto.a"))
		    (file-exists? (build-path x "lib" "libcrypto.so"))
		    (file-exists? (build-path x "lib" "libcrypto.dylib")))
		x))
    search-path))

(unless mzssl-path
  (error 'ssl-installer
    "can't find ssl include files and/or library; try editing 'search-path'"))

(require (lib "make.ss" "make"))
(require (lib "link.ss" "dynext"))
(require (lib "compile.ss" "dynext"))
(require (lib "file.ss" "dynext"))

(require (lib "file.ss"))
(require (lib "list.ss"))

(make-print-checking #f)

;; Used as make dependencies:
(define header (build-path (collection-path "mzscheme") 'up 'up
		 "include" "scheme.h"))
(define version-header (build-path (collection-path "mzscheme") 'up 'up
			 "include" "schvers.h"))

(define dir (build-path "compiled" "native" (system-library-subpath)))
(define mzssl.so (build-path dir (append-extension-suffix "mzssl")))
(define mzssl.o (build-path dir (append-object-suffix "mzssl")))

;; Function used to add a command-line flag:
(define (add-flags fp flags)
  (fp (append (fp) flags)))

;; Add -I to compiler command-line
(add-flags current-extension-compiler-flags
	   (list (format "-I~a/include" mzssl-path)))

;; More platform-specific compiler flags.
(case mach-id
  [(rs6k-aix)
   (add-flags current-extension-compiler-flags
	      (list "-DNEEDS_SELECT_H"))]
  [else (void)])


;; Add -L and -l for readline:
(add-flags current-extension-linker-flags 
	   (list (format "-L~a/lib" mzssl-path)
		 "-lssl" "-lcrypto"))

; More platform-specific linker flags.
(case mach-id
  [(sparc-solaris i386-solaris)
   (add-flags current-extension-linker-flags
	      (list "-ltermcap"))]
  [(rs6k-aix)
   (add-flags current-extension-linker-flags 
	      (list "-lc"))]
  [else (void)])

;; Add the -lcurses flag:
(define (delete/continue x)
  (with-handlers ([(lambda (x) #t) void])
    (delete-file x)))

(make 
 ((mzssl.so (mzssl.o dir)
            (link-extension #f (list mzssl.o) mzssl.so))
  
  (mzssl.o ("mzssl.c" header version-header dir)
	   (compile-extension #f "mzssl.c" mzssl.o ()))

  ("clean" () (begin (delete/continue mzssl.o) (delete/continue mzssl.so)))
  
  (dir ()
       (make-directory* dir)))

 (current-command-line-arguments))
