#! /bin/sh

string=? ; exec mzscheme -f "$0" "$@"

;; Library functions

(require-library "file.ss")

;; path-list->path-list-string : (listof String) -> String
(define (path-list->path-list-string paths)
  (cond
    [(null? paths) ""]
    [else
     ;; loop : (cons String (listof String)) -> String
     (let loop ([paths paths])
       (cond
         [(null? (cdr paths)) (car paths)]
         [else (string-append (car paths) ":" (loop (cdr paths)))]))]))

;; delete-file-maybe : String -> Void
(define (delete-file-maybe path)
  (when (file-exists? path)
    (delete-file path)))

;; delete-directory*-maybe : String -> Void
(define (delete-directory*-maybe dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))

;; clobber-file : String String -> Void
(define (clobber-file from to)
  (delete-file-maybe to)
  (copy-file from to))

; Tools

;; gen-external : String -> (String* -> Boolean)
(define (gen-external name)
  (lambda args
    (printf "~a " name)
    (for-each (lambda (arg) (printf "~a " arg)) args)
    (printf "~n~n")
    (apply system* name args)))

(define compile-c (gen-external "/usr/local/bin/gcc"))
(define ld (gen-external "/usr/ccs/bin/ld"))
(define chmod (gen-external "/usr/bin/chmod"))
(define java-bin (build-path "/usr" "site" "jdk-1.2.01" "bin"))
(define compile-java (gen-external (build-path java-bin "javac")))
(define jar (gen-external (build-path java-bin "jar")))

;; This needs to change to something not in my home directory tree.
(define jdk-base "/home/ptg/.bin/kaffe-1.0b4")

(define install-dir (collection-path "drjava"))
(define I-flags (list "-I" (build-path jdk-base "include" "kaffe")
		      "-I" (build-path (collection-path "mzscheme") "include")))

(define mzdyn-dot-o
  (build-path (collection-path "mzscheme") "lib" (system-library-subpath) "mzdyn.o"))
(define jdk-libs (build-path jdk-base "lib"))

(define my-libs (build-path install-dir "compiled" "native" (system-library-subpath)))
(define so-path
  (path-list->path-list-string
   (list
    my-libs
    (build-path jdk-libs "kaffe")
    jdk-libs
    (build-path "/usr" "site" "gcc-2.8.1" "lib" "gcc-lib" "sparc-sun-solaris2.5.1" "2.8.1"))))

(define c-flags (cons (format "-DKAFFE=~s" jdk-base) I-flags))
(define ld-flags '("-lkaffevm" "-lsocket" "-lc" "-lnative" "-lgcc" "-lio"))
(define tmp-dir "classes")

; Targets and Sources
(define mzjvm "libmzjvm.so")
(define embed-obj "hello.o")
(define embed-src "hello.c")

(define scheme-val-obj "SchemeValue.o")
(define scheme-val "libSchemeValue.so")
(define scheme-val-src "SchemeValue.c")

(define objs (list embed-obj scheme-val-obj))
(define sos (list mzjvm scheme-val))

(define drjava-jar (build-path install-dir "jars" "DrJava.jar"))
(define launcher-name (build-path (collection-path "drjava") 'up 'up "bin" "drjava"))

;; build : -> Void
(define (build)
  (clobber)
  (build-sos)
  (build-classfiles))

;; install assumes build has run
;; install : -> Void
(define (install)
  (unless (directory-exists? my-libs)
    (make-directory* my-libs))
  (for-each (lambda (so) (clobber-file so (build-path my-libs so))) sos)
  (parameterize ([current-directory tmp-dir])
    (apply jar "-cvf"
	   drjava-jar
	   (directory-list)))
  (install-launcher))

;; build-classfiles : -> Void
(define (build-classfiles)
  (make-directory tmp-dir)
  (let ([src-dir (build-path "java" "edu" "rice" "cs" "drj")])
    (apply compile-java "-d" "classes"
	   (filter file-exists?
		   (map (lambda (f) (build-path src-dir f))
			(directory-list src-dir))))))

;; build-sos : -> Void
(define (build-sos)
  (load "gen-wrappers.ss")
  (build-so embed-src embed-obj "-o" mzjvm)
  (build-so scheme-val-src scheme-val-obj "-lmzjvm" "-o" scheme-val))

;; build-so : String String (listof String) -> Void
(define (build-so src obj . rest)
  (apply compile-c (list* "-c" src c-flags))
  (apply ld
	 (list* "-G" obj mzdyn-dot-o "-R" so-path "-Y"
		(string-append "P,.:/usr/lib:" so-path)
		(append ld-flags rest))))

;; nuke : -> Void
(define (nuke) (uninstall) (clobber))

;; clean : -> Void
(define (clean)
  (for-each delete-file-maybe
	    (list* "generated.c" "SchemeValue-gen.c" objs)))

;; clobber : -> Void
(define (clobber)
  (clean)
  (for-each delete-file-maybe sos)
  (delete-directory*-maybe tmp-dir))

;; uninstall : -> Void
(define (uninstall)
  (for-each delete-file-maybe
	    (list* drjava-jar launcher-name
		   (map (lambda (so) (build-path my-libs so)) sos)))
  (delete-directory*-maybe (build-path (collection-path "drjava") "compiled")))
  
;; install-launcher
(define (install-launcher)
  (let ([out (open-output-file launcher-name 'truncate)])
    (fprintf out
"#! /bin/sh

string=? ; LD_LIBRARY_PATH=~a export LD_LIBRARY_PATH ; exec $PLTHOME/bin/mred -g -q -l core.ss -v -m -f \"$0\" -e '(yield (make-semaphore))' < /dev/null

(define libpath (getenv \"LD_LIBRARY_PATH\"))

(define classpath
  (let ([jar-path (lambda (jar) (build-path (collection-path \"drjava\") \"jars\" jar))])
    (string-append
     (jar-path \"gjc.jar\") \":\" (jar-path \"DrJava.jar\") \":\" (jar-path \"Klasses.jar\"))))

(putenv \"CLASSPATH\" classpath)
(putenv \"GJC_PATH\" classpath)

(require-library \"start.ss\" \"drjava\")
"
     so-path)
    (close-output-port out))
  (chmod "755" launcher-name))
