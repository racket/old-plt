(require-library "compile.ss")

(define file-names
  '("corelate" "invoke" "link" "misc" "pattern" "back"
     "scm-core" "scm-main" "scm-obj" "scm-unit" "scm-ou" "scm-spdy"
     "sexp" "sigs" "x" "zcode" "zsigs"))

(printf "Deleting ...~n")
(for-each (lambda (f)
	    (delete-file (string-append f ".zo")))
  file-names)

(printf "Loading ...~n")
(load "load.ss")

(for-each (lambda (f)
	    (printf "Compiling ~a~n" f)
	    (compile-file (string-append f ".ss")
	      (string-append f ".zo")))
  file-names)

(printf "Done!~n")

(exit)
