#!/bin/sh

string=? ; exec mzscheme -x -qge "(define name \"$0\")" -r $0 "$@"

;(require-library "function.ss")

(define default-collections-to-compile
  (let ([tools-collections
	 (let ([tools-path (build-path (collection-path "drscheme" "tools"))])
	   (let loop ([in (directory-list tools-path)]
		      [out null])
	     (cond
	      [(null? in) out]
	      [else (let ([x (car in)])
		      (if (or (string=? x "CVS")
			      (string=? x "RCS"))
			  (loop (cdr in) out)
			  (loop (cdr in)
				(cons (list "drscheme" "tools" x)
				      out))))])))]
	[other-collections
	 (list (list "cogen")
	       (list "drscheme")
	       (list "graphics")
	       (list "gusrspce")
	       (list "mred")
	       (list "mzlib")
	       (list "mzos")
	       (list "system")
	       (list "userspce")
	       (list "zodiac"))])
   (append other-collections
	   tools-collections)))

(define (delete-directory/files path)
  (cond
    [(directory-exists? path)
     (and (andmap (lambda (e) (delete-directory/files (build-path path e)))
		  (directory-list path))
	  (delete-directory path))]
    [(file-exists? path)
     (delete-file path)]
    [else (error 'delete-directory/files
		 "encountered ~a, neither a file nor a directory"
		 path)]))

;; delete mzlib first...
(when (member "--clean" (vector->list argv))
  (let* ([collection-path (with-handlers ([(lambda (x) #t)
					   (lambda (x) #f)])
			    (collection-path "mzlib"))]
	 [path (build-path collection-path "compiled")])
    (when (directory-exists? path)
      (printf "deleting ~a~n" path)
      (delete-directory/files path))))

(require-library "cmdline.ss")
(require-library "make.ss" "make")
(require-library "compile.ss" "compiler")

(define verbose (make-parameter #f))

(define flags
  (parse-command-line
   name
   argv
   `((once-each
      [("--clean")
       ,(lambda (flag) 'clean)
       ("Delete all existing compiled files")]
      [("-d" "--no-default")
       ,(lambda (flag) 'no-default)
       ("Do not use default collections")]
      [("-n" "--no-zo")
       ,(lambda (flag) 'no-zo)
       ("Do not produce .zo files.")]
      [("-s" "--so")
       ,(lambda (flag) 'so)
       ("Produce .so files.")]
      [("-r" "--so-verbose")
       ,(lambda (flag)
	  (verbose #t)
	  (compiler:option:verbose #t))
       ("See make and compiler verbose messages")]
      [("-v" "--verbose")
       ,(lambda (flag)
	  (verbose #t))
       ("See make and compiler usual messages")])
     (multi
      [("-l" "--collection")
       ,(lambda (flag collection) (list collection))
       ("Also compile <collection>"
	"collection")]))
   (lambda (flag-accum) flag-accum)
   '()))

(define clean? (member 'clean flags))
(define zo? (not (member 'no-zo flags)))
(define so? (member 'so flags))
(define use-default? (not (member 'no-default flags)))

(define collections-to-compile
  (let ([command-line-collections
	 (let loop ([in flags]
		    [out null])
	   (cond
	    [(null? in) out]
	    [else (let ([x (car in)])
		    (if (list? x)
			(loop (cdr in) (cons x out))
			(loop (cdr in) out)))]))])
    (if use-default?
	(append command-line-collections default-collections-to-compile)
	command-line-collections)))

(define control-io-apply
  (let ([op (make-output-port void void)])
    (lambda (f args)
      (if (verbose)
	  (apply f args)
	  (parameterize ([current-output-port op])
	    (apply f args))))))

(when clean?
  (for-each (lambda (collection-list)
	      (let* ([path (build-path (apply collection-path collection-list)
				       "compiled")])
		(when (directory-exists? path)
		  (printf "deleting ~a~n" path)
		  (delete-directory/files path))))
	    collections-to-compile))

(when zo?
  (for-each (lambda (collection-list)
	      (printf "zo compiling ~a~n" collection-list)
	      (control-io-apply compile-collection-zos collection-list))
	    collections-to-compile))

(when so?
  (for-each (lambda (collection-list)
	      (printf "so compiling ~a~n" collection-list)
	      (control-io-apply compile-collection-extension collection-list))
	    collections-to-compile))

