#!/bin/sh

string=? ; exec mred -qvf $0

(require-library "function.ss")
(require-library "compile.ss" "compiler")
(require-library "pretty.ss")
(require-library "macro.ss")

(define framework-dir (collection-path "framework"))
(define classhack "classhack")
(define classhack.so (build-path framework-dir (string-append classhack ".so")))

(unless (file-exists? classhack.so)
  (let ([classhack.c (build-path (collection-path "mzlib") 'up 'up "tests" "mred" (string-append classhack ".c"))])
    (require-library "compiler.ss" "compiler")
    (compile-c-extensions (list classhack.c) framework-dir)))

(load-extension classhack.so) ;; now we have interface->names and class->names

(define (class-name->interface-name f)
  (let* ([str (symbol->string f)]
	 [len (string-length str)])
    (when (<= len 1)
      (error 'class-name->interface-name "empty name"))
    (unless (char=? (string-ref str (- len 1)) #\%)
      (error 'class-name->interface-name "~a is a class"))
    (string->symbol
     (string-append
      (substring str 0 (- len 1))
      "<%>"))))

(define (skipped-name? name)
  (with-handlers ([void (lambda (exn)
			  (printf "skipping ~a (~a)~n" name (exn-message exn))
			  #t)])
    (eval `(class-asi ,name))
    #f))

(define signature-names (vector->list (signature->symbols mred^)))

(define class-names (foldl (lambda (f l)
			     (if (and (class? (global-defined-value f))
				      (not (defined? (class-name->interface-name f)))
				      (not (skipped-name? f)))
				 (cons f l)
				 l))
			   null
			   signature-names))
(define interface-names (map class-name->interface-name class-names))
(define leftover-names
  (foldl (lambda (n l)
	   (if (member n class-names)
	       l
	       (cons n l)))
	 null
	 signature-names))

(define (build-interface class-name)
  (printf "building interface for ~a~n" class-name)
  `(define ,(class-name->interface-name class-name)
     (interface () ,@(class->names (global-defined-value class-name)))))

(define (add-export-prefix x)
  (string->symbol (string-append "-" (symbol->string x))))

(define (build-new-class class-name)
  `(define ,(add-export-prefix class-name)
     (class* ,class-name (,(class-name->interface-name class-name)) args
       (sequence
	 (apply super-init args)))))

(define new-signature-definition
  `(define-signature mred-interfaces^
     (,@(append signature-names
		interface-names))))

(define new-unit-definition
  `(define mred-interfaces@
     (compound-unit/sig
      (import)
      (link [mred : mred^ (mred@)]
	    [interfaces
	     :
	     (,@(append interface-names class-names))
	     ((unit/sig (,@(append interface-names class-names))
		(import mred^)
		(rename ,@(map (lambda (x) `[,(add-export-prefix x) ,x]) class-names))

		,@(append
		   (map build-interface class-names)
		   (map build-new-class class-names)))
	      mred)])
      (export (open (mred : (,@leftover-names)))
	      (open interfaces)))))

(printf "Added ~a interface definitions~n" (length interface-names))

(call-with-output-file (build-path framework-dir "mred-interfaces.ss")
  (lambda (port)
    (pretty-print new-signature-definition port)
    (newline port)
    (pretty-print new-unit-definition port))
  'truncate)