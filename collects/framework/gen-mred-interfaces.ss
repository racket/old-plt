#!/bin/sh

string=? ; exec mred -magqvf $0

(require-library "function.ss")
(require-library "pretty.ss")
(require-library "macro.ss")

(load-relative "classhack.ss")

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
(unless (andmap symbol? signature-names)
  (error 'gen-mred-interfaces.ss "found non-symbol in mred^ signature"))

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
  `(define ,(class-name->interface-name class-name)
     (interface () ,@(class->names (global-defined-value class-name)))))

(define (add-export-prefix x)
  (string->symbol (string-append "-" (symbol->string x))))

(define (build-new-class class-name)
  (let* ([class (eval class-name)]
	 [interfaces (foldl (lambda (this-class-name so-far)
			      (if (subclass? class (eval this-class-name))
				  (cons (class-name->interface-name this-class-name) so-far)
				  so-far))
			    null
			    class-names)])
    `(define ,(add-export-prefix class-name)
       (class* ,class-name (,@interfaces) args
	 (sequence
	   (apply super-init args))))))

(define new-signature-definition
  `(define-signature mred-interfaces^
     ((open mred^)
      ,@interface-names)))

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

(printf "added ~a interface definitions~nmred^ has ~a names, mred-interfaces^ has ~a names~n"
	(length interface-names)
	(length signature-names)
	(+ (length signature-names) (length interface-names)))

(define cdb-file (build-path framework-dir 'up 'up "src" "doc" "framework-mred-interfaces.cdb"))
(fprintf "building cdb file: ~a~n" cdb-file)
(call-with-output-file cdb-file
  (lambda (port)
    (parameterize ([current-output-port port])
      (printf "@external framework-mred.cdb~n")
      (for-each (lambda (interface-name-sym)
		  (let* ([interface-name (symbol->string interface-name-sym)]
			 [len (string-length interface-name)]
			 [short (substring interface-name 0 (- len 3))])
		    (printf "@interface ~a~n" short)
		    (printf "@super ~a~n" short)
		    (printf "This interface was automatically generated in order to use mixins.~n")
		    (printf "For documentation, refer to the corresponding class: \\iscmclass{~a}.~n~n" short)))
		interface-names)))
  'truncate)

(call-with-output-file (build-path framework-dir "mred-interfaces.ss")
  (lambda (port)
    (pretty-print new-signature-definition port)
    (newline port)
    (pretty-print new-unit-definition port))
  'truncate)
