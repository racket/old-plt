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
(define original-names
  (foldl (lambda (n l)
	   (if (member n class-names)
	       (cons n l)
	       l))
	 null
	 signature-names))
(define leftover-names
  (foldl (lambda (n l)
	   (if (member n class-names)
	       l
	       (cons n l)))
	 null
	 signature-names))

(define (add-let-prefix sym)
  (string->symbol
   (string-append "$$"
		  (symbol->string sym))))

(define (build-interface class-name)
  `[,(add-let-prefix (class-name->interface-name class-name))
    (interface () ,@(class->names (global-defined-value class-name)))])

(define (add-export-prefix x)
  (string->symbol (string-append "-" (symbol->string x))))

(define (build-new-class class-name)
  (let* ([class (eval class-name)]
	 [interfaces (foldl (lambda (this-class-name so-far)
			      (if (subclass? class (eval this-class-name))
				  (cons (add-let-prefix (class-name->interface-name this-class-name)) so-far)
				  so-far))
			    null
			    class-names)])
    `[,(add-let-prefix class-name)
      (class* ,class-name (,@interfaces) args
	(sequence
	  (apply super-init args)))]))

(define new-signature-definition
  `(define-signature mred-interfaces^
     ((open mred^)
      (unit original : (,@original-names))
      ,@interface-names)))

(define (build-interface-definition name)
  `(define ,(class-name->interface-name name)
     ,(add-let-prefix (class-name->interface-name name))))

(define (build-new-class-definition name)
  `(define ,name ,(add-let-prefix name)))

(define new-unit-definition
  `(define mred-interfaces@
     (compound-unit/sig
      (import)
      (link [mred : mred^ (mred@)]
	    [interfaces
	     :
	     (,@(append interface-names class-names))
	     ((letrec ,(append
			(map build-interface class-names)
			(map build-new-class class-names))
		(unit/sig (,@(append interface-names class-names))
		  (import)
		  
		  ,@(append
		     (map build-interface-definition class-names)
		     (map build-new-class-definition class-names)))))])
      (export (open (mred : (,@leftover-names)))
	      (unit (mred : (,@original-names)) original)
	      (open interfaces)))))

(printf "added ~a interface definitions~nmred^ has ~a names, mred-interfaces^ has ~a names~n"
	(length interface-names)
	(length signature-names)
	(+ (length signature-names) (length interface-names)))

(define plt-home (or (getenv "PLTHOME")
		     (build-path framework-dir 'up 'up)))
(define cdb-file (build-path (collection-path "framework") "framework-mred-interfaces.cdb"))
(fprintf (current-error-port) "building cdb file: ~a~n" cdb-file)
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
  'text 'truncate)

(define (version-check filename)
  `(unless (equal? (version) ,(version))
     (error 'mred-interfaces
	    ,(format
	     "mred-interfaces.ss and mred-interfacess.ss compiled for version ~a, not version ~~a"
	     (version))
	    (version))))

(let ([fn (build-path framework-dir "mred-interfacess.ss")])
  (call-with-output-file fn
    (lambda (port)
      (pretty-print (version-check fn) port)
      (pretty-print new-signature-definition port)
      (newline port))
  'truncate))

(let ([fn (build-path framework-dir "mred-interfaces.ss")])
  (call-with-output-file fn
    (lambda (port)
      (pretty-print (version-check fn) port)
      (pretty-print '(require-library "mred-interfacess.ss" "framework") port)
      (newline port)
      (pretty-print new-unit-definition port))
    'truncate))
