;; Mario Latendresse, July 2000
;;
;; The main function to call the Java compiler is java-compile-file.
;; Consult the description of that function.
;;

;; Define JAVACOMPHOME to point where the compiler was untared.
;; ( No need to worry about the trailing slash. )
(define JAVACOMPHOME "/home/latendre/java/")

(if (not (my-directory-exists? JAVACOMPHOME))
    (error "The variable JAVACOMPHOME points to a non-existing directory. Redefine it in file compiler.scm."))

;; Define the global variable CLASSPATH to point to the locations of
;; the java libraries and personal packages.

(define CLASSPATH  (++Path (++dir JAVACOMPHOME "comp/testSuite")
			   (++dir JAVACOMPHOME "comp/testPrgms")
			   (++dir JAVACOMPHOME "comp/Klasses")
			   (++dir JAVACOMPHOME "comp/jikestst/hpj/src")
			   (++dir JAVACOMPHOME "comp/benchmarks") 
			   ))

;; Compile a .java file into a .class file. Package name is added to
;; type names in the class file.
;;
;; I: dir, a directory name to access filename.
;;    filename, str. This is the filename with the extension `.java'.
;; O: void
;;    Write result in every class files for every type in filename.
;;
(define (java-compile-file dir fileName)
  (display-ln "Starting compilation of "fileName".")
  (set! *parsing-error* #f)
  (set! *semantic-error* #f)
  ;; Parse the source code and creates *global-table*.
  (let* ((ast (java-parse dir fileName)))
    ;; Do semantic analysis.
    (if (and ast (not *parsing-error*)) (java-ver-sem ast))
    (if (and ast (not *semantic-error*) (not *parsing-error*))
	;; Generates bytecode for all types.
	(for-each
	 (lambda (mt)
	   (let* ((type  (mt-type mt))
		  (classFileName (++dir-file dir (++ (qName->classfName (type-name type))
						     ".class")))
		  (value       (mt-value mt))
		  (pName       (get-pack-name *global-table*))
		  (pFileName   (++dir-file (qName->s pName) fileName))
		  ;; Generates bytecode for one type.
		  (className   (++dir-file (qName->s pName)
					   (qName->classfName (type-name type))))
		  (classJvm (type->jvm type value pFileName pName)))
	     ;; TBF should verify that fileName and class Name match.
	     (display-ln "Writing class file " classFileName)
	     ;; Write the class file.
	     (ecrire-classe classJvm "" classFileName)))

	 ;; keeps only the types from the source file.
	 (filter
	  (lambda (mt) 
	    (let* ((type (mt-type mt))) (and (type? type) (type-inside? type))))
	  (map cdr (table-names *global-table*)))
	 ))))

(define *parsing-error*  #f)
(define *semantic-error* #f)

(define (java-parse dir file)
  (if (not (file-exists? (++dir-file dir file)))
      (begin 
	(display-ln "Cannot read the file "(++dir-file dir file)".")
	#f)
      (let* ((my-port       (open-input-file (++dir-file dir file)))
	     (input-system  (lexer-make-IS 'port my-port 'all))
	     (get-line      (lexer-get-func-line input-system))
	     (get-column    (lexer-get-func-column input-system))
	     (lexer         (lexer-make-lexer  java-lex-table input-system))
	     (trace-lexer   (lambda () (let ((t (lexer)))  (display t)  t))))
	
	(set! java:source-filename file)
	(set! java:lexeme   (lambda (text) (make-lexeme text (get-line) (get-column))))
	(set! java:get-line get-line)
	(set! java:get-column get-column)
	(let ((parse_result (java-parser lexer (error_parse get-line))))
	  parse_result))))

(define java:lexeme          #f)
(define java:get-line        #f)
(define java:get-column      #f)
(define java:source-filename #f)

(define (error_parse get-line)
  (lambda (msg1 msg2 )
    (set! *parsing-error* #t)
    (display-ln msg1 msg2 " at line "  (get-line)))  )

(define (error-lexer yygetc yytext yycontinue)
  (let ((c (yygetc))
	)
    (display-ln "Invalid token, " (string-append yytext (string c)) 
		" at "java:source-filename":"(java:get-line)":"(java:get-column) 
		"; skipping.")
    (yycontinue)
    ))

(define (dump-class dir file)
  (txt-ecrire-class (lire-class dir file)))






