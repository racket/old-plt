;; Mario Latendresse, June 2000
;;
;; Translation from the class format to the internal AST format for
;; the Java compiler. The Section numbers refer to Tim Lindholm and Frank Yellin
;; books, ``The Java Virtual Machine''.
;;
;; The main function is class->ast.
;;
;; The class file has been read by another function found in readWriteClass.scm.
;; 
;; One class file contains only one type, either an interface or a class.
;;
;; When the class file is read, the type names are not verified. They
;; may not exist.  They are actually left as name in the AST structure
;; and have to be resolved later.
;; 


;; I: class, as returned by lire-class
;;    Gtable, (s Gtable)
;; O: (s type)
;;
(define (class->ast class Gtable)
  (let* ((magic          (list-ref class 0))
	 (minor-version  (list-ref class 1))
	 (major-version  (list-ref class 2))

	 ;; The cst-pool is a vector of pairs (tag cst)
	 ;; Tag is in [1,12].
	 ;; The cst can be a pair, an integer, or a string.
	 (cst-pool       (list-ref class 3))
	 (access-flags   (list-ref class 4))
	 (this-class     (list-ref class 5))
	 (super-class    (list-ref class 6))
	 (interfaces     (list-ref class 7))
	 (fields         (list-ref class 8))
	 (methods        (list-ref class 9))
	 (attributes     (list-ref class 10)))

    ;; TBF, we should use the following information to report badly constructed
    ;; class file.
    ;; (verify-magic magic)
    ;; (verify-version minor-version major-version)
    ;; The name of the class (interface) should be the same as the file name with
    ;; the path (qualified name).
    ;; (verify-class-name this-class)
    ;;    (txt-ecrire-fields fields)
    ;;
    (let* ((mdfs           (access-flags->mdfs access-flags 'class))
	   (class?         (not (access-flags-interface? access-flags)))
	   (extendName     (class-index->name cst-pool super-class))
	   (fullClassName  (class-index->name cst-pool this-class))
	   (interfacesName (map (lambda (index) (class-index->name cst-pool index)) 
				interfaces))
	   (table          (build-table cst-pool fields methods fullClassName Gtable)))
      (make-type class? fullClassName mdfs extendName interfacesName table #f))
    ))

;; Returns class name given its index in the constant pool.
;;
;; I: index, int
;; O: (u #f qName).
(define (class-index->name cst-pool index)
  (let* ((tag-index (vector-ref cst-pool index))
	 (tag       (and (pair? tag-index) (car tag-index))))
    ;; It is possible that there is no tag, that means there is no class.
    ;; This happens for Object where there is no superclass.
    (and tag
	 (if (not (= tag 7))
	     (error "CE, class-index->name, the tag is not a class tag " tag-index)
	     (name-index->name cst-pool (cadr tag-index)))
	 )))

;; I : index, int
;; O : QualifiedName
(define (name-index->name cst-pool index)
  (let* ((tag-str (vector-ref cst-pool index)))
    (if (not (string? (cadadr tag-str)))
	(error "CE, index->name, should be a string, found " (car tag-str)))
    (string->qualified-name (cadadr tag-str))
    ))

;; Returns a type table of a class file.
;;
;; I: cst-pool, (tag cst)
;;    fields,  (access-flags name-index descriptor-index nb-attributes attributes)
;;    methods, (access-flags name-index descriptor-index nb-attributes attributes)
;;
;;    fullClassName, (u str [str]). The full name of the class with the path.
;;    Gtable, (s Gtable)
;; O: (s Ttable)
;;
;; For fields, the only recognised attribute is ConstantValue For a
;; method, the only attribute recognised is `Exceptions'. The code
;; attribute is not used in building the type.
;;
;; The methods called `<clinit>' are not included in the list of
;; methods.
;;
(define (build-table cst-pool fields methods fullClassName Gtable)
  (let* ((methods-mt  (build-methods cst-pool methods fullClassName))
	 (fields-mt   (build-fields cst-pool fields fullClassName)))
    (make-Ttable (append methods-mt fields-mt) Gtable)))

(define (build-methods cst-pool methods fullClassName)
  ;; I: [(name-index att-length (tag info))]
  ;; O: [qualifiedName]
  ;;    The output is a list of qualified name refering to exception classes.
  ;;          
  (define (attributes->throws attributes)
    ;; Output : (u #f [qualifiedName])
    ;;          If it is #f that means the attribute att is not 
    ;;          an Exceptions attribute.
    (define (one-attribute att)
      (let*  ((name-index (car  att))
	      (att-length (cadr att))
	      (name       (name-index->name cst-pool name-index))
	      (tag-info   (caddr att))
	      (tag        (car tag-info))
	      (info       (cdr tag-info)))
	(if (eqv? tag 'exception)
	    (let* ((number-of-exceptions  (car info))
		   ;; [index]
		   ;; Each index should be a constant class in cst-pool
		   (exception-index-table (cadr info)))
	      (map
	       (lambda (index)
		 (name-index->name cst-pool (cadr (vector-ref cst-pool index))))
	       exception-index-table)
	      )
	    #f)))
    (let* ((exceptions (elem-all-eqv #f (map one-attribute attributes))))
      (if (pair? exceptions)
	  (begin
	    (if (< 1 (length exceptions))
		(error "There are several exceptions attributes in this class file "))
	    (car exceptions))
	  '())))

  (define (method->mt method)
    (let* ((access-flags      (list-ref method 0))
	   (mdfs              (access-flags->mdfs access-flags 'method))
	   (name-index        (list-ref method 1))
	   (name              (name-index->name cst-pool name-index))
	   (descriptor-index  (list-ref method 2))
	   (method-type       (index->type cst-pool descriptor-index))
	   (attributes        (list-ref method 4))
	   (throws            (attributes->throws attributes)))

      (set-tyM-throws!     method-type throws)
      (set-tyM-mdfs!       method-type mdfs)
      (set-tyM-mName!      method-type name)
      (set-tyM-classType!  method-type fullClassName)
      (: name (make-mt name method-type 'method #f #f mdfs #f))
      ))

  (map method->mt 
       (filter (lambda (method) 
		 (let* ((name-index (list-ref method 1))
			(name       (name-index->name cst-pool name-index)))
		   (not (string=? name "<clinit>"))))
	       methods)))

;; I : index, int
;; O : the type of the string descriptor 
(define (index->type cst-pool index-descr)
  (let* ((tag-cst (vector-ref cst-pool index-descr))
	 (tag     (car tag-cst))
	 (cst     (cadr tag-cst)))
    (if (or (not (string? (cadr cst))) (not (= tag 1)))
	(error "index->type, the descriptor must be of type utf8 (1) " tag cst))
    (descriptor->type (cadr cst))
    ))

;; Return the string describing the type.
(define (index->typeDescr cst-pool index-descr)
  (let* ((tag-cst (vector-ref cst-pool index-descr))
	 (tag     (car tag-cst))
	 (cst     (cadr tag-cst)))
    (if (or (not (string? (cadr cst))) (not (= tag 1)))
	(error "CE, index->typeDescr, the descriptor must be of type utf8 (1) " tag cst))
    (cadr cst)
    ))

;; The object returned is the type of the descriptor, which can be a
;; primitive type, a name or one of the structure tyM or
;; tyA.  The string descriptor grammars are described in 4.3.2
;; and 4.3.3.
;;
;; I: s, str
;; O: (u sym str (s tyM) (s tyA))
(define (descriptor->type  s)

  ;; I: d, [char]
  ;; O: (cons the-rest-of-d Type)
  (define (field-type d)
    (if (null? d) (error "field-type, premature end of a field type " s))
    (case (car d)
      ((#\L) ;; class type
       (let loop ((l (cdr d)) (name '()))
	 (if (not (pair? l))
	     (error "descriptor->type, a L field type does not end with a ;" s)
	     (if (char=? #\; (car l))
		 (if (null? name)
		     (error "descriptor->type, the type name after L is empty " s)
		     (: (cdr l) (string->qualified-name 
				 (list->string (reverse name)))))
		 (loop (cdr l) (: (car l) name)))
	     )))
      ((#\[) ;; array type
       (let loop ((l (cdr d)) (dim 1))
	 (if (not (pair? l))
	     (error "descriptor->type, a dimension is not followed by a type " s)
	     (if (char=? (car l) #\[)
		 (loop (cdr l) (+ dim 1))
		 (let* ((type (field-type l)))
		   (: (car type) (make-tyA dim (cdr type))))
		 ))))
      (else  ;; basic type
       (let* ((basic-type (:~. (car d) '((#\B . byte)	(#\C . char)
					(#\D . double)  (#\F . float)
					(#\I . int)     (#\J . long)
					(#\S . short)   (#\Z . boolean)))))
	 (if (not basic-type)
	     (error "CE, descriptor->type, basic type not recognised " (car d) " in " s))
	 (: (cdr d) basic-type)))))
 
  (let* ((d  (string->list s)))
    (if (null? d)
	(error "descriptor->type, the descriptor is empty " s))
    (if (char=? (car d) #\() ;; This is a method type
	(let loop ((d (cdr d)) (p '())) ;; Build the list of parameter type
	  (if (not (pair? d))
	      (error "descriptor->type, parameter list does not end properly " s)
	      (if (not (char=? (car d) #\)))
		  (let ((type-d (field-type d)))
		    (loop (car type-d) (: (cdr type-d) p)))
		  ;; This is the end of the list of parameter types
		  ;; There should be a result type.
		  (if (char=? (car (cdr d)) #\V) ;; Is it void?
		      (make-tyM #f '() 'void (reverse p) '() #f #f #f #f #f)
		      (make-tyM #f '() (cdr (field-type (cdr d))) 
				(reverse p) '() #f #f #f #f #f))
		  )))
	;; This is not a method type.
	(cdr (field-type d))
	)))

;; I : fields, [(access-flags name-index descr-index nb-attributes attributes)]
;;     fullClassName, (u str [str]). The full name of the class.
;; O : [(str . (s mt))]
(define (build-fields cst-pool fields fullClassName)

  (define (attributes->value atts)
    (define (one-attribute att)
	(let*  ((name-index (car  att))
		(att-length (cadr att))
		(tag        (and (pair? (caddr att)) (car (caddr att)))))
	  (if (eqv? tag 'constantvalue)
	      (cadr (vector-ref cst-pool (cadr (caddr att))))
	      #f)))
    (let* ((values (map one-attribute atts)))
      (if (= (length values) 0)
	  #f
	  (if (> (length values) 1)
	      (display-ln "Warning, class file "fullClassName" contains more than one constant value for a field: " values)
	      (car values)))))

  (define (field->mt field)
    (let* ((access-flags      (list-ref field 0))
	   (mdfs              (access-flags->mdfs access-flags 'field))
	   (name-index        (list-ref field 1))
	   (name  	      (name-index->name cst-pool name-index))
	   (descriptor-index  (list-ref field 2))
	   (type              (index->type cst-pool descriptor-index))
	   (typeDescr         (index->typeDescr cst-pool descriptor-index))
	   (attributes        (list-ref field 4))
	   (value             (attributes->value  attributes)))

      (: name (make-mt name type 'field value #f mdfs
		       (make-varAccess #f typeDescr fullclassName
				       name (if (member 'static mdfs) #t #f))))))
  (map field->mt fields))

;; I:  s, str
;; O:  (u str [str])
;;     Transforms "java/lang/Object" into ("java" "lang" "Object")
;;     Transforms "java" into "java"
;;
(define (string->qualified-name s)
  (let loop ((l (string->list s)) (name '()) (r '()))
    (if (pair? l)
	(if (char=? (car l) #\/)
	    (loop (cdr l) '() (: (list->string (reverse name)) r))
	    (loop (cdr l) (: (car l) name) r))
	(if (null? r)
	    (list->string (reverse name))
	    (reverse (: (list->string (reverse name)) r)))
	)))

;; O: #t iff if access-flags describe an interface.
(define (access-flags-interface? access-flags)
  ;; Bit 9 says if it is an interface
  (odd? (quotient access-flags 512)))

(define (access-flags->mdfs access-flags kind)
  (let* ((bit0  (odd? access-flags))
	 (bit1  (odd? (quotient access-flags 2)))
	 (bit2  (odd? (quotient access-flags 4)))
	 (bit3  (odd? (quotient access-flags 8)))
	 (bit4  (odd? (quotient access-flags 16)))
	 (bit5  (odd? (quotient access-flags 32)))
	 (bit6  (odd? (quotient access-flags 64)))
	 (bit7  (odd? (quotient access-flags 128)))
	 (bit8  (odd? (quotient access-flags 256)))
	 (bit10 (odd? (quotient access-flags 1024))))
    `(,@(if bit0 '(public) '())
      ,@(if bit1 '(private) '())
      ,@(if bit2 '(protected) '())
      ,@(if bit3 '(static) '())
      ,@(if bit4 '(final) '())
      ,@(if (eqv? kind 'method)
	    (if bit5 '(synchronized) '())
	    (if bit5 '(super) '()))
      ,@(if bit6 '(volatile) '())
      ,@(if bit7 '(transient) '())
      ,@(if bit8 '(native) '())
      ,@(if bit10 '(abstract) '())
    )))

;; Returns the first existing directory for which dir is a suffix
;; directory of the first directory of classPath.
;;
;; I : classPath, str
;;     dir, str
;; O : (u #f str) The string returned is a full path directory. It
;;     is the first for which a path exists by concatenating the dir
;;     with one of the classPath directories.
;;
(define (search-classpath-dir classPath dir)
  (let loop ((l (breaks-paths classPath)))
    (if (pair? l)
	(if (directory-exists? (++dir (car l) dir))
	    (++dir (car l) dir)
	    (loop (cdr l)))
	#f)))

;; The string returned is a complete file name with path directory.
;; It is the first existing file by concatenating the file-name with
;; one of the classPath directories.
;;
;; I : classPath, str. As it is defined in a CLASSPATH environment variable.
;;     file-name, str. This is a file name with extension `.class'. It may
;;                     contain a path.
;;     dir, (u #f str). An optional directory.
;; O : (u #f str)
;;
(define (search-classpath-file classPath dir file-name)
  (let loop ((l (breaks-paths classPath)))
    (if (pair? l)
	(let* ((full-file-name (++dir-file (++dir (car l) dir)	file-name)))
	  (trace "check " full-file-name)
	  (if (file-exists? full-file-name) 
	      (if (member 'read (file-or-directory-permissions full-file-name))
		  full-file-name
		  (loop (cdr l)))
	      (loop (cdr l))))
	#f)))

;; Break a classPath variable string into multiple directory
;; strings. The separator is `:'. Example: ".:/a/b:e/" returns ("."
;; "/a/b" "e/")
;;
;; I : s, str
;; O : [str]
(define (breaks-paths s) (string-breaks s #\:))

;; Add character "/" at end of string d if none is present.
(define (++dir1 d)
  (if (= (string-length d) 0)  ""
      (if (string=? "/" (string-suffix d 1)) d (++ d "/"))))

(define (++dir-file dir fname) (++ (++dir1 dir) fname))

;; Append d1 and d2 and insert a "/" character between them
;; if necessary and one at the end. There is always a "/" character
;; at the end of the returned string.
;;
(define (++dir d1 d2)  (++ (++dir1 d1) d2))

;; O: str. All paths are separated by :.
(define (++Path . l)  (++del l ":"))

